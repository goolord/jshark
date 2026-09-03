{-# LANGUAGE OverloadedStrings #-}

-- | Webserver-agnostic WAI middleware and handlers for JShark hot reload
-- (@/__jshark/events@ SSE, @/__jshark/client.js@, optional HTML inject).
module JShark.HotReload.Wai
  ( hotReloadMiddleware
  , handleSseRequest
  , handleClientScript
  , clientRuntimeScript
  , injectHotReloadClient
  , injectScriptIntoHtml
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, bracket, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import JShark.HotReload.Client (clientRuntimeScript)
import JShark.HotReload.Core
  ( HotReloadConfig (..)
  , HotReloadEvent (..)
  , HotReloadHub
  , currentJsHashes
  , encodeEvent
  , lastBuildError
  , lastCompiling
  , subscribe
  )
import Network.HTTP.Types
  ( HeaderName
  , Status
  , hContentType
  , methodGet
  , status200
  )
import Network.Wai
  ( Application
  , Middleware
  , Request
  , Response
  , pathInfo
  , requestMethod
  , responseLBS
  , responseStream
  )
import Network.Wai.Internal (Response (..))

-- | Intercept @/__jshark/*@ and optionally inject the client script into
-- HTML responses from the underlying app.
hotReloadMiddleware :: HotReloadConfig -> HotReloadHub -> Middleware
hotReloadMiddleware cfg hub app req respond
  | not (hrEnabled cfg) = app req respond
  | requestMethod req == methodGet && matchPath cfgEvents req =
      handleSseRequest hub req respond
  | requestMethod req == methodGet && matchPath cfgClient req =
      handleClientScript respond
  | otherwise =
      app req $ \resp ->
        if hrAutoInject cfg
          then respond (injectHotReloadClient cfg resp)
          else respond resp
 where
  cfgEvents = pathSegments (hrEventsPath cfg)
  cfgClient = pathSegments (hrClientPath cfg)

matchPath :: [Text] -> Request -> Bool
matchPath segs req = pathInfo req == segs

pathSegments :: Text -> [Text]
pathSegments =
  filter (not . T.null) . T.splitOn "/" . T.dropWhile (== '/')

-- | Standalone SSE application (also used by the middleware).
handleSseRequest :: HotReloadHub -> Application
handleSseRequest hub _req respond = do
  next <- subscribe hub
  hashes <- currentJsHashes hub
  merr <- lastBuildError hub
  compiling <- lastCompiling hub
  alive <- newTVarIO True
  respond $
    responseStream status200 headers $ \write flush ->
      bracket
        (forkIO (keepaliveLoop alive write flush))
        (\_ -> atomically (writeTVar alive False))
        ( \_ -> do
            writeEvent write flush (Hello hashes)
            case merr of
              Just msg -> writeEvent write flush (BuildError msg)
              Nothing -> pure ()
            case compiling of
              Just app -> writeEvent write flush (BuildStart app)
              Nothing -> pure ()
            eventLoop alive write flush next
        )
 where
  headers =
    [ (hContentType, "text/event-stream; charset=utf-8")
    , ("Cache-Control", "no-cache")
    , ("Connection", "keep-alive")
    , ("X-Accel-Buffering", "no")
    ]

eventLoop ::
  TVar Bool
  -> (B.Builder -> IO ())
  -> IO ()
  -> IO HotReloadEvent
  -> IO ()
eventLoop alive write flush next = do
  still <- atomically (readTVar alive)
  if not still
    then pure ()
    else do
      ev <- next
      ok <- try (writeEvent write flush ev) :: IO (Either SomeException ())
      case ok of
        Left _ -> atomically (writeTVar alive False)
        Right () -> eventLoop alive write flush next

keepaliveLoop ::
  TVar Bool -> (B.Builder -> IO ()) -> IO () -> IO ()
keepaliveLoop alive write flush = go
 where
  go = do
    threadDelay (15 * 1000 * 1000)
    still <- atomically (readTVar alive)
    if not still
      then pure ()
      else do
        ok <-
          try (write (B.byteString ": keepalive\n\n") >> flush) ::
            IO (Either SomeException ())
        case ok of
          Left _ -> atomically (writeTVar alive False)
          Right () -> go

writeEvent ::
  (B.Builder -> IO ()) -> IO () -> HotReloadEvent -> IO ()
writeEvent write flush ev = do
  let
    payload = TE.encodeUtf8 (encodeEvent ev)
  write (B.byteString "data: ")
  write (B.byteString payload)
  write (B.byteString "\n\n")
  flush

handleClientScript :: (Response -> IO a) -> IO a
handleClientScript respond =
  respond $
    responseLBS
      status200
      [ (hContentType, "application/javascript; charset=utf-8")
      , ("Cache-Control", "no-store")
      ]
      (LBS.fromStrict clientRuntimeScript)

-- | Inject @<script src=...>@ into @<head>@ (no defer) so timer patches
-- install before the page's @app.js@.
injectHotReloadClient :: HotReloadConfig -> Response -> Response
injectHotReloadClient cfg resp =
  case resp of
    ResponseBuilder status headers builder ->
      rewrite status headers (B.toLazyByteString builder)
    ResponseRaw _ original -> injectHotReloadClient cfg original
    other ->
      -- Streaming / file responses: leave untouched (use Lucid helper).
      case drainToLBS other of
        Just (status, headers, body) -> rewrite status headers body
        Nothing -> other
 where
  rewrite status headers body
    | isHtml headers && not (alreadyInjected body) =
        responseLBS status headers (injectScriptIntoHtml (scriptTag cfg) body)
    | otherwise = resp

drainToLBS ::
  Response -> Maybe (Status, [(HeaderName, BS.ByteString)], LBS.ByteString)
drainToLBS resp =
  case resp of
    ResponseBuilder status headers builder ->
      Just (status, headers, B.toLazyByteString builder)
    _ ->
      -- Avoid blocking on streams/files in middleware.
      Nothing

isHtml :: [(HeaderName, BS.ByteString)] -> Bool
isHtml hdrs =
  case lookup hContentType hdrs of
    Just ct -> "text/html" `BS.isInfixOf` ct
    Nothing -> False

alreadyInjected :: LBS.ByteString -> Bool
alreadyInjected body =
  "/__jshark/client.js" `BS.isInfixOf` LBS.toStrict body

scriptTag :: HotReloadConfig -> LBS.ByteString
scriptTag cfg =
  LBS.fromStrict . TE.encodeUtf8 $
    "<script src=\""
      <> hrClientPath cfg
      <> "\"></script>"

-- | Pure HTML rewrite used by middleware and tests.
-- Prefer @</head>@ so the client runs before body @app.js@.
injectScriptIntoHtml :: LBS.ByteString -> LBS.ByteString -> LBS.ByteString
injectScriptIntoHtml tag body =
  case splitBefore "</head>" body of
    Just (before, after) -> before <> tag <> after
    Nothing ->
      case splitBefore "</body>" body of
        Just (before, after) -> before <> tag <> after
        Nothing -> body <> tag

splitBefore ::
  BS.ByteString -> LBS.ByteString -> Maybe (LBS.ByteString, LBS.ByteString)
splitBefore needle body =
  case breakSubstringCI needle body of
    (_, rest) | LBS.null rest -> Nothing
    pair -> Just pair

breakSubstringCI ::
  BS.ByteString -> LBS.ByteString -> (LBS.ByteString, LBS.ByteString)
breakSubstringCI needle body =
  let
    strict = LBS.toStrict body
    lower = BS8.map toLowerAscii strict
    n = BS8.map toLowerAscii needle
   in
    case BS8.breakSubstring n lower of
      (_, b)
        | BS.null b -> (body, LBS.empty)
      (pre, _) ->
        let
          (hi, lo) = BS.splitAt (BS.length pre) strict
         in
          (LBS.fromStrict hi, LBS.fromStrict lo)
 where
  toLowerAscii c
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c
