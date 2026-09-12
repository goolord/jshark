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

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
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
  , HotReloadSnapshot (..)
  , encodeEvent
  , subscribeWithSnapshot
  )
import Network.HTTP.Types
  ( HeaderName
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
import System.Timeout (timeout)

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
  -- Snapshot and subscription in one atomic transaction: every event
  -- published before this point is reflected in the snapshot, and every
  -- event after it is delivered on the stream. Taking the snapshot and
  -- then subscribing could otherwise lose an update broadcast in between.
  (snap, next) <- subscribeWithSnapshot hub
  alive <- newTVarIO True
  -- One writer: the keepalive worker and the event loop share the response's
  -- @write@/@flush@, so interleaving them would corrupt SSE frames.
  writeLock <- newMVar ()
  respond $
    responseStream status200 headers $ \write flush ->
      bracket
        (forkIO (keepaliveLoop writeLock alive write flush))
        (\tid -> atomically (writeTVar alive False) >> killThread tid)
        ( \_ -> do
            writeEvent writeLock write flush (Hello (snapshotJsHashes snap))
            case snapshotBuildError snap of
              Just msg -> writeEvent writeLock write flush (BuildError msg)
              Nothing -> pure ()
            case snapshotCompiling snap of
              Just app -> writeEvent writeLock write flush (BuildStart app)
              Nothing -> pure ()
            eventLoop writeLock alive write flush next
        )
 where
  headers =
    [ (hContentType, "text/event-stream; charset=utf-8")
    , ("Cache-Control", "no-cache")
    , ("Connection", "keep-alive")
    , ("X-Accel-Buffering", "no")
    ]

eventLoop ::
  MVar ()
  -> TVar Bool
  -> (B.Builder -> IO ())
  -> IO ()
  -> IO HotReloadEvent
  -> IO ()
eventLoop lock alive write flush next = do
  still <- atomically (readTVar alive)
  if not still
    then pure ()
    else do
      -- Poll rather than block forever on the channel: when a client
      -- disconnects, the keepalive write fails and flips @alive@; without
      -- this wake-up the event loop would hold the handler open until the
      -- next broadcast.
      m <- timeout ssePollMicroseconds next
      case m of
        Nothing -> eventLoop lock alive write flush next
        Just ev -> do
          ok <- try (writeEvent lock write flush ev) :: IO (Either SomeException ())
          case ok of
            Left _ -> atomically (writeTVar alive False)
            Right () -> eventLoop lock alive write flush next

-- | How often the SSE event loop wakes to notice a disconnected client.
ssePollMicroseconds :: Int
ssePollMicroseconds = 1000 * 1000

keepaliveLoop ::
  MVar () -> TVar Bool -> (B.Builder -> IO ()) -> IO () -> IO ()
keepaliveLoop lock alive write flush = go
 where
  go = do
    threadDelay (15 * 1000 * 1000)
    still <- atomically (readTVar alive)
    if not still
      then pure ()
      else do
        ok <-
          try (withMVar lock (\_ -> write (B.byteString ": keepalive\n\n") >> flush)) ::
            IO (Either SomeException ())
        case ok of
          Left _ -> atomically (writeTVar alive False)
          Right () -> go

writeEvent ::
  MVar ()
  -> (B.Builder -> IO ())
  -> IO ()
  -> HotReloadEvent
  -> IO ()
writeEvent lock write flush ev =
  withMVar lock $ \_ -> do
    let
      payload = TE.encodeUtf8 (encodeEvent ev)
    write (B.byteString "data: ")
    write (B.byteString payload)
    write (B.byteString "\n\n")
    flush

-- | Serve the embedded browser runtime at @/__jshark/client.js@.
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
    -- Raw responses (WebSocket upgrades and other opaque streams) must go
    -- to the server untouched; rewriting the fallback would corrupt them.
    ResponseRaw {} -> resp
    -- Streaming / file responses: leave untouched (use Lucid helper).
    _ -> resp
 where
  rewrite status headers body
    | isRewritableHtml headers && not (alreadyInjected body) =
        responseLBS
          status
          (dropStaleHeaders headers)
          (injectScriptIntoHtml (scriptTag cfg) body)
    | otherwise = resp

-- | Inject only into uncompressed HTML: a compressed or otherwise encoded
-- body is not text we can edit.
isRewritableHtml :: [(HeaderName, BS.ByteString)] -> Bool
isRewritableHtml hdrs = isHtml hdrs && identityEncoded hdrs

identityEncoded :: [(HeaderName, BS.ByteString)] -> Bool
identityEncoded hdrs =
  case lookup "Content-Encoding" hdrs of
    Nothing -> True
    Just ce -> BS.null ce || ce == "identity"

-- | The body grew (or was otherwise edited), so declared length and any
-- content validators that describe the old bytes are now wrong.
dropStaleHeaders ::
  [(HeaderName, BS.ByteString)] -> [(HeaderName, BS.ByteString)]
dropStaleHeaders =
  filter
    ( \(name, _) ->
        name
          `notElem` ["Content-Length", "ETag", "Content-MD5", "Digest"]
    )

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
