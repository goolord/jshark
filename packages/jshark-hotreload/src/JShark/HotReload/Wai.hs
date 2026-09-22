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
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAsciiUpper, toLower)
import Data.Either (isRight)
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import JShark.HotReload.Client (clientRuntimeScript)
import JShark.HotReload.Core
import Network.HTTP.Types (hContentType, methodGet, status200)
import Network.Wai
import Network.Wai.Internal (Response (..))
import System.Timeout (timeout)

-- | Intercept @/__jshark/*@ and optionally inject the client script into
-- HTML responses from the underlying app.
hotReloadMiddleware :: HotReloadConfig -> HotReloadHub -> Middleware
hotReloadMiddleware cfg hub app req respond
  | not (hrEnabled cfg) = app req respond
  | isGet (hrEventsPath cfg) = handleSseRequest hub req respond
  | isGet (hrClientPath cfg) = handleClientScript respond
  | hrAutoInject cfg = app req (respond . injectHotReloadClient cfg)
  | otherwise = app req respond
 where
  isGet path =
    requestMethod req == methodGet
      && pathInfo req == filter (not . T.null) (T.splitOn "/" path)

-- | Standalone SSE application (also used by the middleware).
handleSseRequest :: HotReloadHub -> Application
handleSseRequest hub _req respond = do
  -- Snapshot and subscription in one atomic transaction: every event
  -- published before this point is reflected in the snapshot, and every
  -- event after it is delivered on the stream.
  (snap, next) <- subscribeWithSnapshot hub
  alive <- newTVarIO True
  -- One writer: the keepalive worker and the event loop share the response's
  -- @write@/@flush@, so interleaving them would corrupt SSE frames.
  lock <- newMVar ()
  respond . responseStream status200 headers $ \write flush -> do
    let
      send ev = withMVar lock $ \_ -> do
        mapM_ (write . B.byteString) ev
        flush
      frame ev = ["data: ", TE.encodeUtf8 (encodeEvent ev), "\n\n"]
      -- A failed write means the client disconnected.
      trySend ev = isRight <$> (try (send ev) :: IO (Either SomeException ()))
      whileAlive step = do
        still <- readTVarIO alive
        when still $ do
          ok <- step
          if ok then whileAlive step else atomically (writeTVar alive False)
      keepalive = threadDelay 15000000 >> trySend [": keepalive\n\n"]
      -- Poll rather than block forever on the channel: when a client
      -- disconnects, the keepalive write fails and flips @alive@; without
      -- this wake-up the loop would hold the handler open until the next
      -- broadcast.
      relay = timeout 1000000 next >>= maybe (pure True) (trySend . frame)
    bracket
      (forkIO (whileAlive keepalive))
      (\tid -> atomically (writeTVar alive False) >> killThread tid)
      . const
      $ do
        mapM_ (send . frame) $
          Hello (snapshotJsHashes snap)
            : map BuildError (maybeToList (snapshotBuildError snap))
            ++ map BuildStart (maybeToList (snapshotCompiling snap))
        whileAlive relay
 where
  headers =
    [ (hContentType, "text/event-stream; charset=utf-8")
    , ("Cache-Control", "no-cache")
    , ("Connection", "keep-alive")
    , ("X-Accel-Buffering", "no")
    ]

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
-- install before the page's @app.js@. Only buffered, uncompressed HTML is
-- rewritten: raw responses (WebSocket upgrades), streams, and files pass
-- through untouched.
injectHotReloadClient :: HotReloadConfig -> Response -> Response
injectHotReloadClient cfg resp = case resp of
  ResponseBuilder status hdrs builder
    | isHtml hdrs
    , identity (lookup "Content-Encoding" hdrs)
    , not ("/__jshark/client.js" `BS.isInfixOf` LBS.toStrict body) ->
        -- The body grew, so its declared length and validators are stale.
        responseLBS
          status
          [ h
          | h@(n, _) <- hdrs
          , n `notElem` ["Content-Length", "ETag", "Content-MD5", "Digest"]
          ]
          (injectScriptIntoHtml tag body)
   where
    body = B.toLazyByteString builder
  _ -> resp
 where
  isHtml = maybe False ("text/html" `BS.isInfixOf`) . lookup hContentType
  identity = maybe True (\ce -> BS.null ce || ce == "identity")
  tag =
    LBS.fromStrict . TE.encodeUtf8 $
      "<script src=\"" <> hrClientPath cfg <> "\"></script>"

-- | Pure HTML rewrite used by middleware and tests: insert @tag@ before
-- @</head>@ (so the client runs before body @app.js@), else before
-- @</body>@, else at the end. Tag matching ignores ASCII case.
injectScriptIntoHtml :: LBS.ByteString -> LBS.ByteString -> LBS.ByteString
injectScriptIntoHtml tag body =
  case mapMaybe offset ["</head>", "</body>"] of
    i : _ -> let (pre, post) = LBS.splitAt i body in pre <> tag <> post
    [] -> body <> tag
 where
  lower = BS8.map (\c -> if isAsciiUpper c then toLower c else c) strict
  strict = LBS.toStrict body
  offset needle = case BS.breakSubstring needle lower of
    (pre, rest) | not (BS.null rest) -> Just (fromIntegral (BS.length pre))
    _ -> Nothing
