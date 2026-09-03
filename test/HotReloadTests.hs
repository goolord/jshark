{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HotReloadTests (hotReloadTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import JShark.HotReload.Core
  ( HotReloadEvent (..)
  , broadcastEvent
  , defaultHotReloadConfig
  , encodeEvent
  , newHotReloadHub
  , registerJs
  , subscribe
  )
import JShark.HotReload.Wai
  ( handleClientScript
  , hotReloadMiddleware
  , injectScriptIntoHtml
  )
import Network.HTTP.Types (HeaderName, statusCode)
import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (status200)
import Network.Wai
  ( Application
  , Response
  , defaultRequest
  , pathInfo
  , requestMethod
  , responseLBS
  , responseStatus
  )
import Network.Wai.Internal (Response (..), ResponseReceived (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

hotReloadTests :: TestTree
hotReloadTests =
  testGroup
    "hot-reload"
    [ testCase "encodeEvent css/js/page/error shapes" encodeShapes
    , testCase "GET /__jshark/client.js is javascript" clientJsOk
    , testCase "middleware serves client.js" middlewareClientOk
    , testCase "SSE response is event-stream" eventsHeaderOk
    , testCase "broadcast reaches subscribers" sseBroadcastOk
    , testCase "HTML inject inserts client script before </body>" injectOk
    , testCase "middleware auto-injects client into HTML" middlewareInjectOk
    ]

encodeShapes :: IO ()
encodeShapes = do
  assertBool "css" $
    "\"type\":\"css-update\"" `T.isInfixOf` encodeEvent (CssUpdate "/static/x.css" 1)
  assertBool "js" $
    "\"type\":\"js-update\""
      `T.isInfixOf` encodeEvent (JsUpdate "todo-mvc" "/todo-mvc/app.js" "1-deadbeef")
  assertBool "page" $
    "\"type\":\"page-reload\"" `T.isInfixOf` encodeEvent (PageReload "Page.hs")
  assertBool "err" $
    "\"type\":\"build-error\"" `T.isInfixOf` encodeEvent (BuildError "boom")
  assertBool "hello" $
    "\"type\":\"hello\"" `T.isInfixOf` encodeEvent (Hello [("todo-mvc", "1-ab")])

baseApp :: Application
baseApp _ respond =
  respond $
    responseLBS
      status200
      [("Content-Type", "text/html; charset=utf-8")]
      "<html><body><h1>hi</h1></body></html>"

clientJsOk :: IO ()
clientJsOk = do
  (code, hdrs, body) <-
    captureResponse $ \respond -> handleClientScript respond
  assertEqual "status" 200 code
  assertBool "content-type" ("javascript" `BS.isInfixOf` contentType hdrs)
  assertBool "body" ("EventSource" `BS8.isInfixOf` LBS.toStrict body)

middlewareClientOk :: IO ()
middlewareClientOk = do
  hub <- newHotReloadHub defaultHotReloadConfig
  _ <- registerJs hub "todo-mvc" "console.log(1)"
  let
    app = hotReloadMiddleware defaultHotReloadConfig hub baseApp
    req =
      defaultRequest
        { requestMethod = methodGet
        , pathInfo = ["__jshark", "client.js"]
        }
  (code, hdrs, body) <-
    captureResponse $ \respond -> app req respond
  assertEqual "status" 200 code
  assertBool "content-type" ("javascript" `BS.isInfixOf` contentType hdrs)
  assertBool "runtime" ("__jshark/events" `BS8.isInfixOf` LBS.toStrict body)

eventsHeaderOk :: IO ()
eventsHeaderOk = do
  hub <- newHotReloadHub defaultHotReloadConfig
  let
    app = hotReloadMiddleware defaultHotReloadConfig hub baseApp
    req =
      defaultRequest
        { requestMethod = methodGet
        , pathInfo = ["__jshark", "events"]
        }
  (code, hdrs, _) <-
    captureResponseHeaders $ \respond -> app req respond
  assertEqual "status" 200 code
  assertBool "event-stream" ("text/event-stream" `BS.isInfixOf` contentType hdrs)

sseBroadcastOk :: IO ()
sseBroadcastOk = do
  hub <- newHotReloadHub defaultHotReloadConfig
  next <- subscribe hub
  broadcastEvent hub (CssUpdate "/static/todo-mvc.css" 42)
  ev <- next
  assertEqual "event" (CssUpdate "/static/todo-mvc.css" 42) ev
  assertBool "json" ("css-update" `T.isInfixOf` encodeEvent ev)

injectOk :: IO ()
injectOk = do
  let
    html = "<html><body><p>x</p></body></html>"
    tag = "<script src=\"/__jshark/client.js\" defer></script>"
    out = injectScriptIntoHtml tag html
  assertEqual
    "before body close"
    "<html><body><p>x</p><script src=\"/__jshark/client.js\" defer></script></body></html>"
    out

middlewareInjectOk :: IO ()
middlewareInjectOk = do
  hub <- newHotReloadHub defaultHotReloadConfig
  let
    app = hotReloadMiddleware defaultHotReloadConfig hub baseApp
    req =
      defaultRequest
        { requestMethod = methodGet
        , pathInfo = []
        }
  (_, _, body) <- captureResponse $ \respond -> app req respond
  let
    strict = LBS.toStrict body
  assertBool "client script" ("/__jshark/client.js" `BS8.isInfixOf` strict)
  assertBool "still has body close" ("</body>" `BS8.isInfixOf` strict)

contentType :: [(HeaderName, BS.ByteString)] -> BS.ByteString
contentType = fromMaybe "" . lookup "Content-Type"

captureResponse ::
  ((Response -> IO ResponseReceived) -> IO ResponseReceived)
  -> IO (Int, [(HeaderName, BS.ByteString)], LBS.ByteString)
captureResponse run = do
  ref <- newIORef Nothing
  _ <-
    run $ \resp -> do
      body <- responseBodyLBS resp
      writeIORef ref (Just (statusCodeOf resp, responseHeadersOf resp, body))
      pure ResponseReceived
  readCaptured ref

captureResponseHeaders ::
  ((Response -> IO ResponseReceived) -> IO ResponseReceived)
  -> IO (Int, [(HeaderName, BS.ByteString)], ())
captureResponseHeaders run = do
  ref <- newIORef Nothing
  _ <-
    run $ \resp -> do
      writeIORef ref (Just (statusCodeOf resp, responseHeadersOf resp, ()))
      pure ResponseReceived
  readCaptured ref

readCaptured :: IORef (Maybe a) -> IO a
readCaptured ref = do
  m <- readIORef ref
  case m of
    Just a -> pure a
    Nothing -> fail "hot-reload test: no response captured"

statusCodeOf :: Response -> Int
statusCodeOf = statusCode . responseStatus

responseHeadersOf :: Response -> [(HeaderName, BS.ByteString)]
responseHeadersOf = \case
  ResponseBuilder _ hs _ -> hs
  ResponseFile _ hs _ _ -> hs
  ResponseStream _ hs _ -> hs
  ResponseRaw _ inner -> responseHeadersOf inner

responseBodyLBS :: Response -> IO LBS.ByteString
responseBodyLBS = \case
  ResponseBuilder _ _ b -> pure (B.toLazyByteString b)
  ResponseFile {} -> pure ""
  ResponseStream {} -> pure ""
  ResponseRaw _ inner -> responseBodyLBS inner
