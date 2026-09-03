{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import JShark (escapeJsString)
import JShark.Bun
  ( HappyDomOptions (..)
  , defaultHappyDomOptions
  , domTimeoutMicroseconds
  )
import JShark.Bun.Internal (JSProgram (..), runProgram)
import JShark.HotReload.Client (clientRuntimeText)
import JShark.HotReload.Core
  ( HotReloadConfig (..)
  , HotReloadEvent (..)
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
import JShark.HotReload.Watcher
  ( WatchTargets (..)
  , defaultWatchTargets
  , exampleAppForHs
  , exampleAppsForHs
  , isLucidShellPath
  , startWatcher
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
import System.Directory
  ( createDirectoryIfMissing
  , findExecutable
  , getTemporaryDirectory
  , removePathForcibly
  , renameFile
  )
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

main :: IO ()
main = defaultMain hotReloadTests

hotReloadTests :: TestTree
hotReloadTests =
  testGroup
    "hot-reload"
    [ testCase "encodeEvent css/js/page/error shapes" encodeShapes
    , testCase "GET /__jshark/client.js is javascript" clientJsOk
    , testCase "middleware serves client.js" middlewareClientOk
    , testCase "SSE response is event-stream" eventsHeaderOk
    , testCase "broadcast reaches subscribers" sseBroadcastOk
    , testCase "HTML inject inserts client script before </head>" injectOk
    , testCase "middleware auto-injects client into HTML" middlewareInjectOk
    , testCase "exampleAppForHs maps Client.hs paths" exampleAppMapOk
    , testCase "startWatcher sees a second same-size save" watcherSecondSaveOk
    , testCase "startWatcher sees an atomic-rename save" watcherRenameSaveOk
    , testCase
        "disposeTracked keeps EventSource and shell listeners"
        hmrDisposeKeepsSse
    , testCase
        "disposeTracked stops setTimeout chains after module eval"
        hmrTimeoutChainDies
    ]

encodeShapes :: IO ()
encodeShapes = do
  assertBool "css" $
    "\"type\":\"css-update\""
      `T.isInfixOf` encodeEvent (CssUpdate "/static/x.css" 1)
  assertBool "js" $
    "\"type\":\"js-update\""
      `T.isInfixOf` encodeEvent (JsUpdate "todo-mvc" "/todo-mvc/app.js" "1-deadbeef")
  assertBool "page" $
    "\"type\":\"page-reload\"" `T.isInfixOf` encodeEvent (PageReload "Page.hs")
  assertBool "err" $
    "\"type\":\"build-error\"" `T.isInfixOf` encodeEvent (BuildError "boom")
  assertBool "hello" $
    "\"type\":\"hello\"" `T.isInfixOf` encodeEvent (Hello [("todo-mvc", "1-ab")])
  assertBool "start" $
    "\"type\":\"build-start\""
      `T.isInfixOf` encodeEvent (BuildStart "breakout")

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
  assertBool "debug panel" ("__jshark-hr-panel" `BS8.isInfixOf` LBS.toStrict body)
  assertBool "build-start" ("build-start" `BS8.isInfixOf` LBS.toStrict body)
  assertBool "blob script" ("createObjectURL" `BS8.isInfixOf` LBS.toStrict body)
  assertBool "module tracking" ("tracking" `BS8.isInfixOf` LBS.toStrict body)
  assertBool "host untracked" ("untracked" `BS8.isInfixOf` LBS.toStrict body)
  assertBool
    "keeps EventSource"
    ("shouldTrackTarget" `BS8.isInfixOf` LBS.toStrict body)

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
    html =
      "<html><head><title>x</title></head><body><script src=\"/b/app.js\"></script></body></html>"
    tag = "<script src=\"/__jshark/client.js\"></script>"
    out = injectScriptIntoHtml tag html
  assertEqual
    "before head close, ahead of app.js"
    ( "<html><head><title>x</title>"
        <> "<script src=\"/__jshark/client.js\"></script>"
        <> "</head><body><script src=\"/b/app.js\"></script></body></html>"
    )
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
  assertBool "no defer" (not ("defer" `BS8.isInfixOf` strict))
  assertBool "still has body close" ("</body>" `BS8.isInfixOf` strict)

exampleAppMapOk :: IO ()
exampleAppMapOk = do
  assertEqual
    "todo"
    (Just "todo-mvc")
    (exampleAppForHs "examples/src/JShark/Example/TodoMvc/Client.hs")
  assertEqual
    "breakout"
    (Just "breakout")
    (exampleAppForHs "examples\\src\\JShark\\Example\\Breakout\\Types.hs")
  assertEqual
    "page maps"
    (Just "todo-mvc")
    (exampleAppForHs "examples/src/JShark/Example/TodoMvc/Page.hs")
  assertEqual
    "server skip"
    Nothing
    (exampleAppForHs "examples/app/server/DevServer.hs")
  assertEqual
    "theme all"
    ["breakout", "todo-mvc", "synth", "life", "hvm2-demo"]
    (exampleAppsForHs "examples/src/JShark/Example/Theme.hs")
  assertBool
    "lucid shell"
    (isLucidShellPath "examples/src/JShark/Example/Breakout/Page.hs")
  assertBool
    "not lucid"
    (not (isLucidShellPath "examples/src/JShark/Example/Breakout/Client.hs"))

-- | Same-length overwrite must still enqueue a second Haskell recompile.
watcherSecondSaveOk :: IO ()
watcherSecondSaveOk = do
  tmp <- getTemporaryDirectory
  let
    dir = tmp </> "jshark-hr-watch-second-save"
    hsDir = dir </> "Life"
    hs = hsDir </> "Client.hs"
  bracket (setup dir hsDir) (\_ -> removePathForcibly dir) $ \_ -> do
    hits <- newIORef (0 :: Int)
    hub <-
      newHotReloadHub defaultHotReloadConfig {hrDebounceMs = 50}
    let
      targets =
        (defaultWatchTargets [dir])
          { onHaskellSource =
              \_ -> atomicModifyIORef' hits $ \n -> (n + 1, ())
          }
    stop <- startWatcher hub targets
    -- Let fsnotify register the watch before the first write.
    threadDelay 250000
    writeFile hs "module Client where\n-- a\n"
    n1 <- waitHits hits 1
    writeFile hs "module Client where\n-- b\n"
    n2 <- waitHits hits 2
    stop
    assertBool ("first save seen, got " <> show n1) (n1 >= 1)
    assertBool ("second save seen, got " <> show n2) (n2 >= 2)
 where
  setup dir hsDir = do
    removePathForcibly dir
    createDirectoryIfMissing True hsDir
  waitHits ref want = go (0 :: Int)
   where
    go waited
      | waited >= 4000000 = readIORef ref
      | otherwise = do
          n <- readIORef ref
          if n >= want
            then pure n
            else threadDelay 50000 >> go (waited + 50000)

-- | An editor-style write-temp-then-rename save (over an existing file)
-- must still reach the hook: GHC 'renameFile' is MoveFileEx REPLACE_EXISTING.
watcherRenameSaveOk :: IO ()
watcherRenameSaveOk = do
  tmp <- getTemporaryDirectory
  let
    dir = tmp </> "jshark-hr-watch-rename"
    hs = dir </> "Life" </> "Client.hs"
    tmpHs = dir </> "Life" </> "Client.hs.tmp"
  bracket (setup dir) (\_ -> removePathForcibly dir) $ \_ -> do
    hits <- newIORef (0 :: Int)
    hub <-
      newHotReloadHub defaultHotReloadConfig {hrDebounceMs = 50}
    let
      targets =
        (defaultWatchTargets [dir])
          { onHaskellSource =
              \_ -> atomicModifyIORef' hits $ \n -> (n + 1, ())
          }
    stop <- startWatcher hub targets
    threadDelay 250000
    writeFile hs "module Client where\n-- a\n"
    n1 <- waitRename hits 1
    writeFile tmpHs "module Client where\n-- b\n"
    renameFile tmpHs hs
    n2 <- waitRename hits 2
    stop
    assertBool ("first save seen, got " <> show n1) (n1 >= 1)
    assertBool ("rename save seen, got " <> show n2) (n2 >= 2)
 where
  setup d = do
    removePathForcibly d
    createDirectoryIfMissing True (d </> "Life")
  waitRename ref want = go (0 :: Int)
   where
    go waited
      | waited >= 8000000 = readIORef ref
      | otherwise = do
          n <- readIORef ref
          if n >= want
            then pure n
            else threadDelay 50000 >> go (waited + 50000)

jsLit :: String -> String
jsLit s = '"' : escapeJsString s ++ "\""

happyDomHmrProgram :: String -> JSProgram
happyDomHmrProgram expr =
  JSProgram
    { jsFlags = ["--install=fallback"]
    , jsPrelude =
        unlines
          [ "import { GlobalRegistrator } from \"@happy-dom/global-registrator\";"
          , "GlobalRegistrator.register({ url: "
              ++ jsLit (T.unpack (happyDomUrl defaultHappyDomOptions))
              ++ " });"
          ]
    , jsExpression = expr
    , jsEpilogue = "await GlobalRegistrator.unregister();"
    }

hmrDisposeKeepsSseExpr :: String
hmrDisposeKeepsSseExpr =
  unlines
    [ "(async () => {"
    , "  class FakeES extends EventTarget {"
    , "    constructor(u) { super(); this.url = u; this.readyState = 1; }"
    , "    close() { this.readyState = 2; }"
    , "  }"
    , "  globalThis.EventSource = FakeES;"
    , "  window.EventSource = FakeES;"
    , "  (0, eval)(" ++ jsLit (T.unpack clientRuntimeText) ++ ");"
    , "  if (!document.getElementById('__jshark-hr-panel')) throw new Error('missing panel');"
    , "  const api = window.__JSHARK_HR_API__;"
    , "  if (!api) throw new Error('missing __JSHARK_HR_API__');"
    , "  const es = api.eventSource();"
    , "  if (!es) throw new Error('missing EventSource');"
    , "  if (typeof es.onmessage !== 'function') throw new Error('onmessage missing');"
    , "  let sseHits = 0;"
    , "  es.addEventListener('message', function () { sseHits += 1; });"
    , "  let shellHits = 0;"
    , "  api.untracked(function () {"
    , "    window.addEventListener('click', function () { shellHits += 1; });"
    , "  });"
    , "  let appHits = 0;"
    , "  let boom = false;"
    , "  api.withModule(function () {"
    , "    window.addEventListener('click', function () { appHits += 1; });"
    , "    es.addEventListener('message', function () { sseHits += 10; });"
    , "    window.setTimeout(function () { boom = true; }, 0);"
    , "  });"
    , "  api.disposeTracked();"
    , "  if (typeof es.onmessage !== 'function') throw new Error('onmessage gone');"
    , "  await new Promise(function (r) { setTimeout(r, 30); });"
    , "  if (boom) throw new Error('tracked timeout survived');"
    , "  window.dispatchEvent(new Event('click'));"
    , "  if (shellHits !== 1) throw new Error('shell listener died: ' + shellHits);"
    , "  if (appHits !== 0) throw new Error('app listener survived: ' + appHits);"
    , "  es.dispatchEvent(new MessageEvent('message', { data: '{\"type\":\"css-update\",\"url\":\"/x.css\",\"timestamp\":1}' }));"
    , "  if (sseHits < 1) throw new Error('EventSource listener died: ' + sseHits);"
    , "  es.onmessage({ data: '{\"type\":\"css-update\",\"url\":\"/x.css\",\"timestamp\":1}' });"
    , "  return 'ok';"
    , "})()"
    ]

hmrDisposeKeepsSse :: IO ()
hmrDisposeKeepsSse = do
  m <- findExecutable "bun"
  case m of
    Nothing ->
      assertFailure "bun not found on PATH; install https://bun.sh"
    Just _ -> do
      got <-
        T.unpack
          <$> runProgram
            domTimeoutMicroseconds
            (happyDomHmrProgram hmrDisposeKeepsSseExpr)
      assertEqual "disposeTracked keeps SSE + shell listeners" "\"ok\"" got

hmrTimeoutChainExpr :: String
hmrTimeoutChainExpr =
  unlines
    [ "(async () => {"
    , "  class FakeES extends EventTarget {"
    , "    constructor(u) { super(); this.url = u; this.readyState = 1; }"
    , "    close() { this.readyState = 2; }"
    , "  }"
    , "  globalThis.EventSource = FakeES;"
    , "  window.EventSource = FakeES;"
    , "  (0, eval)(" ++ jsLit (T.unpack clientRuntimeText) ++ ");"
    , "  const api = window.__JSHARK_HR_API__;"
    , "  if (!api) throw new Error('missing __JSHARK_HR_API__');"
    , "  let hops = 0;"
    , "  api.withModule(function () {"
    , "    function loop() {"
    , "      hops += 1;"
    , "      if (hops < 80) window.setTimeout(loop, 0);"
    , "    }"
    , "    window.setTimeout(loop, 0);"
    , "  });"
    , "  await new Promise(function (r) { setTimeout(r, 25); });"
    , "  if (hops < 2) throw new Error('chain never ran: ' + hops);"
    , "  const frozen = hops;"
    , "  api.disposeTracked();"
    , "  await new Promise(function (r) { setTimeout(r, 40); });"
    , "  if (hops !== frozen) throw new Error('timeout chain survived: ' + hops + ' vs ' + frozen);"
    , "  return 'ok';"
    , "})()"
    ]

hmrTimeoutChainDies :: IO ()
hmrTimeoutChainDies = do
  m <- findExecutable "bun"
  case m of
    Nothing ->
      assertFailure "bun not found on PATH; install https://bun.sh"
    Just _ -> do
      got <-
        T.unpack
          <$> runProgram
            domTimeoutMicroseconds
            (happyDomHmrProgram hmrTimeoutChainExpr)
      assertEqual "disposeTracked stops timeout chain" "\"ok\"" got

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
