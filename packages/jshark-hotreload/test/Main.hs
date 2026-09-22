{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isNothing)
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
import JShark.HotReload.Wai
import JShark.HotReload.Watcher (WatchTargets (..), startWatcher)
import Network.HTTP.Types (ResponseHeaders, status200, statusCode)
import Network.Wai
import Network.Wai.Internal (Response (..), ResponseReceived (..))
import System.Directory
  ( createDirectoryIfMissing
  , findExecutable
  , getTemporaryDirectory
  , removePathForcibly
  , renameFile
  )
import System.FilePath ((</>))
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain . testGroup "hot-reload" $
    [ testCase "encodeEvent css/js/page/error shapes" encodeShapes
    , testCase "GET /__jshark/client.js is javascript" clientJsOk
    , testCase "middleware serves client.js" middlewareClientOk
    , testCase "SSE response is event-stream" eventsHeaderOk
    , testCase "broadcast reaches subscribers" sseBroadcastOk
    , testCase "snapshot and subscription are coherent" sseSnapshotCoherent
    , testCase
        "publication revisions are monotonic and snapshotted"
        sseRevisionOk
    , testCase "two applications reload independently" sseTwoAppsOk
    , testCase "reconnect after disconnect sees updates" sseReconnectOk
    , testCase "HTML inject inserts client script before </head>" injectOk
    , testCase "middleware auto-injects client into HTML" middlewareInjectOk
    , testCase "raw responses are never rewritten" rawResponsePreserved
    , testCase "inject drops a stale Content-Length" injectDropsLength
    , testCase "inject skips an encoded body" injectSkipsCompressed
    , testCase "startWatcher sees a second same-size save" watcherSecondSaveOk
    , testCase "startWatcher sees an atomic-rename save" watcherRenameSaveOk
    , testCase "startWatcher can be restarted repeatedly" watcherRestartOk
    , hmrCase
        "disposeTracked keeps EventSource and shell listeners"
        hmrDisposeKeepsSse
    , hmrCase
        "disposeTracked stops setTimeout chains after module eval"
        hmrTimeoutChainDies
    ]

encodeShapes :: Assertion
encodeShapes =
  forM_
    [ ("css-update", CssUpdate "/static/x.css" 1)
    , ("js-update", JsUpdate "todo-mvc" "/todo-mvc/app.js" "1-deadbeef")
    , ("page-reload", PageReload "Page.hs")
    , ("build-error", BuildError "boom")
    , ("hello", Hello [("todo-mvc", "1-ab")])
    , ("build-start", BuildStart "breakout")
    ]
    $ \(ty, ev) ->
      assertBool (T.unpack ty) $
        ("\"type\":\"" <> ty <> "\"") `T.isInfixOf` encodeEvent ev

baseApp :: Application
baseApp _ respond =
  respond $
    responseLBS
      status200
      [("Content-Type", "text/html; charset=utf-8")]
      "<html><body><h1>hi</h1></body></html>"

-- | The middleware over 'baseApp' with a fresh hub.
middlewareApp :: IO Application
middlewareApp =
  (\hub -> hotReloadMiddleware defaultHotReloadConfig hub baseApp)
    <$> newHotReloadHub defaultHotReloadConfig

clientJsOk :: Assertion
clientJsOk = do
  (code, hdrs, body) <- run (const handleClientScript) []
  code @?= 200
  assertBool "content-type" ("javascript" `BS.isInfixOf` contentType hdrs)
  forM_
    [ ("body", "EventSource")
    , ("debug panel", "__jshark-hr-panel")
    , ("build-start", "build-start")
    , ("blob script", "createObjectURL")
    , ("module tracking", "tracking")
    , ("host untracked", "untracked")
    , ("keeps EventSource", "shouldTrackTarget")
    ]
    $ \(what, needle) -> assertBool what (needle `BS.isInfixOf` body)

middlewareClientOk :: Assertion
middlewareClientOk = do
  app <- middlewareApp
  (code, hdrs, body) <- run app ["__jshark", "client.js"]
  code @?= 200
  assertBool "content-type" ("javascript" `BS.isInfixOf` contentType hdrs)
  assertBool "runtime" ("__jshark/events" `BS.isInfixOf` body)

eventsHeaderOk :: Assertion
eventsHeaderOk = do
  app <- middlewareApp
  (code, hdrs, _) <- run app ["__jshark", "events"]
  code @?= 200
  assertBool "event-stream" ("text/event-stream" `BS.isInfixOf` contentType hdrs)

sseBroadcastOk :: Assertion
sseBroadcastOk = do
  hub <- newHotReloadHub defaultHotReloadConfig
  next <- subscribe hub
  broadcastEvent hub (CssUpdate "/static/todo-mvc.css" 42)
  ev <- next
  ev @?= CssUpdate "/static/todo-mvc.css" 42
  assertBool "json" ("css-update" `T.isInfixOf` encodeEvent ev)

-- | The snapshot and the subscription must cover the whole timeline with
-- no gap: anything published before the atomic connect is in the snapshot;
-- anything published after is delivered on the stream.
sseSnapshotCoherent :: Assertion
sseSnapshotCoherent = do
  hub <- newHotReloadHub defaultHotReloadConfig
  h1 <- registerJs hub "app" "v1"
  broadcastEvent hub (JsUpdate "app" "/app.js" h1)
  (snap, _) <- subscribeWithSnapshot hub
  assertEqual
    "pre-connect hash is in the snapshot"
    [("app", h1)]
    (snapshotJsHashes snap)
  (_, next) <- subscribeWithSnapshot hub
  h2 <- registerJs hub "app" "v2"
  broadcastEvent hub (JsUpdate "app" "/app.js" h2)
  ev <- next
  assertEqual "post-connect update is delivered" (JsUpdate "app" "/app.js" h2) ev
  -- Build status is part of the same coherent view.
  broadcastEvent hub (BuildError "boom")
  (snap2, _) <- subscribeWithSnapshot hub
  assertEqual "snapshot error" (Just "boom") (snapshotBuildError snap2)

-- | Each publication bumps a monotonic revision, and a snapshot records
-- the revision it was taken at.
sseRevisionOk :: Assertion
sseRevisionOk = do
  hub <- newHotReloadHub defaultHotReloadConfig
  let
    revision = snapshotRevision <$> currentSnapshot hub
  r0 <- revision
  broadcastEvent hub (CssUpdate "/a.css" 1)
  broadcastEvent hub (CssUpdate "/a.css" 2)
  r2 <- revision
  assertEqual "two broadcasts bump twice" (r0 + 2) r2
  (snap, _) <- subscribeWithSnapshot hub
  r3 <- revision
  assertEqual "snapshot carries the current revision" r3 (snapshotRevision snap)

-- | Two apps share a hub but their artifacts and updates stay scoped.
sseTwoAppsOk :: Assertion
sseTwoAppsOk = do
  hub <- newHotReloadHub defaultHotReloadConfig
  ha <- registerJs hub "app-a" "console.log('a')"
  hb <- registerJs hub "app-b" "console.log('b')"
  next <- subscribe hub
  forM_ [("app-a", ha), ("app-b", hb)] $ \(app, h) -> do
    let
      ev = JsUpdate app ("/" <> app <> "/app.js") h
    broadcastEvent hub ev
    got <- next
    assertEqual (T.unpack app <> " event") ev got
  (snap, _) <- subscribeWithSnapshot hub
  assertBool "app-a hash present" (("app-a", ha) `elem` snapshotJsHashes snap)
  assertBool "app-b hash present" (("app-b", hb) `elem` snapshotJsHashes snap)

-- | A client that connects, disconnects, and reconnects must still receive
-- subsequent updates (the old subscription does not poison the channel).
sseReconnectOk :: Assertion
sseReconnectOk = do
  hub <- newHotReloadHub defaultHotReloadConfig
  _first <- subscribe hub
  (_, next2) <- subscribeWithSnapshot hub
  h <- registerJs hub "app" "v"
  broadcastEvent hub (JsUpdate "app" "/app.js" h)
  ev <- next2
  assertEqual "second client sees the update" (JsUpdate "app" "/app.js" h) ev

injectOk :: Assertion
injectOk =
  assertEqual
    "before head close, ahead of app.js"
    ( "<html><head><title>x</title>"
        <> "<script src=\"/__jshark/client.js\"></script>"
        <> "</head><body><script src=\"/b/app.js\"></script></body></html>"
    )
    ( injectScriptIntoHtml
        "<script src=\"/__jshark/client.js\"></script>"
        "<html><head><title>x</title></head><body><script src=\"/b/app.js\"></script></body></html>"
    )

middlewareInjectOk :: Assertion
middlewareInjectOk = do
  app <- middlewareApp
  (_, _, body) <- run app []
  assertBool "client script" ("/__jshark/client.js" `BS.isInfixOf` body)
  assertBool "no defer" (not ("defer" `BS.isInfixOf` body))
  assertBool "still has body close" ("</body>" `BS.isInfixOf` body)

-- | A raw response's fallback may be HTML, but the middleware must hand the
-- raw response back unchanged (it is the wire output).
rawResponsePreserved :: Assertion
rawResponsePreserved =
  case injectHotReloadClient defaultHotReloadConfig raw of
    ResponseRaw {} -> pure ()
    other ->
      assertFailure
        ("raw response was rewritten: " <> show (responseHeaders other))
 where
  raw =
    ResponseRaw (\_ _ -> pure ()) $
      responseLBS
        status200
        [("Content-Type", "text/html; charset=utf-8")]
        "<html><body></body></html>"

injectDropsLength :: Assertion
injectDropsLength = do
  let
    out =
      injectHotReloadClient defaultHotReloadConfig $
        ResponseBuilder
          status200
          [ ("Content-Type", "text/html")
          , ("Content-Length", "13")
          , ("ETag", "\"v1\"")
          , ("Content-MD5", "abc")
          ]
          (B.byteString "<html></html>")
  forM_ ["Content-Length", "ETag", "Content-MD5"] $ \name ->
    assertEqual ("stale " <> show name <> " is dropped") Nothing $
      lookup name (responseHeaders out)
  body <- bodyOf out
  assertBool "client is injected" ("/__jshark/client.js" `BS.isInfixOf` body)

injectSkipsCompressed :: Assertion
injectSkipsCompressed = do
  body <-
    bodyOf . injectHotReloadClient defaultHotReloadConfig $
      ResponseBuilder
        status200
        [("Content-Type", "text/html"), ("Content-Encoding", "gzip")]
        (B.byteString "<html></html>")
  assertBool
    "encoded body is left alone"
    (not ("/__jshark/client.js" `BS.isInfixOf` body))

-- | Run @body@ with a fresh temp dir's @Life/Client.hs@ path, an action
-- that starts a watcher over the dir counting 'onHaskellSource' hits, and
-- a wait for at least @n@ hits (giving up after 8 s).
withWatchDir ::
  String -> (FilePath -> IO (IO ()) -> (Int -> IO Int) -> IO ()) -> Assertion
withWatchDir name body = do
  dir <- (</> name) <$> getTemporaryDirectory
  let
    setup = removePathForcibly dir >> createDirectoryIfMissing True (dir </> "Life")
  bracket_ setup (removePathForcibly dir) $ do
    hits <- newIORef (0 :: Int)
    hub <- newHotReloadHub defaultHotReloadConfig {hrDebounceMs = 50}
    let
      bump _ = atomicModifyIORef' hits $ \n -> (n + 1, ())
      waitHits want = go (0 :: Int)
       where
        go waited = do
          n <- readIORef hits
          if n >= want || waited >= 8000000
            then pure n
            else threadDelay 50000 >> go (waited + 50000)
    body
      (dir </> "Life" </> "Client.hs")
      (startWatcher hub (WatchTargets [dir] (const Nothing) bump))
      waitHits

-- | Same-length overwrite must still enqueue a second Haskell recompile.
watcherSecondSaveOk :: Assertion
watcherSecondSaveOk =
  withWatchDir "jshark-hr-watch-second-save" $ \hs start waitHits -> do
    stop <- start
    -- Let fsnotify register the watch before the first write.
    threadDelay 250000
    writeFile hs "module Client where\n-- a\n"
    n1 <- waitHits 1
    writeFile hs "module Client where\n-- b\n"
    n2 <- waitHits 2
    stop
    assertBool ("first save seen, got " <> show n1) (n1 >= 1)
    assertBool ("second save seen, got " <> show n2) (n2 >= 2)

-- | An editor-style write-temp-then-rename save (over an existing file)
-- must still reach the hook: GHC 'renameFile' is MoveFileEx REPLACE_EXISTING.
watcherRenameSaveOk :: Assertion
watcherRenameSaveOk =
  withWatchDir "jshark-hr-watch-rename" $ \hs start waitHits -> do
    stop <- start
    threadDelay 250000
    writeFile hs "module Client where\n-- a\n"
    n1 <- waitHits 1
    writeFile (hs <> ".tmp") "module Client where\n-- b\n"
    renameFile (hs <> ".tmp") hs
    n2 <- waitHits 2
    stop
    assertBool ("first save seen, got " <> show n1) (n1 >= 1)
    assertBool ("rename save seen, got " <> show n2) (n2 >= 2)

-- | Repeated start/stop cycles must not leak or wedge: each fresh watcher
-- still sees saves, and the disposer joins the previous drain worker.
watcherRestartOk :: Assertion
watcherRestartOk =
  withWatchDir "jshark-hr-watch-restart" $ \hs start waitHits -> do
    forM_ [1 .. 3 :: Int] $ \i -> do
      stop <- start
      threadDelay 200000
      writeFile hs ("module Client where\n-- cycle " <> show i <> "\n")
      _ <- waitHits i
      stop
    n <- waitHits 3
    assertBool ("three restarts each saw a save, got " <> show n) (n >= 3)

-- | Evaluate the client runtime under happy-dom with a fake EventSource,
-- then run a JS body against @api = window.__JSHARK_HR_API__@; the body
-- throws on failure.
hmrCase :: TestName -> [String] -> TestTree
hmrCase name body = testCase name $ do
  bun <- findExecutable "bun"
  when (isNothing bun) $
    assertFailure "bun not found on PATH; install https://bun.sh"
  got <- runProgram domTimeoutMicroseconds program
  assertEqual name "\"ok\"" (T.unpack got)
 where
  jsLit s = '"' : escapeJsString s ++ "\""
  program =
    JSProgram
      { jsFlags = ["--install=fallback"]
      , jsPrelude =
          unlines
            [ "import { GlobalRegistrator } from \"@happy-dom/global-registrator\";"
            , "GlobalRegistrator.register({ url: "
                ++ jsLit (T.unpack (happyDomUrl defaultHappyDomOptions))
                ++ " });"
            ]
      , jsExpression =
          unlines $
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
            ]
              ++ map ("  " ++) body
              ++ ["  return 'ok';", "})()"]
      , jsEpilogue = "await GlobalRegistrator.unregister();"
      }

hmrDisposeKeepsSse :: [String]
hmrDisposeKeepsSse =
  [ "if (!document.getElementById('__jshark-hr-panel')) throw new Error('missing panel');"
  , "const es = api.eventSource();"
  , "if (!es) throw new Error('missing EventSource');"
  , "if (typeof es.onmessage !== 'function') throw new Error('onmessage missing');"
  , "let sseHits = 0;"
  , "es.addEventListener('message', function () { sseHits += 1; });"
  , "let shellHits = 0;"
  , "api.untracked(function () {"
  , "  window.addEventListener('click', function () { shellHits += 1; });"
  , "});"
  , "let appHits = 0;"
  , "let boom = false;"
  , "api.withModule(function () {"
  , "  window.addEventListener('click', function () { appHits += 1; });"
  , "  es.addEventListener('message', function () { sseHits += 10; });"
  , "  window.setTimeout(function () { boom = true; }, 0);"
  , "});"
  , "api.disposeTracked();"
  , "if (typeof es.onmessage !== 'function') throw new Error('onmessage gone');"
  , "await new Promise(function (r) { setTimeout(r, 30); });"
  , "if (boom) throw new Error('tracked timeout survived');"
  , "window.dispatchEvent(new Event('click'));"
  , "if (shellHits !== 1) throw new Error('shell listener died: ' + shellHits);"
  , "if (appHits !== 0) throw new Error('app listener survived: ' + appHits);"
  , "es.dispatchEvent(new MessageEvent('message', { data: '{\"type\":\"css-update\",\"url\":\"/x.css\",\"timestamp\":1}' }));"
  , "if (sseHits < 1) throw new Error('EventSource listener died: ' + sseHits);"
  , "es.onmessage({ data: '{\"type\":\"css-update\",\"url\":\"/x.css\",\"timestamp\":1}' });"
  ]

hmrTimeoutChainDies :: [String]
hmrTimeoutChainDies =
  [ "let hops = 0;"
  , "api.withModule(function () {"
  , "  function loop() {"
  , "    hops += 1;"
  , "    if (hops < 80) window.setTimeout(loop, 0);"
  , "  }"
  , "  window.setTimeout(loop, 0);"
  , "});"
  , "await new Promise(function (r) { setTimeout(r, 25); });"
  , "if (hops < 2) throw new Error('chain never ran: ' + hops);"
  , "const frozen = hops;"
  , "api.disposeTracked();"
  , "await new Promise(function (r) { setTimeout(r, 40); });"
  , "if (hops !== frozen) throw new Error('timeout chain survived: ' + hops + ' vs ' + frozen);"
  ]

-- | Run an application on a GET of @path@, capturing status, headers, and
-- a buffered body.
run :: Application -> [T.Text] -> IO (Int, ResponseHeaders, BS.ByteString)
run app path = do
  ref <- newIORef Nothing
  _ <- app defaultRequest {pathInfo = path} $ \resp -> do
    body <- bodyOf resp
    writeIORef
      ref
      (Just (statusCode (responseStatus resp), responseHeaders resp, body))
    pure ResponseReceived
  readIORef ref >>= maybe (assertFailure "no response captured") pure

contentType :: ResponseHeaders -> BS.ByteString
contentType = fromMaybe "" . lookup "Content-Type"

-- | A buffered body; streams and files read as empty.
bodyOf :: Response -> IO BS.ByteString
bodyOf = \case
  ResponseBuilder _ _ b -> pure (LBS.toStrict (B.toLazyByteString b))
  ResponseRaw _ inner -> bodyOf inner
  _ -> pure ""
