{-# LANGUAGE OverloadedStrings #-}

-- | Spawn @jshark-compile@ after Haskell example sources change so the
-- hot-reload hub can broadcast fresh JS / Lucid HTML (Mode B).
module Recompile
  ( exampleAppForHs
  , startHsRecompiler
  )
where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, forever, void, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import JShark.HotReload.Core
  ( HotReloadEvent (..)
  , HotReloadHub
  , broadcastEvent
  , registerHtml
  , registerJs
  , setBuildError
  )
import JShark.HotReload.Watcher
  ( exampleAppForHs
  , exampleAppsForHs
  , isLucidShellPath
  )
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Process (readProcessWithExitCode)

cacheDir :: FilePath
cacheDir = ".jshark-cache"

data PendingApp = PendingApp
  { pendingName :: T.Text
  , pendingPage :: Bool
  -- ^ True when a Lucid shell / ThemeHead edit forced a full page reload.
  }
  deriving (Eq)

-- | Background worker: queue example names, rebuild @jshark-compile@, run it,
-- then register JS+HTML and broadcast @JsUpdate@ or @PageReload@.
startHsRecompiler :: HotReloadHub -> FilePath -> IO (FilePath -> IO ())
startHsRecompiler hub compileBin = do
  pending <- newTVarIO ([] :: [PendingApp])
  wake <- newEmptyMVar
  lock <- newEmptyMVar
  putMVar lock ()
  void $
    forkIO $
      forever $ do
        takeMVar wake
        apps <- atomically $ do
          xs <- readTVar pending
          writeTVar pending []
          pure (mergePending xs)
        forM_ apps $ \job -> do
          takeMVar lock
          result <-
            try (recompileOne hub compileBin job) :: IO (Either SomeException ())
          case result of
            Left ex -> do
              hPutStrLn stderr ("hot-reload: recompile crashed: " <> show ex)
              setBuildError hub (T.pack (show ex))
            Right () -> pure ()
          putMVar lock ()
  pure $ \path ->
    case exampleAppsForHs path of
      [] -> pure ()
      names -> do
        let
          page = isLucidShellPath path
          jobs = [PendingApp n page | n <- names]
        atomically $ do
          xs <- readTVar pending
          writeTVar pending (jobs ++ xs)
        void (tryPutMVar wake ())

mergePending :: [PendingApp] -> [PendingApp]
mergePending = foldr step []
 where
  step job acc =
    case filter ((== pendingName job) . pendingName) acc of
      [] -> job : acc
      (old : _) ->
        let
          merged =
            PendingApp
              (pendingName job)
              (pendingPage job || pendingPage old)
          rest = filter ((/= pendingName job) . pendingName) acc
         in
          merged : rest

recompileOne :: HotReloadHub -> FilePath -> PendingApp -> IO ()
recompileOne hub compileBin job = do
  let
    app = pendingName job
  hPutStrLn stdout ("hot-reload: compiling " <> T.unpack app <> " …")
  hFlush stdout
  (buildEc, buildOut, buildErr) <-
    readProcessWithExitCode
      "cabal"
      ["build", "-v0", "exe:jshark-compile"]
      ""
  case buildEc of
    ExitFailure code -> do
      let
        msg =
          T.pack $
            "cabal build jshark-compile failed ("
              <> show code
              <> ")\n"
              <> buildOut
              <> buildErr
      hPutStrLn stderr (T.unpack msg)
      setBuildError hub msg
    ExitSuccess -> do
      bin <- resolveCompileBin compileBin
      (ec, out, err) <-
        readProcessWithExitCode bin [T.unpack app] ""
      case ec of
        ExitFailure code -> do
          let
            msg =
              T.pack $
                "jshark-compile "
                  <> T.unpack app
                  <> " failed ("
                  <> show code
                  <> ")\n"
                  <> out
                  <> err
          hPutStrLn stderr (T.unpack msg)
          setBuildError hub msg
        ExitSuccess -> loadArtifacts hub job

loadArtifacts :: HotReloadHub -> PendingApp -> IO ()
loadArtifacts hub job = do
  let
    app = pendingName job
    jsFile = cacheDir </> (T.unpack app <> ".js")
    htmlFile = cacheDir </> (T.unpack app <> ".html")
  jsOk <- doesFileExist jsFile
  htmlOk <- doesFileExist htmlFile
  if not jsOk
    then setBuildError hub ("missing " <> T.pack jsFile)
    else do
      js <- T.readFile jsFile
      hJs <- registerJs hub app js
      when htmlOk $ do
        html <- T.readFile htmlFile
        void (registerHtml hub app html)
      if pendingPage job
        then do
          broadcastEvent hub (PageReload ("lucid:" <> app))
          hPutStrLn
            stdout
            ("hot-reload: " <> T.unpack app <> " page reload (" <> T.unpack hJs <> ")")
        else do
          broadcastEvent hub (JsUpdate app ("/" <> app <> "/app.js") hJs)
          hPutStrLn
            stdout
            ("hot-reload: " <> T.unpack app <> " js ok (" <> T.unpack hJs <> ")")
      hFlush stdout

resolveCompileBin :: FilePath -> IO FilePath
resolveCompileBin hint = do
  ok <- doesFileExist hint
  if ok
    then pure hint
    else do
      (ec, out, _) <-
        readProcessWithExitCode "cabal" ["list-bin", "jshark-compile"] ""
      case ec of
        ExitSuccess ->
          case filter (not . null) (lines out) of
            (p : _) -> pure (trim p)
            [] -> pure hint
        ExitFailure _ -> pure hint
 where
  trim = reverse . dropWhile isSp . reverse . dropWhile isSp
  isSp c = c == ' ' || c == '\r' || c == '\n' || c == '\t'
