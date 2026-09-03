{-# LANGUAGE OverloadedStrings #-}

-- | Spawn @jshark-compile@ after Haskell example sources change so the
-- hot-reload hub can broadcast fresh JS (Mode B).
module Recompile
  ( exampleAppForHs
  , startHsRecompiler
  )
where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, forever, void)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import JShark.HotReload.Core
  ( HotReloadEvent (..)
  , HotReloadHub
  , broadcastEvent
  , registerJs
  , setBuildError
  )
import JShark.HotReload.Watcher (exampleAppForHs)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Process (readProcessWithExitCode)

cacheDir :: FilePath
cacheDir = ".jshark-cache"

-- | Background worker: queue example names, rebuild @jshark-compile@, run it,
-- then register JS + broadcast @JsUpdate@ (or @BuildError@).
startHsRecompiler :: HotReloadHub -> FilePath -> IO (FilePath -> IO ())
startHsRecompiler hub compileBin = do
  pending <- newTVarIO ([] :: [T.Text])
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
          pure (nubKeep xs)
        forM_ apps $ \app -> do
          takeMVar lock
          result <- try (recompileOne hub compileBin app) :: IO (Either SomeException ())
          case result of
            Left ex -> do
              hPutStrLn stderr ("hot-reload: recompile crashed: " <> show ex)
              setBuildError hub (T.pack (show ex))
            Right () -> pure ()
          putMVar lock ()
  pure $ \path ->
    case exampleAppForHs path of
      Nothing -> pure ()
      Just app -> do
        atomically $ do
          xs <- readTVar pending
          writeTVar pending (app : xs)
        void (tryPutMVar wake ())

nubKeep :: [T.Text] -> [T.Text]
nubKeep = go []
 where
  go acc [] = reverse acc
  go acc (x : xs)
    | x `elem` acc = go acc xs
    | otherwise = go (x : acc) xs

recompileOne :: HotReloadHub -> FilePath -> T.Text -> IO ()
recompileOne hub compileBin app = do
  hPutStrLn stdout ("hot-reload: compiling " <> T.unpack app <> " …")
  hFlush stdout
  -- Rebuild so Client.hs edits land in the compile binary's object code.
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
        ExitSuccess -> do
          let
            cacheFile = cacheDir </> (T.unpack app <> ".js")
          exists <- doesFileExist cacheFile
          if not exists
            then setBuildError hub ("missing " <> T.pack cacheFile)
            else do
              js <- T.readFile cacheFile
              h <- registerJs hub app js
              broadcastEvent hub (JsUpdate app ("/" <> app <> "/app.js") h)
              hPutStrLn
                stdout
                ("hot-reload: " <> T.unpack app <> " ok (" <> T.unpack h <> ")")
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
