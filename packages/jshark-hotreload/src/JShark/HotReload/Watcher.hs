{-# LANGUAGE LambdaCase #-}

-- | Filesystem watcher that maps source/static edits to 'HotReloadEvent's,
-- driven by @fsnotify@ instead of a content-hash poll loop.
module JShark.HotReload.Watcher
  ( WatchTargets (..)
  , startWatcher
  )
where

import Control.Concurrent.Async (async, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, void)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import JShark.HotReload.Core
import System.Directory (doesDirectoryExist)
import System.FSNotify
  ( Event (..)
  , EventIsDirectory (..)
  , defaultConfig
  , startManagerConf
  , stopManager
  , watchTree
  )
import System.FilePath (splitDirectories, takeDirectory, takeExtension)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

-- | Directories and URL mapping for watched assets.
data WatchTargets = WatchTargets
  { watchDirs :: [FilePath]
  -- ^ Roots to watch recursively.
  , cssUrlFor :: FilePath -> Maybe T.Text
  -- ^ Map a changed path to the browser CSS URL.
  , onHaskellSource :: FilePath -> IO ()
  -- ^ Fired for @.hs@ / @.html@ edits (Mode B recompiler hook).
  }

-- | Start an fsnotify watch over the configured roots. The manager uses
-- fsnotify's OS watch wherever one exists; on Windows that native watch
-- omits LAST_WRITE, so a save that rewrites a file in place without changing
-- its size (no rename / truncate) can go unreported. Events are only queued
-- in the callback thread; a drain loop flushes unique changed paths every
-- @hrDebounceMs@, so a burst of writes to one file yields a single
-- 'HotReloadEvent' / recompile hook and handlers never run on the fsnotify
-- thread. Returns an IO action that stops the watches and joins the drain
-- worker, so the watcher can be restarted without leaking threads.
startWatcher :: HotReloadHub -> WatchTargets -> IO (IO ())
startWatcher hub targets = do
  dirs <- filterM doesDirectoryExist (watchDirs targets)
  mgr <- startManagerConf defaultConfig
  pending <- newIORef Set.empty
  stop <- newEmptyMVar
  let
    debounceUs = max 1 (hrDebounceMs (hotReloadConfig hub)) * 1000
    queue ev =
      atomicModifyIORef' pending (\ps -> (Set.insert (eventPath ev) ps, ()))
    watch dir =
      try (watchTree mgr dir interesting queue) >>= \case
        Right unwatch -> pure [unwatch]
        Left ex -> [] <$ warn ("cannot watch " <> dir) ex
    drain = do
      stopped <- timeout debounceUs (takeMVar stop)
      case stopped of
        Just () -> pure ()
        Nothing -> do
          changed <- atomicModifyIORef' pending (\ps -> (Set.empty, ps))
          mapM_ handleSafe changed
          drain
    handleSafe path =
      try (handlePath path) >>= \case
        Right () -> pure ()
        Left ex -> warn "watch path failed" ex
    handlePath path = case cssUrlFor targets path of
      Just url -> do
        ts <- floor <$> getPOSIXTime
        broadcastEvent hub (CssUpdate url ts)
      Nothing
        -- Page.hs / Theme / Client.hs all go through the Mode B recompile;
        -- the recompiler ignores paths it does not know.
        | takeExtension path `elem` [".hs", ".html"] ->
            onHaskellSource targets path
        | otherwise -> pure ()
  stopWatches <- concat <$> mapM watch dirs
  worker <- async drain
  pure $ do
    putMVar stop ()
    sequence_ stopWatches
    stopManager mgr
    void (waitCatch worker)

-- | Keep only added/modified watched files outside ignored dirs; the drain
-- loop deduplicates anything left over.
interesting :: Event -> Bool
interesting ev =
  eventIsDirectory ev == IsFile
    && takeExtension path `elem` [".hs", ".html", ".css"]
    && not (any (`elem` ignored) (splitDirectories (takeDirectory path)))
    && case ev of
      Added {} -> True
      Modified {} -> True
      _ -> False
 where
  path = eventPath ev
  ignored =
    [ ".git"
    , "dist"
    , "dist-newstyle"
    , "node_modules"
    , "speed-highlight"
    , ".stack-work"
    ]

warn :: String -> SomeException -> IO ()
warn what ex = hPutStrLn stderr ("hot-reload: " <> what <> ": " <> show ex)
