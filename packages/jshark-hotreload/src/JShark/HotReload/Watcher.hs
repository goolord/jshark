{-# LANGUAGE OverloadedStrings #-}

-- | Filesystem watcher that maps source/static edits to 'HotReloadEvent's,
-- driven by @fsnotify@ instead of a content-hash poll loop.
module JShark.HotReload.Watcher
  ( WatchTargets (..)
  , defaultWatchTargets
  , startWatcher
  , exampleAppForHs
  , exampleAppsForHs
  , isLucidShellPath
  )
where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM, void)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import JShark.HotReload.Core
  ( HotReloadConfig (..)
  , HotReloadEvent (..)
  , HotReloadHub
  , broadcastEvent
  , hotReloadConfig
  )
import System.Directory (doesDirectoryExist)
import System.FSNotify
  ( Event (..)
  , EventIsDirectory (..)
  , defaultConfig
  , startManagerConf
  , stopManager
  , watchTree
  )
import System.FilePath
  ( splitDirectories
  , takeDirectory
  , takeExtension
  , takeFileName
  )
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

-- | Directories and URL mapping for watched assets.
data WatchTargets = WatchTargets
  { watchDirs :: [FilePath]
  -- ^ Roots to watch recursively.
  , cssUrlFor :: FilePath -> Maybe T.Text
  -- ^ Map a changed path to the browser CSS URL.
  , onHaskellSource :: FilePath -> IO ()
  -- ^ Fired for non-Page @.hs@ edits (Mode B recompiler hook).
  }

defaultWatchTargets :: [FilePath] -> WatchTargets
defaultWatchTargets dirs =
  WatchTargets
    { watchDirs = dirs
    , cssUrlFor = defaultCssUrl
    , onHaskellSource = \_ -> pure ()
    }

defaultCssUrl :: FilePath -> Maybe T.Text
defaultCssUrl path
  | takeExtension path /= ".css" = Nothing
  | otherwise =
      Just (T.pack ("/static/" <> map slash (takeFileName path)))
 where
  slash c = if c == '\\' then '/' else c

-- | Map a changed @.hs@ path under @examples/@ to an example app name.
exampleAppForHs :: FilePath -> Maybe T.Text
exampleAppForHs path =
  case exampleAppsForHs path of
    [one] -> Just one
    _ -> Nothing

-- | Like 'exampleAppForHs', but @ThemeHead@ maps to every example.
exampleAppsForHs :: FilePath -> [T.Text]
exampleAppsForHs path
  | takeExtension path /= ".hs" = []
  | serverOrCompile path = []
  | isThemeHead path =
      ["breakout", "todo-mvc", "synth", "life", "hvm2-demo"]
  | otherwise =
      case matchDir path of
        Just app -> [app]
        Nothing -> []
 where
  serverOrCompile p =
    any
      (`isInfixOf` p)
      [ "examples\\app\\server"
      , "examples/app/server"
      , "examples\\app\\compile"
      , "examples/app/compile"
      , "examples\\app\\wasm"
      , "examples/app/wasm"
      ]
  isThemeHead p = "Theme" `isInfixOf` p
  matchDir p
    | "TodoMvc" `isInfixOf` p = Just "todo-mvc"
    | "Breakout" `isInfixOf` p = Just "breakout"
    | "Synth" `isInfixOf` p = Just "synth"
    | "Hvm2Demo" `isInfixOf` p = Just "hvm2-demo"
    | "Life" `isInfixOf` p = Just "life"
    | otherwise = Nothing

-- | Lucid shell / shared head — prefer full page reload after rebuild.
isLucidShellPath :: FilePath -> Bool
isLucidShellPath path =
  takeFileName path == "Page.hs"
    || "Theme" `isInfixOf` path
    || takeExtension path == ".html"

-- | Start an fsnotify watch over the configured roots. The manager uses
-- fsnotify's OS watch wherever one exists; on Windows that native watch
-- omits LAST_WRITE, so a save that rewrites a file in place without changing
-- its size (no rename / truncate) can go unreported. Events are only queued
-- in the callback thread; a drain loop flushes unique changed paths every
-- @hrDebounceMs@, so a burst of writes to one file yields a single
-- 'HotReloadEvent' / recompile hook and handlers never run on the fsnotify
-- thread. Returns an IO action that stops the watcher.
startWatcher :: HotReloadHub -> WatchTargets -> IO (IO ())
startWatcher hub targets = do
  let
    cfg = hotReloadConfig hub
    debounceUs = max 1 (hrDebounceMs cfg) * 1000
  dirs <- filterM doesDirectoryExist (watchDirs targets)
  mgr <- startManagerConf defaultConfig
  pending <- newIORef Set.empty
  stop <- newEmptyMVar
  stopWatches <-
    fmap catMaybes $
      forM dirs $ \dir -> do
        registered <-
          try (watchTree mgr dir interestingEvent (queueEvent pending)) ::
            IO (Either SomeException (IO ()))
        case registered of
          Right stopWatching -> pure (Just stopWatching)
          Left ex -> do
            hPutStrLn stderr ("hot-reload: cannot watch " <> dir <> ": " <> show ex)
            pure Nothing
  void $
    forkIO (drainLoop hub targets debounceUs pending stop)
  pure $ do
    putMVar stop ()
    mapM_ id stopWatches
    stopManager mgr

-- | Keep only file events for watched extensions outside ignored dirs.
-- 'Removed', 'CloseWrite', and attribute churn are dropped; the drain loop
-- deduplicates anything left over.
interestingEvent :: Event -> Bool
interestingEvent ev =
  eventIsDirectory ev == IsFile
    && isWatchedFile path
    && not (underIgnoredDir path)
    && actionable ev
 where
  path = eventPath ev
  actionable (Added {}) = True
  actionable (Modified {}) = True
  actionable _ = False

-- | fsnotify callback: remember the changed path, never do real work here.
queueEvent :: IORef (Set.Set FilePath) -> Event -> IO ()
queueEvent pending ev =
  atomicModifyIORef' pending $ \paths ->
    (Set.insert (eventPath ev) paths, ())

-- | Flush pending paths once per debounce window until told to stop.
drainLoop ::
  HotReloadHub
  -> WatchTargets
  -> Int
  -> IORef (Set.Set FilePath)
  -> MVar ()
  -> IO ()
drainLoop hub targets debounceUs pending stop = do
  m <- timeout debounceUs (takeMVar stop)
  case m of
    Just () -> pure ()
    Nothing -> do
      changed <- atomicModifyIORef' pending $ \paths -> (Set.empty, Set.toList paths)
      mapM_ (handlePathSafe hub targets) changed
      drainLoop hub targets debounceUs pending stop

handlePathSafe :: HotReloadHub -> WatchTargets -> FilePath -> IO ()
handlePathSafe hub targets path = do
  result <- try (handlePath hub targets path) :: IO (Either SomeException ())
  case result of
    Left ex ->
      hPutStrLn stderr ("hot-reload: watch path failed: " <> show ex)
    Right () -> pure ()

handlePath :: HotReloadHub -> WatchTargets -> FilePath -> IO ()
handlePath hub targets path =
  case cssUrlFor targets path of
    Just url -> do
      ts <- fmap floor getPOSIXTime :: IO Int64
      broadcastEvent hub (CssUpdate url ts)
    Nothing
      | takeExtension path == ".hs" || takeExtension path == ".html" ->
          -- Page.hs / ThemeHead / Client.hs all go through Mode B recompile.
          -- Bare .html (if any) still hits the hook; recompiler no-ops unknowns.
          onHaskellSource targets path
      | otherwise -> pure ()

-- | True when the file lives under a directory fsnotify reports but the
-- hot-reload watcher ignores (@dist@, @node_modules@, ...).
underIgnoredDir :: FilePath -> Bool
underIgnoredDir = any skipWatchName . splitDirectories . takeDirectory

skipWatchName :: FilePath -> Bool
skipWatchName name =
  name
    `elem` [ ".git"
           , "dist"
           , "dist-newstyle"
           , "node_modules"
           , "speed-highlight"
           , ".stack-work"
           ]

isWatchedFile :: FilePath -> Bool
isWatchedFile path = takeExtension path `elem` [".hs", ".html", ".css"]
