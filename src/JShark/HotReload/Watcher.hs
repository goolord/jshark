{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Filesystem watcher that maps source/static edits to 'HotReloadEvent's.
module JShark.HotReload.Watcher
  ( WatchTargets (..)
  , defaultWatchTargets
  , startWatcher
  , exampleAppForHs
  )
where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (forever, void)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Int (Int64)
import Data.List (isInfixOf, isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import JShark.HotReload.Core
  ( HotReloadConfig (..)
  , HotReloadEvent (..)
  , HotReloadHub
  , broadcastEvent
  , hotReloadConfig
  , registerJs
  )
import System.FilePath (takeExtension, takeFileName)
import qualified System.FSNotify as FS

-- | Directories and URL mapping for watched assets.
data WatchTargets = WatchTargets
  { watchDirs :: [FilePath]
  -- ^ Roots to watch recursively.
  , cssUrlFor :: FilePath -> Maybe T.Text
  -- ^ Map a changed path to the browser CSS URL.
  , cacheJsAppFor :: FilePath -> Maybe T.Text
  -- ^ Map @.jshark-cache/<app>.js@ updates to an app name.
  , onHaskellSource :: FilePath -> IO ()
  -- ^ Fired for non-Page @.hs@ edits (Mode B recompiler hook).
  }

defaultWatchTargets :: [FilePath] -> WatchTargets
defaultWatchTargets dirs =
  WatchTargets
    { watchDirs = dirs
    , cssUrlFor = defaultCssUrl
    , cacheJsAppFor = defaultCacheJs
    , onHaskellSource = \_ -> pure ()
    }

defaultCssUrl :: FilePath -> Maybe T.Text
defaultCssUrl path
  | takeExtension path /= ".css" = Nothing
  | otherwise =
      Just (T.pack ("/static/" <> map slash (takeFileName path)))
 where
  slash c = if c == '\\' then '/' else c

defaultCacheJs :: FilePath -> Maybe T.Text
defaultCacheJs path
  | not (".js" `isSuffixOf` path) = Nothing
  | not (".jshark-cache" `isInfixOf` path) = Nothing
  | otherwise =
      let
        base = takeFileName path
       in
        Just (T.pack (stripSuffix ".js" base))
 where
  stripSuffix sfx s
    | sfx `isSuffixOf` s = take (length s - length sfx) s
    | otherwise = s

-- | Map a changed @.hs@ path under @examples/@ to an example app name.
exampleAppForHs :: FilePath -> Maybe T.Text
exampleAppForHs path
  | takeExtension path /= ".hs" = Nothing
  | takeFileName path == "Page.hs" = Nothing
  | serverOrTheme path = Nothing
  | otherwise = matchDir path
 where
  serverOrTheme p =
    any
      (`isInfixOf` p)
      [ "examples\\server"
      , "examples/server"
      , "ThemeHead"
      , "examples\\compile"
      , "examples/compile"
      ]
  matchDir p
    | "TodoMvc" `isInfixOf` p = Just "todo-mvc"
    | "Breakout" `isInfixOf` p = Just "breakout"
    | "Synth" `isInfixOf` p = Just "synth"
    | "Hvm2Demo" `isInfixOf` p = Just "hvm2-demo"
    | "Life" `isInfixOf` p = Just "life"
    | otherwise = Nothing

-- | Start a debounced fsnotify loop. Returns an IO action that stops it.
startWatcher :: HotReloadHub -> WatchTargets -> IO (IO ())
startWatcher hub targets = do
  stop <- newEmptyMVar
  pending <- newIORef ([] :: [FilePath])
  let
    cfg = hotReloadConfig hub
    debounceUs = max 1 (hrDebounceMs cfg) * 1000
  void $
    forkIO $
      FS.withManager $ \mgr -> do
        stops <-
          mapM
            ( \dir ->
                FS.watchTree mgr dir (const True) $ \ev ->
                  case eventPath ev of
                    Just p ->
                      atomicModifyIORef' pending $ \ps -> (p : ps, ())
                    Nothing -> pure ()
            )
            (watchDirs targets)
        void $
          forkIO $
            forever $ do
              threadDelay debounceUs
              paths <- atomicModifyIORef' pending $ \ps -> ([], nubKeep (reverse ps))
              mapM_ (handlePath hub targets) paths
        takeMVar stop
        sequence_ stops
  pure (putMVar stop ())

eventPath :: FS.Event -> Maybe FilePath
eventPath = \case
  FS.Added p _ _ -> Just p
  FS.Modified p _ _ -> Just p
  FS.Removed p _ _ -> Just p
  FS.Unknown p _ _ _ -> Just p
  FS.ModifiedAttributes p _ _ -> Just p
  FS.WatchedDirectoryRemoved p _ _ -> Just p
  FS.CloseWrite p _ _ -> Just p

nubKeep :: [FilePath] -> [FilePath]
nubKeep = go []
 where
  go acc [] = reverse acc
  go acc (x : xs)
    | x `elem` acc = go acc xs
    | otherwise = go (x : acc) xs

handlePath :: HotReloadHub -> WatchTargets -> FilePath -> IO ()
handlePath hub targets path =
  case cssUrlFor targets path of
    Just url -> do
      ts <- fmap floor getPOSIXTime :: IO Int64
      broadcastEvent hub (CssUpdate url ts)
    Nothing
      | takeFileName path == "Page.hs" || takeExtension path == ".html" ->
          broadcastEvent hub (PageReload (T.pack path))
      | takeExtension path == ".hs" ->
          onHaskellSource targets path
      | otherwise ->
          case cacheJsAppFor targets path of
            Just app -> do
              src <- T.readFile path
              h <- registerJs hub app src
              broadcastEvent hub (JsUpdate app ("/" <> app <> "/app.js") h)
            Nothing -> pure ()
