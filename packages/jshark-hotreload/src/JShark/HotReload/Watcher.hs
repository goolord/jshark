{-# LANGUAGE OverloadedStrings #-}

-- | Filesystem watcher that maps source/static edits to 'HotReloadEvent's.
module JShark.HotReload.Watcher
  ( WatchTargets (..)
  , defaultWatchTargets
  , startWatcher
  , exampleAppForHs
  , exampleAppsForHs
  , isLucidShellPath
  )
where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, void)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import JShark.HotReload.Core
  ( HotReloadConfig (..)
  , HotReloadEvent (..)
  , HotReloadHub
  , broadcastEvent
  , hotReloadConfig
  )
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath (takeExtension, takeFileName, (</>))
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

-- | Start a content-hash poll loop. Returns an IO action that stops it.
--
-- Native Windows watches omit LAST_WRITE, so a 1-character in-place
-- save can keep size and mtime unchanged. Content hash sees every save.
startWatcher :: HotReloadHub -> WatchTargets -> IO (IO ())
startWatcher hub targets = do
  stop <- newEmptyMVar
  snap <- newIORef Map.empty
  let
    cfg = hotReloadConfig hub
    debounceUs = max 1 (hrDebounceMs cfg) * 1000
  void $
    forkIO $ do
      seed <- snapshotDirs (watchDirs targets)
      writeIORef snap seed
      let
        loop = do
          m <- timeout debounceUs (takeMVar stop)
          case m of
            Just () -> pure ()
            Nothing -> do
              changed <- pollChanges snap (watchDirs targets)
              mapM_ (handlePathSafe hub targets) changed
              loop
      loop
  pure (putMVar stop ())

handlePathSafe :: HotReloadHub -> WatchTargets -> FilePath -> IO ()
handlePathSafe hub targets path = do
  result <- try (handlePath hub targets path) :: IO (Either SomeException ())
  case result of
    Left ex ->
      hPutStrLn stderr ("hot-reload: watch path failed: " <> show ex)
    Right () -> pure ()

type FingerSnap = Map.Map FilePath Int

pollChanges :: IORef FingerSnap -> [FilePath] -> IO [FilePath]
pollChanges snap dirs = do
  next <- snapshotDirs dirs
  old <- readIORef snap
  writeIORef snap next
  pure
    [ path
    | (path, h) <- Map.toList next
    , Map.lookup path old /= Just h
    ]

snapshotDirs :: [FilePath] -> IO FingerSnap
snapshotDirs dirs = do
  paths <- fmap concat (mapM listWatchedFiles dirs)
  pairs <-
    forM (nubKeep paths) $ \path -> do
      mh <- fileFinger path
      pure (path, mh)
  pure $
    Map.fromList
      [(path, h) | (path, Just h) <- pairs]

listWatchedFiles :: FilePath -> IO [FilePath]
listWatchedFiles root = do
  isDir <- doesDirectoryExist root
  isFile <- doesFileExist root
  if isDir
    then walkDir root
    else
      if isFile && isWatchedFile root
        then pure [root]
        else pure []

walkDir :: FilePath -> IO [FilePath]
walkDir dir = do
  names <- listDirectory dir
  fmap concat $
    forM names $ \name ->
      if skipWatchName name
        then pure []
        else do
          let
            path = dir </> name
          isDir <- doesDirectoryExist path
          if isDir
            then walkDir path
            else
              if isWatchedFile path
                then pure [path]
                else pure []

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

fileFinger :: FilePath -> IO (Maybe Int)
fileFinger path = do
  result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
  case result of
    Left _ -> pure Nothing
    Right bs -> pure (Just (fnv32 bs))

fnv32 :: BS.ByteString -> Int
fnv32 = BS.foldl' step 2166136261
 where
  step h b =
    let
      h' = h `xor` fromIntegral b
     in
      h' * 16777619

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
      | takeExtension path == ".hs" || takeExtension path == ".html" ->
          -- Page.hs / ThemeHead / Client.hs all go through Mode B recompile.
          -- Bare .html (if any) still hits the hook; recompiler no-ops unknowns.
          onHaskellSource targets path
      | otherwise -> pure ()
