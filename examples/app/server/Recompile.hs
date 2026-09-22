{-# LANGUAGE OverloadedStrings #-}

-- | Spawn @jshark-compile@ after Haskell example sources change so the
-- hot-reload hub can broadcast fresh JS / Lucid HTML (Mode B).
--
-- Nested @cabal@ must not inherit @GHC_PACKAGE_PATH@ from @cabal run@.
-- Do not pass extra configure flags (@--offline@, @--store-dir@, @--builddir@):
-- Cabal treats those as a new configuration and does a full rebuild.
module Recompile
  ( CabalHot (..)
  , prepareCabalHot
  , startHsRecompiler
  )
where

import Control.Concurrent (forkIO, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM_, forever, unless, void, when)
import Data.Char (isDigit, toLower)
import Data.Function (on)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List
  ( dropWhileEnd
  , intercalate
  , isInfixOf
  , isPrefixOf
  , nub
  , nubBy
  , sortOn
  )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import JShark.Example.Watch (exampleAppsForHs, isLucidShellPath)
import JShark.HotReload.Core
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getCurrentDirectory
  , getExecSearchPath
  )
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Info (os)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

cacheDir :: FilePath
cacheDir = ".jshark-cache"

-- | Cabal used by this @cabal run@, plus the @jshark-compile@ binary path.
data CabalHot = CabalHot
  { hotRoot :: FilePath
  , hotCabal :: FilePath
  , hotCompileBin :: FilePath
  }

-- | Run a program without the GHC package environment that @cabal run@
-- sets (CABAL_DIR / CABAL_CONFIG are left alone). The child environment is
-- replaced (Windows @CreateProcess@ lpEnvironment) instead of spawning a
-- shell to unset variables.
runClean ::
  Maybe FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runClean dir exe args = do
  env0 <- getEnvironment
  readCreateProcessWithExitCode
    (proc exe args) {cwd = dir, env = Just (filter (not . poisoned . fst) env0)}
    ""
 where
  poisoned name =
    let
      n = map toLower name
     in
      n
        `elem` [ "ghc_package_path"
               , "ghc_environment"
               , "haskell_dist_dir"
               , "cabal_sandbox_package_path"
               ]
        || "ghc_package_path" `isPrefixOf` n

-- | Same flags as the parent @cabal run@: no extra configure switches.
runCabal :: CabalHot -> [String] -> IO (ExitCode, String, String)
runCabal hot = runClean (Just (hotRoot hot)) (hotCabal hot)

prepareCabalHot :: IO CabalHot
prepareCabalHot = do
  root <- getCurrentDirectory >>= findRoot
  cabal <- findCabal
  createDirectoryIfMissing True (root </> cacheDir)
  let
    hot = CabalHot root cabal (root </> "jshark-compile")
  bin <- listCompileBin hot
  pure hot {hotCompileBin = bin}
 where
  -- The directory that contains the @cabal.project@ (repo root).
  findRoot dir = do
    found <- doesFileExist (dir </> "cabal.project")
    if found
      then pure dir
      else
        if takeDirectory dir == dir
          then fail ("hot-reload: cannot find cabal.project above " <> dir)
          else findRoot (takeDirectory dir)

-- | @findExecutables@ on directory-1.3.10/Windows returns only the first
-- PATH hit. @cabal run@ prepends @%APPDATA%\\cabal\\bin@ (often 2.x) so
-- ghcup's 3.x is invisible unless we walk every search-path entry.
-- Probe ghcup first and stop at the first cabal-install >= 3.8.
findCabal :: IO FilePath
findCabal = do
  dirs <- getExecSearchPath
  let
    names = if os == "mingw32" then ["cabal.exe", "cabal"] else ["cabal"]
  existing <- filterM doesFileExist [dir </> name | dir <- dirs, name <- names]
  go [] . sortOn rank . filter (not . isScript) $
    nubBy ((==) `on` map toLower) existing
 where
  go scored [] =
    fail $
      "hot-reload: need cabal-install >= 3.8. Found: "
        <> intercalate ", " (map describe scored)
  go scored (p : ps) = do
    m <- timeout 2000000 (runClean Nothing p ["--numeric-version"])
    let
      v = do
        Just (ExitSuccess, out, _) <- pure m
        l : _ <- pure (filter (not . null) (map trim (lines out)))
        parseVersion l
    if maybe False (>= [3, 8]) v then pure p else go ((p, v) : scored) ps
  describe (p, Nothing) = p <> " (not cabal-install 3.x)"
  describe (p, Just v) = p <> " (" <> intercalate "." (map show v) <> ")"
  -- Prefer ghcup; skip installdir 2.x and scoop shims until later.
  rank p
    | "ghcup" `isInfixOf` l = 0 :: Int
    | "scoop" `isInfixOf` l && "shims" `isInfixOf` l = 8
    | "roaming" `isInfixOf` l && "cabal" `isInfixOf` l = 7
    | otherwise = 1
   where
    l = map toLower p
  isScript p =
    map toLower (takeExtension p) `elem` [".cmd", ".bat"]
      || ".cmd" `isInfixOf` map toLower p

-- | Leading dot-separated numbers: @"3.12.1.0"@ is @[3, 12, 1, 0]@.
parseVersion :: String -> Maybe [Int]
parseVersion s = case go s of
  [] -> Nothing
  vs -> Just vs
 where
  go xs = case span isDigit xs of
    ([], _) -> []
    (ds, []) -> [read ds]
    (ds, '.' : more) -> read ds : go more
    _ -> []

listCompileBin :: CabalHot -> IO FilePath
listCompileBin hot = do
  (ec, out, _) <- runCabal hot ["list-bin", "--", "exe:jshark-compile"]
  pure $ case (ec, filter (not . null) (lines out)) of
    (ExitSuccess, p : _) -> trim p
    _ -> hotCompileBin hot

-- | Background worker: queue example names, rebuild @jshark-compile@, run it,
-- then register JS+HTML and broadcast @JsUpdate@ or @PageReload@. Returns
-- the hook for a changed source path.
startHsRecompiler :: HotReloadHub -> CabalHot -> IO (FilePath -> IO ())
startHsRecompiler hub hot = do
  -- (app, full page reload?) requests, newest first.
  pending <- newIORef []
  wake <- newEmptyMVar
  void . forkIO . forever $ do
    takeMVar wake
    jobs <- atomicModifyIORef' pending (\js -> ([], js))
    -- One build per app, newest first; a Lucid shell / Theme edit in any
    -- request forces a full page reload.
    forM_ (nub (map fst jobs)) $ \app -> do
      let
        page = or [p | (a, p) <- jobs, a == app]
      result <- try (recompileOne hub hot app page)
      case result of
        Left ex -> do
          hPutStrLn stderr ("hot-reload: recompile crashed: " <> show ex)
          broadcastEvent hub (BuildError (T.pack (show (ex :: SomeException))))
        Right () -> pure ()
  pure $ \path -> do
    let
      jobs = [(app, isLucidShellPath path) | app <- exampleAppsForHs path]
    unless (null jobs) $ do
      atomicModifyIORef' pending (\js -> (jobs ++ js, ()))
      void (tryPutMVar wake ())

recompileOne :: HotReloadHub -> CabalHot -> T.Text -> Bool -> IO ()
recompileOne hub hot app page = do
  broadcastEvent hub (BuildStart app)
  say ("compiling " <> name <> " ...")
  built <- runCabal hot ["build", "-v0", "--", "exe:jshark-compile"]
  whenOk "cabal build jshark-compile" built $ do
    binExists <- doesFileExist (hotCompileBin hot)
    bin <- if binExists then pure (hotCompileBin hot) else listCompileBin hot
    ran <- runClean (Just (hotRoot hot)) bin [name]
    whenOk ("jshark-compile " <> name) ran $ do
      jsOk <- doesFileExist jsFile
      htmlOk <- doesFileExist htmlFile
      if not jsOk
        then broadcastEvent hub (BuildError ("missing " <> T.pack jsFile))
        else do
          h <- registerJs hub app =<< T.readFile jsFile
          when htmlOk $ void (registerHtml hub app =<< T.readFile htmlFile)
          if page
            then do
              broadcastEvent hub (PageReload ("lucid:" <> app))
              say (name <> " page reload (" <> T.unpack h <> ")")
            else do
              broadcastEvent hub (JsUpdate app ("/" <> app <> "/app.js") h)
              say (name <> " js ok (" <> T.unpack h <> ")")
 where
  name = T.unpack app
  jsFile = hotRoot hot </> cacheDir </> (name <> ".js")
  htmlFile = hotRoot hot </> cacheDir </> (name <> ".html")
  say msg = putStrLn ("hot-reload: " <> msg) >> hFlush stdout
  whenOk what (ec, out, err) k = case ec of
    ExitSuccess -> k
    ExitFailure code -> do
      let
        msg = what <> " failed (" <> show code <> ")\n" <> out <> err
      hPutStrLn stderr msg
      broadcastEvent hub (BuildError (T.pack msg))

trim :: String -> String
trim = dropWhileEnd isSp . dropWhile isSp
 where
  isSp c = c `elem` [' ', '\r', '\n', '\t']
