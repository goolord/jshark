{-# LANGUAGE OverloadedStrings #-}

-- | Spawn @jshark-compile@ after Haskell example sources change so the
-- hot-reload hub can broadcast fresh JS / Lucid HTML (Mode B).
--
-- Nested @cabal@ must not inherit @GHC_PACKAGE_PATH@ from @cabal run@.
-- Do not pass extra configure flags (@--offline@, @--store-dir@, @--builddir@):
-- Cabal treats those as a new configuration and does a full rebuild.
module Recompile
  ( exampleAppForHs
  , startHsRecompiler
  , findJsharkRoot
  , prepareCabalHot
  , CabalHot (..)
  )
where

import Control.Concurrent (forkIO, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM_, forever, void, when)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (intercalate, isInfixOf, isPrefixOf, nubBy, sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import JShark.HotReload.Core
  ( HotReloadEvent (..)
  , HotReloadHub
  , broadcastEvent
  , registerHtml
  , registerJs
  , setBuildError
  , setBuildStart
  )
import JShark.HotReload.Watcher
  ( exampleAppForHs
  , exampleAppsForHs
  , isLucidShellPath
  )
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
import System.Process
  ( CreateProcess (..)
  , proc
  , readCreateProcessWithExitCode
  )
import System.Timeout (timeout)

cacheDir :: FilePath
cacheDir = ".jshark-cache"

-- | Cabal used by this @cabal run@, plus the @jshark-compile@ binary path.
data CabalHot = CabalHot
  { hotRoot :: FilePath
  , hotCabal :: FilePath
  , hotCompileBin :: FilePath
  }

data PendingApp = PendingApp
  { pendingName :: T.Text
  , pendingPage :: Bool
  -- ^ True when a Lucid shell / ThemeHead edit forced a full page reload.
  }
  deriving Eq

-- GHC package-env from @cabal run@; do not touch CABAL_DIR / CABAL_CONFIG.
poisonGhcEnv :: [String]
poisonGhcEnv =
  [ "GHC_PACKAGE_PATH"
  , "GHC_ENVIRONMENT"
  , "HASKELL_DIST_DIR"
  , "CABAL_SANDBOX_PACKAGE_PATH"
  ]

-- | Directory that contains @jshark.cabal@ and @cabal.project@.
findJsharkRoot :: IO FilePath
findJsharkRoot = getCurrentDirectory >>= go
 where
  go dir = do
    let
      cabal = dir </> "jshark.cabal"
      proj = dir </> "cabal.project"
    hasCabal <- doesFileExist cabal
    hasProj <- doesFileExist proj
    if hasCabal && hasProj
      then pure dir
      else do
        let
          parent = takeDirectory dir
        if parent == dir
          then fail ("hot-reload: cannot find jshark.cabal above " <> dir)
          else go parent

isPoison :: [String] -> String -> Bool
isPoison names name =
  let
    n = map toLower name
   in
    n `elem` map (map toLower) names
      || "ghc_package_path" `isPrefixOf` n

stripNames :: [String] -> [(String, String)] -> [(String, String)]
stripNames names = filter (not . isPoison names . fst)

-- | @findExecutables@ on directory-1.3.10/Windows returns only the first
-- PATH hit. @cabal run@ prepends @%APPDATA%\\cabal\\bin@ (often 2.x) so
-- ghcup's 3.x is invisible unless we walk every search-path entry.
-- Probe ghcup first and stop at the first cabal-install >= 3.8.
findCabal :: IO FilePath
findCabal = do
  candidates <- listCabalCandidates
  go candidates []
 where
  go [] scored =
    fail $
      "hot-reload: need cabal-install >= 3.8. Found: "
        <> intercalate ", " (map describe scored)
  go (p : ps) scored = do
    mv <- probeCabalVersion p
    case mv of
      Just v | v >= [3, 8] -> pure p
      _ -> go ps ((p, mv) : scored)
  describe (p, Nothing) = p <> " (not cabal-install 3.x)"
  describe (p, Just v) =
    p <> " (" <> intercalate "." (map show v) <> ")"

listCabalCandidates :: IO [FilePath]
listCabalCandidates = do
  dirs <- getExecSearchPath
  let
    names
      | os == "mingw32" = ["cabal.exe", "cabal"]
      | otherwise = ["cabal"]
  existing <- filterM doesFileExist [dir </> name | dir <- dirs, name <- names]
  pure
    ( sortOn
        cabalRank
        (filter (not . isScript) (nubBy ((==) `on` map toLower) existing))
    )

-- Prefer ghcup; skip installdir 2.x and scoop shims until later.
cabalRank :: FilePath -> Int
cabalRank p =
  let
    l = map toLower p
   in
    if "ghcup" `isInfixOf` l
      then 0
      else
        if "scoop" `isInfixOf` l && "shims" `isInfixOf` l
          then 8
          else
            if "roaming" `isInfixOf` l && "cabal" `isInfixOf` l
              then 7
              else 1

isScript :: FilePath -> Bool
isScript p =
  let
    ext = map toLower (takeExtension p)
   in
    ext `elem` [".cmd", ".bat"]
      || ".cmd" `isInfixOf` map toLower p

probeCabalVersion :: FilePath -> IO (Maybe [Int])
probeCabalVersion cabal = do
  env0 <- getEnvironment
  m <-
    timeout 2000000 $
      readCreateProcessWithExitCode
        (proc cabal ["--numeric-version"])
          { env = Just (stripNames poisonGhcEnv env0)
          }
        ""
  pure $ case m of
    Just (ExitSuccess, out, _) ->
      case filter (not . null) (map trim (lines out)) of
        (line : _) -> parseNumericVersion line
        [] -> Nothing
    _ -> Nothing

parseNumericVersion :: String -> Maybe [Int]
parseNumericVersion s =
  case go s of
    [] -> Nothing
    vs -> Just vs
 where
  go [] = []
  go xs =
    let
      (digits, rest) = span (`elem` ['0' .. '9']) xs
     in
      case (digits, rest) of
        ([], _) -> []
        (ds, []) -> [intFromDigits ds]
        (ds, '.' : more) -> intFromDigits ds : go more
        _ -> []
  intFromDigits =
    foldl (\acc c -> acc * 10 + (fromEnum c - fromEnum '0')) 0

-- | Same flags as the parent @cabal run@: no extra configure switches.
runCabalHot :: CabalHot -> [String] -> IO (ExitCode, String, String)
runCabalHot hot =
  runCabal (hotRoot hot) (hotCabal hot) poisonGhcEnv []

-- | Replace the child environment (Windows @CreateProcess@ lpEnvironment)
-- instead of spawning PowerShell to unset variables.
runCabal ::
  FilePath
  -> FilePath
  -> [String]
  -> [(String, String)]
  -> [String]
  -> IO (ExitCode, String, String)
runCabal root cabal unsetNames extra args = do
  env0 <- getEnvironment
  let
    extraKeys = map fst extra
    envBlock = extra ++ stripNames (unsetNames ++ extraKeys) env0
  readCreateProcessWithExitCode
    (proc cabal args)
      { cwd = Just root
      , env = Just envBlock
      }
    ""

prepareCabalHot :: IO CabalHot
prepareCabalHot = do
  root <- findJsharkRoot
  cabal <- findCabal
  createDirectoryIfMissing True (root </> cacheDir)
  let
    hot0 =
      CabalHot
        { hotRoot = root
        , hotCabal = cabal
        , hotCompileBin = root </> "jshark-compile"
        }
  bin <- listCompileBin hot0
  pure hot0 {hotCompileBin = bin}

listCompileBin :: CabalHot -> IO FilePath
listCompileBin hot = do
  (ec, out, _) <-
    runCabalHot hot ["list-bin", "--", "exe:jshark-compile"]
  case ec of
    ExitSuccess ->
      case filter (not . null) (lines out) of
        (p : _) -> pure (trim p)
        [] -> pure (hotCompileBin hot)
    ExitFailure _ -> pure (hotCompileBin hot)

-- | Background worker: queue example names, rebuild @jshark-compile@, run it,
-- then register JS+HTML and broadcast @JsUpdate@ or @PageReload@.
startHsRecompiler :: HotReloadHub -> CabalHot -> IO (FilePath -> IO ())
startHsRecompiler hub hot = do
  pending <- newTVarIO ([] :: [PendingApp])
  wake <- newEmptyMVar
  void $
    forkIO $
      forever $ do
        takeMVar wake
        apps <- atomically $ do
          xs <- readTVar pending
          writeTVar pending []
          pure (mergePending xs)
        forM_ apps $ \job -> do
          result <-
            try (recompileOne hub hot job) :: IO (Either SomeException ())
          case result of
            Left ex -> do
              hPutStrLn stderr ("hot-reload: recompile crashed: " <> show ex)
              setBuildError hub (T.pack (show ex))
            Right () -> pure ()
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

recompileOne :: HotReloadHub -> CabalHot -> PendingApp -> IO ()
recompileOne hub hot job = do
  let
    app = pendingName job
    root = hotRoot hot
  setBuildStart hub app
  hPutStrLn stdout ("hot-reload: compiling " <> T.unpack app <> " ...")
  hFlush stdout
  (buildEc, buildOut, buildErr) <-
    runCabalHot hot ["build", "-v0", "--", "exe:jshark-compile"]
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
      binExists <- doesFileExist (hotCompileBin hot)
      bin <-
        if binExists
          then pure (hotCompileBin hot)
          else listCompileBin hot
      env0 <- getEnvironment
      (ec, out, err) <-
        readCreateProcessWithExitCode
          (proc bin [T.unpack app])
            { cwd = Just root
            , env = Just (stripNames poisonGhcEnv env0)
            }
          ""
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
        ExitSuccess -> loadArtifacts hub root job

loadArtifacts :: HotReloadHub -> FilePath -> PendingApp -> IO ()
loadArtifacts hub root job = do
  let
    app = pendingName job
    jsFile = root </> cacheDir </> (T.unpack app <> ".js")
    htmlFile = root </> cacheDir </> (T.unpack app <> ".html")
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

trim :: String -> String
trim = reverse . dropWhile isSp . reverse . dropWhile isSp
 where
  isSp c = c == ' ' || c == '\r' || c == '\n' || c == '\t'
