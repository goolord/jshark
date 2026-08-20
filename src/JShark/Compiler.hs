{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}

-- | Post-process generated JavaScript: minify, and cache the result.
--
-- Google Closure Compiler is no longer the best default. Its Advanced
-- mode is still unique for whole-program property renaming, but it is a
-- Java tool, slow to start, and unsafe on JShark FFI without externs.
-- In 2026 the practical choice is [esbuild](https://esbuild.github.io)
-- (fast, ubiquitous, what this module uses for 'Auto'). Terser remains
-- as the more aggressive size-oriented option; Closure is still available
-- when you actually want Advanced.
module JShark.Compiler
  ( -- * Compiler Configuration
    CompilerConfig(..)
  , defaultCompilerConfig
  , passthroughConfig
  , CompilerBackend(..)
  , ClosureLevel(..)
  , CompilerClosureConfig(..)
  , defaultClosureConfig
  , CompilerEsbuildConfig(..)
  , defaultEsbuildConfig
  , CompilerTerserConfig(..)
  , defaultTerserConfig
  , CacheStrategy(..)

    -- * Compilation
  , compileJS
  , compileWith
  , tryCompileWith
  , compileClosure
  , compileEsbuild
  , compileTerser
  , compileEffect
  , compilePure

    -- * Cache
  , clearCompilerCache
  ) where

import Control.Exception (IOException, SomeException, catch, throwIO)
import Control.Monad (guard)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , findExecutable
  , removeFile
  , renameFile
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, hPutStrLn, openBinaryTempFile, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

import JShark (effectfulProgram, pureProgram, renderJS)
import JShark.Types (Effect, Expr, Universe)

-- | Compilation level for Google Closure Compiler.
-- Encoded as the long names ('SIMPLE_OPTIMIZATIONS' /
-- 'ADVANCED_OPTIMIZATIONS') that both old jars and current
-- @google-closure-compiler@ accept.
data ClosureLevel
  = WhitespaceOnly
  | Simple
  | Advanced
  deriving (Show, Eq, Ord)

closureLevelString :: ClosureLevel -> String
closureLevelString = \case
  WhitespaceOnly -> "WHITESPACE_ONLY"
  Simple -> "SIMPLE_OPTIMIZATIONS"
  Advanced -> "ADVANCED_OPTIMIZATIONS"

-- | Options for Google Closure Compiler.
data CompilerClosureConfig = CompilerClosureConfig
  { closureLevel :: ClosureLevel
  , closureExtraArgs :: [String]
  } deriving (Show, Eq, Ord)

-- | Default Closure configuration: SIMPLE (not Advanced — Advanced
-- renames properties and will break DOM/FFI without externs).
defaultClosureConfig :: CompilerClosureConfig
defaultClosureConfig = CompilerClosureConfig Simple []

-- | Options for esbuild minification.
data CompilerEsbuildConfig = CompilerEsbuildConfig
  { esbuildMinify :: Bool
  , esbuildTarget :: Maybe String
  , esbuildExtraArgs :: [String]
  } deriving (Show, Eq, Ord)

-- | Default esbuild configuration with minification enabled.
defaultEsbuildConfig :: CompilerEsbuildConfig
defaultEsbuildConfig = CompilerEsbuildConfig True Nothing []

-- | Options for Terser minification.
data CompilerTerserConfig = CompilerTerserConfig
  { terserCompress :: Bool
  , terserMangle :: Bool
  , terserExtraArgs :: [String]
  } deriving (Show, Eq, Ord)

-- | Default Terser configuration with compression and mangling enabled.
defaultTerserConfig :: CompilerTerserConfig
defaultTerserConfig = CompilerTerserConfig True True []

-- | Minifier backend. 'Auto' picks the first of esbuild, Closure, Terser
-- found on @PATH@. If none are present, 'tryCompileWith' returns 'Left';
-- 'compileWith' then honors 'configFallback' (unminified source vs throw).
data CompilerBackend
  = Auto
  | Closure CompilerClosureConfig
  | Esbuild CompilerEsbuildConfig
  | Terser CompilerTerserConfig
  | Passthrough
  deriving (Show, Eq, Ord)

-- | Caching strategy for minified JavaScript artifacts.
--
-- Memory entries are keyed by the full source+backend string (no hash
-- collisions) and capped at 256; when full, the 'Ord'-least key is
-- dropped (not LRU). Disk entries store that same key in the file and
-- verify it on read.
data CacheStrategy
  = NoCache
  | MemoryCache
  | DiskCache FilePath
  deriving (Show, Eq, Ord)

-- | Top-level compiler configuration.
data CompilerConfig = CompilerConfig
  { configBackend :: CompilerBackend
  , configCache :: CacheStrategy
  -- | When 'True', minifier failure (missing binary, crash, empty DCE'd
  -- output) logs to stderr and returns the unminified source. When
  -- 'False', 'compileWith' throws. Named helpers ('compileEsbuild' etc.)
  -- set this to 'False'.
  , configFallback :: Bool
  } deriving (Show, Eq, Ord)

-- | Auto backend, in-memory cache, fall back to the unminified source if
-- no minifier is installed (or if it crashes). Failures are logged to
-- stderr. Prefer 'tryCompileWith' if you need to distinguish success
-- from fallback.
defaultCompilerConfig :: CompilerConfig
defaultCompilerConfig = CompilerConfig Auto MemoryCache True

-- | Skip minification entirely. Useful in tests.
passthroughConfig :: CompilerConfig
passthroughConfig = CompilerConfig Passthrough NoCache False

cacheFormatVersion :: Text
cacheFormatVersion = "jshark-minify-1"

memoryCacheMaxEntries :: Int
memoryCacheMaxEntries = 256

diskCacheMagic :: BS.ByteString
diskCacheMagic = "jshark-cache-v1\n"

globalMemoryCache :: IORef (Map Text Text)
{-# NOINLINE globalMemoryCache #-}
globalMemoryCache = unsafePerformIO (newIORef Map.empty)

-- | Drop the in-memory minifier cache.
clearCompilerCache :: IO ()
clearCompilerCache = writeIORef globalMemoryCache Map.empty

fnv1a64 :: BS.ByteString -> Word64
fnv1a64 = BS.foldl' step 14695981039346656037
  where
    step !h !w = (h `xor` fromIntegral w) * 1099511628211

hashText :: Text -> String
hashText t = showHex (fnv1a64 (TE.encodeUtf8 t)) ""

cacheKey :: CompilerConfig -> Text -> Text
cacheKey cfg source =
  cacheFormatVersion <> ":" <> T.pack (show (configBackend cfg)) <> ":" <> source

insertBounded :: Text -> Text -> Map Text Text -> Map Text Text
insertBounded k v m =
  let m' = Map.insert k v m
   in if Map.size m' > memoryCacheMaxEntries
        then Map.deleteMin m'
        else m'

-- | Minify raw JavaScript using 'defaultCompilerConfig'.
--
-- Does __not__ wrap the input in an IIFE: a bare expression with no side
-- effects may be DCE'd to empty by esbuild/Terser. Prefer 'compilePure' /
-- 'compileEffect' for JShark output. Empty minifier output on non-empty
-- input is treated as failure.
compileJS :: Text -> IO Text
compileJS = compileWith defaultCompilerConfig

-- | Minify using 'tryCompileWith'. On 'Left', either throw or (when
-- 'configFallback' is set) log to stderr and return the original source.
compileWith :: CompilerConfig -> Text -> IO Text
compileWith cfg source = do
  res <- tryCompileWith cfg source
  case res of
    Right out -> pure out
    Left err
      | configFallback cfg -> do
          hPutStrLn stderr ("JShark.Compiler: " ++ err ++ "; using unminified source")
          pure source
      | otherwise ->
          throwIO (userError ("JShark.Compiler: compilation failed: " ++ err))

-- | Minify without fallback or throwing on minifier failure.
-- Cache is consulted only for successful results (fallback source is
-- never stored).
tryCompileWith :: CompilerConfig -> Text -> IO (Either String Text)
tryCompileWith cfg source = case configCache cfg of
  NoCache -> tryRunCompile cfg source
  MemoryCache -> do
    let key = cacheKey cfg source
    hit <- Map.lookup key <$> readIORef globalMemoryCache
    case hit of
      Just cached -> pure (Right cached)
      Nothing -> do
        compiled <- tryRunCompile cfg source
        case compiled of
          Right out ->
            atomicModifyIORef' globalMemoryCache (\m -> (insertBounded key out m, Right out))
          left -> pure left
  DiskCache dir -> do
    createDirectoryIfMissing True dir
    let key = cacheKey cfg source
        cacheFile = dir </> (hashText key ++ ".js")
    loaded <- loadDiskCache cacheFile key
    case loaded of
      Just cached -> pure (Right cached)
      Nothing -> do
        compiled <- tryRunCompile cfg source
        case compiled of
          Right out -> do
            atomicWriteFile cacheFile (encodeDiskCache key out)
            pure (Right out)
          left -> pure left

encodeDiskCache :: Text -> Text -> BS.ByteString
encodeDiskCache key compiled =
  let keyBs = TE.encodeUtf8 key
      lenLine = TE.encodeUtf8 (T.pack (show (BS.length keyBs) <> "\n"))
   in diskCacheMagic <> lenLine <> keyBs <> TE.encodeUtf8 compiled

loadDiskCache :: FilePath -> Text -> IO (Maybe Text)
loadDiskCache path expectedKey = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      raw <- BS.readFile path
      case decodeDiskCache raw of
        Just (key, body) | key == expectedKey -> pure (Just body)
        _ -> pure Nothing

decodeDiskCache :: BS.ByteString -> Maybe (Text, Text)
decodeDiskCache raw = do
  rest0 <- BS.stripPrefix diskCacheMagic raw
  let (lenBs, rest) = BC.break (== '\n') rest0
  rest1 <- BS.stripPrefix "\n" rest
  n <- readMaybe (BC.unpack lenBs)
  guard (n >= 0 && BS.length rest1 >= n)
  let (keyBs, body) = BS.splitAt n rest1
  key <- either (const Nothing) Just (TE.decodeUtf8' keyBs)
  compiled <- either (const Nothing) Just (TE.decodeUtf8' body)
  pure (key, compiled)

atomicWriteFile :: FilePath -> BS.ByteString -> IO ()
atomicWriteFile dest bytes = do
  let dir = takeDirectory dest
  createDirectoryIfMissing True dir
  (tmp, h) <- openBinaryTempFile dir "jshark-cache.tmp"
  let cleanupTmp = removeFile tmp `catch` (\(_ :: IOException) -> pure ())
      closeH = hClose h `catch` (\(_ :: IOException) -> pure ())
  (do
      BS.hPut h bytes
      hClose h
      removeFile dest `catch` (\(_ :: IOException) -> pure ())
      renameFile tmp dest
    ) `catch` (\(e :: SomeException) -> closeH >> cleanupTmp >> throwIO e)

-- | Minify with Google Closure Compiler at the given level.
-- Throws if the compiler is missing or fails ('configFallback' is false).
compileClosure :: ClosureLevel -> Text -> IO Text
compileClosure lvl =
  compileWith (CompilerConfig (Closure (CompilerClosureConfig lvl [])) MemoryCache False)

-- | Minify with esbuild. Throws if esbuild is missing or fails.
compileEsbuild :: Text -> IO Text
compileEsbuild = compileWith (CompilerConfig (Esbuild defaultEsbuildConfig) MemoryCache False)

-- | Minify with Terser. Throws if terser is missing or fails.
compileTerser :: Text -> IO Text
compileTerser = compileWith (CompilerConfig (Terser defaultTerserConfig) MemoryCache False)

-- | Compile an effectful JShark computation to an IIFE, then minify.
compileEffect :: forall (u :: Universe).
     CompilerConfig
  -> (forall (f :: Universe -> Type). Effect f u)
  -> IO Text
compileEffect cfg eff = compileWith cfg (T.pack (renderJS (effectfulProgram eff)))

-- | Compile a pure JShark expression to an IIFE, then minify.
compilePure :: forall (u :: Universe).
     CompilerConfig
  -> (forall (f :: Universe -> Type). Expr f u)
  -> IO Text
compilePure cfg e = compileWith cfg (T.pack (renderJS (pureProgram e)))

tryRunCompile :: CompilerConfig -> Text -> IO (Either String Text)
tryRunCompile cfg source = case configBackend cfg of
  Passthrough -> pure (Right source)
  Esbuild ebCfg -> runEsbuild ebCfg source
  Closure clCfg -> runClosure clCfg source
  Terser tCfg -> runTerser tCfg source
  Auto -> runAuto source

-- Prefer esbuild, then Closure, then Terser. Never shell out to npx in
-- Auto: npx may hit the network. Explicit backends may use npx --no-install.
runAuto :: Text -> IO (Either String Text)
runAuto source = do
  tryEsbuild <- hasExecutable "esbuild"
  if tryEsbuild
    then runEsbuild defaultEsbuildConfig source
    else do
      tryClosure <- (||) <$> hasExecutable "google-closure-compiler" <*> hasExecutable "closure-compiler"
      if tryClosure
        then runClosure defaultClosureConfig source
        else do
          tryTerser <- hasExecutable "terser"
          if tryTerser
            then runTerser defaultTerserConfig source
            else pure (Left "no minifier on PATH (install esbuild, google-closure-compiler, or terser)")

hasExecutable :: String -> IO Bool
hasExecutable name = do
  mExe <- findExecutable name
  pure (maybe False (const True) mExe)

runEsbuild :: CompilerEsbuildConfig -> Text -> IO (Either String Text)
runEsbuild cfg source = do
  mDirect <- findExecutable "esbuild"
  mNpx <- findExecutable "npx"
  let args =
        ["--loader=js", "--log-level=error"]
        ++ [ "--minify" | esbuildMinify cfg ]
        ++ maybe [] (\t -> ["--target=" ++ t]) (esbuildTarget cfg)
        ++ esbuildExtraArgs cfg
  case (mDirect, mNpx) of
    (Just exe, _) -> executeProcess exe args source
    (Nothing, Just npxExe) -> executeProcess npxExe (["--no-install", "esbuild"] ++ args) source
    (Nothing, Nothing) -> pure (Left "esbuild executable not found on PATH")

runClosure :: CompilerClosureConfig -> Text -> IO (Either String Text)
runClosure cfg source = do
  mJar <- lookupEnv "CLOSURE_COMPILER_JAR"
  mDirect <- findExecutable "google-closure-compiler"
  mClosure <- findExecutable "closure-compiler"
  mNpx <- findExecutable "npx"
  let args = ["--compilation_level", closureLevelString (closureLevel cfg)] ++ closureExtraArgs cfg
  case mJar of
    Just jar -> do
      mJava <- findExecutable "java"
      case mJava of
        Just javaExe -> executeProcess javaExe (["-jar", jar] ++ args) source
        Nothing -> pure (Left "java executable not found for CLOSURE_COMPILER_JAR")
    Nothing -> case (mDirect, mClosure, mNpx) of
      (Just exe, _, _) -> executeProcess exe args source
      (Nothing, Just exe, _) -> executeProcess exe args source
      (Nothing, Nothing, Just npxExe) ->
        executeProcess npxExe (["--no-install", "google-closure-compiler"] ++ args) source
      (Nothing, Nothing, Nothing) -> pure (Left "Google Closure Compiler not found on PATH")

runTerser :: CompilerTerserConfig -> Text -> IO (Either String Text)
runTerser cfg source = do
  mDirect <- findExecutable "terser"
  mNpx <- findExecutable "npx"
  let args =
        ["--compress" | terserCompress cfg]
        ++ ["--mangle" | terserMangle cfg]
        ++ terserExtraArgs cfg
  case (mDirect, mNpx) of
    (Just exe, _) -> executeProcess exe args source
    (Nothing, Just npxExe) -> executeProcess npxExe (["--no-install", "terser"] ++ args) source
    (Nothing, Nothing) -> pure (Left "terser executable not found on PATH")

executeProcess :: FilePath -> [String] -> Text -> IO (Either String Text)
executeProcess cmd args source =
  (do
    (code, stdoutStr, stderrStr) <- readProcessWithExitCode cmd args (T.unpack source)
    case code of
      ExitSuccess ->
        let out = T.strip (T.pack stdoutStr)
         in if T.null out && not (T.null (T.strip source))
              then pure (Left "minifier produced empty output (possible DCE of a bare expression; use compilePure/compileEffect)")
              else pure (Right out)
      ExitFailure c ->
        pure (Left (if null stderrStr then "Process exited with code " ++ show c else stderrStr))
  ) `catch` (\(e :: SomeException) -> pure (Left (show e)))
