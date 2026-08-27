{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Post-process generated JavaScript and optional external minification.
--
-- JShark codegen already emits compact JS ('renderJSCompact'). The default
-- config ('defaultCompilerConfig') wraps an IIFE and skips external tools.
-- Opt into esbuild / Terser / Closure via 'CompilerBackend' when you want
-- another shrink pass.
--
-- Use 'readableConfig' for a debug snippet (no IIFE, assignment inlining,
-- then 'prettyJS' — not a full JS pretty-printer).
--
-- == @*Pure@ / @*IO@ entry points
--
-- Post-process helpers ('compileWith', 'compileJS', 'compileClosure', …):
--
-- * @*Pure@ — 'quietCfg' (suppresses minifier fallback on stderr)
-- * default and @*IO@ — identical; no progress bars (minify only)
--
-- JShark program helpers ('compileEffect', 'compilePure', …):
--
-- * @*Pure@ — silent 'CompileReport' interpreter
-- * @*IO@ — terminal progress + timing ('compileEffect' / 'compileEffects')
-- * 'compilePure' / 'compilePures' — never draw progress, any suffix
module JShark.Compiler
  ( -- * Compiler Configuration
    CompilerConfig (..)
  , defaultCompilerConfig
  , passthroughConfig
  , readableConfig
  , CompilerBackend (..)
  , OutputStyle (..)
  , ClosureLevel (..)
  , CompilerClosureConfig (..)
  , defaultClosureConfig
  , CompilerEsbuildConfig (..)
  , defaultEsbuildConfig
  , CompilerTerserConfig (..)
  , defaultTerserConfig
  , CacheStrategy (..)

    -- * Compilation
  , compileJS
  , compileJSPure
  , compileJSIO
  , compileWith
  , compileWithPure
  , compileWithIO
  , tryCompileWith
  , tryCompileWithPure
  , tryCompileWithIO
  , compileClosure
  , compileClosurePure
  , compileClosureIO
  , compileEsbuild
  , compileEsbuildPure
  , compileEsbuildIO
  , compileTerser
  , compileTerserPure
  , compileTerserIO
  , compileEffect
  , compileEffectPure
  , compileEffectIO
  , compileEffects
  , compileEffectsPure
  , compileEffectsIO
  , compileEffectsLabeled
  , compileEffectsLabeledPure
  , compileEffectsLabeledIO
  , compileJobsLabeled
  , compileJobsLabeledPure
  , compileJobsLabeledIO
  , compilePure
  , compilePurePure
  , compilePureIO
  , compilePures
  , compilePuresPure
  , compilePuresIO
  , compilePuresLabeled
  , compilePuresLabeledPure
  , compilePuresLabeledIO
  , prettyJS

    -- * Cache
  , clearCompilerCache

    -- * HVM2 lint
  , applyCompilerArgs
  , isCompilerFlag
  , CompileJobStats (..)
  )
where

import Control.Concurrent.Async (mapConcurrently, wait, withAsync)
import Control.Exception
  ( IOException
  , SomeException
  , catch
  , evaluate
  , finally
  , throwIO
  )
import Control.Monad (guard, unless, when)
import Data.Atomics.Counter (newCounter, readCounter, writeCounter)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isSpace)
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word64)
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import GHC.Clock (getMonotonicTime)
import JShark
  ( ClosedEffect
  , ClosedExpr
  , effectfulAST
  , effectfulProgram
  , pureAST
  , pureProgram
  , renderJSCompact
  )
import qualified JShark.CompileProgress as CP
import qualified JShark.CompileReport as CR
import JShark.CompileTiming
  ( CompileForm (..)
  , CompileJobStats (..)
  , seconds
  )
import JShark.Emit (JS, renderJS)
import JShark.Hvm2Lint (warnHvm2CandidatesEffect, warnHvm2CandidatesExpr)
import Numeric (showHex)
import qualified Streaming.ByteString as Q
  ( hGetContents
  , toStrict_
  )
import System.CPUTime (getCPUTime)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , findExecutable
  , removeFile
  , renameFile
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO
  ( Handle
  , hClose
  , openBinaryTempFile
  )
import System.Process.Typed
  ( byteStringInput
  , createPipe
  , getStderr
  , getStdout
  , proc
  , setStderr
  , setStdin
  , setStdout
  , waitExitCode
  , withProcessWait
  )
import Text.Read (readMaybe)
import qualified TextBuilder as TB

quietCfg :: CompilerConfig -> CompilerConfig
quietCfg cfg = cfg {configProgress = False, configQuiet = True}

numberedEffectJob :: Int -> ClosedEffect u -> (Text, ClosedEffect u)
numberedEffectJob i eff = ("#" <> T.pack (show i), eff)

numberedPureJob :: Int -> ClosedExpr u -> (Text, ClosedExpr u)
numberedPureJob i e = ("#" <> T.pack (show i), e)

closureCompilerConfig :: ClosureLevel -> CompilerConfig
closureCompilerConfig lvl =
  CompilerConfig
    (Closure (CompilerClosureConfig lvl []))
    MemoryCache
    False
    Minified
    False
    False
    False
    Nothing

esbuildCompilerConfig :: CompilerConfig
esbuildCompilerConfig =
  CompilerConfig
    (Esbuild defaultEsbuildConfig)
    MemoryCache
    False
    Minified
    False
    False
    False
    Nothing

terserCompilerConfig :: CompilerConfig
terserCompilerConfig =
  CompilerConfig
    (Terser defaultTerserConfig)
    MemoryCache
    False
    Minified
    False
    False
    False
    Nothing

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
  }
  deriving (Show, Eq, Ord)

-- | Default Closure configuration: SIMPLE (not Advanced — Advanced
-- renames properties and will break DOM/FFI without externs).
defaultClosureConfig :: CompilerClosureConfig
defaultClosureConfig = CompilerClosureConfig Simple []

-- | Options for esbuild minification.
data CompilerEsbuildConfig = CompilerEsbuildConfig
  { esbuildMinify :: Bool
  , esbuildTarget :: Maybe String
  , esbuildExtraArgs :: [String]
  }
  deriving (Show, Eq, Ord)

-- | Default esbuild configuration with minification enabled.
defaultEsbuildConfig :: CompilerEsbuildConfig
defaultEsbuildConfig = CompilerEsbuildConfig True Nothing []

-- | Options for Terser minification.
data CompilerTerserConfig = CompilerTerserConfig
  { terserCompress :: Bool
  , terserMangle :: Bool
  , terserExtraArgs :: [String]
  }
  deriving (Show, Eq, Ord)

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
-- 'DiskCache' stores entries on disk and verifies keys on read.
-- 'MemoryCache' is kept for API compatibility but does not retain an
-- in-process table (each call recompiles).
data CacheStrategy
  = NoCache
  | MemoryCache
  | DiskCache FilePath
  deriving (Show, Eq, Ord)

-- | How to present compiled JavaScript.
data OutputStyle
  = -- | Pretty-print, do not minify. Not wrapped in an IIFE.
    Readable
  | -- | Wrap in an IIFE and run the configured minifier.
    Minified
  deriving (Show, Eq, Ord)

-- | Top-level compiler configuration.
data CompilerConfig = CompilerConfig
  { configBackend :: CompilerBackend
  , configCache :: CacheStrategy
  , configFallback :: Bool
  -- ^ When 'True', minifier failure (missing binary, crash, empty DCE'd
  --     output) logs to stderr and returns the unminified source. When
  --     'False', 'compileWith' throws. Named helpers ('compileEsbuild' etc.)
  --     set this to 'False'.
  , configStyle :: OutputStyle
  , configWarnHvm2Candidates :: Bool
  , configProgress :: Bool
  -- ^ Print a terminal progress bar for batch compiles and elapsed time when
  --     done. Off by default so tests stay quiet.
  , configQuiet :: Bool
  -- ^ Suppress non-fatal compiler stderr (for example minifier fallback
  --     notices) from concurrent batch workers.
  , configProgressSlot :: Maybe Int
  -- ^ Active job index for sub-progress reporting during batch compiles.
  }
  deriving (Show, Eq, Ord)

-- | Passthrough backend: compact IIFE from codegen, no external minifier.
-- Set 'configBackend' to 'Esbuild', 'Terser', or 'Closure' for a second pass.
defaultCompilerConfig :: CompilerConfig
defaultCompilerConfig =
  CompilerConfig Passthrough NoCache True Minified False False False Nothing

-- | Skip minification entirely. Useful in tests of the IIFE wrapper.
passthroughConfig :: CompilerConfig
passthroughConfig =
  CompilerConfig Passthrough NoCache False Minified False False False Nothing

-- | Human-readable JS: assignment elimination, no minifier, no IIFE.
readableConfig :: CompilerConfig
readableConfig =
  CompilerConfig Passthrough NoCache False Readable False False False Nothing

cacheFormatVersion :: Text
cacheFormatVersion = "jshark-minify-2"

diskCacheMagic :: BS.ByteString
diskCacheMagic = "jshark-cache-v1\n"

-- | No-op; retained for test harness compatibility.
clearCompilerCache :: IO ()
clearCompilerCache = pure ()

fnv1a64 :: BS.ByteString -> Word64
fnv1a64 = BS.foldl' step 14695981039346656037
 where
  step !h !w = (h `xor` fromIntegral w) * 1099511628211

hashText :: Text -> String
hashText t = showHex (fnv1a64 (TE.encodeUtf8 t)) ""

cacheKey :: CompilerConfig -> Text -> Text
cacheKey cfg source =
  cacheFormatVersion
    <> ":"
    <> T.pack (show (configBackend cfg))
    <> ":"
    <> T.pack (show (configStyle cfg))
    <> ":"
    <> source

-- | Post-process raw JavaScript using 'defaultCompilerConfig' (passthrough
-- by default — no external minifier). Set 'configBackend' to 'Esbuild',
-- 'Terser', or 'Auto' for an extra shrink pass.
--
-- Does __not__ wrap the input in an IIFE: a bare expression with no side
-- effects may be DCE'd to empty by esbuild/Terser. Prefer 'compilePure' /
-- 'compileEffect' for JShark output. Empty minifier output on non-empty
-- input is treated as failure.
compileJS :: Text -> IO Text
compileJS = compileWith defaultCompilerConfig

compileJSPure :: Text -> IO Text
compileJSPure = compileWithPure defaultCompilerConfig

-- | Same as 'compileJS'; minify has no progress output ('*IO' symmetry).
compileJSIO :: Text -> IO Text
compileJSIO = compileJS

-- | Minify using 'tryCompileWith'. On 'Left', either throw or (when
-- 'configFallback' is set) log to stderr and return the original source.
compileWith :: CompilerConfig -> Text -> IO Text
compileWith cfg source =
  runEff $ CR.runCompileReportSilent (compileWithEff cfg source)

compileWithPure :: CompilerConfig -> Text -> IO Text
compileWithPure cfg source =
  runEff $ CR.runCompileReportSilent (compileWithEff (quietCfg cfg) source)

-- | Same as 'compileWith'; minify has no progress output ('*IO' symmetry).
compileWithIO :: CompilerConfig -> Text -> IO Text
compileWithIO = compileWith

compileWithEff ::
  (CR.CompileReport :> es, IOE :> es) =>
  CompilerConfig
  -> Text
  -> Eff es Text
compileWithEff cfg source = do
  res <- tryCompileWithEff cfg source
  case res of
    Right out -> pure out
    Left err
      | configFallback cfg -> do
          unless (configQuiet cfg) (CR.logFallback err)
          pure source
      | otherwise ->
          liftIO (throwIO (userError ("JShark.Compiler: compilation failed: " ++ err)))

-- | Minify without fallback or throwing on minifier failure.
-- Cache is consulted only for successful results (fallback source is
-- never stored). 'Readable' forces 'Passthrough' so a minifying backend
-- cannot run.
tryCompileWith :: CompilerConfig -> Text -> IO (Either String Text)
tryCompileWith cfg source = runEff (tryCompileWithEff cfg source)

-- | Like 'tryCompileWith' with 'quietCfg' (for API symmetry).
tryCompileWithPure :: CompilerConfig -> Text -> IO (Either String Text)
tryCompileWithPure cfg source = tryCompileWith (quietCfg cfg) source

-- | Same as 'tryCompileWith'; try-compile never emits progress output.
tryCompileWithIO :: CompilerConfig -> Text -> IO (Either String Text)
tryCompileWithIO = tryCompileWith

tryCompileWithEff ::
  IOE :> es =>
  CompilerConfig
  -> Text
  -> Eff es (Either String Text)
tryCompileWithEff cfg0 source =
  let
    cfg = styleConfig cfg0
   in
    case configCache cfg of
      NoCache -> tryRunCompileEff cfg source
      MemoryCache -> tryRunCompileEff cfg source
      DiskCache dir -> do
        liftIO $ createDirectoryIfMissing True dir
        let
          key = cacheKey cfg source
          cacheFile = dir </> (hashText key ++ ".js")
        loaded <- liftIO $ loadDiskCache cacheFile key
        case loaded of
          Just cached -> pure (Right cached)
          Nothing ->
            compileAndStoreEff (tryRunCompileEff cfg source) $ \out -> do
              liftIO $ atomicWriteFile cacheFile (encodeDiskCache key out)
              pure (Right out)

tryRunCompileEff ::
  IOE :> es =>
  CompilerConfig
  -> Text
  -> Eff es (Either String Text)
tryRunCompileEff cfg source = liftIO (tryRunCompile cfg source)

compileAndStoreEff ::
  Eff es (Either String Text)
  -> (Text -> Eff es (Either String Text))
  -> Eff es (Either String Text)
compileAndStoreEff compile persist = do
  compiled <- compile
  case compiled of
    Right out -> persist out
    left -> pure left

encodeDiskCache :: Text -> Text -> BS.ByteString
encodeDiskCache key compiled =
  let
    keyBs = TE.encodeUtf8 key
    lenLine = TE.encodeUtf8 (T.pack (show (BS.length keyBs) <> "\n"))
   in
    diskCacheMagic <> lenLine <> keyBs <> TE.encodeUtf8 compiled

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
  let
    (lenBs, rest) = BC.break (== '\n') rest0
  rest1 <- BS.stripPrefix "\n" rest
  n <- readMaybe (BC.unpack lenBs)
  guard (n >= 0 && BS.length rest1 >= n)
  let
    (keyBs, body) = BS.splitAt n rest1
  key <- either (const Nothing) Just (TE.decodeUtf8' keyBs)
  compiled <- either (const Nothing) Just (TE.decodeUtf8' body)
  pure (key, compiled)

atomicWriteFile :: FilePath -> BS.ByteString -> IO ()
atomicWriteFile dest bytes = do
  let
    dir = takeDirectory dest
  createDirectoryIfMissing True dir
  (tmp, h) <- openBinaryTempFile dir "jshark-cache.tmp"
  let
    cleanupTmp = removeFile tmp `catch` (\(_ :: IOException) -> pure ())
    closeH = hClose h `catch` (\(_ :: IOException) -> pure ())
  ( do
      BS.hPut h bytes
      hClose h
      removeFile dest `catch` (\(_ :: IOException) -> pure ())
      renameFile tmp dest
    )
    `catch` (\(e :: SomeException) -> closeH >> cleanupTmp >> throwIO e)

-- | Minify with Google Closure Compiler at the given level.
-- Throws if the compiler is missing or fails ('configFallback' is false).
compileClosure :: ClosureLevel -> Text -> IO Text
compileClosure lvl = compileWith (closureCompilerConfig lvl)

compileClosurePure :: ClosureLevel -> Text -> IO Text
compileClosurePure lvl = compileWithPure (closureCompilerConfig lvl)

-- | Same as 'compileClosure'; minify has no progress output.
compileClosureIO :: ClosureLevel -> Text -> IO Text
compileClosureIO = compileClosure

-- | Minify with esbuild. Throws if esbuild is missing or fails.
compileEsbuild :: Text -> IO Text
compileEsbuild = compileWith esbuildCompilerConfig

compileEsbuildPure :: Text -> IO Text
compileEsbuildPure = compileWithPure esbuildCompilerConfig

-- | Same as 'compileEsbuild'; minify has no progress output.
compileEsbuildIO :: Text -> IO Text
compileEsbuildIO = compileEsbuild

-- | Minify with Terser. Throws if terser is missing or fails.
compileTerser :: Text -> IO Text
compileTerser = compileWith terserCompilerConfig

compileTerserPure :: Text -> IO Text
compileTerserPure = compileWithPure terserCompilerConfig

-- | Same as 'compileTerser'; minify has no progress output.
compileTerserIO :: Text -> IO Text
compileTerserIO = compileTerser

-- | Indent generated JavaScript. Understands double/single-quoted strings
-- (with backslash escapes). Regexes are emitted as @new RegExp(\"…\")@,
-- not @/re/@ literals, so those are not treated as strings. Not a general
-- JS parser.
prettyJS :: Text -> Text
prettyJS = renderJS . formatJS . T.strip

formatJS :: Text -> JS
formatJS = go 0
 where
  indentLevels :: [JS]
  indentLevels = take 128 $ iterate (<> "  ") mempty

  indent n = indentLevels !! min n (length indentLevels - 1)

  go :: Int -> Text -> JS
  go _ t | T.null t = mempty
  go n t =
    case T.uncons t of
      Nothing -> mempty
      Just ('"', xs) -> TB.char '"' <> string '"' xs (go n)
      Just ('\'', xs) -> TB.char '\'' <> string '\'' xs (go n)
      Just ('{', xs) ->
        let
          xs' = T.dropWhile isSpace xs
         in
          case T.uncons xs' of
            Just ('}', rest) -> "{}" <> afterClose n rest
            _ -> TB.char '{' <> TB.char '\n' <> indent (n + 1) <> go (n + 1) xs'
      Just ('}', xs) ->
        let
          n' = max 0 (n - 1)
         in
          TB.char '\n'
            <> indent n'
            <> TB.char '}'
            <> afterClose n' (T.dropWhile isSpace xs)
      Just (';', xs) ->
        let
          xs' = T.dropWhile isSpace xs
         in
          TB.char ';' <> case T.uncons xs' of
            Just ('}', _) -> go n xs'
            Nothing -> mempty
            _ -> TB.char '\n' <> indent n <> go n xs'
      Just (c, xs) ->
        if isSpace c
          then
            let
              xs' = T.dropWhile isSpace xs
             in
              case T.uncons xs' of
                Nothing -> mempty
                Just ('}', _) -> go n xs'
                _ -> TB.char ' ' <> go n xs'
          else TB.char c <> go n xs

  afterClose n s =
    let
      t' = T.dropWhile isSpace s
     in
      case keyword "else" t' of
        Just rest -> TB.char ' ' <> go n ("else" <> rest)
        Nothing ->
          case keyword "catch" t' of
            Just rest -> TB.char ' ' <> go n ("catch" <> rest)
            Nothing ->
              case T.uncons t' of
                Nothing -> mempty
                Just (c, _) | c `elem` (");,.}(" :: String) -> go n t'
                Just _ -> TB.char '\n' <> indent n <> go n t'

  keyword kw s =
    let
      (pre, rest) = T.splitAt (T.length kw) s
     in
      if pre == kw && not (startsIdent rest)
        then Just rest
        else Nothing

  startsIdent t = case T.uncons t of
    Just (c, _) -> isAlphaNum c || c == '_' || c == '$'
    Nothing -> False

  string q t k = case T.uncons t of
    Nothing -> mempty
    Just (c, cs) ->
      if c == '\\'
        then case T.uncons cs of
          Just (d, ds) -> TB.char c <> TB.char d <> string q ds k
          Nothing -> TB.char c
        else
          if c == q
            then TB.char c <> k cs
            else TB.char c <> string q cs k

-- | Compile an effectful JShark computation. 'Readable' emits a pretty
-- snippet (no IIFE, no minifier); 'Minified' wraps an IIFE then minifies.
--
-- Batch slot phases use 'JShark.CompileProgress' directly (see
-- 'JShark.CompileReport').
compileTreeEff ::
  (CR.CompileReport :> es, IOE :> es) =>
  CompilerConfig
  -> (OutputStyle -> JS)
  -> Eff es Text
compileTreeEff cfg doc = do
  let
    !style = configStyle cfg
  tCodegen0 <- liftIO getMonotonicTime
  let
    !js = renderJSCompact (doc style)
  tCodegen1 <- liftIO getMonotonicTime
  liftIO $ CP.recordJobCodegenSec (seconds tCodegen0 tCodegen1)
  -- Batch slot ticks bypass 'CompileReport'; see 'JShark.CompileReport'.
  liftIO CP.finishEmitPhase
  let
    postCfg = styleConfig cfg
  out <- case configBackend postCfg of
    Passthrough -> pure js
    _ -> do
      case configProgressSlot cfg of
        Nothing -> pure ()
        Just slot -> liftIO $ CP.reportJobPhase slot CP.PhaseMinify 0 1
      tMin0 <- liftIO getMonotonicTime
      minified <- compileWithEff postCfg js
      tMin1 <- liftIO getMonotonicTime
      liftIO $ CP.recordJobMinifySec (seconds tMin0 tMin1)
      case configProgressSlot cfg of
        Nothing -> pure ()
        Just slot -> liftIO $ CP.reportJobPhase slot CP.PhaseMinify 1 1
      pure minified
  liftIO $ CP.recordJobJsBytes (T.length out)
  liftIO $ forceCompiled (finishStyle style out)

compileEffect :: CompilerConfig -> ClosedEffect u -> IO Text
compileEffect cfg eff =
  runEff $
    CR.runCompileReportFromConfig (configProgress cfg) (compileEffectEff cfg eff)

compileEffectPure :: CompilerConfig -> ClosedEffect u -> IO Text
compileEffectPure cfg eff =
  runEff $ CR.runCompileReportSilent (compileEffectEff (quietCfg cfg) eff)

compileEffectIO :: CompilerConfig -> ClosedEffect u -> IO Text
compileEffectIO cfg eff =
  runEff $
    CR.runCompileReportIO (compileEffectEff (cfg {configProgress = True}) eff)

compileEffectEff ::
  (CR.CompileReport :> es, IOE :> es) =>
  CompilerConfig
  -> ClosedEffect u
  -> Eff es Text
compileEffectEff cfg eff = do
  start <- liftIO getCPUTime
  liftIO $ CP.recordJobForm (compileForm cfg)
  tLint0 <- liftIO getMonotonicTime
  -- Batch slot ticks bypass 'CompileReport'; see 'JShark.CompileReport'.
  case configProgressSlot cfg of
    Just slot -> liftIO $ CP.reportJobPhase slot CP.PhaseLint 0 1
    Nothing -> pure ()
  when (configWarnHvm2Candidates cfg) $
    liftIO (warnHvm2CandidatesEffect eff)
  case configProgressSlot cfg of
    Just slot -> liftIO $ CP.reportJobPhase slot CP.PhaseLint 1 1
    Nothing -> pure ()
  tLint1 <- liftIO getMonotonicTime
  liftIO $ CP.recordJobLintSec (seconds tLint0 tLint1)
  out <- compileTreeEff cfg (`effectDoc` eff)
  end <- liftIO getCPUTime
  CR.drawSingleDone (CR.picosecondsToSecs (end - start))
  pure out

-- | Compile a pure JShark expression. Never draws progress bars.
compilePure :: CompilerConfig -> ClosedExpr u -> IO Text
compilePure = compilePurePure

compilePurePure :: CompilerConfig -> ClosedExpr u -> IO Text
compilePurePure cfg e =
  runEff $ CR.runCompileReportSilent (compilePureEff (quietCfg cfg) e)

-- | Same as 'compilePurePure'; pure JShark never draws progress (API symmetry).
compilePureIO :: CompilerConfig -> ClosedExpr u -> IO Text
compilePureIO = compilePurePure

compilePureEff ::
  (CR.CompileReport :> es, IOE :> es) =>
  CompilerConfig
  -> ClosedExpr u
  -> Eff es Text
compilePureEff cfg e = do
  -- Batch slot ticks bypass 'CompileReport'; see 'JShark.CompileReport'.
  case configProgressSlot cfg of
    Just slot -> liftIO $ CP.reportJobPhase slot CP.PhaseLint 0 1
    Nothing -> pure ()
  when (configWarnHvm2Candidates cfg) $
    liftIO (warnHvm2CandidatesExpr e)
  case configProgressSlot cfg of
    Just slot -> liftIO $ CP.reportJobPhase slot CP.PhaseLint 1 1
    Nothing -> pure ()
  compileTreeEff cfg (`pureDoc` e)

-- | Compile many effectful programs concurrently (one capability per item).
-- When 'configProgress' is set, prints a live progress bar and total time.
compileEffects ::
  CompilerConfig -> [ClosedEffect u] -> IO [Text]
compileEffects cfg effs =
  compileEffectsLabeled cfg (zipWith numberedEffectJob ([1 ..] :: [Int]) effs)

compileEffectsPure :: CompilerConfig -> [ClosedEffect u] -> IO [Text]
compileEffectsPure cfg effs =
  compileEffectsLabeledPure cfg (zipWith numberedEffectJob ([1 ..] :: [Int]) effs)

compileEffectsIO :: CompilerConfig -> [ClosedEffect u] -> IO [Text]
compileEffectsIO cfg effs =
  compileEffectsLabeledIO cfg (zipWith numberedEffectJob ([1 ..] :: [Int]) effs)

-- | Like 'compileEffects' but labels each job on the progress bar.
compileEffectsLabeled ::
  CompilerConfig -> [(Text, ClosedEffect u)] -> IO [Text]
compileEffectsLabeled cfg jobs =
  runEff $
    CR.runCompileReportFromConfig
      (configProgress cfg)
      (compileBatchEff cfg compileEffectEff jobs)

compileEffectsLabeledPure ::
  CompilerConfig -> [(Text, ClosedEffect u)] -> IO [Text]
compileEffectsLabeledPure cfg jobs =
  runEff $
    CR.runCompileReportSilent (compileBatchEff (quietCfg cfg) compileEffectEff jobs)

compileEffectsLabeledIO ::
  CompilerConfig -> [(Text, ClosedEffect u)] -> IO [Text]
compileEffectsLabeledIO cfg jobs =
  runEff $
    CR.runCompileReportIO
      (compileBatchEff (cfg {configProgress = True}) compileEffectEff jobs)

-- | Mixed-config batch compile. When 'configProgress' is enabled, draws a
-- progress bar, prints per-job compile stats, and returns those stats.
compileJobsLabeled ::
  CompilerConfig
  -> [(Text, CompilerConfig, ClosedEffect u)]
  -> IO ([Text], [CompileJobStats])
compileJobsLabeled cfg jobs =
  runEff $
    CR.runCompileReportFromConfig
      (configProgress cfg)
      (compileMixedBatchEff cfg jobs)

compileJobsLabeledPure ::
  CompilerConfig
  -> [(Text, CompilerConfig, ClosedEffect u)]
  -> IO ([Text], [CompileJobStats])
compileJobsLabeledPure cfg jobs =
  runEff $
    CR.runCompileReportSilent (compileMixedBatchEff (quietCfg cfg) jobs)

compileJobsLabeledIO ::
  CompilerConfig
  -> [(Text, CompilerConfig, ClosedEffect u)]
  -> IO ([Text], [CompileJobStats])
compileJobsLabeledIO cfg jobs =
  runEff $
    CR.runCompileReportIO
      (compileMixedBatchEff (cfg {configProgress = True}) jobs)

-- | Compile many pure programs concurrently. Never draws progress bars.
compilePures :: CompilerConfig -> [ClosedExpr u] -> IO [Text]
compilePures cfg exprs =
  compilePuresLabeled cfg (zipWith numberedPureJob ([1 ..] :: [Int]) exprs)

compilePuresPure :: CompilerConfig -> [ClosedExpr u] -> IO [Text]
compilePuresPure cfg exprs =
  compilePuresLabeledPure cfg (zipWith numberedPureJob ([1 ..] :: [Int]) exprs)

compilePuresIO :: CompilerConfig -> [ClosedExpr u] -> IO [Text]
compilePuresIO cfg exprs =
  compilePuresLabeledIO cfg (zipWith numberedPureJob ([1 ..] :: [Int]) exprs)

-- | Like 'compilePures' but labels each job on the progress bar.
compilePuresLabeled ::
  CompilerConfig -> [(Text, ClosedExpr u)] -> IO [Text]
compilePuresLabeled cfg jobs =
  runEff $
    CR.runCompileReportSilent (compileBatchEff (quietCfg cfg) compilePureEff jobs)

compilePuresLabeledPure ::
  CompilerConfig -> [(Text, ClosedExpr u)] -> IO [Text]
compilePuresLabeledPure cfg jobs = compilePuresLabeled cfg jobs

-- | Same as 'compilePuresLabeledPure'; pure JShark never draws progress.
compilePuresLabeledIO ::
  CompilerConfig -> [(Text, ClosedExpr u)] -> IO [Text]
compilePuresLabeledIO cfg jobs = compilePuresLabeled cfg jobs

type CompileEff = '[CR.CompileReport, IOE]

compileBatchEff ::
  CompilerConfig
  -> (CompilerConfig -> item -> Eff CompileEff Text)
  -> [(Text, item)]
  -> Eff CompileEff [Text]
compileBatchEff cfg compileOne jobs
  | configProgress cfg =
      compileBatchProgressEff cfg (compileOneIO compileOne) jobs
  | otherwise =
      liftIO $ mapConcurrently (\(_, item) -> compileOneIO compileOne cfg item) jobs
 where
  compileOneIO ::
    (CompilerConfig -> item -> Eff CompileEff Text)
    -> CompilerConfig
    -> item
    -> IO Text
  compileOneIO run c item =
    runEff $ CR.runCompileReportSilent $ run c item

compileBatchProgressEff ::
  CompilerConfig
  -> (CompilerConfig -> item -> IO Text)
  -> [(Text, item)]
  -> Eff CompileEff [Text]
compileBatchProgressEff cfg compileOneIO jobs = do
  let
    total = length jobs
  (results, stats, secs) <-
    liftIO $
      batchProgressLabeledIO
        cfg
        total
        ( \slot _label item ->
            compileOneIO (quietCfg cfg {configProgressSlot = Just slot}) item
        )
        jobs
  CR.drawBatchDone total secs
  CR.drawBatchStats secs stats
  pure results

compileMixedBatchEff ::
  CompilerConfig
  -> [(Text, CompilerConfig, ClosedEffect u)]
  -> Eff CompileEff ([Text], [CompileJobStats])
compileMixedBatchEff baseCfg jobs
  | configProgress baseCfg = do
      let
        total = length jobs
      (results, stats, secs) <-
        liftIO $ batchProgressMixedIO baseCfg jobs
      CR.drawBatchDone total secs
      CR.drawBatchStats secs stats
      pure (results, stats)
  | otherwise = do
      results <-
        liftIO $
          mapConcurrently
            ( \(_label, jobCfg, eff) ->
                compileEffectPure (mergeJobConfig baseCfg jobCfg) eff
            )
            jobs
      pure (results, [])

batchProgressMixedIO ::
  CompilerConfig
  -> [(Text, CompilerConfig, ClosedEffect u)]
  -> IO ([Text], [CompileJobStats], Double)
batchProgressMixedIO baseCfg jobs =
  batchProgressCore
    (length jobs)
    ( map
        ( \(label, jobCfg, eff) ->
            ( label
            , \slot ->
                compileEffectPure
                  (mergeJobConfig baseCfg jobCfg {configProgressSlot = Just slot})
                  eff
            )
        )
        jobs
    )

batchProgressLabeledIO ::
  CompilerConfig
  -> Int
  -> (Int -> Text -> job -> IO Text)
  -> [(Text, job)]
  -> IO ([Text], [CompileJobStats], Double)
batchProgressLabeledIO _cfg total compileOne labeledJobs =
  batchProgressCore
    total
    [ (label, \slot -> compileOne slot label job)
    | (label, job) <- labeledJobs
    ]

batchProgressCore ::
  Int
  -> [(Text, Int -> IO Text)]
  -> IO ([Text], [CompileJobStats], Double)
batchProgressCore total jobs = do
  start <- getCPUTime
  board <- CP.newProgressBoard total
  CP.setProgressBoardHandle board
  styleIO <- CR.progressStyleIO
  lineCount <- newCounter 0
  let
    refresh = do
      fdMode <- CP.progressFdActive
      b <- CP.readProgressBoard board
      if fdMode
        then CP.emitProgressBoard b
        else do
          prev <- readCounter lineCount
          let
            block = CP.renderBatchProgress styleIO b prev
            lineCount' =
              1
                + length
                  [ ()
                  | j <- V.toList (CP.pbJobs b)
                  , not (CP.jpDone j)
                  , not (T.null (CP.jpLabel j))
                  ]
          writeCounter lineCount lineCount'
          CR.writeProgressLine block
  CP.setProgressRedraw refresh
  indexed <-
    ( mapConcurrently
        ( \(slot, (label, compile)) -> do
            tJob0 <- getMonotonicTime
            CP.initJob board slot label
            CP.withProgressIO refresh
            out <- CP.withActiveJob slot board $ compile slot
            tJob1 <- getMonotonicTime
            jobStats <-
              CP.snapshotJobStatsFromSlot board slot label (seconds tJob0 tJob1)
            CP.markJobDone board slot
            CP.withProgressIO refresh
            pure (slot, out, jobStats)
        )
        (zip ([0 ..] :: [Int]) jobs)
    )
      `finally` do
        CP.clearProgressRedraw
        CP.clearProgressBoardHandle
  end <- getCPUTime
  let
    sorted = sortOn (\(s, _, _) -> s) indexed
  pure
    ( map (\(_, out, _) -> out) sorted
    , map (\(_, _, st) -> st) sorted
    , CR.picosecondsToSecs (end - start)
    )

-- | Banner-before-serve only means JS is ready if this ran.
forceCompiled :: Text -> IO Text
forceCompiled t = t <$ evaluate (T.length t)

finishStyle :: OutputStyle -> Text -> Text
finishStyle Readable = prettyJS
finishStyle Minified = id

styleConfig :: CompilerConfig -> CompilerConfig
styleConfig cfg = case configStyle cfg of
  Readable -> cfg {configBackend = Passthrough}
  Minified -> cfg

pureDoc :: OutputStyle -> ClosedExpr u -> JS
pureDoc Readable e = pureAST e
pureDoc Minified e = pureProgram e

effectDoc :: OutputStyle -> ClosedEffect u -> JS
effectDoc Readable e = effectfulAST e
effectDoc Minified e = effectfulProgram e

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
runAuto source = go probes
 where
  probes =
    [ (hasExecutable "esbuild", runEsbuild defaultEsbuildConfig source)
    ,
      ( (||)
          <$> hasExecutable "google-closure-compiler"
          <*> hasExecutable "closure-compiler"
      , runClosure defaultClosureConfig source
      )
    , (hasExecutable "terser", runTerser defaultTerserConfig source)
    ]
  go [] =
    pure
      (Left "no minifier on PATH (install esbuild, google-closure-compiler, or terser)")
  go ((check, run) : rest) = do
    ok <- check
    if ok then run else go rest

hasExecutable :: String -> IO Bool
hasExecutable name = isJust <$> findExecutable name

runEsbuild :: CompilerEsbuildConfig -> Text -> IO (Either String Text)
runEsbuild cfg source = do
  found <- lookupNamedTool "esbuild"
  let
    args =
      ["--loader=js", "--log-level=error"]
        ++ ["--minify" | esbuildMinify cfg]
        ++ maybe [] (\t -> ["--target=" ++ t]) (esbuildTarget cfg)
        ++ esbuildExtraArgs cfg
  case found of
    Left err -> pure (Left err)
    Right (exe, wrap) -> run exe (wrap args)
 where
  -- A pure IIFE (or bare expression) is an unused statement to esbuild, so
  -- '--minify' can DCE it to empty — especially after constant folding turns
  -- 'return 1+2' into 'return 3'. Re-run as 'export default (…)' / ESM so the
  -- value is live, then strip the export to leave an expression again.
  run exe args = do
    first <- executeProcessRaw exe args source
    case first of
      Left err -> pure (Left err)
      Right out
        | not (T.null out) || T.null (T.strip source) -> pure (Right out)
        | otherwise -> do
            let
              wrapped = "export default (" <> dropTrailingSemis (T.strip source) <> ")\n"
              args' = args ++ ["--format=esm"]
            second <- executeProcessRaw exe args' wrapped
            case second of
              Left err -> pure (Left err)
              Right out' ->
                let
                  stripped = stripExportDefault out'
                 in
                  if T.null stripped
                    then
                      pure
                        ( Left
                            "minifier produced empty output (possible DCE of a bare expression; use compilePure/compileEffect)"
                        )
                    else pure (Right stripped)

dropTrailingSemis :: Text -> Text
dropTrailingSemis =
  T.dropWhileEnd
    (\c -> c == ';' || c == ' ' || c == '\n' || c == '\r' || c == '\t')

-- | Undo the 'export default (…)' / ESM anchor. esbuild '--minify' often
-- rewrites 'export default EXPR' to 'var e=EXPR;export{e as default};'.
stripExportDefault :: Text -> Text
stripExportDefault t =
  let
    t' = T.strip t
   in
    case stripVarAsDefault t' of
      Just v -> v
      Nothing -> case T.stripPrefix "export default" t' of
        Just rest -> dropTrailingSemis (T.strip rest)
        Nothing -> t'

-- | 'var name=VALUE;export{name as default};' → VALUE
stripVarAsDefault :: Text -> Maybe Text
stripVarAsDefault t = do
  afterVar <- T.stripPrefix "var " t
  let
    (name0, rest0) = T.break (== '=') afterVar
  rest1 <- T.stripPrefix "=" rest0
  let
    name = T.strip name0
    suffix = ";export{" <> name <> " as default}"
    body = T.strip rest1
  case T.stripSuffix (suffix <> ";") body of
    Just v -> Just (dropTrailingSemis (T.strip v))
    Nothing -> fmap (dropTrailingSemis . T.strip) (T.stripSuffix suffix body)

runClosure :: CompilerClosureConfig -> Text -> IO (Either String Text)
runClosure cfg source = do
  mJar <- lookupEnv "CLOSURE_COMPILER_JAR"
  mDirect <- findExecutable "google-closure-compiler"
  mClosure <- findExecutable "closure-compiler"
  mNpx <- findExecutable "npx"
  let
    args =
      ["--compilation_level", closureLevelString (closureLevel cfg)]
        ++ closureExtraArgs cfg
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
        executeProcess
          npxExe
          (["--no-install", "google-closure-compiler"] ++ args)
          source
      (Nothing, Nothing, Nothing) -> pure (Left "Google Closure Compiler not found on PATH")

lookupNamedTool :: String -> IO (Either String (FilePath, [String] -> [String]))
lookupNamedTool name = do
  mDirect <- findExecutable name
  mNpx <- findExecutable "npx"
  case (mDirect, mNpx) of
    (Just exe, _) -> pure (Right (exe, id))
    (Nothing, Just npxExe) ->
      pure (Right (npxExe, (["--no-install", name] ++)))
    (Nothing, Nothing) ->
      pure (Left (name ++ " executable not found on PATH"))

runNamedTool :: String -> [String] -> Text -> IO (Either String Text)
runNamedTool name args source = do
  found <- lookupNamedTool name
  case found of
    Left err -> pure (Left err)
    Right (exe, wrap) -> executeProcess exe (wrap args) source

runTerser :: CompilerTerserConfig -> Text -> IO (Either String Text)
runTerser cfg source =
  runNamedTool
    "terser"
    ( ["--compress" | terserCompress cfg]
        ++ ["--mangle" | terserMangle cfg]
        ++ terserExtraArgs cfg
    )
    source

executeProcess :: FilePath -> [String] -> Text -> IO (Either String Text)
executeProcess cmd args source = do
  res <- executeProcessRaw cmd args source
  case res of
    Right out
      | T.null out && not (T.null (T.strip source)) ->
          pure
            ( Left
                "minifier produced empty output (possible DCE of a bare expression; use compilePure/compileEffect)"
            )
    _ -> pure res

-- | Like 'executeProcess' but allows empty stdout (used when esbuild may DCE
-- a pure expression and we want to retry with an ESM export anchor).
executeProcessRaw :: FilePath -> [String] -> Text -> IO (Either String Text)
executeProcessRaw cmd args source =
  ( do
      let
        pConfig =
          setStdin (byteStringInput (BL.fromStrict (TE.encodeUtf8 source)))
            $ setStdout createPipe
            $ setStderr createPipe
            $ proc cmd args
      withProcessWait pConfig $ \p -> do
        (code, outBs, errBs) <-
          withAsync (drainHandle (getStdout p)) $ \outA ->
            withAsync (drainHandle (getStderr p)) $ \errA -> do
              exitCode <- waitExitCode p
              outBs' <- wait outA
              errBs' <- wait errA
              pure (exitCode, outBs', errBs')
        case code of
          ExitSuccess -> pure (Right (T.strip (TE.decodeUtf8 outBs)))
          ExitFailure c ->
            pure
              ( Left
                  ( if BS.null errBs
                      then "Process exited with code " ++ show c
                      else BC.unpack errBs
                  )
              )
  )
    `catch` (\(e :: SomeException) -> pure (Left (show e)))

-- | Drain a process pipe; minifier stdout/stderr are small enough to hold.
drainHandle :: Handle -> IO BS.ByteString
drainHandle h = Q.toStrict_ (Q.hGetContents h)

compileForm :: CompilerConfig -> CompileForm
compileForm cfg = case configStyle cfg of
  Readable -> FormReadable
  Minified -> FormMinified

mergeJobConfig :: CompilerConfig -> CompilerConfig -> CompilerConfig
mergeJobConfig base job =
  job
    { configProgress = configProgress base
    , configWarnHvm2Candidates =
        configWarnHvm2Candidates base || configWarnHvm2Candidates job
    , configQuiet = True
    }

-- | Recognized compiler CLI flags (for example servers and build tools).
isCompilerFlag :: String -> Bool
isCompilerFlag = \case
  "--warn-hvm2-candidates" -> True
  "--progress" -> True
  _ -> False

-- | Apply recognized CLI flags to a 'CompilerConfig'.
applyCompilerArgs :: [String] -> CompilerConfig -> CompilerConfig
applyCompilerArgs args cfg =
  foldl' applyCompilerArg cfg args

applyCompilerArg :: CompilerConfig -> String -> CompilerConfig
applyCompilerArg cfg = \case
  "--warn-hvm2-candidates" -> cfg {configWarnHvm2Candidates = True}
  "--progress" -> cfg {configProgress = True}
  _ -> cfg
