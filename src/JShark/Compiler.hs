{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Post-process generated JavaScript: pretty-print or minify, and cache.
--
-- Google Closure Compiler is no longer the best default. Its Advanced
-- mode is still unique for whole-program property renaming, but it is a
-- Java tool, slow to start, and unsafe on JShark FFI without externs.
-- In 2026 the practical choice is [esbuild](https://esbuild.github.io)
-- (fast, ubiquitous, what this module uses for 'Auto'). Terser remains
-- as the more aggressive size-oriented option; Closure is still available
-- when you actually want Advanced.
--
-- Use 'readableConfig' for a human-readable snippet (single-use bindings
-- inlined, no IIFE, no minifier). 'defaultCompilerConfig' wraps an IIFE
-- and minifies.
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
  , compileWith
  , tryCompileWith
  , compileClosure
  , compileEsbuild
  , compileTerser
  , compileEffect
  , compilePure
  , prettyJS

    -- * Cache
  , clearCompilerCache
  )
where

import Control.Exception (IOException, SomeException, catch, evaluate, throwIO)
import Control.Monad (guard)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAlphaNum, isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import JShark
  ( effectfulAST
  , effectfulProgram
  , pureAST
  , pureProgram
  , renderJSCompact
  )
import JShark.Types (ClosedEffect, ClosedExpr)
import Numeric (showHex)
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
import System.IO (hClose, hPutStrLn, openBinaryTempFile, stderr)
import System.Process (readProcessWithExitCode)
import Prettyprinter (Doc)
import Text.Read (readMaybe)

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
  }
  deriving (Show, Eq, Ord)

-- | Auto backend, minified output, no in-process cache, fall back to the
-- unminified source if no minifier is installed (or if it crashes).
defaultCompilerConfig :: CompilerConfig
defaultCompilerConfig = CompilerConfig Auto NoCache True Minified

-- | Skip minification entirely. Useful in tests of the IIFE wrapper.
passthroughConfig :: CompilerConfig
passthroughConfig = CompilerConfig Passthrough NoCache False Minified

-- | Human-readable JS: assignment elimination, no minifier, no IIFE.
readableConfig :: CompilerConfig
readableConfig = CompilerConfig Passthrough NoCache False Readable

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
-- never stored). 'Readable' forces 'Passthrough' so a minifying backend
-- cannot run.
tryCompileWith :: CompilerConfig -> Text -> IO (Either String Text)
tryCompileWith cfg0 source =
  let
    cfg = styleConfig cfg0
   in
    case configCache cfg of
      NoCache -> tryRunCompile cfg source
      MemoryCache -> tryRunCompile cfg source
      DiskCache dir -> do
        createDirectoryIfMissing True dir
        let
          key = cacheKey cfg source
          cacheFile = dir </> (hashText key ++ ".js")
        loaded <- loadDiskCache cacheFile key
        case loaded of
          Just cached -> pure (Right cached)
          Nothing ->
            compileAndStore (tryRunCompile cfg source) $ \out -> do
              atomicWriteFile cacheFile (encodeDiskCache key out)
              pure (Right out)

compileAndStore ::
  IO (Either String Text)
  -> (Text -> IO (Either String Text))
  -> IO (Either String Text)
compileAndStore compile persist = do
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
compileClosure lvl =
  compileWith
    ( CompilerConfig
        (Closure (CompilerClosureConfig lvl []))
        MemoryCache
        False
        Minified
    )

-- | Minify with esbuild. Throws if esbuild is missing or fails.
compileEsbuild :: Text -> IO Text
compileEsbuild =
  compileWith
    (CompilerConfig (Esbuild defaultEsbuildConfig) MemoryCache False Minified)

-- | Minify with Terser. Throws if terser is missing or fails.
compileTerser :: Text -> IO Text
compileTerser =
  compileWith
    (CompilerConfig (Terser defaultTerserConfig) MemoryCache False Minified)

-- | Indent generated JavaScript. Understands double/single-quoted strings
-- (with backslash escapes). Regexes are emitted as @new RegExp(\"…\")@,
-- not @/re/@ literals, so those are not treated as strings. Not a general
-- JS parser.
prettyJS :: Text -> Text
prettyJS = formatJS . T.strip

formatJS :: Text -> Text
formatJS = TL.toStrict . TB.toLazyText . go 0
 where
  indent n = TB.fromString (replicate (n * 2) ' ')

  go :: Int -> Text -> TB.Builder
  go _ t | T.null t = mempty
  go n t =
    case T.uncons t of
      Nothing -> mempty
      Just ('"', xs) -> TB.singleton '"' <> string '"' xs (go n)
      Just ('\'', xs) -> TB.singleton '\'' <> string '\'' xs (go n)
      Just ('{', xs) ->
        let
          xs' = T.dropWhile isSpace xs
         in
          case T.uncons xs' of
            Just ('}', rest) -> "{}" <> afterClose n rest
            _ -> TB.singleton '{' <> TB.singleton '\n' <> indent (n + 1) <> go (n + 1) xs'
      Just ('}', xs) ->
        let
          n' = max 0 (n - 1)
         in
          TB.singleton '\n' <> indent n' <> TB.singleton '}' <> afterClose n' (T.dropWhile isSpace xs)
      Just (';', xs) ->
        let
          xs' = T.dropWhile isSpace xs
         in
          TB.singleton ';' <> case T.uncons xs' of
            Just ('}', _) -> go n xs'
            Nothing -> mempty
            _ -> TB.singleton '\n' <> indent n <> go n xs'
      Just (c, xs) ->
        if isSpace c
          then
            let
              xs' = T.dropWhile isSpace xs
             in
              case T.uncons xs' of
                Nothing -> mempty
                Just ('}', _) -> go n xs'
                _ -> TB.singleton ' ' <> go n xs'
          else TB.singleton c <> go n xs

  afterClose n s =
    let
      t' = T.dropWhile isSpace s
     in
      case keyword "else" t' of
        Just rest -> TB.singleton ' ' <> go n ("else" <> rest)
        Nothing ->
          case keyword "catch" t' of
            Just rest -> TB.singleton ' ' <> go n ("catch" <> rest)
            Nothing ->
              case T.uncons t' of
                Nothing -> mempty
                Just (c, _) | c `elem` (");,.}(" :: String) -> go n t'
                Just _ -> TB.singleton '\n' <> indent n <> go n t'

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
          Just (d, ds) -> TB.singleton c <> TB.singleton d <> string q ds k
          Nothing -> TB.singleton c
        else if c == q
          then TB.singleton c <> k cs
          else TB.singleton c <> string q cs k

-- | Compile an effectful JShark computation. 'Readable' emits a pretty
-- snippet (no IIFE, no minifier); 'Minified' wraps an IIFE then minifies.
compileTree :: CompilerConfig -> (OutputStyle -> Doc ann) -> IO Text
compileTree cfg doc =
  forceCompiled
    =<< finishStyle (configStyle cfg)
      <$> compileWith
        cfg
        (renderJSCompact (doc (configStyle cfg)))

compileEffect :: CompilerConfig -> ClosedEffect u -> IO Text
compileEffect cfg eff = compileTree cfg (`effectDoc` eff)

-- | Compile a pure JShark expression. See 'compileEffect'.
compilePure :: CompilerConfig -> ClosedExpr u -> IO Text
compilePure cfg e = compileTree cfg (`pureDoc` e)

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

pureDoc :: OutputStyle -> ClosedExpr u -> Doc ann
pureDoc Readable e = pureAST e
pureDoc Minified e = pureProgram e

effectDoc :: OutputStyle -> ClosedEffect u -> Doc ann
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
      (code, stdoutStr, stderrStr) <-
        readProcessWithExitCode cmd args (T.unpack source)
      case code of
        ExitSuccess -> pure (Right (T.strip (T.pack stdoutStr)))
        ExitFailure c ->
          pure
            ( Left
                (if null stderrStr then "Process exited with code " ++ show c else stderrStr)
            )
  )
    `catch` (\(e :: SomeException) -> pure (Left (show e)))
