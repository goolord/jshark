{-# LANGUAGE
    BangPatterns
  , DataKinds
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
#-}
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
    CompilerConfig(..)
  , defaultCompilerConfig
  , passthroughConfig
  , readableConfig
  , CompilerBackend(..)
  , OutputStyle(..)
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
  , prettyJS

    -- * Cache
  , clearCompilerCache
  ) where

import Control.Exception (IOException, SomeException, catch, evaluate, throwIO)
import Control.Monad (guard)
import Data.Char (isAlphaNum, isSpace)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
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

import JShark (effectfulAST, effectfulProgram, pureAST, pureProgram, renderJSCompact)
import JShark.Types (ClosedEffect, ClosedExpr)
import Text.PrettyPrint (Doc)

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
  -- | When 'True', minifier failure (missing binary, crash, empty DCE'd
  -- output) logs to stderr and returns the unminified source. When
  -- 'False', 'compileWith' throws. Named helpers ('compileEsbuild' etc.)
  -- set this to 'False'.
  , configFallback :: Bool
  , configStyle :: OutputStyle
  } deriving (Show, Eq, Ord)

-- | Auto backend, minified output, in-memory cache, fall back to the
-- unminified source if no minifier is installed (or if it crashes).
defaultCompilerConfig :: CompilerConfig
defaultCompilerConfig = CompilerConfig Auto MemoryCache True Minified

-- | Skip minification entirely. Useful in tests of the IIFE wrapper.
passthroughConfig :: CompilerConfig
passthroughConfig = CompilerConfig Passthrough NoCache False Minified

-- | Human-readable JS: assignment elimination, no minifier, no IIFE.
readableConfig :: CompilerConfig
readableConfig = CompilerConfig Passthrough NoCache False Readable

cacheFormatVersion :: Text
cacheFormatVersion = "jshark-minify-2"

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
  cacheFormatVersion
  <> ":" <> T.pack (show (configBackend cfg))
  <> ":" <> T.pack (show (configStyle cfg))
  <> ":" <> source

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
-- never stored). 'Readable' forces 'Passthrough' so a minifying backend
-- cannot run.
tryCompileWith :: CompilerConfig -> Text -> IO (Either String Text)
tryCompileWith cfg0 source =
  let cfg = styleConfig cfg0
   in case configCache cfg of
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
  compileWith (CompilerConfig (Closure (CompilerClosureConfig lvl [])) MemoryCache False Minified)

-- | Minify with esbuild. Throws if esbuild is missing or fails.
compileEsbuild :: Text -> IO Text
compileEsbuild = compileWith (CompilerConfig (Esbuild defaultEsbuildConfig) MemoryCache False Minified)

-- | Minify with Terser. Throws if terser is missing or fails.
compileTerser :: Text -> IO Text
compileTerser = compileWith (CompilerConfig (Terser defaultTerserConfig) MemoryCache False Minified)

-- | Indent generated JavaScript. Understands double/single-quoted strings
-- (with backslash escapes). Regexes are emitted as @new RegExp(\"…\")@,
-- not @/re/@ literals, so those are not treated as strings. Not a general
-- JS parser.
prettyJS :: Text -> Text
prettyJS = T.pack . formatJS . T.unpack . T.strip

formatJS :: String -> String
formatJS = go 0
  where
    indent n = replicate (n * 2) ' '

    go :: Int -> String -> String
    go _ [] = []
    go n ('"':xs) = '"' : string '"' xs (go n)
    go n ('\'':xs) = '\'' : string '\'' xs (go n)
    go n ('{':xs) =
      let xs' = dropWhile isSpace xs
       in case xs' of
            '}':rest -> "{}" ++ afterClose n rest
            _ -> '{' : '\n' : indent (n + 1) ++ go (n + 1) xs'
    go n ('}':xs) =
      let n' = max 0 (n - 1)
       in '\n' : indent n' ++ '}' : afterClose n' (dropWhile isSpace xs)
    go n (';':xs) =
      let xs' = dropWhile isSpace xs
       in ';' : case xs' of
            '}':_ -> go n xs'
            [] -> []
            _ -> '\n' : indent n ++ go n xs'
    go n (c:xs)
      | isSpace c =
          let xs' = dropWhile isSpace xs
           in case xs' of
                [] -> []
                '}':_ -> go n xs'
                _ -> ' ' : go n xs'
      | otherwise = c : go n xs

    afterClose n s =
      case dropWhile isSpace s of
        s' | Just rest <- keyword "else" s' -> ' ' : go n ("else" ++ rest)
           | Just rest <- keyword "catch" s' -> ' ' : go n ("catch" ++ rest)
        -- Stay on this line for expression tails (`})`, `}()`, `},`, `};`).
        c:_ | c `elem` (");,.}(" :: String) -> go n (dropWhile isSpace s)
        [] -> []
        s' -> '\n' : indent n ++ go n s'

    keyword kw s = case splitAt (length kw) s of
      (pre, rest)
        | pre == kw, not (startsIdent rest) -> Just rest
      _ -> Nothing

    startsIdent (c:_) = isAlphaNum c || c == '_' || c == '$'
    startsIdent [] = False

    string _ [] _ = []
    string q (c:cs) k
      | c == '\\' = case cs of
          d:ds -> c : d : string q ds k
          [] -> [c]
      | c == q = c : k cs
      | otherwise = c : string q cs k

-- | Compile an effectful JShark computation. 'Readable' emits a pretty
-- snippet (no IIFE, no minifier); 'Minified' wraps an IIFE then minifies.
compileEffect :: CompilerConfig -> ClosedEffect u -> IO Text
compileEffect cfg eff = forceCompiled =<< finishStyle (configStyle cfg) <$> compileWith cfg
  (T.pack (renderJSCompact (effectDoc (configStyle cfg) eff)))

-- | Compile a pure JShark expression. See 'compileEffect'.
compilePure :: CompilerConfig -> ClosedExpr u -> IO Text
compilePure cfg e = forceCompiled =<< finishStyle (configStyle cfg) <$> compileWith cfg
  (T.pack (renderJSCompact (pureDoc (configStyle cfg) e)))

-- | Banner-before-serve only means JS is ready if this ran.
forceCompiled :: Text -> IO Text
forceCompiled t = t <$ evaluate (T.length t)

finishStyle :: OutputStyle -> Text -> Text
finishStyle Readable = prettyJS
finishStyle Minified = id

styleConfig :: CompilerConfig -> CompilerConfig
styleConfig cfg = case configStyle cfg of
  Readable -> cfg { configBackend = Passthrough }
  Minified -> cfg

pureDoc :: OutputStyle -> ClosedExpr u -> Doc
pureDoc Readable e = pureAST e
pureDoc Minified e = pureProgram e

effectDoc :: OutputStyle -> ClosedEffect u -> Doc
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
    (Just exe, _) -> run exe args
    (Nothing, Just npxExe) -> run npxExe (["--no-install", "esbuild"] ++ args)
    (Nothing, Nothing) -> pure (Left "esbuild executable not found on PATH")
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
            let wrapped = "export default (" <> dropTrailingSemis (T.strip source) <> ")\n"
                args' = args ++ ["--format=esm"]
            second <- executeProcessRaw exe args' wrapped
            case second of
              Left err -> pure (Left err)
              Right out' ->
                let stripped = stripExportDefault out'
                 in if T.null stripped
                      then pure (Left "minifier produced empty output (possible DCE of a bare expression; use compilePure/compileEffect)")
                      else pure (Right stripped)

dropTrailingSemis :: Text -> Text
dropTrailingSemis = T.dropWhileEnd (\c -> c == ';' || c == ' ' || c == '\n' || c == '\r' || c == '\t')

-- | Undo the 'export default (…)' / ESM anchor. esbuild '--minify' often
-- rewrites 'export default EXPR' to 'var e=EXPR;export{e as default};'.
stripExportDefault :: Text -> Text
stripExportDefault t =
  let t' = T.strip t
   in case stripVarAsDefault t' of
        Just v -> v
        Nothing -> case T.stripPrefix "export default" t' of
          Just rest -> dropTrailingSemis (T.strip rest)
          Nothing -> t'

-- | 'var name=VALUE;export{name as default};' → VALUE
stripVarAsDefault :: Text -> Maybe Text
stripVarAsDefault t = do
  afterVar <- T.stripPrefix "var " t
  let (name0, rest0) = T.break (== '=') afterVar
  rest1 <- T.stripPrefix "=" rest0
  let name = T.strip name0
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
executeProcess cmd args source = do
  res <- executeProcessRaw cmd args source
  case res of
    Right out
      | T.null out && not (T.null (T.strip source)) ->
          pure (Left "minifier produced empty output (possible DCE of a bare expression; use compilePure/compileEffect)")
    _ -> pure res

-- | Like 'executeProcess' but allows empty stdout (used when esbuild may DCE
-- a pure expression and we want to retry with an ESM export anchor).
executeProcessRaw :: FilePath -> [String] -> Text -> IO (Either String Text)
executeProcessRaw cmd args source =
  (do
    (code, stdoutStr, stderrStr) <- readProcessWithExitCode cmd args (T.unpack source)
    case code of
      ExitSuccess -> pure (Right (T.strip (T.pack stdoutStr)))
      ExitFailure c ->
        pure (Left (if null stderrStr then "Process exited with code " ++ show c else stderrStr))
  ) `catch` (\(e :: SomeException) -> pure (Left (show e)))
