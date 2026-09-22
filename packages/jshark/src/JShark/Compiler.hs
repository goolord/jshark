{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Compile JShark programs to JavaScript bytes.
--
-- 'defaultCompilerConfig' emits a compact IIFE; run an external minifier
-- over it if you want more. 'readableConfig' emits an unwrapped snippet
-- with source-derived names, formatted with Biome when it is available
-- ('prettyJS').
--
-- 'compileEffect' honors 'configProgress'; 'compileEffectPure' and
-- 'compilePure' are silent; 'compileEffectIO' always reports.
module JShark.Compiler
  ( -- * Compiler Configuration
    CompilerConfig (..)
  , defaultCompilerConfig
  , readableConfig
  , OutputStyle (..)

    -- * Compilation
  , compileEffect
  , compileEffectPure
  , compileEffectSyntax
  , compileEffectIO
  , compileJobsLabeled
  , compilePure
  , prettyJS
  , biomeAvailable
  , applyCompilerArgs
  , isCompilerFlag
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Api.Syntax (EffectSyntax, fromSyntax)
import JShark.Api.Types (ClosedEffect, ClosedExpr)
import JShark.Compiler.Codegen (effectfulAST, effectfulProgram, pureAST, pureProgram)
import JShark.Compiler.Emit (JS, renderJS)
import Numeric (showFFloat)
import System.CPUTime (getCPUTime)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hFlush, hPutStr, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)

-- | How to present compiled JavaScript.
data OutputStyle
  = -- | Pretty-print, do not minify. Not wrapped in an IIFE.
    Readable
  | -- | Wrap in an IIFE.
    Minified
  deriving (Show, Eq, Ord)

-- | Top-level compiler configuration.
data CompilerConfig = CompilerConfig
  { configStyle :: OutputStyle
  -- ^ Output mode: IIFE-wrapped 'Minified' or unwrapped 'Readable'.
  , configProgress :: Bool
  -- ^ Report batch progress and elapsed time on stderr. Off by default so
  -- tests stay quiet.
  , configQuiet :: Bool
  -- ^ Suppress non-fatal compiler warnings on stderr.
  }
  deriving (Show, Eq, Ord)

-- | Compact IIFE output from codegen. Minify with an external tool if wanted.
defaultCompilerConfig :: CompilerConfig
defaultCompilerConfig = CompilerConfig Minified False False

-- | Human-readable JS: no IIFE, formatted with Biome when available.
readableConfig :: CompilerConfig
readableConfig = CompilerConfig Readable False False

quiet :: CompilerConfig -> CompilerConfig
quiet cfg = cfg {configProgress = False, configQuiet = True}

-- | Serializes stderr output across concurrent compiles.
stderrLock :: MVar ()
stderrLock = unsafePerformIO (newMVar ())
{-# NOINLINE stderrLock #-}

report :: String -> IO ()
report s = withMVar stderrLock (\() -> hPutStr stderr s >> hFlush stderr)

render :: CompilerConfig -> JS -> IO ByteString
render cfg js = do
  let
    src = renderJS js
  out <- case configStyle cfg of
    Minified -> pure src
    Readable ->
      tryPrettyJS src >>= \case
        Right o -> pure o
        Left err -> do
          unless (configQuiet cfg) $
            report ("JShark.Compiler: " ++ err ++ "; using compact emit\n")
          pure (BC.strip src)
  -- Forced so that "compiled" really means the bytes exist.
  out <$ evaluate (BS.length out)

timed :: IO a -> IO (a, Double)
timed io = do
  t0 <- getCPUTime
  a <- io
  t1 <- getCPUTime
  pure (a, fromIntegral (t1 - t0) / 1e12)

-- | Compile an effectful program to JS bytes, reporting the elapsed time
-- when 'configProgress' is set.
compileEffect :: CompilerConfig -> ClosedEffect u -> IO ByteString
compileEffect cfg eff = do
  (out, secs) <- timed (render cfg doc)
  when (configProgress cfg) $ report ("JShark.Compiler: compiled in " ++ duration secs ++ "\n")
  pure out
 where
  doc = case configStyle cfg of
    Readable -> effectfulAST eff
    Minified -> effectfulProgram eff

-- | Compile a program written directly in 'JShark.Api.EffectSyntax'.
compileEffectSyntax ::
  CompilerConfig -> (forall f. EffectSyntax f (f u)) -> IO ByteString
compileEffectSyntax cfg body = compileEffect cfg (fromSyntax body)

-- | Like 'compileEffect' but silent.
compileEffectPure :: CompilerConfig -> ClosedEffect u -> IO ByteString
compileEffectPure cfg = compileEffect (quiet cfg)

-- | Like 'compileEffect' but always reports.
compileEffectIO :: CompilerConfig -> ClosedEffect u -> IO ByteString
compileEffectIO cfg = compileEffect cfg {configProgress = True}

-- | Compile a pure JShark expression. Never reports progress.
compilePure :: CompilerConfig -> ClosedExpr u -> IO ByteString
compilePure cfg e = render (quiet cfg) $ case configStyle cfg of
  Readable -> pureAST e
  Minified -> pureProgram e

-- | Compile many labeled effectful programs concurrently. Each job's own
-- config picks its style; jobs never report. When the batch config sets
-- 'configProgress', a progress line lists the jobs still running.
compileJobsLabeled ::
  CompilerConfig
  -> [(Text, CompilerConfig, ClosedEffect u)]
  -> IO [ByteString]
compileJobsLabeled cfg jobs
  | not (configProgress cfg) = mapConcurrently run jobs
  | otherwise = do
      pending <- newMVar (map (\(l, _, _) -> l) jobs)
      let
        total = length jobs
        draw left =
          report $
            "\r\ESC[Kcompile "
              ++ bar (total - length left) total
              ++ " "
              ++ show (total - length left)
              ++ "/"
              ++ show total
              ++ concatMap ((' ' :) . T.unpack) left
      draw (map (\(l, _, _) -> l) jobs)
      (outs, secs) <-
        timed . flip mapConcurrently jobs $ \job@(l, _, _) -> do
          out <- run job
          modifyMVar_ pending (\left -> let left' = filter (/= l) left in left' <$ draw left')
          pure out
      report ("\nJShark.Compiler: compiled " ++ show total ++ " programs in " ++ duration secs ++ "\n")
      pure outs
 where
  run :: (Text, CompilerConfig, ClosedEffect v) -> IO ByteString
  run (_, jobCfg, eff) = compileEffectPure jobCfg eff
  bar done total =
    let w = 28; k = if total == 0 then w else done * w `div` total
     in "[" ++ replicate k '=' ++ replicate (w - k) '-' ++ "]"

duration :: Double -> String
duration s
  | s < 1 = show (round (s * 1000) :: Integer) ++ "ms"
  | otherwise = showFFloat (Just 2) s "s"

-- | The compiler CLI flags, and what each one sets.
compilerFlags :: [(String, CompilerConfig -> CompilerConfig)]
compilerFlags =
  [ ("--progress", \cfg -> cfg {configProgress = True})
  , ("--readable", \cfg -> cfg {configStyle = Readable})
  ]

-- | Whether the argument is a compiler flag, so a caller can split it out of
-- its own argument list (for example servers and build tools).
isCompilerFlag :: String -> Bool
isCompilerFlag arg = any ((== arg) . fst) compilerFlags

-- | Apply recognized CLI flags to a 'CompilerConfig'; ignore anything else.
applyCompilerArgs :: [String] -> CompilerConfig -> CompilerConfig
applyCompilerArgs args cfg =
  foldl' (\c a -> maybe c ($ c) (lookup a compilerFlags)) cfg args

-- Biome -----------------------------------------------------------------------

biomePackage :: String
biomePackage = "@biomejs/biome@2.5.11"

-- | @biome@ on PATH, else @bunx@ \/ @bun x@ with the pinned package.
biome :: [String] -> IO (Either String (FilePath, [String]))
biome args = do
  direct <- findExecutable "biome"
  bunx <- findExecutable "bunx"
  bun <- findExecutable "bun"
  pure $ case (direct, bunx, bun) of
    (Just exe, _, _) -> Right (exe, args)
    (_, Just exe, _) -> Right (exe, biomePackage : args)
    (_, _, Just exe) -> Right (exe, "x" : biomePackage : args)
    _ -> Left "biome executable not found on PATH (install biome or use bun)"

-- | Run a process with @input@ on stdin; stdout, or stderr on failure.
runTool :: (FilePath, [String]) -> ByteString -> IO (Either String ByteString)
runTool (exe, args) input =
  try (readProcess (setStdin (byteStringInput (BL.fromStrict input)) (proc exe args))) >>= \case
    Left e -> pure (Left (show (e :: SomeException)))
    Right (ExitSuccess, out, _) -> pure (Right (BL.toStrict out))
    Right (ExitFailure c, _, err)
      | BL.null err -> pure (Left ("Process exited with code " ++ show c))
      | otherwise -> pure (Left (BLC.unpack err))

tryPrettyJS :: ByteString -> IO (Either String ByteString)
tryPrettyJS src =
  biome ["format", "--stdin-file-path=jshark.js", "--indent-style=space", "--indent-width=2"]
    >>= either (pure . Left) (\tool -> fmap BC.strip <$> runTool tool (BC.strip src))

-- | Pretty-print compact JS with Biome when available; otherwise return the
-- input unchanged.
prettyJS :: ByteString -> IO ByteString
prettyJS src = either (const (BC.strip src)) id <$> tryPrettyJS src

-- | Whether Biome can be run.
biomeAvailable :: IO Bool
biomeAvailable =
  biome ["--version"] >>= either (const (pure False)) (fmap (either (const False) (const True)) . (`runTool` BS.empty))
