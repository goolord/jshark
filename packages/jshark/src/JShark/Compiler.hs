{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Compile generated JavaScript: IIFE (minified style) or readable snippet,
-- plus batch/progress plumbing.
--
-- Codegen already emits compact JS ('renderJS'). Default config wraps an IIFE
-- and does no external post-processing; run an external minifier over the
-- output if you want more. 'readableConfig' emits a debug snippet (no IIFE,
-- then Biome via 'finishReadableIO'). 'prettyJS' is @Text -> IO Text@; see
-- CHANGELOG.
--
-- 'compileEffect' honors 'configProgress'. 'compileEffectPure' is silent;
-- 'compileEffectIO' always draws. 'compilePure' never draws.
module JShark.Compiler
  ( -- * Compiler Configuration
    CompilerConfig (..)
  , defaultCompilerConfig
  , passthroughConfig
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
import Control.Exception (evaluate, finally)
import Control.Monad (unless, when)
import Data.Atomics.Counter (newCounter, readCounter, writeCounter)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import JShark
  ( ClosedEffect
  , ClosedExpr
  , effectfulAST
  , effectfulProgram
  , pureAST
  , pureProgram
  , renderJS
  )
import JShark.Api.Types (EffectSyntax, fromSyntax)
import qualified JShark.Compiler.CompileProgress as CP
import JShark.Compiler.Emit (JS)
import JShark.Compiler.JsFormat
  ( biomeAvailable
  , prettyJS
  , tryPrettyJSIO
  )
import System.CPUTime (getCPUTime)
import System.IO (hPutStrLn, stderr)

-- | CPU-time seconds.
picosecondsToSecs :: Integer -> Double
picosecondsToSecs ps = fromIntegral ps / 1e12

quietCfg :: CompilerConfig -> CompilerConfig
quietCfg cfg = cfg {configProgress = False, configQuiet = True}

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
  -- ^ Print a terminal progress bar for batch compiles and elapsed time when
  --     done. Off by default so tests stay quiet.
  , configQuiet :: Bool
  -- ^ Suppress non-fatal compiler stderr from concurrent batch workers.
  }
  deriving (Show, Eq, Ord)

-- | Compact IIFE output from codegen. Minify with an external tool if wanted.
defaultCompilerConfig :: CompilerConfig
defaultCompilerConfig = CompilerConfig Minified False False

-- | Alias of 'defaultCompilerConfig', kept for call sites that previously
-- skipped external minification.
passthroughConfig :: CompilerConfig
passthroughConfig = defaultCompilerConfig

-- | Human-readable JS: no IIFE, formatted with Biome when available.
readableConfig :: CompilerConfig
readableConfig = CompilerConfig Readable False False

compileTreeEff ::
  IOE :> es =>
  CompilerConfig
  -> (OutputStyle -> JS)
  -> Eff es Text
compileTreeEff cfg doc = do
  let
    !js = renderJS (doc (configStyle cfg))
  liftIO CP.finishEmitPhase
  formatted <- finishReadableEff cfg js
  liftIO $ forceCompiled formatted

finishReadableEff ::
  IOE :> es => CompilerConfig -> Text -> Eff es Text
finishReadableEff cfg src
  | configStyle cfg == Readable =
      liftIO (finishReadableIO (configQuiet cfg) src)
  | otherwise = pure src

finishReadableIO :: Bool -> Text -> IO Text
finishReadableIO quiet src =
  tryPrettyJSIO src >>= \case
    Right out -> pure out
    Left err -> do
      unless quiet
        $ CP.withProgressIO
        $ hPutStrLn stderr ("JShark.Compiler: " ++ err ++ "; using compact emit")
      pure (T.strip src)

-- | Compile an effectful program to JS text, drawing progress when
-- 'configProgress' is set.
compileEffect :: CompilerConfig -> ClosedEffect u -> IO Text
compileEffect cfg eff = do
  start <- getCPUTime
  out <- runEff (compileEffectEff cfg eff)
  when (configProgress cfg) $ do
    end <- getCPUTime
    style <- CP.terminalStyleIO
    CP.withProgressIO $
      hPutStrLn stderr (CP.renderDoneLine style (picosecondsToSecs (end - start)))
  pure out

-- | Compile a program written directly in 'JShark.Api.EffectSyntax'
-- (absorbs the @fromSyntax@ wrap at the compile boundary).
compileEffectSyntax ::
  CompilerConfig -> (forall f. EffectSyntax f (f u)) -> IO Text
compileEffectSyntax cfg body = compileEffect cfg (fromSyntax body)

-- | Like 'compileEffect' but silent: never draws progress bars.
compileEffectPure :: CompilerConfig -> ClosedEffect u -> IO Text
compileEffectPure cfg eff = runEff (compileEffectEff (quietCfg cfg) eff)

-- | Like 'compileEffect' but always draws progress bars.
compileEffectIO :: CompilerConfig -> ClosedEffect u -> IO Text
compileEffectIO cfg = compileEffect cfg {configProgress = True}

compileEffectEff ::
  IOE :> es =>
  CompilerConfig
  -> ClosedEffect u
  -> Eff es Text
compileEffectEff cfg eff =
  compileTreeEff cfg (`effectDoc` eff)

-- | Compile a pure JShark expression. Never draws progress bars.
compilePure :: CompilerConfig -> ClosedExpr u -> IO Text
compilePure cfg e = runEff (compilePureEff (quietCfg cfg) e)

compilePureEff ::
  IOE :> es =>
  CompilerConfig
  -> ClosedExpr u
  -> Eff es Text
compilePureEff cfg e = compileTreeEff cfg (`pureDoc` e)

-- | Compile many labeled effectful programs concurrently (one capability per
-- item). When 'configProgress' is set, prints a live progress bar and a total
-- time line; otherwise runs quietly.
compileJobsLabeled ::
  CompilerConfig
  -> [(Text, CompilerConfig, ClosedEffect u)]
  -> IO [Text]
compileJobsLabeled baseCfg jobs
  | configProgress baseCfg = do
      let
        total = length jobs
      start <- getCPUTime
      board <- CP.newProgressBoard total
      styleIO <- CP.terminalStyleIO
      lineCount <- newCounter 0
      let
        refresh = do
          b <- CP.readProgressBoard board
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
          CP.writeProgressLine block
      CP.setProgressRedraw refresh
      CP.withProgressIO refresh
      indexed <-
        mapConcurrently
          ( \(slot, (label, jobCfg, eff)) -> do
              CP.initJob board slot label
              out <-
                CP.withActiveJob slot board $
                  compileEffectPure (mergeJobConfig jobCfg) eff
              CP.markJobDone board slot
              CP.withProgressIO refresh
              pure (slot, out)
          )
          (zip ([0 ..] :: [Int]) jobs)
          `finally` CP.clearProgressRedraw
      end <- getCPUTime
      CP.withProgressIO $ do
        hPutStrLn stderr ""
        hPutStrLn
          stderr
          (CP.renderBatchDoneLine styleIO total (picosecondsToSecs (end - start)))
      pure (map snd (sortOn fst indexed))
  | otherwise =
      mapConcurrently
        (\(_label, jobCfg, eff) -> compileEffectPure (mergeJobConfig jobCfg) eff)
        jobs

-- | Banner-before-serve only means JS is ready if this ran.
forceCompiled :: Text -> IO Text
forceCompiled t = t <$ evaluate (T.length t)

pureDoc :: OutputStyle -> ClosedExpr u -> JS
pureDoc Readable e = pureAST e
pureDoc Minified e = pureProgram e

effectDoc :: OutputStyle -> ClosedEffect u -> JS
effectDoc Readable e = effectfulAST e
effectDoc Minified e = effectfulProgram e

-- | Worker jobs never drive the shared progress display or log to stderr.
mergeJobConfig :: CompilerConfig -> CompilerConfig
mergeJobConfig job =
  job
    { configProgress = False
    , configQuiet = True
    }

-- | Recognized compiler CLI flags (for example servers and build tools).
isCompilerFlag :: String -> Bool
isCompilerFlag = \case
  "--progress" -> True
  "--readable" -> True
  _ -> False

-- | Apply recognized CLI flags to a 'CompilerConfig'.
applyCompilerArgs :: [String] -> CompilerConfig -> CompilerConfig
applyCompilerArgs args cfg =
  foldl' applyCompilerArg cfg args

applyCompilerArg :: CompilerConfig -> String -> CompilerConfig
applyCompilerArg cfg = \case
  "--progress" -> cfg {configProgress = True}
  "--readable" -> cfg {configStyle = Readable}
  _ -> cfg
