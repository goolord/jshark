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
  , compileEffects
  , compileEffectsLabeled
  , compileJobsLabeled
  , compilePure
  , compilePures
  , compilePuresLabeled
  , prettyJS
  , biomeAvailable
  , applyCompilerArgs
  , isCompilerFlag
  , CompileJobStats (..)
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (evaluate, finally)
import Control.Monad (unless)
import Data.Atomics.Counter (newCounter, readCounter, writeCounter)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import GHC.Clock (getMonotonicTime)
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
import qualified JShark.Compiler.CompileReport as CR
import JShark.Compiler.CompileTiming
  ( CompileForm (..)
  , CompileJobStats (..)
  , seconds
  )
import JShark.Compiler.Emit (JS)
import JShark.Compiler.JsFormat
  ( biomeAvailable
  , prettyJS
  , tryPrettyJSIO
  )
import System.CPUTime (getCPUTime)

quietCfg :: CompilerConfig -> CompilerConfig
quietCfg cfg = cfg {configProgress = False, configQuiet = True}

numberedEffectJob :: Int -> ClosedEffect u -> (Text, ClosedEffect u)
numberedEffectJob i eff = ("#" <> T.pack (show i), eff)

numberedPureJob :: Int -> ClosedExpr u -> (Text, ClosedExpr u)
numberedPureJob i e = ("#" <> T.pack (show i), e)

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
  , configProgress :: Bool
  -- ^ Print a terminal progress bar for batch compiles and elapsed time when
  --     done. Off by default so tests stay quiet.
  , configQuiet :: Bool
  -- ^ Suppress non-fatal compiler stderr from concurrent batch workers.
  , configProgressSlot :: Maybe Int
  -- ^ Active job index for sub-progress reporting during batch compiles.
  }
  deriving (Show, Eq, Ord)

-- | Compact IIFE output from codegen. Minify with an external tool if wanted.
defaultCompilerConfig :: CompilerConfig
defaultCompilerConfig =
  CompilerConfig Minified False False Nothing

-- | Alias of 'defaultCompilerConfig', kept for call sites that previously
-- skipped external minification.
passthroughConfig :: CompilerConfig
passthroughConfig = defaultCompilerConfig

-- | Human-readable JS: no IIFE, formatted with Biome when available.
readableConfig :: CompilerConfig
readableConfig =
  CompilerConfig Readable False False Nothing

compileTreeEff ::
  IOE :> es =>
  CompilerConfig
  -> (OutputStyle -> JS)
  -> Eff es Text
compileTreeEff cfg doc = do
  let
    !style = configStyle cfg
  tCodegen0 <- liftIO getMonotonicTime
  let
    !js = renderJS (doc style)
  tCodegen1 <- liftIO getMonotonicTime
  liftIO $ CP.recordJobCodegenSec (seconds tCodegen0 tCodegen1)
  liftIO CP.finishEmitPhase
  liftIO $ CP.recordJobJsBytes (T.length js)
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
      unless quiet (CR.logReadableFallbackIO err)
      pure (T.strip src)

compileEffect :: CompilerConfig -> ClosedEffect u -> IO Text
compileEffect cfg eff =
  runEff $
    CR.runCompileReportFromConfig (configProgress cfg) (compileEffectEff cfg eff)

-- | Compile a program written directly in 'JShark.Api.EffectSyntax'
-- (absorbs the @fromSyntax@ wrap at the compile boundary).
compileEffectSyntax ::
  CompilerConfig -> (forall f. EffectSyntax f (f u)) -> IO Text
compileEffectSyntax cfg body = compileEffect cfg (fromSyntax body)

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
  out <- compileTreeEff cfg (`effectDoc` eff)
  end <- liftIO getCPUTime
  CR.drawSingleDone (CR.picosecondsToSecs (end - start))
  pure out

-- | Compile a pure JShark expression. Never draws progress bars.
compilePure :: CompilerConfig -> ClosedExpr u -> IO Text
compilePure cfg e =
  runEff $ CR.runCompileReportSilent (compilePureEff (quietCfg cfg) e)

compilePureEff ::
  IOE :> es =>
  CompilerConfig
  -> ClosedExpr u
  -> Eff es Text
compilePureEff cfg e = do
  compileTreeEff cfg (`pureDoc` e)

-- | Compile many effectful programs concurrently (one capability per item).
-- When 'configProgress' is set, prints a live progress bar and total time.
compileEffects ::
  CompilerConfig -> [ClosedEffect u] -> IO [Text]
compileEffects cfg effs =
  compileEffectsLabeled cfg (zipWith numberedEffectJob ([1 ..] :: [Int]) effs)

-- | Like 'compileEffects' but labels each job on the progress bar.
compileEffectsLabeled ::
  CompilerConfig -> [(Text, ClosedEffect u)] -> IO [Text]
compileEffectsLabeled cfg jobs =
  runEff $
    CR.runCompileReportFromConfig
      (configProgress cfg)
      (compileBatchEff cfg compileEffectEff jobs)

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

-- | Compile many pure programs concurrently. Never draws progress bars.
compilePures :: CompilerConfig -> [ClosedExpr u] -> IO [Text]
compilePures cfg exprs =
  compilePuresLabeled cfg (zipWith numberedPureJob ([1 ..] :: [Int]) exprs)

-- | Like 'compilePures' but labels each job on the progress bar.
compilePuresLabeled ::
  CompilerConfig -> [(Text, ClosedExpr u)] -> IO [Text]
compilePuresLabeled cfg jobs =
  runEff $
    CR.runCompileReportSilent (compileBatchEff (quietCfg cfg) compilePureEff jobs)

type CompileEff = '[CR.CompileReport, IOE]

compileBatchEff ::
  CompilerConfig
  -> (CompilerConfig -> item -> Eff CompileEff Text)
  -> [(Text, item)]
  -> Eff CompileEff [Text]
compileBatchEff cfg compileOne jobs
  | configProgress cfg = do
      let
        total = length jobs
        jobs' =
          [ ( label
            , \slot -> compileOneIO (quietCfg cfg {configProgressSlot = Just slot}) item
            )
          | (label, item) <- jobs
          ]
      (results, stats, secs) <- liftIO $ batchProgressCore total jobs'
      CR.drawBatchDone total secs
      CR.drawBatchStats secs stats
      pure results
  | otherwise =
      liftIO $ mapConcurrently (\(_, item) -> compileOneIO cfg item) jobs
 where
  compileOneIO c item =
    runEff $ CR.runCompileReportSilent $ compileOne c item

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

-- | Mixed-config jobs as slot-keyed IO actions (the separate signature
-- keeps the rank-2 'ClosedEffect' polymorphism over the batch).
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

batchProgressCore ::
  Int
  -> [(Text, Int -> IO Text)]
  -> IO ([Text], [CompileJobStats], Double)
batchProgressCore total jobs = do
  start <- getCPUTime
  board <- CP.newProgressBoard total
  styleIO <- CR.progressStyleIO
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

pureDoc :: OutputStyle -> ClosedExpr u -> JS
pureDoc Readable e = pureAST e
pureDoc Minified e = pureProgram e

effectDoc :: OutputStyle -> ClosedEffect u -> JS
effectDoc Readable e = effectfulAST e
effectDoc Minified e = effectfulProgram e

compileForm :: CompilerConfig -> CompileForm
compileForm cfg = case configStyle cfg of
  Readable -> FormReadable
  Minified -> FormMinified

mergeJobConfig :: CompilerConfig -> CompilerConfig -> CompilerConfig
mergeJobConfig base job =
  job
    { configProgress = configProgress base
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
