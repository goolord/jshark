{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Flat / SoA test helpers (not part of the main codegen API).
module JShark.FlatTest
  ( flatSoaPureNodeCount
  , flatDirectPackDeterministic
  , flatDirectPackForRangeOk
  , flatDirectPackOptimizeStable
  , freezeEncColumnsOrderOk
  , lowerOptEffectRegressionOk
  , optIrEffectForRangeImpure
  , batchJobSlotTimingOk
  )
where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import JShark (irEffectFromClosed)
import JShark.Api.Types
  ( ClosedEffect
  , Universe (Unit)
  , Value (..)
  )
import JShark.Compiler.Lower
  ( lowerEffectAt
  , lowerOptEffectAt
  , reifyEffect
  )
import JShark.Compiler.CompileProgress
  ( newProgressBoard
  , recordJobFlatPrepare
  , recordJobLintSec
  , snapshotJobStatsFromSlot
  , withActiveJob
  )
import JShark.Compiler.CompileTiming
  ( FlatPrepareTiming (..)
  , cjsIrPrepareSec
  , cjsLintSec
  )
import qualified JShark.Compiler.FlatEnc as FlatEnc
import qualified JShark.Compiler.FlatSoA as FlatSoA
import qualified JShark.Compiler.Ir as Ir

freezeEncColumnsOrderOk :: Bool
freezeEncColumnsOrderOk = FlatEnc.freezeEncColumnsOrderOk

flatDirectPackDeterministic :: ClosedEffect u -> Bool
flatDirectPackDeterministic e =
  let
    ir = irEffectFromClosed e
    soa1 = FlatSoA.packEffectProgramDirect ir
    soa2 = FlatSoA.packEffectProgramDirect ir
   in
    FlatSoA.soaColumnsEqual soa1 soa2

flatDirectPackForRangeOk :: Bool
flatDirectPackForRangeOk =
  let
    soa1 = FlatSoA.packEffectProgramDirect forRangeU8SetLoop
    soa2 = FlatSoA.packEffectProgramDirect forRangeU8SetLoop
   in
    FlatSoA.soaColumnsEqual soa1 soa2

-- | Composed 'lowerOptEffectAt' must match 'lowerEffectAt' then 'optIrEffect'.
lowerOptEffectRegressionOk :: Bool
lowerOptEffectRegressionOk =
  let
    ?keepLets = False
   in
    let
      probe = reifyEffect forRangeU8SetLoop
      (tLower, irLower) = lowerEffectAt (-2) probe
      (_, irOpt, mdOpt) = lowerOptEffectAt (-2) probe
      (_, irManual, mdManual) = Ir.optIrEffect tLower irLower
      nOpt = Ir.irSize mdOpt
      soaOpt = FlatSoA.packEffectProgramDirect irOpt
      soaManual = FlatSoA.packEffectProgramDirect irManual
     in
      FlatSoA.soaColumnsEqual soaOpt soaManual && nOpt == Ir.irSize mdManual

flatDirectPackOptimizeStable :: ClosedEffect u -> Bool
flatDirectPackOptimizeStable e =
  let
    ir = irEffectFromClosed e
    soa0 = FlatSoA.packEffectProgramDirect ir
    soa1 = FlatSoA.optimizeFlatPack soa0
    soa2 = FlatSoA.optimizeFlatPack soa1
   in
    FlatSoA.soaColumnsEqual soa1 soa2

flatSoaPureNodeCount :: ClosedEffect u -> Int
flatSoaPureNodeCount e =
  let
    soa =
      FlatSoA.optimizeFlatPack
        (FlatSoA.packEffectProgramDirect (irEffectFromClosed e))
    n = FlatSoA.flatSoaNodeCount soa
   in
    countPure (FlatSoA.soaPureVector soa) n

countPure :: Vector Word8 -> Int -> Int
countPure v n =
  length [i | i <- [0 .. n - 1], i < V.length v, v V.! i == 1]

forRangeU8SetLoop :: Ir.IrEffect 'Unit
forRangeU8SetLoop =
  Ir.IrForRange
    (Ir.IrLiteral (ValueNumber 0))
    (Ir.IrLiteral (ValueNumber 4))
    0
    ( Ir.IrU8Set
        (Ir.IrVar 99)
        (Ir.IrLiteral (ValueNumber 0))
        (Ir.IrLiteral (ValueNumber 1))
    )

-- | @optIrEffect@ must mark @ForRange@ + @IrU8Set@ impure so flat codegen
-- keeps mutation loops.
optIrEffectForRangeImpure :: Bool
optIrEffectForRangeImpure =
  let
    ?keepLets = False
   in
    let
      (_, _, md) = Ir.optIrEffect 0 forRangeU8SetLoop
     in
      not (Ir.irPure md)

-- | Slot-backed timing refs survive snapshot after 'withActiveJob' returns.
batchJobSlotTimingOk :: IO Bool
batchJobSlotTimingOk = do
  board <- newProgressBoard 1
  _ <-
    withActiveJob 0 board $ do
      recordJobLintSec 0.001
      recordJobFlatPrepare
        FlatPrepareTiming
          { fptIrPrepareSec = 0.01
          , fptPackSec = 0
          , fptFlatOptSec = 0
          , fptTotalSec = 0.01
          }
      pure ()
  stats <- snapshotJobStatsFromSlot board 0 "test" 0.05
  pure (cjsLintSec stats == 0.001 && cjsIrPrepareSec stats == 0.01)
