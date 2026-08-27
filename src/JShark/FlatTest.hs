{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Flat / SoA test helpers (not part of the main codegen API).
module JShark.FlatTest
  ( flatSoaColumnsRoundTrip
  , flatProgramRoundTrip
  , flatSoaPureNodeCount
  , optIrEffectForRangeImpure
  , batchJobSlotTimingOk
  )
where

import JShark.CompileProgress
  ( newProgressBoard
  , recordJobFlatPrepare
  , recordJobLintSec
  , snapshotJobStatsFromSlot
  , withActiveJob
  )
import JShark.CompileTiming (FlatPrepareTiming (..), cjsLowerSec, cjsLintSec)

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import JShark (ClosedEffect, irEffectFromClosed)
import qualified JShark.Flat as Flat
import qualified JShark.FlatSoA as FlatSoA
import qualified JShark.Ir as Ir
import JShark.Types (Universe (Unit), Value (..))

packFlat :: ClosedEffect u -> Flat.FlatProgram
packFlat e = Flat.packEffectProgram (irEffectFromClosed e)

flatSoaColumnsRoundTrip :: ClosedEffect u -> Bool
flatSoaColumnsRoundTrip e =
  let
    soa = FlatSoA.fromProgram (packFlat e)
    soa' = FlatSoA.fromProgram (FlatSoA.toProgram soa)
   in
    FlatSoA.soaColumnsEqual soa soa'

flatProgramRoundTrip :: ClosedEffect u -> Bool
flatProgramRoundTrip e =
  let
    p = packFlat e
    soa0 = FlatSoA.fromProgram p
    p' = FlatSoA.toProgram soa0
    soa1 = FlatSoA.fromProgram p'
   in
    FlatSoA.soaColumnsEqual soa0 soa1
      && V.length (Flat.fpNodes p) == V.length (Flat.fpNodes p')
      && Flat.fpRoot p == Flat.fpRoot p'

flatSoaPureNodeCount :: ClosedEffect u -> Int
flatSoaPureNodeCount e =
  let
    p = FlatSoA.optimizeFlatProgram (packFlat e)
    n = V.length (Flat.fpNodes p)
   in
    countPure (Flat.fpPure p) n

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
    (_, _, md) = Ir.optIrEffect 0 forRangeU8SetLoop
   in
    not (Ir.irMetaPure md)

-- | Slot-backed timing refs survive snapshot after 'withActiveJob' returns.
batchJobSlotTimingOk :: IO Bool
batchJobSlotTimingOk = do
  board <- newProgressBoard 1
  _ <-
    withActiveJob 0 board $ do
      recordJobLintSec 0.001
      recordJobFlatPrepare
        FlatPrepareTiming
          { fptLowerSec = 0.01
          , fptIrOptSec = 0
          , fptPackSec = 0
          , fptFlatOptSec = 0
          , fptTotalSec = 0.01
          }
      pure ()
  stats <- snapshotJobStatsFromSlot board 0 "test" 0.05
  pure (cjsLintSec stats == 0.001 && cjsLowerSec stats == 0.01)
