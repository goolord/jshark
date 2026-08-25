{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Flat / SoA test helpers (not part of the main codegen API).
module JShark.FlatTest
  ( flatSoaColumnsRoundTrip
  , flatProgramRoundTrip
  , flatSoaPureNodeCount
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import JShark (ClosedEffect, irEffectFromClosed)
import qualified JShark.Flat as Flat
import qualified JShark.FlatSoA as FlatSoA

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
