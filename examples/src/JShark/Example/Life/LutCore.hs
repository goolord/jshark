{-# LANGUAGE OverloadedStrings #-}

-- | Pure reference for Conway LUT chunk stepping.
--
-- Canonical byte/chunk semantics: 'stepChunk' and 'computeNextByte' here.
-- Full-grid stepping: native JS via 'Lut.stepRegionLUT' / 'EngineFinish.finishStep';
-- 'LutBoot.lifeLutWorkerBootJs' (worker emit) — parity in
-- @test/LifeWorkerTests.hs@ and @test/LifeTests.hs@.
module JShark.Example.Life.LutCore
  ( computeNextByte
  , stepChunk
  , lifeLutEntry
  , lifeLutTable
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8)

computeNextByte ::
  Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
computeNextByte top cur bot lt lc lb rt rc rb =
  go 0 0
 where
  go :: Int -> Word8 -> Word8
  go bit acc
    | bit >= 8 = acc
    | otherwise =
        let
          sh = bit
          alive = (cur `shiftR` sh) .&. 1
          left =
            if bit > 0
              then (cur `shiftR` (bit - 1)) .&. 1
              else lc
          right =
            if bit < 7
              then (cur `shiftR` (bit + 1)) .&. 1
              else rc
          topL =
            if bit > 0
              then (top `shiftR` (bit - 1)) .&. 1
              else lt
          topC = (top `shiftR` sh) .&. 1
          topR =
            if bit < 7
              then (top `shiftR` (bit + 1)) .&. 1
              else rt
          botL =
            if bit > 0
              then (bot `shiftR` (bit - 1)) .&. 1
              else lb
          botC = (bot `shiftR` sh) .&. 1
          botR =
            if bit < 7
              then (bot `shiftR` (bit + 1)) .&. 1
              else rb
          n =
            topL + topC + topR + left + right + botL + botC + botR
          next =
            if alive /= 0
              then n == 2 || n == 3
              else n == 3
          acc' =
            if next
              then acc .|. (1 `shiftL` bit)
              else acc
         in
          go (bit + 1) acc'

stepChunk ::
  [Word8]
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> Word8
stepChunk table top cur bot lt lc lb rt rc rb =
  let
    edge = lt .|. lc .|. lb .|. rt .|. rc .|. rb
   in
    if top .|. cur .|. bot .|. edge == 0
      then 0
      else
        if bot == 0 && edge == 0
          then table !! (fromIntegral top `shiftL` 8 .|. fromIntegral cur)
          else computeNextByte top cur bot lt lc lb rt rc rb

lifeLutEntry :: Int -> Word8
lifeLutEntry key =
  computeNextByte top cur 0 0 0 0 0 0 0
 where
  top = fromIntegral ((key `shiftR` 8) .&. 0xFF) :: Word8
  cur = fromIntegral (key .&. 0xFF) :: Word8

lifeLutTable :: [Word8]
lifeLutTable = [lifeLutEntry k | k <- [0 .. 65535]]
