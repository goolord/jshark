{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | JShark helpers and miniature grid fixtures for Conway rule tests.
module JShark.Example.Life.LifeTestSupport
  ( assertEqual
  , assertAlive
  , aliveBit
  , setAlive
  , gridPop
  , runProcessCellAt
  , newCellGrids
  , runStepGridOnce
  , seedBlock
  , seedBeehive
  , seedBlinkerHorizontal
  , blockCoords
  , beehiveCoords
  , coordsMatch
  , blinkerHorizontalCoords
  , blinkerVerticalCoords
  , blinkerLutStepJson
  , runtimeBlockPhaseKey
  , runtimeBlockPhaseHashLen
  )
where

import Control.Monad (forM_)
import JShark.Api
import JShark.Api.Generic (toObject)
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Array as Array
import JShark.Example.Life.DiscoverRuntime (collectPhaseKey)
import JShark.Example.Life.Grid
  ( CellGrids (..)
  , CellStep (..)
  , StepCtx (..)
  , StepRegion (..)
  , StepScratch (..)
  , cellIdx
  , processCell
  , rebuildPackedCounts
  , setPackedAlive
  , setU8
  , stepGrid
  , u8Get
  )
import qualified JShark.Example.Life.Lut as Lut

assertEqual :: Expr f 'Number -> Expr f 'Number -> EffectSyntax f (f 'Unit)
assertEqual expected actual = do
  whenS (not_ (expected .== actual)) $ do
    toSyntax_ $
      ffi
        "((e,a)=>{throw new Error('assertEqual: '+String(e)+' !== '+String(a))})"
        (arg expected <: arg actual <: RecNil)
    done
  done

assertAlive ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
assertAlive grid w x y = do
  let
    i = cellIdx w x y
  b <- aliveBit grid i
  assertEqual (number 1) b

aliveBit ::
  Expr f 'Uint8Array -> Expr f 'Number -> EffectSyntax f (Expr f 'Number)
aliveBit grid i = do
  b <- u8Get grid i
  pure (bitAnd b (number 1))

setAlive ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
setAlive grid w x y = do
  let
    i = cellIdx w x y
  setPackedAlive grid i (number 1)
  done

gridPop ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
gridPop grid w h = do
  ref <- hold (toObject (StepScratch 0 0 0 0 0))
  let
    cells = w * h
  forRange_ (number 0) cells $ \i -> do
    b <- aliveBit grid i
    whenS (b .== 1) $ do
      p <- ref.pop
      set @"pop" ref (p + 1)
  ref.pop

-- | Clear @grid@, set the given cells live, and rebuild packed counts.
seedCells ::
  [(Double, Double)]
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
seedCells cells grid w h = do
  toSyntax_ (u8Fill grid (number 0))
  forM_ cells $ \(x, y) -> setAlive grid w (number x) (number y)
  rebuildPackedCounts grid w h
  done

-- | The cells as a bound @[[x, y], …]@ array.
cellsArray ::
  [(Double, Double)] -> EffectSyntax f (Expr f ('Array ('Array 'Number)))
cellsArray cells =
  bindExpr $
    Array.fromEffects
      [Array.fromEffects [expr (number x), expr (number y)] | (x, y) <- cells]

block, beehive, blinkerH, blinkerV :: [(Double, Double)]
block = [(1, 1), (2, 1), (1, 2), (2, 2)]
beehive = [(1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (2, 2)]
blinkerH = [(1, 2), (2, 2), (3, 2)]
blinkerV = [(2, 1), (2, 2), (2, 3)]

seedBlock
  , seedBeehive
  , seedBlinkerHorizontal ::
    Expr f 'Uint8Array
    -> Expr f 'Number
    -> Expr f 'Number
    -> EffectSyntax f (f 'Unit)
seedBlock = seedCells block
seedBeehive = seedCells beehive
seedBlinkerHorizontal = seedCells blinkerH

blockCoords
  , beehiveCoords
  , blinkerHorizontalCoords
  , blinkerVerticalCoords ::
    EffectSyntax f (Expr f ('Array ('Array 'Number)))
blockCoords = cellsArray block
beehiveCoords = cellsArray beehive
blinkerHorizontalCoords = cellsArray blinkerH
blinkerVerticalCoords = cellsArray blinkerV

coordsMatch ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f ('Array ('Array 'Number))
  -> EffectSyntax f (f 'Unit)
coordsMatch grid w coords = do
  forRange_ (number 0) (Array.length coords) $ \k -> do
    let
      cell = Array.index coords k
      x = Array.index cell 0
      y = Array.index cell 1
    assertAlive grid w x y
  done

runProcessCellAt ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
runProcessCellAt alive species nextAlive nextSpecies w h x y = do
  stepCtx <- hold (toObject (StepCtx 0 0 (-1) (-1) 0 0 0 0 0))
  counts <- bindExpr (newByteArray (number 256))
  touchedBuf <- bindExpr (newByteArray (number 8))
  nextLiveList <- bindExpr $ Array.fromEffects []
  nextChangedList <- bindExpr $ Array.fromEffects []
  toSyntax_ (u8Copy nextAlive alive)
  toSyntax_ (u8Fill nextSpecies (number 0))
  processCell CellStep {grids = CellGrids {..}, ..} x y
  let
    i = cellIdx w x y
  aliveBit nextAlive i

-- | Four empty cell grids for a @w * h@ world, and a region covering all
-- of it. Every stepping test starts from this.
newCellGrids ::
  Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (CellGrids f, StepRegion f)
newCellGrids w h = do
  alive <- bindExpr (newByteArray (w * h))
  species <- bindExpr (newByteArray (w * h))
  nextAlive <- bindExpr (newByteArray (w * h))
  nextSpecies <- bindExpr (newByteArray (w * h))
  pure
    ( CellGrids {..}
    , StepRegion
        { srW = w
        , srH = h
        , srX0 = number 0
        , srY0 = number 0
        , srX1 = w - number 1
        , srY1 = h - number 1
        }
    )

-- | Run one generation with fresh scratch buffers.
runStepGridOnce ::
  CellGrids f -> StepRegion f -> EffectSyntax f (Expr f 'Number)
runStepGridOnce cells region = do
  prevLiveList <- bindExpr $ Array.fromEffects []
  nextLiveList <- bindExpr $ Array.fromEffects []
  nextChangedList <- bindExpr $ Array.fromEffects []
  stepStamp <- bindExpr (newByteArray (srW region * srH region))
  stepCtx <- hold (toObject (StepCtx 0 0 (-1) (-1) 0 0 0 0 0))
  counts <- bindExpr (newByteArray (number 256))
  touchedBuf <- bindExpr (newByteArray (number 8))
  stepGrid
    cells
    region
    prevLiveList
    nextLiveList
    nextChangedList
    stepStamp
    (number 1)
    (number 0)
    stepCtx
    counts
    touchedBuf

blinkerLutStepJson :: forall f. Effect f 'String
blinkerLutStepJson = fromSyntax $ do
  let
    w = number 8
    h = number 8
    cellsN = w * h
  lut <- Lut.createLifeLUT
  a <- bindExpr (newByteArray cellsN)
  b <- bindExpr (newByteArray cellsN)
  _ <- setU8 a (number 18) (number 1)
  _ <- setU8 a (number 19) (number 1)
  _ <- setU8 a (number 20) (number 1)
  _ <- Lut.stepRegionLUT lut a b w h (number 0) h
  json <-
    bindExpr $
      ffi
        "(function(b){return JSON.stringify(Array.from(b));})"
        (arg b <: RecNil)
  yield json

runtimeBlockPhaseKey :: forall f. Effect f 'String
runtimeBlockPhaseKey = fromSyntax $ do
  coords <- blockCoords
  (key, _) <- collectPhaseKey coords
  yield key

runtimeBlockPhaseHashLen :: forall f. Effect f 'Number
runtimeBlockPhaseHashLen = fromSyntax $ do
  coords <- blockCoords
  (_, hashes) <- collectPhaseKey coords
  yield (Array.length hashes)
