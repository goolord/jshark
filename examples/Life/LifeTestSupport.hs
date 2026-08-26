{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | JShark helpers and miniature grid fixtures for Conway rule tests.
module LifeTestSupport
  ( assertEqual
  , assertAlive
  , aliveBit
  , setAlive
  , clearBinaryGrid
  , gridPop
  , runProcessCellAt
  , runStepGridOnce
  , seedBlock
  , seedBeehive
  , seedBlinkerHorizontal
  , seedBlinkerVertical
  , blockCoords
  , beehiveCoords
  , coordsMatch
  , blinkerHorizontalCoords
  , blinkerVerticalCoords
  )
where

import Grid
  ( StepCtx (..)
  , StepScratch (..)
  , cellIdx
  , processCell
  , rebuildPackedCounts
  , setPackedAlive
  , stepGrid
  , u8Get
  )
import JShark.Api
import qualified JShark.Array as Array
import JShark.Generic (toObject)
import JShark.Rec (Rec (..), (<:))

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

clearBinaryGrid :: Expr f 'Uint8Array -> EffectSyntax f (f 'Unit)
clearBinaryGrid grid = do
  toSyntax_ (u8Fill grid (number 0))
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

seedBlock ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
seedBlock grid w h = do
  toSyntax_ (u8Fill grid (number 0))
  setAlive grid w (number 1) (number 1)
  setAlive grid w (number 2) (number 1)
  setAlive grid w (number 1) (number 2)
  setAlive grid w (number 2) (number 2)
  rebuildPackedCounts grid w h
  done

seedBeehive ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
seedBeehive grid w h = do
  toSyntax_ (u8Fill grid (number 0))
  setAlive grid w (number 1) (number 0)
  setAlive grid w (number 2) (number 0)
  setAlive grid w (number 0) (number 1)
  setAlive grid w (number 3) (number 1)
  setAlive grid w (number 1) (number 2)
  setAlive grid w (number 2) (number 2)
  rebuildPackedCounts grid w h
  done

seedBlinkerHorizontal ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
seedBlinkerHorizontal grid w h = do
  toSyntax_ (u8Fill grid (number 0))
  setAlive grid w (number 1) (number 2)
  setAlive grid w (number 2) (number 2)
  setAlive grid w (number 3) (number 2)
  rebuildPackedCounts grid w h
  done

seedBlinkerVertical ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
seedBlinkerVertical grid w h = do
  toSyntax_ (u8Fill grid (number 0))
  setAlive grid w (number 2) (number 1)
  setAlive grid w (number 2) (number 2)
  setAlive grid w (number 2) (number 3)
  rebuildPackedCounts grid w h
  done

blockCoords :: EffectSyntax f (Expr f ('Array ('Array 'Number)))
blockCoords =
  bindExpr $
    Array.fromEffects
      [ Array.fromEffects [expr (number 1), expr (number 1)]
      , Array.fromEffects [expr (number 2), expr (number 1)]
      , Array.fromEffects [expr (number 1), expr (number 2)]
      , Array.fromEffects [expr (number 2), expr (number 2)]
      ]

beehiveCoords :: EffectSyntax f (Expr f ('Array ('Array 'Number)))
beehiveCoords =
  bindExpr $
    Array.fromEffects
      [ Array.fromEffects [expr (number 1), expr (number 0)]
      , Array.fromEffects [expr (number 2), expr (number 0)]
      , Array.fromEffects [expr (number 0), expr (number 1)]
      , Array.fromEffects [expr (number 3), expr (number 1)]
      , Array.fromEffects [expr (number 1), expr (number 2)]
      , Array.fromEffects [expr (number 2), expr (number 2)]
      ]

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

blinkerHorizontalCoords :: EffectSyntax f (Expr f ('Array ('Array 'Number)))
blinkerHorizontalCoords =
  bindExpr $
    Array.fromEffects
      [ Array.fromEffects [expr (number 1), expr (number 2)]
      , Array.fromEffects [expr (number 2), expr (number 2)]
      , Array.fromEffects [expr (number 3), expr (number 2)]
      ]

blinkerVerticalCoords :: EffectSyntax f (Expr f ('Array ('Array 'Number)))
blinkerVerticalCoords =
  bindExpr $
    Array.fromEffects
      [ Array.fromEffects [expr (number 2), expr (number 1)]
      , Array.fromEffects [expr (number 2), expr (number 2)]
      , Array.fromEffects [expr (number 2), expr (number 3)]
      ]

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
  liveList <- bindExpr $ Array.fromEffects []
  changedList <- bindExpr $ Array.fromEffects []
  toSyntax_ (u8Copy nextAlive alive)
  toSyntax_ (u8Fill nextSpecies (number 0))
  processCell
    alive
    species
    nextAlive
    nextSpecies
    liveList
    changedList
    stepCtx
    counts
    touchedBuf
    w
    h
    x
    y
  let
    i = cellIdx w x y
  aliveBit nextAlive i

runStepGridOnce ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
runStepGridOnce alive species nextAlive nextSpecies w h x0 y0 x1 y1 = do
  prevLiveList <- bindExpr $ Array.fromEffects []
  nextLiveList <- bindExpr $ Array.fromEffects []
  nextChangedList <- bindExpr $ Array.fromEffects []
  stepStamp <- bindExpr (newByteArray (w * h))
  stepCtx <- hold (toObject (StepCtx 0 0 (-1) (-1) 0 0 0 0 0))
  counts <- bindExpr (newByteArray (number 256))
  touchedBuf <- bindExpr (newByteArray (number 8))
  stepGrid
    alive
    species
    nextAlive
    nextSpecies
    w
    h
    x0
    y0
    x1
    y1
    prevLiveList
    nextLiveList
    nextChangedList
    stepStamp
    (number 1)
    (number 0)
    stepCtx
    counts
    touchedBuf
