{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Typed grid buffer and canvas helpers. Hot grid loops use native JShark
--    @forRange_@ and @u8Index@/@u8Set@ so the compiler emits tight JS @for@
--    loops instead of @forEach@ callbacks.
module Grid
  ( ImageData
  , u8Get
  , setU8
  , createImageData
  , putImageData
  , imageDataBytes
  , stepGrid
  , renderGrid
  , cellIdx
  , toroidal
  )
where

import GHC.Generics (Generic)
import JShark.Api
import qualified JShark.Canvas as Canvas
import JShark.Generic (MutableObjectOf, toObject)
import JShark.Rec (Rec (..), (<:))

data ImageData

data StepScratch = StepScratch
  { pop :: Double
  , touchedLen :: Double
  , best :: Double
  , bestCount :: Double
  , n :: Double
  }
  deriving Generic

u8Get ::
  Expr f 'Uint8Array -> Expr f 'Number -> EffectSyntax f (Expr f 'Number)
u8Get buf i = pure (u8Index buf i)

setU8 ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
setU8 buf i v = toSyntax (u8Set buf i v)

createImageData ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Effect f ('MutableObject ImageData))
createImageData ctx w h =
  hold $ callMethod ctx "createImageData" (arg w <: arg h <: RecNil)

putImageData ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f ('MutableObject ImageData)
  -> EffectSyntax f (f 'Unit)
putImageData ctx img = do
  toSyntax_
    $ discard
    $ callMethod
      ctx
      "putImageData"
      (arg img <: arg (number 0) <: arg (number 0) <: RecNil)
  done

imageDataBytes ::
  Expr f ('MutableObject ImageData) -> EffectSyntax f (Expr f 'Uint8Array)
imageDataBytes img = bindExpr $ ffi "((img) => img.data)" (arg img <: RecNil)

stepGrid ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
stepGrid alive species nextAlive nextSpecies w h = do
  counts <- bindExpr (newByteArray (number 256))
  touchedBuf <- bindExpr (newByteArray (number 8))
  popScratch <- hold (toObject (StepScratch 0 0 0 0 0))
  cellScratch <- hold (toObject (StepScratch 0 0 0 0 0))
  forRange_ (number 0) h $ \y ->
    forRange_ (number 0) w $ \x ->
      processCell
        alive
        species
        nextAlive
        nextSpecies
        counts
        touchedBuf
        popScratch
        cellScratch
        w
        h
        x
        y
  popScratch.pop

processCell ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f (MutableObjectOf StepScratch)
  -> Effect f (MutableObjectOf StepScratch)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
processCell alive species nextAlive nextSpecies counts touchedBuf popScratch cellScratch w h x y = do
  set @"touchedLen" cellScratch 0
  set @"best" cellScratch 0
  set @"bestCount" cellScratch 0
  set @"n" cellScratch 0
  forRange_ (number (-1)) (number 2) $ \dy ->
    forRange_ (number (-1)) (number 2) $ \dx ->
      whenS (not_ (dx .== 0 .&& dy .== 0)) $
        countNeighbor alive species counts touchedBuf cellScratch w h x y dx dy
  let
    i = cellIdx w x y
  nCount <- cellScratch.n
  a <- u8Get alive i
  sp <- u8Get species i
  bestSp <- cellScratch.best
  ifS
    (a .== 1)
    ( ifS
        (nCount .== 2 .|| nCount .== 3)
        ( do
            setU8 nextAlive i 1
            setU8 nextSpecies i sp
            bumpPop popScratch
        )
        ( do
            setU8 nextAlive i 0
            setU8 nextSpecies i 0
        )
    )
    ( ifS
        (nCount .== 3)
        ( do
            setU8 nextAlive i 1
            setU8 nextSpecies i bestSp
            bumpPop popScratch
        )
        ( do
            setU8 nextAlive i 0
            setU8 nextSpecies i 0
        )
    )
  resetTouched counts touchedBuf cellScratch

countNeighbor ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f (MutableObjectOf StepScratch)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
countNeighbor alive species counts touchedBuf scratch w h x y dx dy = do
  let
    nx = toroidal w (x + dx)
    ny = toroidal h (y + dy)
    ni = cellIdx w nx ny
  a <- u8Get alive ni
  whenS (a .== 1) $ do
    sp <- u8Get species ni
    n0 <- scratch.n
    set @"n" scratch (n0 + 1)
    cur <- u8Get counts sp
    let
      next = cur + 1
    whenS (cur .== 0) $ do
      len <- scratch.touchedLen
      setU8 touchedBuf len sp
      set @"touchedLen" scratch (len + 1)
    setU8 counts sp next
    bestC <- scratch.bestCount
    bestSp <- scratch.best
    whenS
      (next .> bestC .|| (next .== bestC .&& sp .< bestSp))
      ( do
          set @"bestCount" scratch next
          set @"best" scratch sp
      )

resetTouched ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f (MutableObjectOf StepScratch)
  -> EffectSyntax f (f 'Unit)
resetTouched counts touchedBuf scratch = do
  len <- scratch.touchedLen
  forRange_ (number 0) len $ \j -> do
    sp <- u8Get touchedBuf j
    setU8 counts sp 0

bumpPop :: Effect f (MutableObjectOf StepScratch) -> EffectSyntax f (f 'Unit)
bumpPop scratch = do
  p <- scratch.pop
  set @"pop" scratch (p + 1)

renderGrid ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
renderGrid pixels alive species pal w h px cw = do
  let
    pixLen = u8Len pixels
    bgCells = pixLen / number 4
  forRange_ (number 0) bgCells $ \cell ->
    let
      base = cell * number 4
     in
      do
        setU8 pixels base 15
        setU8 pixels (base + 1) 23
        setU8 pixels (base + 2) 42
        setU8 pixels (base + 3) 255
  forRange_ (number 0) h $ \y ->
    forRange_ (number 0) w $ \x -> do
      let
        gi = cellIdx w x y
      a <- u8Get alive gi
      whenS (a .== 1) $ do
        sp <- u8Get species gi
        let
          palBase = sp * number 3
        r <- u8Get pal palBase
        g <- u8Get pal (palBase + 1)
        b <- u8Get pal (palBase + 2)
        let
          py = y * px
          px0 = x * px
        forRange_ (number 0) px $ \dy ->
          let
            row = (py + dy) * cw
           in
            forRange_ (number 0) px $ \dx ->
              let
                pix = (row + px0 + dx) * number 4
               in
                do
                  setU8 pixels pix r
                  setU8 pixels (pix + 1) g
                  setU8 pixels (pix + 2) b
  done

cellIdx :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -> Expr f 'Number
cellIdx w x y = y * w + x

toroidal :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
toroidal w c = rem_ (c + w) w
