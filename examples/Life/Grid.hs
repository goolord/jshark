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
  ( BoundScratch (..)
  , ImageData
  , u8Get
  , setU8
  , createImageData
  , putImageData
  , imageDataBytes
  , stepGrid
  , expandBoundsForLive
  , renderGridViewport
  , cellIdx
  , inBounds
  )
where

import GHC.Generics (Generic)
import JShark.Api
import qualified JShark.Canvas as Canvas
import JShark.Generic (MutableObjectOf, toObject)
import qualified JShark.Math as Math
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

data BoundScratch = BoundScratch
  { bx0 :: Double
  , by0 :: Double
  , bx1 :: Double
  , by1 :: Double
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
imageDataBytes img = getProp (expr img) "data"

stepGrid ::
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
  -> Effect f (MutableObjectOf BoundScratch)
  -> EffectSyntax f (Expr f 'Number)
stepGrid alive species nextAlive nextSpecies w h x0 y0 x1 y1 boundScratch = do
  toSyntax (u8Fill nextAlive (number 0))
  toSyntax (u8Fill nextSpecies (number 0))
  set @"bx0" boundScratch (number 1e9)
  set @"by0" boundScratch (number 1e9)
  set @"bx1" boundScratch (number (-1))
  set @"by1" boundScratch (number (-1))
  counts <- bindExpr (newByteArray (number 256))
  touchedBuf <- bindExpr (newByteArray (number 8))
  popScratch <- hold (toObject (StepScratch 0 0 0 0 0))
  cellScratch <- hold (toObject (StepScratch 0 0 0 0 0))
  let
    xStart = Math.max (number 0) (x0 - number 1)
    yStart = Math.max (number 0) (y0 - number 1)
    xEnd = Math.min (w - number 1) (x1 + number 1)
    yEnd = Math.min (h - number 1) (y1 + number 1)
  forRange_ yStart (yEnd + number 1) $ \y ->
    forRange_ xStart (xEnd + number 1) $ \x ->
      processCell
        alive
        species
        nextAlive
        nextSpecies
        counts
        touchedBuf
        popScratch
        cellScratch
        boundScratch
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
  -> Effect f (MutableObjectOf BoundScratch)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
processCell alive species nextAlive nextSpecies counts touchedBuf popScratch cellScratch boundScratch w h x y = do
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
            bumpBounds boundScratch x y
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
            bumpBounds boundScratch x y
        )
        ( do
            setU8 nextAlive i 0
            setU8 nextSpecies i 0
        )
    )
  resetTouched counts touchedBuf cellScratch

bumpBounds ::
  Effect f (MutableObjectOf BoundScratch)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
bumpBounds scratch x y = do
  x0 <- scratch.bx0
  y0 <- scratch.by0
  x1 <- scratch.bx1
  y1 <- scratch.by1
  _ <- set @"bx0" scratch (Math.min x0 x)
  _ <- set @"by0" scratch (Math.min y0 y)
  _ <- set @"bx1" scratch (Math.max x1 x)
  set @"by1" scratch (Math.max y1 y)

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
    nx = x + dx
    ny = y + dy
  whenS (inBounds w h nx ny) $ do
    let
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

expandBoundsForLive ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Effect f (MutableObjectOf BoundScratch))
expandBoundsForLive alive w h x0 y0 x1 y1 = do
  scratch <- hold (toObject (BoundScratch 0 0 (-1) (-1)))
  set @"bx0" scratch x0
  set @"by0" scratch y0
  set @"bx1" scratch x1
  set @"by1" scratch y1
  let
    xa = Math.max (number 0) (x0 - number 1)
    ya = Math.max (number 0) (y0 - number 1)
    xb = Math.min (w - number 1) (x1 + number 1)
    yb = Math.min (h - number 1) (y1 + number 1)
  forRange_ ya (yb + number 1) $ \y ->
    forRange_ xa (xb + number 1) $ \x -> do
      let
        i = cellIdx w x y
      a <- u8Get alive i
      whenS (a .== 1) (bumpBounds scratch x y)
  pure scratch

renderGridViewport ::
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
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
renderGridViewport pixels alive species pal w h px cw ch panX panY zoom = do
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
  let
    cellDraw = Math.max (number 1) (Math.floor (px * zoom))
    gx0 =
      Math.max
        (number 0)
        (Math.floor ((number 0 - panX) / zoom / px))
    gx1 =
      Math.min
        w
        (Math.ceil ((cw - panX) / zoom / px))
    gy0 =
      Math.max
        (number 0)
        (Math.floor ((number 0 - panY) / zoom / px))
    gy1 =
      Math.min
        h
        (Math.ceil ((ch - panY) / zoom / px))
  forRange_ gy0 gy1 $ \y ->
    forRange_ gx0 gx1 $ \x -> do
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
          sx = Math.floor (x * px * zoom + panX)
          sy = Math.floor (y * px * zoom + panY)
        forRange_ (number 0) cellDraw $ \dy ->
          let
            py = sy + dy
           in
            whenS (py .>= 0 .&& py .< ch) $
              forRange_ (number 0) cellDraw $ \dx ->
                let
                  pxX = sx + dx
                  pix = (py * cw + pxX) * number 4
                 in
                  whenS (pxX .>= 0 .&& pxX .< cw) $
                    do
                      setU8 pixels pix r
                      setU8 pixels (pix + 1) g
                      setU8 pixels (pix + 2) b
  done

cellIdx :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -> Expr f 'Number
cellIdx w x y = y * w + x

inBounds ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Bool
inBounds w h x y = x .>= 0 .&& y .>= 0 .&& x .< w .&& y .< h
