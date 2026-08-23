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
  , u8Get
  , setU8
  , stepGrid
  , expandBoundsForLive
  , drawGridViewport
  , initPaletteCss
  , rebuildLiveList
  , cellIdx
  , inBounds
  , clampLiveBounds
  )
where

import GHC.Generics (Generic)
import JShark.Api
import qualified JShark.Array as Array
import qualified JShark.Canvas as Canvas
import JShark.Generic (MutableObjectOf, toObject)
import qualified JShark.Math as Math
import Types (canvasBg, gridH)

data StepScratch = StepScratch
  { pop :: Double
  , touchedLen :: Double
  , best :: Double
  , bestCount :: Double
  , n :: Double
  }
  deriving Generic

data DrawScratch = DrawScratch
  { lastSp :: Double
  , pathLen :: Double
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

fillCtx ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
fillCtx ctx col = set @"fillStyle" ctx col

rebuildLiveList ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f ('Array 'Number)
  -> EffectSyntax f (f 'Unit)
rebuildLiveList alive w h x0 y0 x1 y1 liveList = do
  Array.clear_ liveList
  let
    (xStart, yStart, xStop, yStop) =
      clampLiveBounds w h x0 y0 x1 y1 (number 0)
  forRange_ yStart yStop $ \y ->
    forRange_ xStart xStop $ \x -> do
      let
        i = cellIdx w x y
      a <- u8Get alive i
      whenS (a .== 1) (Array.push_ liveList i)
  done

paletteCssColor ::
  Expr f 'Uint8Array -> Expr f 'Number -> EffectSyntax f (Expr f 'String)
paletteCssColor pal sid = do
  let
    base = sid * number 3
  r <- u8Get pal base
  g <- u8Get pal (base + number 1)
  b <- u8Get pal (base + number 2)
  pure
    ( string "rgb("
        <> toString r
        <> string ","
        <> toString g
        <> string ","
        <> toString b
        <> string ")"
    )

initPaletteCss ::
  Expr f 'Uint8Array -> EffectSyntax f (Expr f ('Array 'String))
initPaletteCss pal = do
  css <- bindExpr $ Array.fromEffects []
  forRange_ (number 0) (number 256) $ \sid -> do
    col <- paletteCssColor pal sid
    Array.push_ css col
  pure css

-- | Half-open scan range @\[xStart, xStop) × \[yStart, yStop)@ clamped to the grid.
-- @margin@ expands live bounds (use @1@ for step/discovery halos).
clampLiveBounds ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> ( Expr f 'Number
     , Expr f 'Number
     , Expr f 'Number
     , Expr f 'Number
     )
clampLiveBounds w h x0 y0 x1 y1 margin =
  let
    xStart = Math.max (number 0) (x0 - margin)
    yStart = Math.max (number 0) (y0 - margin)
    xEnd = Math.min (w - number 1) (x1 + margin)
    yEnd = Math.min (h - number 1) (y1 + margin)
   in
    (xStart, yStart, xEnd + number 1, yEnd + number 1)

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
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f (MutableObjectOf BoundScratch)
  -> EffectSyntax f (Expr f 'Number)
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
  stepStamp
  stepTag
  prevPop
  boundScratch = do
  let
    (xStart, yStart, xStop, yStop) =
      clampLiveBounds w h x0 y0 x1 y1 (number 1)
    regionCells = (xStop - xStart) * (yStop - yStart)
  ifS
    (regionCells .> (w * h) / number 2)
    ( do
        toSyntax_ (u8Fill nextAlive (number 0))
        toSyntax_ (u8Fill nextSpecies (number 0))
        done
    )
    ( do
        toSyntax_ (u8FillRegion nextAlive w xStart yStart xStop yStop (number 0))
        toSyntax_ (u8FillRegion nextSpecies w xStart yStart xStop yStop (number 0))
        done
    )
  set @"bx0" boundScratch (number 1e9)
  set @"by0" boundScratch (number 1e9)
  set @"bx1" boundScratch (number (-1))
  set @"by1" boundScratch (number (-1))
  counts <- bindExpr (newByteArray (number 256))
  touchedBuf <- bindExpr (newByteArray (number 8))
  popScratch <- hold (toObject (StepScratch 0 0 0 0 0))
  cellScratch <- hold (toObject (StepScratch 0 0 0 0 0))
  let
    runCell x y =
      processCell
        alive
        species
        nextAlive
        nextSpecies
        nextLiveList
        counts
        touchedBuf
        popScratch
        cellScratch
        boundScratch
        w
        h
        x
        y
    runIndex i = do
      -- Stamp dedup only on the sparse path; dense scans call 'runCell' directly.
      s <- u8Get stepStamp i
      whenS (s .!= stepTag) $ do
        setU8 stepStamp i stepTag
        let
          x = rem_ i w
          y = Math.floor (i / w)
        runCell x y
    runIndexWithNeighbors i = do
      runIndex i
      let
        x = rem_ i w
        y = Math.floor (i / w)
      forRange_ (number (-1)) (number 2) $ \dy ->
        forRange_ (number (-1)) (number 2) $ \dx ->
          whenS (not_ (dx .== 0 .&& dy .== 0)) $ do
            let
              nx = x + dx
              ny = y + dy
            whenS (inBounds w h nx ny) $ runIndex (cellIdx w nx ny)
  -- Sparse when bbox scan would touch >> live cells (~12× pop crossover in practice).
  ifS
    ( prevPop .> 0
        .&& regionCells .> prevPop * number 12
        .&& Array.length prevLiveList .> 0
    )
    ( forRange_ (number 0) (Array.length prevLiveList) $ \k -> do
        let
          i = Array.index prevLiveList k
        runIndexWithNeighbors i
    )
    ( forRange_ yStart yStop $ \y ->
        forRange_ xStart xStop $ \x ->
          runCell x y
    )
  popScratch.pop

processCell ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f ('Array 'Number)
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
processCell alive species nextAlive nextSpecies nextLiveList counts touchedBuf popScratch cellScratch boundScratch w h x y = do
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
            Array.push_ nextLiveList i
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
            Array.push_ nextLiveList i
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

-- | GPU draw path using a live-cell index list (@O(pop)@), not bbox area.
drawGridViewport ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f 'Uint8Array
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'String)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
drawGridViewport ctx species liveList paletteCss w px cw ch panX panY zoom = do
  fillCtx ctx (string canvasBg)
  Canvas.fillRect ctx (number 0) (number 0) cw ch
  let
    scale = px * zoom
    maxPathLen = number 4096
    sorted =
      Array.toSorted liveList (\i j -> u8Index species i - u8Index species j)
    gx0 =
      Math.max
        (number 0)
        (Math.floor ((number 0 - panX) / scale) - number 1)
    gx1 =
      Math.min
        w
        (Math.ceil ((cw - panX) / scale) + number 1)
    gy0 =
      Math.max
        (number 0)
        (Math.floor ((number 0 - panY) / scale) - number 1)
    gy1 =
      Math.min
        (number (fromIntegral gridH))
        (Math.ceil ((ch - panY) / scale) + number 1)
  Canvas.save ctx
  Canvas.translate ctx panX panY
  Canvas.scale ctx scale scale
  drawScratch <- hold (toObject (DrawScratch (-1) 0))
  forRange_ (number 0) (Array.length sorted) $ \k -> do
    let
      gi = Array.index sorted k
      x = rem_ gi w
      y = Math.floor (gi / w)
    whenS
      ( x .>= gx0
          .&& x .< gx1
          .&& y .>= gy0
          .&& y .< gy1
      )
      ( do
          sp <- u8Get species gi
          prevSp <- drawScratch.lastSp
          pathCount <- drawScratch.pathLen
          whenS (sp .!= prevSp .|| pathCount .>= maxPathLen) $ do
            whenS (prevSp .>= 0) (Canvas.fill ctx)
            Canvas.beginPath ctx
            fillCtx ctx (Array.index paletteCss sp)
            set @"lastSp" drawScratch sp
            set @"pathLen" drawScratch (number 0)
          len0 <- drawScratch.pathLen
          Canvas.rect ctx x y (number 1) (number 1)
          set @"pathLen" drawScratch (len0 + number 1)
      )
  activeSp <- drawScratch.lastSp
  whenS (activeSp .>= 0) (Canvas.fill ctx)
  Canvas.restore ctx
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
