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
  , CanvasDirty (..)
  , StepScratch (..)
  , u8Get
  , setU8
  , stepGrid
  , processCell
  , expandBoundsForLive
  , drawGridViewport
  , initPaletteRgba
  , rebuildPackedCounts
  , rebuildLiveList
  , packedIsAlive
  , bumpPackedNeighbors
  , setPackedAlive
  , refreshPackedAt
  , refreshPackedRegion
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
import JShark.Rec (Rec (..), (<:))

data StepScratch = StepScratch
  { pop :: Double
  , touchedLen :: Double
  , best :: Double
  , bestCount :: Double
  , n :: Double
  }
  deriving Generic

data CanvasDirty = CanvasDirty
  { cx0 :: Double
  , cy0 :: Double
  , cx1 :: Double
  , cy1 :: Double
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

packedIsAlive :: Expr f 'Uint8Array -> Expr f 'Number -> Expr f 'Bool
packedIsAlive grid i = bitAnd (u8Index grid i) (number 1) .== 1

packedCount :: Expr f 'Number -> Expr f 'Number
packedCount b = shr b (number 1)

rebuildPackedCounts ::
  Expr f 'Uint8Array -> Expr f 'Number -> Expr f 'Number -> Effect f 'Unit
rebuildPackedCounts grid w h =
  ffi
    "((g,w,h)=>{const n=w*h|0;for(let i=0;i<n;i++)g[i]&=1;for(let y=0;y<h;y++){for(let x=0;x<w;x++){const i=y*w+x;if(g[i]&1){for(let dy=-1;dy<=1;dy++){for(let dx=-1;dx<=1;dx++){if(!dx&&!dy)continue;const nx=x+dx,ny=y+dy;if(nx<0||ny<0||nx>=w||ny>=h)continue;g[ny*w+nx]+=2;}}}}}})"
    (arg grid <: arg w <: arg h <: RecNil)

bumpPackedNeighbors ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
bumpPackedNeighbors grid w h x y delta =
  forRange_ (number (-1)) (number 2) $ \dy ->
    forRange_ (number (-1)) (number 2) $ \dx ->
      whenS (not_ (dx .== 0 .&& dy .== 0)) $ do
        let
          nx = x + dx
          ny = y + dy
        whenS (inBounds w h nx ny) $ do
          let
            ni = cellIdx w nx ny
          cur <- u8Get grid ni
          setU8 grid ni (cur + delta)

setPackedAlive ::
  Expr f 'Uint8Array -> Expr f 'Number -> Expr f 'Number -> EffectSyntax f (f 'Unit)
setPackedAlive grid i alive = do
  cur <- u8Get grid i
  setU8 grid i (bitAnd cur (number 0xFE) + alive)

-- | Recompute packed neighbor count for one cell from live bits.
refreshPackedAt ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
refreshPackedAt grid w h x y = do
  toSyntax_ $
    ffi
      ( "((g,w,h,x,y)=>{"
          <> "let n=0;"
          <> "for(let dy=-1;dy<=1;dy++){for(let dx=-1;dx<=1;dx++){"
          <> "if(!dx&&!dy)continue;"
          <> "const nx=x+dx,ny=y+dy;"
          <> "if(nx<0||ny<0||nx>=w||ny>=h)continue;"
          <> "if(g[ny*w+nx]&1)n++;"
          <> "}}"
          <> "const i=y*w+x;"
          <> "g[i]=(g[i]&1)+n*2;"
          <> "})"
      )
      (arg grid <: arg w <: arg h <: arg x <: arg y <: RecNil)
  done

-- | Refresh packed counts in a bbox plus one-cell margin.
refreshPackedRegion ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f 'Unit
refreshPackedRegion grid w h x0 y0 x1 y1 =
  ffi
    ( "((g,w,h,x0,y0,x1,y1)=>{"
        <> "const xs=Math.max(0,Math.floor(x0)-1);"
        <> "const ys=Math.max(0,Math.floor(y0)-1);"
        <> "const xe=Math.min(w-1,Math.floor(x1)+1);"
        <> "const ye=Math.min(h-1,Math.floor(y1)+1);"
        <> "for(let y=ys;y<=ye;y++){for(let x=xs;x<=xe;x++){"
        <> "let n=0;"
        <> "for(let dy=-1;dy<=1;dy++){for(let dx=-1;dx<=1;dx++){"
        <> "if(!dx&&!dy)continue;"
        <> "const nx=x+dx,ny=y+dy;"
        <> "if(nx<0||ny<0||nx>=w||ny>=h)continue;"
        <> "if(g[ny*w+nx]&1)n++;"
        <> "}}"
        <> "const i=y*w+x;"
        <> "g[i]=(g[i]&1)+n*2;"
        <> "}}})"
    )
    ( arg grid
        <: arg w
        <: arg h
        <: arg x0
        <: arg y0
        <: arg x1
        <: arg y1
        <: RecNil
    )

initPaletteRgba ::
  Expr f 'Uint8Array -> EffectSyntax f (Expr f 'Uint8Array)
initPaletteRgba pal = do
  rgba <- bindExpr (newByteArray (number (256 * 4)))
  forRange_ (number 0) (number 256) $ \sid -> do
    let
      base = sid * number 3
    r <- u8Get pal base
    g <- u8Get pal (base + number 1)
    b <- u8Get pal (base + number 2)
    let
      px = sid * number 4
    setU8 rgba px r
    setU8 rgba (px + number 1) g
    setU8 rgba (px + number 2) b
    setU8 rgba (px + number 3) (number 255)
  pure rgba

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
      whenS (bitAnd a (number 1) .== 1) (Array.push_ liveList i)
  done

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
  -> Expr f ('Array 'Number)
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f (MutableObjectOf StepScratch)
  -> Effect f (MutableObjectOf StepScratch)
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
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
  nextChangedList
  stepStamp
  stepTag
  prevPop
  popScratch
  cellScratch
  birthCounts
  birthTouched
  boundScratch = do
  let
    (xStart, yStart, xStop, yStop) =
      clampLiveBounds w h x0 y0 x1 y1 (number 1)
    regionCells = (xStop - xStart) * (yStop - yStart)
  ifS
    (regionCells .> (w * h) / number 2)
    ( do
        toSyntax_ (u8Copy nextAlive alive)
        done
    )
    ( do
        toSyntax_
          (u8CopyRegion nextAlive alive w xStart yStart xStop yStop)
        done
    )
  ifS
    (regionCells .> (w * h) / number 2)
    ( do
        toSyntax_ (u8Fill nextSpecies (number 0))
        done
    )
    ( do
        toSyntax_
          (u8FillRegion nextSpecies w xStart yStart xStop yStop (number 0))
        done
    )
  set @"bx0" boundScratch (number 1e9)
  set @"by0" boundScratch (number 1e9)
  set @"bx1" boundScratch (number (-1))
  set @"by1" boundScratch (number (-1))
  set @"pop" popScratch 0
  let
    runCell x y =
      processCell
        alive
        species
        nextAlive
        nextSpecies
        nextLiveList
        nextChangedList
        popScratch
        cellScratch
        boundScratch
        birthCounts
        birthTouched
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
  -> Expr f ('Array 'Number)
  -> Effect f (MutableObjectOf StepScratch)
  -> Effect f (MutableObjectOf StepScratch)
  -> Effect f (MutableObjectOf BoundScratch)
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
processCell
  alive
  species
  nextAlive
  nextSpecies
  nextLiveList
  nextChangedList
  popScratch
  cellScratch
  boundScratch
  counts
  touchedBuf
  w
  h
  x
  y = do
  let
    i = cellIdx w x y
  b <- u8Get alive i
  let
    alive0 = bitAnd b (number 1)
    nCount = packedCount b
  sp <- u8Get species i
  set @"touchedLen" cellScratch 0
  set @"best" cellScratch 0
  set @"bestCount" cellScratch 0
  whenS (alive0 .== 0 .&& nCount .== 3) $
    forRange_ (number (-1)) (number 2) $ \dy ->
      forRange_ (number (-1)) (number 2) $ \dx ->
        whenS (not_ (dx .== 0 .&& dy .== 0)) $
          countBirthSpecies alive species counts touchedBuf cellScratch w h x y dx dy
  bestSp <- cellScratch.best
  whenS
    (alive0 .== 1 .&& (nCount .== 2 .|| nCount .== 3))
    ( do
        setU8 nextSpecies i sp
        Array.push_ nextLiveList i
        bumpPop popScratch
        bumpBounds boundScratch x y
    )
  whenS
    (alive0 .== 1 .&& not_ (nCount .== 2 .|| nCount .== 3))
    ( markDead
        nextAlive
        nextSpecies
        nextLiveList
        nextChangedList
        w
        h
        x
        y
        i
    )
  whenS
    (alive0 .== 0 .&& nCount .== 3)
    ( markBorn
        nextAlive
        nextSpecies
        nextLiveList
        nextChangedList
        popScratch
        boundScratch
        w
        h
        x
        y
        i
        bestSp
    )
  whenS (alive0 .== 0 .&& nCount .!= 3) (setU8 nextSpecies i (number 0))
  resetBirthCounts counts touchedBuf cellScratch

markBorn ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Effect f (MutableObjectOf StepScratch)
  -> Effect f (MutableObjectOf BoundScratch)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
markBorn nextAlive nextSpecies nextLiveList nextChangedList popScratch boundScratch w h x y i sp = do
  setPackedAlive nextAlive i (number 1)
  bumpPackedNeighbors nextAlive w h x y (number 2)
  setU8 nextSpecies i sp
  Array.push_ nextLiveList i
  Array.push_ nextChangedList i
  bumpPop popScratch
  bumpBounds boundScratch x y

markDead ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
markDead nextAlive nextSpecies _nextLiveList nextChangedList w h x y i = do
  setPackedAlive nextAlive i (number 0)
  bumpPackedNeighbors nextAlive w h x y (number (-2))
  setU8 nextSpecies i 0
  Array.push_ nextChangedList i

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

countBirthSpecies ::
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
countBirthSpecies alive species counts touchedBuf scratch w h x y dx dy = do
  let
    nx = x + dx
    ny = y + dy
  whenS (inBounds w h nx ny) $ do
    let
      ni = cellIdx w nx ny
    whenS (packedIsAlive alive ni) $ do
      sp <- u8Get species ni
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

resetBirthCounts ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f (MutableObjectOf StepScratch)
  -> EffectSyntax f (f 'Unit)
resetBirthCounts counts touchedBuf scratch = do
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
      whenS (packedIsAlive alive i) (bumpBounds scratch x y)
  pure scratch

bumpCanvasDirty ::
  Effect f (MutableObjectOf CanvasDirty)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
bumpCanvasDirty scratch x0 y0 x1 y1 = do
  curCx0 <- scratch.cx0
  curCy0 <- scratch.cy0
  curCx1 <- scratch.cx1
  curCy1 <- scratch.cy1
  _ <- set @"cx0" scratch (Math.min curCx0 x0)
  _ <- set @"cy0" scratch (Math.min curCy0 y0)
  _ <- set @"cx1" scratch (Math.max curCx1 x1)
  set @"cy1" scratch (Math.max curCy1 y1)

paintGridCell ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f (MutableObjectOf CanvasDirty)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
paintGridCell pixels cw ch paletteRgba alive species w scale panX panY bg dirtyScratch gi = do
  let
    x = rem_ gi w
    y = Math.floor (gi / w)
    sx0 = Math.floor (x * scale + panX)
    sx1 = Math.ceil ((x + number 1) * scale + panX)
    sy0 = Math.floor (y * scale + panY)
    sy1 = Math.ceil ((y + number 1) * scale + panY)
    cellW = sx1 - sx0
    cellH = sy1 - sy0
  whenS
    ( cellW .> 0
        .&& cellH .> 0
        .&& sx1 .> 0
        .&& sy1 .> 0
        .&& sx0 .< cw
        .&& sy0 .< ch
    )
    ( do
        sp <- u8Get species gi
        let
          base = sp * number 4
        r <- u8Get paletteRgba base
        g <- u8Get paletteRgba (base + number 1)
        b <- u8Get paletteRgba (base + number 2)
        a <- u8Get paletteRgba (base + number 3)
        let
          liveColor = r + shl g (number 8) + shl b (number 16) + shl a (number 24)
          dx0 = Math.max (number 0) sx0
          dy0 = Math.max (number 0) sy0
          dx1 = Math.min cw sx1
          dy1 = Math.min ch sy1
        ifS
          (packedIsAlive alive gi)
          ( do
              toSyntax_ (rgbaFillRect pixels cw ch sx0 sy0 cellW cellH liveColor)
              done
          )
          ( do
              toSyntax_ (rgbaFillRect pixels cw ch sx0 sy0 cellW cellH bg)
              done
          )
        bumpCanvasDirty dirtyScratch dx0 dy0 dx1 dy1
        done
    )

-- | RGBA buffer draw: no @fillRect@ loop; dirty @putImageData@ blit only.
drawGridViewport ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f ('MutableObject Canvas.ImageData)
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Expr f 'Bool
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f (MutableObjectOf CanvasDirty)
  -> EffectSyntax f (f 'Unit)
drawGridViewport
  ctx
  img
  pixels
  paletteRgba
  alive
  species
  liveList
  changedList
  fullRedraw
  w
  h
  px
  cw
  ch
  panX
  panY
  zoomLevel
  dirtyScratch = do
  let
    scale = px * zoomLevel
    visX0 = Math.max (number 0) (Math.floor ((number 0 - panX) / scale) - number 1)
    visX1 = Math.min w (Math.ceil ((cw - panX) / scale) + number 1)
    visY0 = Math.max (number 0) (Math.floor ((number 0 - panY) / scale) - number 1)
    visY1 = Math.min h (Math.ceil ((ch - panY) / scale) + number 1)
    bg =
      number 15
        + shl (number 23) (number 8)
        + shl (number 42) (number 16)
        + shl (number 255) (number 24)
  set @"cx0" dirtyScratch (number 1e9)
  set @"cy0" dirtyScratch (number 1e9)
  set @"cx1" dirtyScratch (number (-1))
  set @"cy1" dirtyScratch (number (-1))
  ifS
    fullRedraw
    ( do
        toSyntax_
          ( fillRgbaImageData
              pixels
              (number 15)
              (number 23)
              (number 42)
              (number 255)
          )
        forRange_ (number 0) (Array.length liveList) $ \k -> do
          let
            gi = Array.index liveList k
            x = rem_ gi w
            y = Math.floor (gi / w)
          whenS
            (x .>= visX0 .&& x .< visX1 .&& y .>= visY0 .&& y .< visY1)
            ( paintGridCell
                pixels
                cw
                ch
                paletteRgba
                alive
                species
                w
                scale
                panX
                panY
                bg
                dirtyScratch
                gi
            )
        Canvas.putImageData ctx img (number 0) (number 0)
    )
    ( whenS (Array.length changedList .> 0) $
        do
          forRange_ (number 0) (Array.length changedList) $ \k -> do
            let
              gi = Array.index changedList k
              x = rem_ gi w
              y = Math.floor (gi / w)
            whenS
              (x .>= visX0 .&& x .< visX1 .&& y .>= visY0 .&& y .< visY1)
              ( paintGridCell
                  pixels
                  cw
                  ch
                  paletteRgba
                  alive
                  species
                  w
                  scale
                  panX
                  panY
                  bg
                  dirtyScratch
                  gi
              )
          dirtyCx0 <- dirtyScratch.cx0
          dirtyCy0 <- dirtyScratch.cy0
          dirtyCx1 <- dirtyScratch.cx1
          dirtyCy1 <- dirtyScratch.cy1
          let
            ix0 = Math.floor dirtyCx0
            iy0 = Math.floor dirtyCy0
            ix1 = Math.ceil dirtyCx1
            iy1 = Math.ceil dirtyCy1
            dw = ix1 - ix0
            dh = iy1 - iy0
          whenS (dw .> 0 .&& dh .> 0) $
            Canvas.putImageDataRegion ctx img (number 0) (number 0) ix0 iy0 dw dh
    )
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
