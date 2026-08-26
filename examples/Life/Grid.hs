{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Typed grid buffer and canvas helpers. Hot loops use native JShark
--    @forRange_@ and @u8Set@; irregular edit paths (@stampPatternCells@,
--    @eraseCircleCells@) stay in FFI for zero-alloc scratch writes.
module Grid
  ( BoundScratch (..)
  , RenderDirty (..)
  , StepScratch (..)
  , StepCtx (..)
  , u8Get
  , setU8
  , stepGrid
  , processCell
  , expandBoundsForLive
  , drawGridViewport
  , drawGridFallback
  , hideFallback2d
  , initPaletteRgba
  , syncPaletteRgbaSid
  , rebuildPackedCounts
  , rebuildLiveList
  , packedIsAlive
  , bumpPackedNeighbors
  , setPackedAlive
  , writeCellState
  , stampPatternCells
  , eraseCircleCells
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
import JShark.Dom (DomElement)
import qualified JShark.Dom as Dom
import JShark.Generic (MutableObjectOf)
import qualified JShark.Math as Math
import JShark.Rec (Rec (..), (<:))
import qualified Pixi

data StepScratch = StepScratch
  { pop :: Double
  , touchedLen :: Double
  , best :: Double
  , bestCount :: Double
  , n :: Double
  }
  deriving Generic

data RenderDirty = RenderDirty
  { dirtyCx0 :: Double
  , dirtyCy0 :: Double
  , dirtyCx1 :: Double
  , dirtyCy1 :: Double
  , dirtyFull :: Bool
  , dirtyPainted :: Bool
  }
  deriving Generic

data BoundScratch = BoundScratch
  { count :: Double
  , bx0 :: Double
  , by0 :: Double
  , bx1 :: Double
  , by1 :: Double
  }
  deriving Generic

-- | Reused per-frame step scratch: bounds, population, birth-tie tallies.
data StepCtx = StepCtx
  { bx0 :: Double
  , by0 :: Double
  , bx1 :: Double
  , by1 :: Double
  , pop :: Double
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

packedIsAlive :: Expr f 'Uint8Array -> Expr f 'Number -> Expr f 'Bool
packedIsAlive grid i = bitAnd (u8Index grid i) (number 1) .== 1

packedCount :: Expr f 'Number -> Expr f 'Number
packedCount b = shr b (number 1)

rebuildPackedCounts ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
rebuildPackedCounts grid w h = do
  let
    totalCells = w * h
  forRange_ (number 0) totalCells $ \i -> do
    a <- u8Get grid i
    setU8 grid i (bitAnd a (number 1))
  forRange_ (number 0) h $ \y ->
    forRange_ (number 0) w $ \x -> do
      a <- u8Get grid (cellIdx w x y)
      whenS (bitAnd a (number 1) .== 1) $
        bumpPackedNeighbors grid w h x y (number 2)
  done

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
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
setPackedAlive grid i alive = do
  cur <- u8Get grid i
  setU8 grid i (bitAnd cur (number 0xFE) + alive)

writeCellState ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Bool
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
writeCellState alive species i live sid =
  ifS
    live
    ( do
        cur <- u8Get alive i
        setU8 alive i (bitAnd cur (number 0xFE) + number 1)
        setU8 species i sid
    )
    ( do
        cur <- u8Get alive i
        setU8 alive i (bitAnd cur (number 0xFE))
        setU8 species i (number 0)
    )

-- | Stamp pattern cells; writes counts and bbox onto @scratch@.
stampPatternCells ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f ('Array ('Array 'Number))
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f (MutableObjectOf BoundScratch)
  -> EffectSyntax f (f 'Unit)
stampPatternCells alive species cells gx gy sid w h scratch = do
  toSyntax_ $
    ffi
      ( "(function(a,sp,cells,gx,gy,sid,w,h,sc){"
          <> "let added=0,bx0=1e9,by0=1e9,bx1=-1,by1=-1;"
          <> "for(let k=0;k<cells.length;k++){"
          <> "const c=cells[k],x=(gx+c[0])|0,y=(gy+c[1])|0;"
          <> "if(x<0||y<0||x>=w||y>=h)continue;"
          <> "const i=y*w+x;"
          <> "if(!(a[i]&1))added++;"
          <> "a[i]=(a[i]&0xFE)|1;sp[i]=sid;"
          <> "if(x<bx0)bx0=x;if(y<by0)by0=y;"
          <> "if(x>bx1)bx1=x;if(y>by1)by1=y;"
          <> "}"
          <> "sc.count=added;sc.bx0=bx0;sc.by0=by0;sc.bx1=bx1;sc.by1=by1;"
          <> "})"
      )
      ( arg alive
          <: arg species
          <: arg cells
          <: arg gx
          <: arg gy
          <: arg sid
          <: arg w
          <: arg h
          <: ArgEffect scratch
          <: RecNil
      )
  done

-- | Erase live cells in a circle; writes counts and bbox onto @scratch@.
eraseCircleCells ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f (MutableObjectOf BoundScratch)
  -> EffectSyntax f (f 'Unit)
eraseCircleCells alive species gx gy radius w h scratch = do
  toSyntax_ $
    ffi
      ( "(function(a,sp,gx,gy,r,w,h,sc){"
          <> "let removed=0,bx0=1e9,by0=1e9,bx1=-1,by1=-1;"
          <> "const ri=Math.max(0,Math.floor(r))|0;"
          <> "const rr=ri*ri;"
          <> "for(let dy=-ri;dy<=ri;dy++){"
          <> "for(let dx=-ri;dx<=ri;dx++){"
          <> "if(dx*dx+dy*dy>rr)continue;"
          <> "const x=(gx+dx)|0,y=(gy+dy)|0;"
          <> "if(x<0||y<0||x>=w||y>=h)continue;"
          <> "const i=y*w+x;"
          <> "if(a[i]&1){"
          <> "a[i]=a[i]&0xFE;sp[i]=0;"
          <> "removed++;"
          <> "if(x<bx0)bx0=x;if(y<by0)by0=y;"
          <> "if(x>bx1)bx1=x;if(y>by1)by1=y;"
          <> "}"
          <> "}"
          <> "}"
          <> "sc.count=removed;sc.bx0=bx0;sc.by0=by0;sc.bx1=bx1;sc.by1=by1;"
          <> "})"
      )
      ( arg alive
          <: arg species
          <: arg gx
          <: arg gy
          <: arg radius
          <: arg w
          <: arg h
          <: ArgEffect scratch
          <: RecNil
      )
  done

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
  -> EffectSyntax f (f 'Unit)
refreshPackedRegion grid w h x0 y0 x1 y1 = do
  let
    fx0 = Math.floor x0
    fy0 = Math.floor y0
    fx1 = Math.floor x1
    fy1 = Math.floor y1
    (xStart, yStart, xStop, yStop) =
      clampLiveBounds w h fx0 fy0 fx1 fy1 (number 1)
  forRange_ yStart yStop $ \y ->
    forRange_ xStart xStop $ \x ->
      refreshPackedAt grid w h x y
  done

-- | Expand RGB palette to RGBA for WebGL texture uploads.
initPaletteRgba ::
  Expr f 'Uint8Array -> EffectSyntax f (Expr f 'Uint8Array)
initPaletteRgba pal = do
  rgba <- bindExpr (newByteArray (number 1024))
  _ <-
    forRange_ (number 0) (number 256) $ \s -> do
      let
        base = s * number 3
        px = s * number 4
      r <- u8Get pal base
      g <- u8Get pal (base + number 1)
      b <- u8Get pal (base + number 2)
      toSyntax_ $
        ffi
          ( "(rgba,px,r,g,b)=>{"
              <> "rgba[px]=r;rgba[px+1]=g;rgba[px+2]=b;rgba[px+3]=255;"
              <> "}"
          )
          ( arg rgba
              <: arg px
              <: arg r
              <: arg g
              <: arg b
              <: RecNil
          )
      done
  pure rgba

syncPaletteRgbaSid ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
syncPaletteRgbaSid pal rgba sid = do
  let
    base = sid * number 3
    px = sid * number 4
  r <- u8Get pal base
  g <- u8Get pal (base + number 1)
  b <- u8Get pal (base + number 2)
  setU8 rgba px r
  setU8 rgba (px + number 1) g
  setU8 rgba (px + number 2) b
  setU8 rgba (px + number 3) (number 255)
  done

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
  -> Effect f (MutableObjectOf StepCtx)
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
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
  stepCtx
  birthCounts
  birthTouched = do
    let
      (xStart, yStart, xStop, yStop) =
        clampLiveBounds w h x0 y0 x1 y1 (number 1)
      regionCells = (xStop - xStart) * (yStop - yStart)
    let
      copyFull = regionCells .> (w * h) / number 2
    toSyntax_ $
      ifE
        (toEffect copyFull)
        (u8Copy nextAlive alive)
        (u8CopyRegion nextAlive alive w xStart yStart xStop yStop)
    toSyntax_ $
      ifE
        (toEffect copyFull)
        (u8Fill nextSpecies (number 0))
        (u8FillRegion nextSpecies w xStart yStart xStop yStop (number 0))
    set @"bx0" stepCtx (number 1e9)
    set @"by0" stepCtx (number 1e9)
    set @"bx1" stepCtx (number (-1))
    set @"by1" stepCtx (number (-1))
    set @"pop" stepCtx 0
    let
      runCell x y =
        processCell
          alive
          species
          nextAlive
          nextSpecies
          nextLiveList
          nextChangedList
          stepCtx
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
      ( prevPop
          .> 0
          .&& regionCells
          .> prevPop
          * number 12
            .&& Array.length prevLiveList
            .> 0
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
    stepCtx.pop

processCell ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Effect f (MutableObjectOf StepCtx)
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
  stepCtx
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
    set @"touchedLen" stepCtx 0
    set @"best" stepCtx 0
    set @"bestCount" stepCtx 0
    whenS (alive0 .== 0 .&& nCount .== 3) $
      forRange_ (number (-1)) (number 2) $ \dy ->
        forRange_ (number (-1)) (number 2) $ \dx ->
          whenS (not_ (dx .== 0 .&& dy .== 0)) $
            countBirthSpecies alive species counts touchedBuf stepCtx w h x y dx dy
    bestSp <- stepCtx.best
    whenS
      (alive0 .== 1 .&& (nCount .== 2 .|| nCount .== 3))
      ( do
          setU8 nextSpecies i sp
          Array.push_ nextLiveList i
          bumpPop stepCtx
          bumpBounds stepCtx x y
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
          stepCtx
          w
          h
          x
          y
          i
          bestSp
      )
    whenS (alive0 .== 0 .&& nCount .!= 3) (setU8 nextSpecies i (number 0))
    resetBirthCounts counts touchedBuf stepCtx

markBorn ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Effect f (MutableObjectOf StepCtx)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
markBorn nextAlive nextSpecies nextLiveList nextChangedList stepCtx w h x y i sp = do
  setPackedAlive nextAlive i (number 1)
  bumpPackedNeighbors nextAlive w h x y (number 2)
  setU8 nextSpecies i sp
  Array.push_ nextLiveList i
  Array.push_ nextChangedList i
  bumpPop stepCtx
  bumpBounds stepCtx x y

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
  Effect f (MutableObjectOf StepCtx)
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
  -> Effect f (MutableObjectOf StepCtx)
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
  -> Effect f (MutableObjectOf StepCtx)
  -> EffectSyntax f (f 'Unit)
resetBirthCounts counts touchedBuf scratch = do
  len <- scratch.touchedLen
  forRange_ (number 0) len $ \j -> do
    sp <- u8Get touchedBuf j
    setU8 counts sp 0

bumpPop :: Effect f (MutableObjectOf StepCtx) -> EffectSyntax f (f 'Unit)
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
  -> Expr f ('Array 'Number)
  -> Expr f 'Number
  -> Effect f (MutableObjectOf StepCtx)
  -> EffectSyntax f (f 'Unit)
expandBoundsForLive alive w h x0 y0 x1 y1 liveList prevPop stepCtx = do
  set @"bx0" stepCtx x0
  set @"by0" stepCtx y0
  set @"bx1" stepCtx x1
  set @"by1" stepCtx y1
  let
    xa = Math.max (number 0) (x0 - number 1)
    ya = Math.max (number 0) (y0 - number 1)
    xb = Math.min (w - number 1) (x1 + number 1)
    yb = Math.min (h - number 1) (y1 + number 1)
    regionCells = (xb - xa + number 1) * (yb - ya + number 1)
  ifS
    ( prevPop
        .> 0
        .&& Array.length liveList
        .> 0
        .&& Array.length liveList
        .< regionCells
        / number 4
    )
    ( forRange_ (number 0) (Array.length liveList) $ \k -> do
        let
          i = Array.index liveList k
          x = rem_ i w
          y = Math.floor (i / w)
        whenS (packedIsAlive alive i) (bumpBounds stepCtx x y)
    )
    ( forRange_ ya (yb + number 1) $ \y ->
        forRange_ xa (xb + number 1) $ \x -> do
          let
            i = cellIdx w x y
          whenS (packedIsAlive alive i) (bumpBounds stepCtx x y)
    )
  done

-- | Grid-resolution atlas + GPU sprite scale: 1 texel/cell, pan/zoom on GPU.
drawGridViewport ::
  Expr f ('MutableObject Pixi.Application)
  -> Expr f ('MutableObject Pixi.Sprite)
  -> Expr f ('MutableObject Pixi.Texture)
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Expr f 'Bool
  -> Expr f 'Bool
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f (MutableObjectOf RenderDirty)
  -> Effect f ('MutableObject ())
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
drawGridViewport
  app
  sprite
  texture
  pixels
  paletteRgba
  alive
  species
  liveList
  changedList
  sceneDirty
  viewportDirty
  w
  h
  px
  cw
  ch
  panX
  panY
  zoomLevel
  renderDirty
  viewport
  now = do
    let
      cellChanged = Array.length changedList .> 0
      -- Atlas only stores the current vis rect; pan/zoom must refill it.
      visRefresh = sceneDirty .|| viewportDirty
      needsPaint = visRefresh .|| cellChanged
      needsDraw = needsPaint .|| viewportDirty
      -- Dead cells get A=0 so the SDF can read liveness from atlas alpha.
      bg =
        number 15
          + shl (number 23) (number 8)
          + shl (number 42) (number 16)
    whenS needsPaint $
      do
        let
          cellScale = px * zoomLevel
          visX0 =
            Math.max (number 0) (Math.floor ((number 0 - panX) / cellScale) - number 1)
          visX1 =
            Math.min w (Math.ceil ((cw - panX) / cellScale) + number 1)
          visY0 =
            Math.max (number 0) (Math.floor ((number 0 - panY) / cellScale) - number 1)
          visY1 =
            Math.min h (Math.ceil ((ch - panY) / cellScale) + number 1)
        toSyntax_ $
          paintGridCells
            pixels
            w
            h
            paletteRgba
            alive
            species
            w
            (number 1)
            (number 0)
            (number 0)
            bg
            liveList
            changedList
            visRefresh
            visX0
            visX1
            visY0
            visY1
            renderDirty
        done
    sprH <- hold (expr sprite)
    gridTex <- hold (expr texture)
    whenS needsDraw $ Pixi.setSpriteViewport sprH panX panY zoomLevel px
    Pixi.presentGrid app viewport gridTex now needsPaint needsDraw
    done

-- | CPU fallback when WebGL is lost or unavailable: paint the atlas, then
--   blit it onto the 2D overlay canvas with the same pan/zoom transform the
--   GPU sprite would use. The overlay sits above the dead WebGL canvas and
--   is pointer-events:none so input still lands on the board.
drawGridFallback ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Expr f 'Bool
  -> Expr f 'Bool
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f (MutableObjectOf RenderDirty)
  -> EffectSyntax f (f 'Unit)
drawGridFallback
  cv
  pixels
  paletteRgba
  alive
  species
  liveList
  changedList
  sceneDirty
  viewportDirty
  w
  h
  px
  cw
  ch
  panX
  panY
  zoomLevel
  renderDirty = do
    let
      visRefresh = sceneDirty .|| viewportDirty
      needsPaint = visRefresh .|| Array.length changedList .> 0
      cellScale = px * zoomLevel
      visX0 =
        Math.max (number 0) (Math.floor ((number 0 - panX) / cellScale) - number 1)
      visX1 =
        Math.min w (Math.ceil ((cw - panX) / cellScale) + number 1)
      visY0 =
        Math.max (number 0) (Math.floor ((number 0 - panY) / cellScale) - number 1)
      visY1 =
        Math.min h (Math.ceil ((ch - panY) / cellScale) + number 1)
      bg =
        number 15
          + shl (number 23) (number 8)
          + shl (number 42) (number 16)
    whenS needsPaint $ do
      toSyntax_ $
        paintGridCells
          pixels
          w
          h
          paletteRgba
          alive
          species
          w
          (number 1)
          (number 0)
          (number 0)
          bg
          liveList
          changedList
          visRefresh
          visX0
          visX1
          visY0
          visY1
          renderDirty
      done
    toSyntax_
      $ discard
      $ ffi
        ( "(cv, pixels, texW, texH, scale, panX, panY, cw, ch) => {"
            <> " if (cv.style.display === 'none') {"
            <> "   cv.style.display = 'block';"
            <> "   console.warn('[Life] rendering via 2D canvas fallback');"
            <> " }"
            <> " let st = cv.__lifeBlit;"
            <> " if (!st || st.img.data.buffer !== pixels.buffer) {"
            <> "   const off = document.createElement('canvas');"
            <> "   off.width = texW; off.height = texH;"
            <> "   st = cv.__lifeBlit = {"
            <> "     off,"
            <> "     offCtx: off.getContext('2d'),"
            <> "     ctx: cv.getContext('2d'),"
            <> "     img: new ImageData(new Uint8ClampedArray(pixels.buffer), texW, texH)"
            <> "   };"
            <> " }"
            <> " st.offCtx.putImageData(st.img, 0, 0);"
            <> " const c = st.ctx;"
            <> " c.setTransform(1, 0, 0, 1, 0, 0);"
            <> " c.fillStyle = '#0f172a';"
            <> " c.fillRect(0, 0, cw, ch);"
            <> " c.imageSmoothingEnabled = false;"
            <> " c.setTransform(scale, 0, 0, scale, panX, panY);"
            <> " c.drawImage(st.off, 0, 0);"
            <> " c.setTransform(1, 0, 0, 1, 0, 0);"
            <> " }"
        )
        ( ArgEffect cv
            <: arg pixels
            <: arg w
            <: arg h
            <: arg cellScale
            <: arg panX
            <: arg panY
            <: arg cw
            <: arg ch
            <: RecNil
        )
    done

-- | Hide the 2D fallback overlay once the GPU path is healthy again.
hideFallback2d ::
  Effect f ('MutableObject DomElement) -> EffectSyntax f (f 'Unit)
hideFallback2d cv = do
  _ <- Dom.setStyleProperty cv "display" (string "none")
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
