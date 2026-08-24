{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Conway step and render in JShark. Grid buffers use typed byte
--    helpers ('Grid').
module Engine
  ( initLife
  , stepLife
  , maybeDiscover
  , renderLife
  , togglePause
  , flipCell
  , placePattern
  , markSceneDirty
  )
where

import Discover (Registry, discoverLife)
import Grid
  ( BoundScratch (..)
  , StepCtx (..)
  , cellIdx
  , drawGridViewport
  , expandBoundsForLive
  , initPaletteRgba
  , rebuildLiveList
  , rebuildPackedCounts
  , refreshPackedRegion
  , setU8
  , setPackedAlive
  , stepGrid
  , u8Get
  )
import JShark.Api
import qualified JShark.Array as Array
import qualified JShark.Canvas as Canvas
import JShark.Generic (MutableObjectOf, newRecord, toObject)
import qualified JShark.Math as Math
import Names (recordDiscoveredName, refreshTakenNames, uniqueNameSid)
import Patterns
  ( initialBoundX0
  , initialBoundX1
  , initialBoundY0
  , initialBoundY1
  , initialCatalogCells
  , initialPop
  , paletteBytes
  )
import Types
  ( LifeState
  , canvasH
  , canvasW
  , cellPx
  , discoverEvery
  , discoverMin
  , gridH
  , gridN
  , gridW
  , manualSpecies
  , seedH
  , seedOx
  , seedOy
  , seedW
  , soupRngSeed
  )
import WorkerBridge
  ( engineCanStep
  , engineStepGeneration
  , initWorkerEngine
  )

initLife ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f ('MutableObject ())
  -> EffectSyntax f (Effect f (MutableObjectOf LifeState))
initLife ctx viewport = do
  state <- hold (newRecord @LifeState)
  set @"gen" state 0
  set @"paused" state false_
  alive <- bindExpr (newByteArray (number (fromIntegral gridN)))
  species <- bindExpr (newByteArray (number (fromIntegral gridN)))
  toSyntax_ $
    seedSoupRegion
      alive
      (number (fromIntegral seedOx))
      (number (fromIntegral seedOy))
      (number (fromIntegral seedW))
      (number (fromIntegral seedH))
      (number (fromIntegral gridW))
      (number (fromIntegral soupRngSeed))
  toSyntax_ (seedLiveCells alive species initialCatalogCells)
  let
    w = number (fromIntegral gridW)
    h = number (fromIntegral gridH)
  toSyntax_ (rebuildPackedCounts alive w h)
  set @"pop" state (fromIntegral initialPop)
  set @"alive" state alive
  set @"species" state species
  nextAlive <- bindExpr (newByteArray (number (fromIntegral gridN)))
  nextSpecies <- bindExpr (newByteArray (number (fromIntegral gridN)))
  set @"nextAlive" state nextAlive
  set @"nextSpecies" state nextSpecies
  set @"palette" state (uint8Array paletteBytes)
  imgEffect <- Canvas.createImageData ctx (number canvasW) (number canvasH)
  img <- bindExpr imgEffect
  _ <- setProp viewport "img" img
  pixels <- Canvas.imageDataBytes img
  set @"rgbaPixels" state pixels
  pal <- state.palette
  paletteRgba <- initPaletteRgba pal
  set @"paletteRgba" state paletteRgba
  set @"nextDiscover" state (fromIntegral discoverMin)
  set @"recentDiscover" state (string "")
  set @"boundX0" state (fromIntegral initialBoundX0)
  set @"boundY0" state (fromIntegral initialBoundY0)
  set @"boundX1" state (fromIntegral initialBoundX1)
  set @"boundY1" state (fromIntegral initialBoundY1)
  discoverVisited <- bindExpr (newByteArray (number (fromIntegral gridN)))
  discoverStackX <- bindExpr (newByteArray (number (fromIntegral gridN)))
  discoverStackY <- bindExpr (newByteArray (number (fromIntegral gridN)))
  set @"discoverVisited" state discoverVisited
  set @"discoverStackX" state discoverStackX
  set @"discoverStackY" state discoverStackY
  liveList <- bindExpr $ Array.fromEffects []
  nextLiveList <- bindExpr $ Array.fromEffects []
  changedList <- bindExpr $ Array.fromEffects []
  nextChangedList <- bindExpr $ Array.fromEffects []
  stepStamp <- bindExpr (newByteArray (number (fromIntegral gridN)))
  set @"liveList" state liveList
  set @"nextLiveList" state nextLiveList
  set @"changedList" state changedList
  set @"nextChangedList" state nextChangedList
  set @"stepStamp" state stepStamp
  birthCounts <- bindExpr (newByteArray (number 256))
  birthTouched <- bindExpr (newByteArray (number 8))
  set @"birthCounts" state birthCounts
  set @"birthTouched" state birthTouched
  _ <- setProp viewport "lastHudMs" (number 0)
  set @"sceneDirty" state true_
  _ <-
    rebuildLiveList
      alive
      w
      h
      (number (fromIntegral initialBoundX0))
      (number (fromIntegral initialBoundY0))
      (number (fromIntegral initialBoundX1))
      (number (fromIntegral initialBoundY1))
      liveList
  _ <- initWorkerEngine
  panX <- getProp viewport "panX"
  panY <- getProp viewport "panY"
  zoom <- getProp viewport "zoom"
  _ <- setProp viewport "renderPanX" panX
  _ <- setProp viewport "renderPanY" panY
  _ <- setProp viewport "renderZoom" zoom
  setProp viewport "renderPanValid" true_
  pure state

stepLife ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject Registry)
  -> Effect f (MutableObjectOf StepCtx)
  -> EffectSyntax f (f 'Unit)
stepLife state registry stepCtx = do
  stepGeneration state stepCtx
  maybeDiscover state registry

maybeDiscover ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject Registry)
  -> EffectSyntax f (f 'Unit)
maybeDiscover state registry = do
  gen <- state.gen
  whenS (rem_ gen (number (fromIntegral discoverEvery)) .== 0) $ do
    alive <- state.alive
    species <- state.species
    pal <- state.palette
    visited <- state.discoverVisited
    stackX <- state.discoverStackX
    stackY <- state.discoverStackY
    liveX0 <- state.boundX0
    liveY0 <- state.boundY0
    liveX1 <- state.boundX1
    liveY1 <- state.boundY1
    nextD <- state.nextDiscover
    (nextOut, mintedArr) <-
      discoverLife
        alive
        species
        pal
        registry
        visited
        stackX
        stackY
        (number (fromIntegral gridW))
        liveX0
        liveY0
        liveX1
        liveY1
        nextD
    set @"nextDiscover" state (Math.floor nextOut)
    _ <- refreshTakenNames registry
    forRange_ (number 0) (Array.length mintedArr) $ \i -> do
      sid <- pure (Array.index mintedArr i)
      nm <- uniqueNameSid sid registry
      _ <- recordDiscoveredName sid nm registry
      set @"recentDiscover" state nm

stepGeneration ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f (MutableObjectOf StepCtx)
  -> EffectSyntax f (f 'Unit)
stepGeneration state stepCtx = do
  w <- pure (number (fromIntegral gridW))
  h <- pure (number (fromIntegral gridH))
  x0 <- state.boundX0
  y0 <- state.boundY0
  x1 <- state.boundX1
  y1 <- state.boundY1
  alive <- state.alive
  species <- state.species
  nextAlive <- state.nextAlive
  nextSpecies <- state.nextSpecies
  prevLiveList <- state.liveList
  nextLiveList <- state.nextLiveList
  nextChangedList <- state.nextChangedList
  stepStamp <- state.stepStamp
  prevPop <- state.pop
  gen <- state.gen
  _ <- set @"touchedLen" stepCtx (number 0)
  _ <- set @"best" stepCtx (number 0)
  _ <- set @"bestCount" stepCtx (number 0)
  let
    -- Tags 1/2 alternate; stamps start at 0. Dense scans skip stamps;
    -- sparse dedup requires the active tag never be 0.
    stepTagVal = rem_ gen (number 2) + number 1
  Array.clear_ nextLiveList
  Array.clear_ nextChangedList
  _ <-
    expandBoundsForLive
      alive
      w
      h
      x0
      y0
      x1
      y1
      prevLiveList
      prevPop
      stepCtx
  x0e <- stepCtx.bx0
  y0e <- stepCtx.by0
  x1e <- stepCtx.bx1
  y1e <- stepCtx.by1
  ifS
    (x1 .< x0)
    ( do
        set @"pop" state 0
        live <- state.liveList
        next <- state.nextLiveList
        Array.clear_ live
        Array.clear_ next
        done
    )
    ( do
        birthCounts <- state.birthCounts
        birthTouched <- state.birthTouched
        canEngine <- engineCanStep
        useEngine <- pure canEngine
        ifS
          useEngine
          ( do
              v <-
                engineStepGeneration
                  alive
                  species
                  nextAlive
                  nextSpecies
                  w
                  h
                  x0e
                  y0e
                  x1e
                  y1e
                  nextLiveList
                  nextChangedList
                  stepCtx
              set @"pop" stepCtx v
          )
          ( do
              v <-
                stepGrid
                  alive
                  species
                  nextAlive
                  nextSpecies
                  w
                  h
                  x0e
                  y0e
                  x1e
                  y1e
                  prevLiveList
                  nextLiveList
                  nextChangedList
                  stepStamp
                  stepTagVal
                  prevPop
                  stepCtx
                  birthCounts
                  birthTouched
              set @"pop" stepCtx v
          )
        p <- stepCtx.pop
        bx0n <- stepCtx.bx0
        by0n <- stepCtx.by0
        bx1n <- stepCtx.bx1
        by1n <- stepCtx.by1
        ifS
          (bx1n .< bx0n)
          ( do
              set @"boundX0" state 1
              set @"boundY0" state 1
              set @"boundX1" state (-1)
              set @"boundY1" state (-1)
          )
          ( do
              set @"boundX0" state (Math.floor bx0n)
              set @"boundY0" state (Math.floor by0n)
              set @"boundX1" state (Math.floor bx1n)
              set @"boundY1" state (Math.floor by1n)
          )
        set @"pop" state (Math.floor p)
    )
  swapLiveLists state
  swapChangedLists state
  swapBuffers state
  set @"gen" state (gen + 1)

markSceneDirty ::
  Effect f (MutableObjectOf LifeState) -> EffectSyntax f (f 'Unit)
markSceneDirty state = set @"sceneDirty" state true_

syncLiveList ::
  Effect f (MutableObjectOf LifeState) -> EffectSyntax f (f 'Unit)
syncLiveList state = do
  alive <- state.alive
  liveList <- state.liveList
  x0 <- state.boundX0
  y0 <- state.boundY0
  x1 <- state.boundX1
  y1 <- state.boundY1
  let
    w = number (fromIntegral gridW)
    h = number (fromIntegral gridH)
  rebuildLiveList alive w h x0 y0 x1 y1 liveList

swapLiveLists :: Effect f (MutableObjectOf LifeState) -> EffectSyntax f (f 'Unit)
swapLiveLists state = do
  live <- state.liveList
  next <- state.nextLiveList
  set @"liveList" state next
  set @"nextLiveList" state live

swapChangedLists :: Effect f (MutableObjectOf LifeState) -> EffectSyntax f (f 'Unit)
swapChangedLists state = do
  cur <- state.changedList
  next <- state.nextChangedList
  set @"changedList" state next
  set @"nextChangedList" state cur

swapBuffers :: Effect f (MutableObjectOf LifeState) -> EffectSyntax f (f 'Unit)
swapBuffers state = do
  a <- state.alive
  na <- state.nextAlive
  sp <- state.species
  ns <- state.nextSpecies
  set @"alive" state na
  set @"nextAlive" state a
  set @"species" state ns
  set @"nextSpecies" state sp

renderLife ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f ('MutableObject ())
  -> Effect f (MutableObjectOf LifeState)
  -> EffectSyntax f (f 'Unit)
renderLife ctx viewport state = do
  w <- pure (number (fromIntegral gridW))
  h <- pure (number (fromIntegral gridH))
  px <- pure (number (fromIntegral cellPx))
  cw <- pure (number canvasW)
  ch <- pure (number canvasH)
  img <- getProp viewport "img"
  pixels <- state.rgbaPixels
  paletteRgba <- state.paletteRgba
  alive <- state.alive
  species <- state.species
  liveList <- state.liveList
  changedList <- state.changedList
  sceneDirty <- state.sceneDirty
  panX <- getProp viewport "panX"
  panY <- getProp viewport "panY"
  zoom <- getProp viewport "zoom"
  renderValid <- getProp viewport "renderPanValid"
  viewportDirty <- pure (not_ renderValid)
  fullRedraw <- pure (sceneDirty .|| viewportDirty)
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
    zoom
  Array.clear_ changedList
  set @"sceneDirty" state false_
  _ <- setProp viewport "renderPanX" panX
  _ <- setProp viewport "renderPanY" panY
  _ <- setProp viewport "renderZoom" zoom
  setProp viewport "renderPanValid" true_

togglePause :: Effect f (MutableObjectOf LifeState) -> EffectSyntax f (f 'Unit)
togglePause state = do
  cur <- state.paused
  set @"paused" state (not_ cur)

flipCell ::
  Effect f (MutableObjectOf LifeState)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
flipCell state gx gy = do
  w <- pure (number (fromIntegral gridW))
  h <- pure (number (fromIntegral gridH))
  whenS (gx .>= 0 .&& gy .>= 0 .&& gx .< w .&& gy .< h) $ do
    alive <- state.alive
    species <- state.species
    let
      i = cellIdx w gx gy
    a <- u8Get alive i
    pop0 <- state.pop
    ifS
      (bitAnd a (number 1) .== 1)
      ( do
          setPackedAlive alive i (number 0)
          setU8 species i 0
          set @"pop" state (pop0 - 1)
          toSyntax_ (refreshPackedRegion alive w h gx gy gx gy)
          done
      )
      ( do
          setPackedAlive alive i (number 1)
          setU8 species i (number (fromIntegral manualSpecies))
          set @"pop" state (pop0 + 1)
          includeBounds state gx gy
          toSyntax_ (refreshPackedRegion alive w h gx gy gx gy)
          done
      )
    syncLiveList state
    done
  markSceneDirty state

placePattern ::
  Effect f (MutableObjectOf LifeState)
  -> Expr f ('Array ('Array 'Number))
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
placePattern state cells gx gy sid = do
  w <- pure (number (fromIntegral gridW))
  h <- pure (number (fromIntegral gridH))
  whenS (gx .>= 0 .&& gy .>= 0 .&& gx .< w .&& gy .< h) $ do
    alive <- state.alive
    species <- state.species
    bbox <- hold (toObject (BoundScratch 1e9 1e9 (-1) (-1)))
    forRange_ (number 0) (Array.length cells) $ \k -> do
      let
        cell = Array.index cells k
        dx = Array.index cell 0
        dy = Array.index cell 1
        x = gx + dx
        y = gy + dy
      whenS (x .>= 0 .&& y .>= 0 .&& x .< w .&& y .< h) $ do
        let
          i = cellIdx w x y
        a <- u8Get alive i
        whenS (bitAnd a (number 1) .== 0) $ do
          setPackedAlive alive i (number 1)
          curPop <- state.pop
          set @"pop" state (curPop + 1)
        setU8 species i sid
        includeBounds state x y
        curBx0 <- bbox.bx0
        curBy0 <- bbox.by0
        curBx1 <- bbox.bx1
        curBy1 <- bbox.by1
        _ <- set @"bx0" bbox (Math.min curBx0 x)
        _ <- set @"by0" bbox (Math.min curBy0 y)
        _ <- set @"bx1" bbox (Math.max curBx1 x)
        set @"by1" bbox (Math.max curBy1 y)
        done
    syncLiveList state
    bx0n <- bbox.bx0
    by0n <- bbox.by0
    bx1n <- bbox.bx1
    by1n <- bbox.by1
    whenS (bx1n .>= bx0n) $ do
      toSyntax_ (refreshPackedRegion alive w h bx0n by0n bx1n by1n)
      done
    done
  markSceneDirty state

includeBounds ::
  Effect f (MutableObjectOf LifeState)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
includeBounds state x y = do
  x0 <- state.boundX0
  y0 <- state.boundY0
  x1 <- state.boundX1
  y1 <- state.boundY1
  ifS
    (x1 .< x0)
    ( do
        set @"boundX0" state (Math.floor x)
        set @"boundY0" state (Math.floor y)
        set @"boundX1" state (Math.floor x)
        set @"boundY1" state (Math.floor y)
    )
    ( do
        _ <- set @"boundX0" state (Math.floor (Math.min x0 x))
        _ <- set @"boundY0" state (Math.floor (Math.min y0 y))
        _ <- set @"boundX1" state (Math.floor (Math.max x1 x))
        set @"boundY1" state (Math.floor (Math.max y1 y))
    )
