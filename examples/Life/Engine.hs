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
  , eraseCircle
  , placePattern
  , markSceneDirty
  )
where

import Catalog (catalogInitialCells, stampCatalogCells)
import Discover (Registry, discoverLife)
import Grid
  ( RenderDirty (..)
  , StepCtx (..)
  , cellIdx
  , drawGridFallback
  , drawGridViewport
  , expandBoundsForLive
  , hideFallback2d
  , initPaletteRgba
  , rebuildLiveList
  , rebuildPackedCounts
  , refreshPackedRegion
  , stampPatternCells
  , stepGrid
  , syncPaletteRgbaSid
  , u8Get
  , writeCellState
  )
import JShark.Api
import qualified JShark.Array as Array
import JShark.Dom (DomElement)
import JShark.Generic (MutableObjectOf, newRecord)
import qualified JShark.Math as Math
import JShark.Rec (Rec (..), (<:))
import JShark.Worker (performanceNow)
import Names (recordDiscoveredName, refreshTakenNames, uniqueNameSid)
import Patterns
  ( initialBoundX0
  , initialBoundX1
  , initialBoundY0
  , initialBoundY1
  , initialPop
  , paletteBytes
  )
import qualified Pixi
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
  , texH
  , texW
  )
import WorkerBridge
  ( engineCanStep
  , engineStepGeneration
  , initWorkerEngine
  )

initLife ::
  Effect f ('MutableObject Pixi.Application)
  -> Effect f ('MutableObject ())
  -> EffectSyntax f (Effect f (MutableObjectOf LifeState))
initLife app viewport = do
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
  cells <- bindExpr catalogInitialCells
  toSyntax_ (stampCatalogCells alive species cells)
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
  pixels <- bindExpr (newByteArray (number (texW * texH * 4)))
  texture <- Pixi.textureFromBuffer pixels (number texW) (number texH)
  gridTex <- hold (expr texture)
  _ <- Pixi.setTextureNearest gridTex
  sprite <- Pixi.newSprite texture
  _ <- Pixi.mountSprite app sprite
  _ <- setProp viewport "texture" texture
  _ <- setProp viewport "sprite" sprite
  _ <- Pixi.installLifeShader app viewport sprite texture
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
    paletteRgba <- state.paletteRgba
    forRange_ (number 0) (Array.length mintedArr) $ \i -> do
      sid <- pure (Array.index mintedArr i)
      nm <- uniqueNameSid sid registry
      _ <- recordDiscoveredName sid nm registry
      _ <- syncPaletteRgbaSid pal paletteRgba sid
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
              engineOk <-
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
              whenS (not_ engineOk) $
                do
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

swapLiveLists ::
  Effect f (MutableObjectOf LifeState) -> EffectSyntax f (f 'Unit)
swapLiveLists state = do
  live <- state.liveList
  next <- state.nextLiveList
  set @"liveList" state next
  set @"nextLiveList" state live

swapChangedLists ::
  Effect f (MutableObjectOf LifeState) -> EffectSyntax f (f 'Unit)
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
  Effect f ('MutableObject ())
  -> Effect f (MutableObjectOf RenderDirty)
  -> Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject DomElement)
  -> EffectSyntax f (f 'Unit)
renderLife viewport renderDirty state fallback = do
  now <- performanceNow
  glLost <- getProp viewport "glLost"
  app <- getProp viewport "app"
  w <- pure (number (fromIntegral gridW))
  h <- pure (number (fromIntegral gridH))
  px <- pure (number (fromIntegral cellPx))
  cw <- pure (number canvasW)
  ch <- pure (number canvasH)
  img <- getProp viewport "texture"
  sprite <- getProp viewport "sprite"
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
  whenS (glLost .== 0) $ do
    hideFallback2d fallback
    drawGridViewport
      app
      sprite
      img
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
      zoom
      renderDirty
      viewport
      now
  whenS (glLost .!= 0) $
    drawGridFallback
      fallback
      pixels
      paletteRgba
      alive
      species
      liveList
      changedList
      sceneDirty
      w
      h
      px
      cw
      ch
      panX
      panY
      zoom
      renderDirty
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
          writeCellState alive species i false_ (number 0)
          set @"pop" state (pop0 - 1)
          toSyntax_ (refreshPackedRegion alive w h gx gy gx gy)
          done
      )
      ( do
          writeCellState alive species i true_ (number (fromIntegral manualSpecies))
          set @"pop" state (pop0 + 1)
          includeBounds state gx gy
          toSyntax_ (refreshPackedRegion alive w h gx gy gx gy)
          done
      )
    syncLiveList state
    done
  markSceneDirty state

eraseCircle ::
  Effect f (MutableObjectOf LifeState)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
eraseCircle state gx gy radius = do
  w <- pure (number (fromIntegral gridW))
  h <- pure (number (fromIntegral gridH))
  whenS (radius .>= 0) $ do
    alive <- state.alive
    species <- state.species
    info <-
      bindExpr $
        ffi
          ( "(function(a,sp,gx,gy,r,w,h){"
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
              <> "return [removed,bx0,by0,bx1,by1];"
              <> "})"
          )
          ( arg alive
              <: arg species
              <: arg gx
              <: arg gy
              <: arg radius
              <: arg w
              <: arg h
              <: RecNil
          )
    let
      removed = Array.index info (number 0)
      bx0n = Array.index info (number 1)
      by0n = Array.index info (number 2)
      bx1n = Array.index info (number 3)
      by1n = Array.index info (number 4)
    whenS (removed .> 0) $ do
      pop0 <- state.pop
      set @"pop" state (pop0 - removed)
      toSyntax_ (refreshPackedRegion alive w h bx0n by0n bx1n by1n)
      syncLiveList state
      markSceneDirty state
    done

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
    info <- stampPatternCells alive species cells gx gy sid w h
    let
      added = Array.index info (number 0)
      bx0n = Array.index info (number 1)
      by0n = Array.index info (number 2)
      bx1n = Array.index info (number 3)
      by1n = Array.index info (number 4)
    whenS (added .> 0) $ do
      curPop <- state.pop
      set @"pop" state (curPop + added)
    whenS (bx1n .>= bx0n) $ do
      x0 <- state.boundX0
      y0 <- state.boundY0
      x1 <- state.boundX1
      y1 <- state.boundY1
      ifS
        (x1 .< x0)
        ( do
            set @"boundX0" state (Math.floor bx0n)
            set @"boundY0" state (Math.floor by0n)
            set @"boundX1" state (Math.floor bx1n)
            set @"boundY1" state (Math.floor by1n)
        )
        ( do
            _ <- set @"boundX0" state (Math.floor (Math.min x0 bx0n))
            _ <- set @"boundY0" state (Math.floor (Math.min y0 by0n))
            _ <- set @"boundX1" state (Math.floor (Math.max x1 bx1n))
            set @"boundY1" state (Math.floor (Math.max y1 by1n))
        )
      toSyntax_ (refreshPackedRegion alive w h bx0n by0n bx1n by1n)
      done
    syncLiveList state
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
