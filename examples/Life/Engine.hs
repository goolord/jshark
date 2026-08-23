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
  )
where

import Discover (Registry, discoverLife)
import Grid
  ( BoundScratch (..)
  , ImageData
  , cellIdx
  , expandBoundsForLive
  , imageDataBytes
  , putImageData
  , renderGridViewport
  , setU8
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

initLife ::
  Effect f ('MutableObject Canvas.Context2D)
  -> EffectSyntax f (Effect f (MutableObjectOf LifeState))
initLife _ctx = do
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
  set @"pop" state (fromIntegral initialPop)
  set @"alive" state alive
  set @"species" state species
  nextAlive <- bindExpr (newByteArray (number (fromIntegral gridN)))
  nextSpecies <- bindExpr (newByteArray (number (fromIntegral gridN)))
  set @"nextAlive" state nextAlive
  set @"nextSpecies" state nextSpecies
  set @"palette" state (uint8Array paletteBytes)
  set @"nextDiscover" state (fromIntegral discoverMin)
  set @"recentDiscover" state (string "")
  set @"boundX0" state (fromIntegral initialBoundX0)
  set @"boundY0" state (fromIntegral initialBoundY0)
  set @"boundX1" state (fromIntegral initialBoundX1)
  set @"boundY1" state (fromIntegral initialBoundY1)
  pure state

stepLife ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject Registry)
  -> EffectSyntax f (f 'Unit)
stepLife state registry = do
  stepGeneration state
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
    nextD <- state.nextDiscover
    (nextOut, mintedArr) <- discoverLife alive species pal registry nextD
    set @"nextDiscover" state (Math.floor nextOut)
    _ <- refreshTakenNames registry
    forRange_ (number 0) (Array.length mintedArr) $ \i -> do
      sid <- pure (Array.index mintedArr i)
      nm <- uniqueNameSid sid registry
      _ <- recordDiscoveredName sid nm registry
      set @"recentDiscover" state nm

stepGeneration ::
  Effect f (MutableObjectOf LifeState) -> EffectSyntax f (f 'Unit)
stepGeneration state = do
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
  expanded <- expandBoundsForLive alive w h x0 y0 x1 y1
  x0e <- expanded.bx0
  y0e <- expanded.by0
  x1e <- expanded.bx1
  y1e <- expanded.by1
  ifS
    (x1 .< x0)
    (set @"pop" state 0)
    ( do
        boundScratch <- hold (toObject (BoundScratch 0 0 (-1) (-1)))
        p <-
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
            boundScratch
        bx0n <- boundScratch.bx0
        by0n <- boundScratch.by0
        bx1n <- boundScratch.bx1
        by1n <- boundScratch.by1
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
  swapBuffers state
  gen <- state.gen
  set @"gen" state (gen + 1)

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
  -> Expr f ('MutableObject ImageData)
  -> Effect f ('MutableObject ())
  -> Effect f (MutableObjectOf LifeState)
  -> EffectSyntax f (f 'Unit)
renderLife ctx img viewport state = do
  pixels <- imageDataBytes img
  w <- pure (number (fromIntegral gridW))
  h <- pure (number (fromIntegral gridH))
  px <- pure (number (fromIntegral cellPx))
  cw <- pure (number canvasW)
  ch <- pure (number canvasH)
  alive <- state.alive
  species <- state.species
  pal <- state.palette
  panX <- getProp viewport "panX"
  panY <- getProp viewport "panY"
  zoom <- getProp viewport "zoom"
  liveX0 <- state.boundX0
  liveY0 <- state.boundY0
  liveX1 <- state.boundX1
  liveY1 <- state.boundY1
  renderGridViewport pixels alive species pal w h px cw ch panX panY zoom liveX0 liveY0 liveX1 liveY1
  putImageData ctx img

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
      (a .== 1)
      ( do
          setU8 alive i 0
          setU8 species i 0
          set @"pop" state (pop0 - 1)
      )
      ( do
          setU8 alive i 1
          setU8 species i (number (fromIntegral manualSpecies))
          set @"pop" state (pop0 + 1)
          includeBounds state gx gy
      )

-- | Stamp @cells@ (local @[x,y]@ pairs) at @(gx, gy)@. Already-live cells
--   keep the population count and take @sid@.
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
        pop0 <- state.pop
        setU8 alive i 1
        setU8 species i sid
        whenS (a .== 0) (set @"pop" state (pop0 + 1))
        includeBounds state x y

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
