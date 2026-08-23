{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Conway step and render in JShark. Grid buffers use typed byte
--    helpers ('Grid'); simulation step and pixel blitting use single ffi
--    calls ('stepGrid', 'renderGrid').
module Engine
  ( initLife
  , stepLife
  , maybeDiscover
  , renderLife
  , togglePause
  , flipCell
  )
where

import Discover (Registry, discoverLife)
import Grid
  ( ImageData
  , cellIdx
  , imageDataBytes
  , putImageData
  , renderGrid
  , setU8
  , stepGrid
  , u8Get
  )
import JShark.Api
import qualified JShark.Array as Array
import qualified JShark.Canvas as Canvas
import JShark.Generic (MutableObjectOf, newRecord)
import qualified JShark.Math as Math
import Names (recordDiscoveredName, refreshTakenNames, uniqueNameSid)
import Patterns (initialAlive, initialPop, initialSpecies, paletteBytes)
import Types
  ( LifeState
  , canvasW
  , cellPx
  , discoverEvery
  , discoverMin
  , gridH
  , gridN
  , gridW
  , manualSpecies
  )

initLife ::
  Effect f ('MutableObject Canvas.Context2D)
  -> EffectSyntax f (Effect f (MutableObjectOf LifeState))
initLife _ctx = do
  state <- hold (newRecord @LifeState)
  set @"gen" state 0
  set @"pop" state (fromIntegral initialPop)
  set @"paused" state false_
  set @"alive" state (uint8Array initialAlive)
  set @"species" state (uint8Array initialSpecies)
  nextAlive <- bindExpr (newByteArray (number (fromIntegral gridN)))
  nextSpecies <- bindExpr (newByteArray (number (fromIntegral gridN)))
  set @"nextAlive" state nextAlive
  set @"nextSpecies" state nextSpecies
  set @"palette" state (uint8Array paletteBytes)
  set @"nextDiscover" state (fromIntegral discoverMin)
  set @"recentDiscover" state (string "")
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
    recentD <- state.recentDiscover
    result <- bindExpr $ discoverLife alive species pal registry nextD recentD
    nextOut <- getProp' result "nextId"
    mintedArr <- getProp' result "mintedSids"
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
  alive <- state.alive
  species <- state.species
  nextAlive <- state.nextAlive
  nextSpecies <- state.nextSpecies
  pop <- stepGrid alive species nextAlive nextSpecies w h
  swapBuffers state
  set @"pop" state (Math.floor pop)
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
  -> Effect f (MutableObjectOf LifeState)
  -> EffectSyntax f (f 'Unit)
renderLife ctx img state = do
  pixels <- imageDataBytes img
  w <- pure (number (fromIntegral gridW))
  h <- pure (number (fromIntegral gridH))
  px <- pure (number (fromIntegral cellPx))
  cw <- pure (number canvasW)
  alive <- state.alive
  species <- state.species
  pal <- state.palette
  renderGrid pixels alive species pal w h px cw
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
      )
