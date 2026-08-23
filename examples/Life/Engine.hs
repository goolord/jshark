{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{- | Conway step and render in JShark. Grid buffers use typed byte
   helpers ('Grid'); pixel blitting uses one minimal 'renderGrid' ffi.
-}
module Engine
  ( initLife
  , stepLife
  , renderLife
  , togglePause
  , flipCell
  )
where

import GHC.Generics (Generic)
import Grid
import JShark.Api
import qualified JShark.Array as Array
import qualified JShark.Canvas as Canvas
import JShark.Generic (MutableObjectOf, newRecord)
import qualified JShark.Generic as G
import qualified JShark.Math as Math
import Patterns (initialAlive, initialPop, initialSpecies, paletteBytes)
import Types
  ( LifeState
  , canvasW
  , cellPx
  , gridH
  , gridN
  , gridW
  , manualSpecies
  )

data Scratch = Scratch
  { pop :: Double
  , best :: Double
  , bestCount :: Double
  }
  deriving Generic

rowsE, colsE, deltasE :: forall f. Effect f ('Array 'Number)
rowsE = Array.fromEffects [expr (number (fromIntegral y)) | y <- [0 .. gridH - 1]]
colsE = Array.fromEffects [expr (number (fromIntegral x)) | x <- [0 .. gridW - 1]]
deltasE = Array.fromEffects [expr (number (-1)), expr (number 0), expr (number 1)]

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
  counts <- bindExpr (newByteArray (number 256))
  set @"nextAlive" state nextAlive
  set @"nextSpecies" state nextSpecies
  set @"counts" state counts
  set @"palette" state (uint8Array paletteBytes)
  set @"rows" state =<< bindExpr rowsE
  set @"cols" state =<< bindExpr colsE
  pure state

stepLife :: Effect f (MutableObjectOf LifeState) -> EffectSyntax f (f 'Unit)
stepLife state = stepGeneration state

stepGeneration :: Effect f (MutableObjectOf LifeState) -> EffectSyntax f (f 'Unit)
stepGeneration state = do
  w <- pure (number (fromIntegral gridW))
  h <- pure (number (fromIntegral gridH))
  genScratch <- hold (G.toObject (Scratch 0 0 0))
  cellScratch <- hold (G.toObject (Scratch 0 0 0))
  rows <- state.rows
  cols <- state.cols
  forEach_ rows $ \y ->
    forEach_ cols $ \x -> processCell state genScratch cellScratch w h x y
  swapBuffers state
  livePop <- genScratch.pop
  set @"pop" state (Math.floor livePop)
  gen <- state.gen
  set @"gen" state (gen + 1)

processCell ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f (MutableObjectOf Scratch)
  -> Effect f (MutableObjectOf Scratch)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
processCell state genScratch cellScratch w h x y = do
  alive <- state.alive
  species <- state.species
  nextAlive <- state.nextAlive
  nextSpecies <- state.nextSpecies
  counts <- state.counts
  fillU8 counts 0
  let
    i = cellIdx w x y
  n <- countNeighbors alive species counts w h x y cellScratch
  a <- bindExpr (u8Get alive i)
  sp <- bindExpr (u8Get species i)
  ifS
    (a .== 1)
    ( ifS
        (n .== 2 .|| n .== 3)
        ( do
            setU8 nextAlive i 1
            setU8 nextSpecies i sp
            bumpPop genScratch
        )
        ( do
            setU8 nextAlive i 0
            setU8 nextSpecies i 0
        )
    )
    ( ifS
        (n .== 3)
        ( do
            winner <- cellScratch.best
            setU8 nextAlive i 1
            setU8 nextSpecies i winner
            bumpPop genScratch
        )
        ( do
            setU8 nextAlive i 0
            setU8 nextSpecies i 0
        )
    )

countNeighbors ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f (MutableObjectOf Scratch)
  -> EffectSyntax f (Expr f 'Number)
countNeighbors alive species counts w h x y cellScratch = do
  set @"pop" cellScratch 0
  set @"best" cellScratch 0
  set @"bestCount" cellScratch 0
  deltas <- bindExpr deltasE
  forEach_ deltas $ \dy ->
    forEach_ deltas $ \dx ->
      whenS (not_ (dx .== 0 .&& dy .== 0)) $
        countOne alive species counts w h x y dx dy cellScratch
  cellScratch.pop

countOne ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f (MutableObjectOf Scratch)
  -> EffectSyntax f (f 'Unit)
countOne alive species counts w h x y dx dy cellScratch = do
  let
    nx = toroidal w (x + dx)
    ny = toroidal h (y + dy)
    ni = cellIdx w nx ny
  a <- bindExpr (u8Get alive ni)
  whenS (a .== 1) $ do
    sp <- bindExpr (u8Get species ni)
    n <- cellScratch.pop
    set @"pop" cellScratch (n + 1)
    cur <- bindExpr (u8Get counts sp)
    let
      next = cur + 1
    setU8 counts sp next
    bestC <- cellScratch.bestCount
    bestSp <- cellScratch.best
    whenS
      (next .> bestC .|| (next .== bestC .&& sp .< bestSp))
      ( do
          set @"bestCount" cellScratch next
          set @"best" cellScratch sp
      )

bumpPop :: Effect f (MutableObjectOf Scratch) -> EffectSyntax f (f 'Unit)
bumpPop scratch = do
  p <- scratch.pop
  set @"pop" scratch (p + 1)

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
  toSyntax_ $ renderGrid pixels alive species pal w h px cw
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
    a <- bindExpr (u8Get alive i)
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

setU8 ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
setU8 buf i v = toSyntax (u8Set buf i v)

fillU8 :: Expr f 'Uint8Array -> Expr f 'Number -> EffectSyntax f (f 'Unit)
fillU8 buf v = toSyntax (u8Fill buf v)
