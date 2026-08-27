{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Binary Conway step + species merge (replaces Main.js finishStep).
module EngineFinish
  ( finishStep
  , initEngineGrids
  , reuseEngineGrids
  )
where

import Grid
  ( StepCtx (..)
  , cellIdx
  , clampLiveBounds
  , packedIsAlive
  , refreshPackedRegion
  , setU8
  , u8Get
  )
import JShark.Api
import JShark.Api.Generic (MutableObjectOf)
import qualified JShark.Array as Array
import qualified JShark.Math as Math
import qualified Lut

initEngineGrids ::
  Expr f 'Number
  -> EffectSyntax f (Expr f 'Uint8Array, Expr f 'Uint8Array, Expr f 'Uint8Array)
initEngineGrids gridLen = do
  lut <- Lut.createLifeLUT
  reuseEngineGrids gridLen lut

reuseEngineGrids ::
  Expr f 'Number
  -> Expr f 'Uint8Array
  -> EffectSyntax f (Expr f 'Uint8Array, Expr f 'Uint8Array, Expr f 'Uint8Array)
reuseEngineGrids gridLen lut = do
  gridA <- bindExpr (newByteArray gridLen)
  gridB <- bindExpr (newByteArray gridLen)
  pure (lut, gridA, gridB)

finishStep ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
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
  -> Effect f (MutableObjectOf StepCtx)
  -> EffectSyntax f (Expr f 'Bool)
finishStep
  alive
  species
  nextAlive
  nextSpecies
  engineGridA
  engineGridB
  lut
  w
  h
  x0
  y0
  x1
  y1
  nextLiveList
  nextChangedList
  stepCtx = do
    let
      gridLen = w * h
      (xStart, yStart, xStop, yStop) =
        clampLiveBounds w h x0 y0 x1 y1 (number 1)
      regionRows = yStop - yStart
      regionCols = xStop - xStart
      copyFull = regionRows * regionCols * number 2 .>= gridLen
    ifS
      copyFull
      ( do
          forRange_ (number 0) gridLen $ \i -> do
            a <- u8Get alive i
            setU8 engineGridA i (bitAnd a (number 1))
            done
          Lut.stepRegionLUT lut engineGridA engineGridB w h (number 0) h
      )
      ( do
          let
            copyY0 = Math.max (number 0) (yStart - number 1)
            copyYStop = Math.min h (yStop + number 1)
          forRange_ copyY0 copyYStop $ \y ->
            forRange_ (number 0) w $ \x -> do
              let
                i = cellIdx w x y
              a <- u8Get alive i
              setU8 engineGridA i (bitAnd a (number 1))
              done
          Lut.stepRegionLUT lut engineGridA engineGridB w h yStart yStop
      )
    let
      grid = engineGridB
    set @"pop" stepCtx 0
    set @"bx0" stepCtx (number 1e9)
    set @"by0" stepCtx (number 1e9)
    set @"bx1" stepCtx (number (-1))
    set @"by1" stepCtx (number (-1))
    Array.clear_ nextLiveList
    Array.clear_ nextChangedList
    counts <- bindExpr (newByteArray (number 256))
    touched <- bindExpr (newByteArray (number 8))
    forRange_ yStart yStop $ \y ->
      forRange_ xStart xStop $ \x -> do
        let
          i = cellIdx w x y
        was <- u8Get alive i
        now <- u8Get grid i
        let
          wasLive = bitAnd was (number 1) .== 1
          nowLive = bitAnd now (number 1) .== 1
        ifS
          (nowLive .&& wasLive)
          ( do
              setU8 nextAlive i now
              sp <- u8Get species i
              setU8 nextSpecies i sp
          )
          ( ifS
              (not_ nowLive)
              ( do
                  setU8 nextAlive i (number 0)
                  setU8 nextSpecies i (number 0)
              )
              ( do
                  setU8 nextAlive i now
                  sp <- pickBirthSpecies alive species w h x y counts touched
                  setU8 nextSpecies i sp
              )
          )
        whenS nowLive $ do
          bumpPop stepCtx
          bumpBounds stepCtx x y
          Array.push_ nextLiveList i
          whenS (wasLive .!= nowLive) (Array.push_ nextChangedList i)
        whenS (wasLive .&& not_ nowLive) (Array.push_ nextChangedList i)
        done
    popV <- stepCtx.pop
    bx0V <- stepCtx.bx0
    by0V <- stepCtx.by0
    bx1V <- stepCtx.bx1
    by1V <- stepCtx.by1
    whenS
      (popV .> 0 .&& bx1V .>= bx0V .&& by1V .>= by0V)
      (refreshPackedRegion nextAlive w h bx0V by0V bx1V by1V)
    whenS
      (not_ (popV .> 0 .&& bx1V .>= bx0V .&& by1V .>= by0V))
      ( whenS
          (x1 .>= x0 .&& y1 .>= y0)
          (refreshPackedRegion nextAlive w h x0 y0 x1 y1)
      )
    pure true_

pickBirthSpecies ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> EffectSyntax f (Expr f 'Number)
pickBirthSpecies alive species w h x y counts touched = do
  st <- hold newObject
  _ <- setProp st "best" (number 0)
  _ <- setProp st "bestSid" (number 0)
  _ <- setProp st "touchedLen" (number 0)
  forRange_ (number (-1)) (number 2) $ \dy ->
    forRange_ (number (-1)) (number 2) $ \dx ->
      whenS (not_ (dx .== 0 .&& dy .== 0)) $ do
        let
          nx = x + dx
          ny = y + dy
        whenS (nx .>= 0 .&& ny .>= 0 .&& nx .< w .&& ny .< h) $ do
          let
            ni = cellIdx w nx ny
          whenS (packedIsAlive alive ni) $ do
            sp <- u8Get species ni
            cur <- u8Get counts sp
            let
              next = cur + number 1
            setU8 counts sp next
            whenS (cur .== 0) $ do
              len <- getProp st "touchedLen"
              setU8 touched len sp
              setProp st "touchedLen" (len + 1)
            bestC <- getProp st "best"
            whenS (next .> bestC) $ do
              setProp st "best" next
              setProp st "bestSid" sp
        done
  len <- getProp st "touchedLen"
  forRange_ (number 0) len $ \k -> do
    sp <- u8Get touched k
    setU8 counts sp (number 0)
    done
  getProp st "bestSid"

bumpPop :: Effect f (MutableObjectOf StepCtx) -> EffectSyntax f (f 'Unit)
bumpPop scratch = do
  p <- scratch.pop
  set @"pop" scratch (p + 1)

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
