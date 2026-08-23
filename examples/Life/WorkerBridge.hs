{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Bridge to the JS LifeEngine (LUT, workers, SIMD) in @js/Main.js@.
module WorkerBridge
  ( initWorkerEngine
  , engineCanStep
  , engineStepGeneration
  , engineTickMs
  , engineModeLabel
  , setEngineRenderMs
  )
where

import Grid (BoundScratch (..))
import JShark.Api
import qualified JShark.Array as Array
import JShark.Generic (MutableObjectOf)
import JShark.Rec (Rec (..), (<:))
import Types (gridH, gridW)

initWorkerEngine :: EffectSyntax f (f 'Unit)
initWorkerEngine =
  toSyntax $
    ffi
      ( "((w,h)=>{const E=globalThis.LifeEngine;if(!E)return;"
          <> "E.init({width:w|0,height:h|0,workerCount:0});"
          <> "})"
      )
      ( arg (number (fromIntegral gridW))
          <: arg (number (fromIntegral gridH))
          <: RecNil
      )

engineCanStep :: EffectSyntax f (Expr f 'Bool)
engineCanStep =
  bindExpr $
    ffi
      ( "(()=>{const E=globalThis.LifeEngine;"
          <> "return!!(E&&E.ready&&E.mode!=='none');"
          <> "})"
      )
      RecNil

engineTickMs :: EffectSyntax f (Expr f 'Number)
engineTickMs =
  bindExpr $
    ffi
      "(()=>{const E=globalThis.LifeEngine;return E?E.lastTickMs:0;})"
      RecNil

engineModeLabel :: EffectSyntax f (Expr f 'String)
engineModeLabel =
  bindExpr $
    ffi
      "(()=>{const E=globalThis.LifeEngine;return E?E.mode:'none';})"
      RecNil

setEngineRenderMs :: Expr f 'Number -> EffectSyntax f (f 'Unit)
setEngineRenderMs ms =
  toSyntax $
    ffi
      "ms=>{const E=globalThis.LifeEngine;if(E)E.setRenderMs(ms);}"
      (arg ms <: RecNil)

engineStepGeneration ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Effect f (MutableObjectOf BoundScratch)
  -> EffectSyntax f (Expr f 'Number)
engineStepGeneration
  alive
  species
  nextAlive
  nextSpecies
  w
  h
  nextLiveList
  nextChangedList
  boundScratch = do
  Array.clear_ nextLiveList
  Array.clear_ nextChangedList
  result <-
    bindExpr $
      ffi
        ( "((alive,species,nextAlive,nextSpecies,w,h,nextLiveList,nextChangedList)=>{"
            <> "const S=globalThis.LifeEngineSync;"
            <> "return S?S.finishStep(alive,species,nextAlive,nextSpecies,w,h,nextLiveList,nextChangedList):null;"
            <> "})"
        )
        ( arg alive
            <: arg species
            <: arg nextAlive
            <: arg nextSpecies
            <: arg w
            <: arg h
            <: arg nextLiveList
            <: arg nextChangedList
            <: RecNil
        )
  pop <- bindExpr $ ffi "r=>r.pop" (arg result <: RecNil)
  bx0n <- bindExpr $ ffi "r=>r.bx0" (arg result <: RecNil)
  by0n <- bindExpr $ ffi "r=>r.by0" (arg result <: RecNil)
  bx1n <- bindExpr $ ffi "r=>r.bx1" (arg result <: RecNil)
  by1n <- bindExpr $ ffi "r=>r.by1" (arg result <: RecNil)
  set @"bx0" boundScratch bx0n
  set @"by0" boundScratch by0n
  set @"bx1" boundScratch bx1n
  set @"by1" boundScratch by1n
  pure pop
