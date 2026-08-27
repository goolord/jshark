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

import Grid (StepCtx)
import JShark.Api
import JShark.Api.Generic (MutableObjectOf)
import JShark.Api.Rec (Rec (..), (<:))

initWorkerEngine :: Expr f 'Number -> Expr f 'Number -> EffectSyntax f (f 'Unit)
initWorkerEngine w h =
  toSyntax $
    ffi
      ( "((w,h)=>{const E=globalThis.LifeEngine;if(!E)return;"
          <> "E.init({width:w|0,height:h|0,workerCount:0});"
          <> "E._wasmReady=false;"
          <> "void E.loadWasm('js/life-simd.wasm');"
          <> "})"
      )
      (arg w <: arg h <: RecNil)

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
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Effect f (MutableObjectOf StepCtx)
  -> EffectSyntax f (Expr f 'Bool)
engineStepGeneration
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
  nextLiveList
  nextChangedList
  stepCtx = do
    bindExpr $
      ffi
        ( "((alive,species,nextAlive,nextSpecies,w,h,x0,y0,x1,y1,nextLiveList,nextChangedList,scratch)=>{"
            <> "const S=globalThis.LifeEngineSync;"
            <> "const r=S?S.finishStep(alive,species,nextAlive,nextSpecies,w,h,x0,y0,x1,y1,nextLiveList,nextChangedList):null;"
            <> "if(!r)return false;"
            <> "scratch.pop=r.pop;"
            <> "scratch.bx0=r.bx0;scratch.by0=r.by0;scratch.bx1=r.bx1;scratch.by1=r.by1;"
            <> "return true;"
            <> "})"
        )
        ( arg alive
            <: arg species
            <: arg nextAlive
            <: arg nextSpecies
            <: arg w
            <: arg h
            <: arg x0
            <: arg y0
            <: arg x1
            <: arg y1
            <: arg nextLiveList
            <: arg nextChangedList
            <: ArgEffect stepCtx
            <: RecNil
        )
