{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Haskell LUT engine (replaces Main.js LifeEngineSync bridge).
module WorkerBridge
  ( initWorkerEngine
  , engineCanStep
  , engineStepGeneration
  , engineTickMs
  , engineModeLabel
  , setEngineRenderMs
  )
where

import EngineFinish (finishStep)
import Grid (StepCtx)
import JShark.Api
import JShark.Api.Generic (MutableObjectOf)
import JShark.Api.Rec (Rec (..), (<:))

initWorkerEngine :: Expr f 'Number -> Expr f 'Number -> EffectSyntax f (f 'Unit)
initWorkerEngine w h = do
  toSyntax_ $
    ffi
      ( "((w,h)=>{const s=globalThis.LifeSimd;if(!s||!s.load)return;"
          <> "void s.load('js/life-simd.wasm',w,h);"
          <> "})"
      )
      (arg w <: arg h <: RecNil)
  done

engineCanStep :: EffectSyntax f (Expr f 'Bool)
engineCanStep = pure true_

engineTickMs :: EffectSyntax f (Expr f 'Number)
engineTickMs = pure (number 0)

engineModeLabel :: EffectSyntax f (Expr f 'String)
engineModeLabel = pure (string "haskell-lut")

setEngineRenderMs :: Expr f 'Number -> EffectSyntax f (f 'Unit)
setEngineRenderMs _ = done

engineStepGeneration ::
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
engineStepGeneration =
  finishStep
