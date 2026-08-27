{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Haskell LUT engine (replaces Main.js LifeEngineSync bridge).
-- Worker pool stepping uses @js/EngineWorker.js@ when enabled.
module WorkerBridge
  ( engineCanStep
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
