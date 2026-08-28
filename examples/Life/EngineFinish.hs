{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module EngineFinish
  ( finishStep
  , initEngineGrids
  , reuseEngineGrids
  )
where

import qualified Data.Text as T
import Grid (StepCtx)
import JShark.Api
import JShark.Api.Generic (MutableObjectOf)
import JShark.Api.Rec (Rec (..), (<:))
import qualified Lut
import LutBoot (lifeLutGlobalJs)

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
    engineOk <-
      bindExpr $
        ffi
          ( "(function(a,sp,na,ns,ga,gb,L,w,h,x0,y0,x1,y1,live,changed,sc){"
              <> "var api="
              <> T.unpack lifeLutGlobalJs
              <> ";"
              <> "if(!api||typeof api.finishStep!=='function')return 0;"
              <> "return api.finishStep("
              <> "a,sp,na,ns,ga,gb,L,w,h,x0,y0,x1,y1,live,changed,sc"
              <> ")?1:0;"
              <> "})"
          )
          ( arg alive
              <: arg species
              <: arg nextAlive
              <: arg nextSpecies
              <: arg engineGridA
              <: arg engineGridB
              <: arg lut
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
    pure (engineOk .== 1)
