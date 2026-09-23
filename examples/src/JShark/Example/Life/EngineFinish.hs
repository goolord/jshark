{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module JShark.Example.Life.EngineFinish
  ( EngineGrids (..)
  , finishStep
  , initEngineGrids
  , reuseEngineGrids
  )
where

import JShark.Api
import JShark.Generic (MutableObjectOf)
import JShark.Example.Life.Grid (CellGrids (..), StepCtx, StepRegion (..))
import qualified JShark.Example.Life.Lut as Lut
import JShark.Example.Life.LutBoot (lifeLutGlobalJs)

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

-- | The cell grids a step works on, plus the native engine's own LUT and
-- scratch buffers.
data EngineGrids f = EngineGrids
  { egCells :: CellGrids f
  , egGridA :: Expr f 'Uint8Array
  , egGridB :: Expr f 'Uint8Array
  , egLut :: Expr f 'Uint8Array
  }

-- | Step @region@ with the native LUT engine, writing into the @next@
-- grids. Returns 'false_' when the engine is unavailable, in which case the
-- caller falls back to the JShark implementation.
finishStep ::
  EngineGrids f
  -> StepRegion f
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Effect f (MutableObjectOf StepCtx)
  -> EffectSyntax f (Expr f 'Bool)
finishStep EngineGrids {egCells = CellGrids {..}, ..} StepRegion {..} live changed stepCtx = do
  engineOk <-
    bindExpr $
      ffi
        ( "(function(a,sp,na,ns,ga,gb,L,w,h,x0,y0,x1,y1,live,changed,sc){"
            <> "var api="
            <> lifeLutGlobalJs
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
            <: arg egGridA
            <: arg egGridB
            <: arg egLut
            <: arg srW
            <: arg srH
            <: arg srX0
            <: arg srY0
            <: arg srX1
            <: arg srY1
            <: arg live
            <: arg changed
            <: ArgEffect stepCtx
            <: RecNil
        )
  pure (engineOk .== 1)
