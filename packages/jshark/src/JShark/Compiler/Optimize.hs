{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IR preparation entry points and HVM2 kernel collection.
--
-- The former PHOAS optimizer (and its Analysis/Elim/Fold/Metadata
-- satellites) lived here; once pure expressions moved onto the same
-- lower -> Ir.opt -> pack pipeline they became dead and were deleted.
-- This module is now a thin re-export of the IR-based preparation.
module JShark.Compiler.Optimize
  ( collectHvm2Kernels
  , closedEffectNodes
  , closedExprNodes
  , optimizedExprSize
  , optimizedEffectSize
  , irOptimizedEffectFromClosed
  , irOptimizedExprFromClosed
  )
where

import JShark.Api.Types (ClosedEffect, ClosedExpr)
import JShark.Compiler.Ir (IrEffect, IrExpr)
import JShark.Compiler.Lower
  ( lowerOptEffectIr
  , lowerOptExprIr
  )
import JShark.Compiler.Optimize.Hvm2 (collectHvm2Kernels)

-- | Nodes after IR optimize of a closed effect.
closedEffectNodes :: forall u. ClosedEffect u -> Int
closedEffectNodes e = snd (lowerOptEffectIr e)
{-# NOINLINE closedEffectNodes #-}

-- | Nodes after IR optimize of a closed expression.
closedExprNodes :: forall u. ClosedExpr u -> Int
closedExprNodes e = snd (lowerOptExprIr False e)
{-# NOINLINE closedExprNodes #-}

optimizedExprSize :: forall u. ClosedExpr u -> Int
optimizedExprSize = closedExprNodes

optimizedEffectSize :: forall u. ClosedEffect u -> Int
optimizedEffectSize = closedEffectNodes

irOptimizedEffectFromClosed :: forall u. ClosedEffect u -> IrEffect u
irOptimizedEffectFromClosed e = fst (lowerOptEffectIr e)
{-# NOINLINE irOptimizedEffectFromClosed #-}

irOptimizedExprFromClosed :: forall u. ClosedExpr u -> IrExpr u
irOptimizedExprFromClosed e = fst (lowerOptExprIr False e)
{-# NOINLINE irOptimizedExprFromClosed #-}
