{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Deliberate internal access for tests, benchmarks, and tooling.
--
-- The ordinary public surface is 'JShark'. This module re-exports the
-- compiler\/IR\/SoA\/codegen internals those callers need, keeping them out
-- of the ordinary API. Its API may change between 0.x releases.
module JShark.Internal
  ( -- * Readable / minified AST renderers
    pureAST
  , pureASTWith
  , effectfulAST
  , effectfulASTWith
  , effectfulASTFromSoA

    -- * IR + flat entry points
  , irEffectFromClosed
  , flatPrepareCore
  , optimizedExprSize
  , optimizedEffectSize
  , validateOptimizedEffect
  , validateOptimizedExpr
  , EmitStyle (..)
  , minifiedStyle

    -- * Compiler internals (IR, Flat, SoA) re-exported for tests/tooling
  , module JShark.Compiler.Flat
  , module JShark.Compiler.Ir

    -- * JsShim
  , Builtin (ValueEq)
  , builtinSrc
  )
where

import Data.Text (Text)
import JShark.Api.Types (ClosedEffect, ClosedExpr)
import JShark.Compiler.Codegen.Core (EmitStyle (..), flatPrepareCore, minifiedStyle)
import JShark.Compiler.Codegen.Flat
  ( effectfulAST
  , effectfulASTFromSoA
  , effectfulASTWith
  , pureAST
  , pureASTWith
  )
import JShark.Compiler.Flat
import JShark.Compiler.Ir
import JShark.Compiler.JsShim
  ( Builtin (ValueEq)
  , builtinSrc
  )
import JShark.Compiler.Lower
  ( irEffectFromClosed
  , lowerOptExprIr
  , optimizedEffectSize
  , optimizedExprSize
  )

-- | Scope/binder problems in the optimized IR of a closed effect (@[]@ is
-- valid). Cheap enough for tests; not run in the production path.
validateOptimizedEffect :: ClosedEffect u -> [Text]
validateOptimizedEffect e = validateIr (irEffectFromClosed e)

-- | 'validateOptimizedEffect' for a closed pure expression.
validateOptimizedExpr :: ClosedExpr u -> [Text]
validateOptimizedExpr e = validateIr (fst (lowerOptExprIr False e))
