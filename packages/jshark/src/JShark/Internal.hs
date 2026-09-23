{-# LANGUAGE RankNTypes #-}

-- | Deliberate internal access for tests, benchmarks, and tooling.
--
-- The ordinary public surface is "JShark". This module re-exports the
-- IR and codegen internals those callers need. Its API may change between
-- 0.x releases.
module JShark.Internal
  ( -- * Readable / minified renderers
    pureAST
  , pureASTWith
  , effectfulAST
  , effectfulASTWith
  , OutputStyle (..)

    -- * IR
  , irEffectFromClosed
  , optimizedExprSize
  , optimizedEffectSize
  , validateOptimizedEffect
  , validateOptimizedExpr
  , module JShark.Compiler.Ir

    -- * Runtime shims
  , Builtin (ValueEq)
  , builtinSrc
  )
where

import Data.Text (Text)
import JShark.Api.Types (ClosedEffect, ClosedExpr)
import JShark.Compiler.Codegen
import JShark.Compiler.Emit (Builtin (ValueEq), builtinSrc)
import JShark.Compiler.Ir
import JShark.Compiler.Lower

-- | Scope\/binder problems in the optimized IR of a closed effect (@[]@ is
-- valid). Cheap enough for tests; not run in the production path.
validateOptimizedEffect :: ClosedEffect u -> [Text]
validateOptimizedEffect e = validateIr (irEffectFromClosed e)

-- | 'validateOptimizedEffect' for a closed pure expression.
validateOptimizedExpr :: ClosedExpr u -> [Text]
validateOptimizedExpr e = validateIr (fst (lowerOptExprIr False e))
