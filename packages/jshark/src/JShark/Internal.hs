{-# LANGUAGE LambdaCase #-}

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
  , flatSoaNodeCount
  , optimizedExprSize
  , optimizedEffectSize

    -- * JsShim
  , Builtin (ValueEq)
  , builtinSrc
  )
where

import JShark.Compiler.Codegen.Core (flatPrepareCore)
import JShark.Compiler.Codegen.Flat
  ( effectfulAST
  , effectfulASTFromSoA
  , effectfulASTWith
  , pureAST
  , pureASTWith
  )
import JShark.Compiler.Flat (flatSoaNodeCount)
import JShark.Compiler.JsShim
  ( Builtin (ValueEq)
  , builtinSrc
  )
import JShark.Compiler.Lower
  ( irEffectFromClosed
  , optimizedEffectSize
  , optimizedExprSize
  )
