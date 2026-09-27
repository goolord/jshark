-- | The @jshark@ core package's entry point: the typed AST, the host
-- evaluator, and the compiler, PHOAS terms to JavaScript.
--
-- The EDSL surface ('JShark.Api') and the one-import "JShark" facade live in
-- @jshark-base@. This module re-exports the AST from 'JShark.Api.Types',
-- the evaluator, and the compile entry points: 'pureProgram' and
-- 'effectfulProgram' (minified IIFEs) and 'pureAST' / 'effectfulAST'
-- (readable snippets).
--
-- == Pipeline
--
-- @
-- ClosedExpr / ClosedEffect   -- "JShark.Api.Types"
--   +--> Evaluate             -- "JShark.Compiler.Evaluate" (tests, REPL)
--   v
-- Lower                       -- "JShark.Compiler.Lower" (PHOAS -> first-order IR)
--   v
-- Optimize                    -- "JShark.Compiler.Ir" (folds, let elimination)
--   v
-- Codegen -> JS               -- "JShark.Compiler.Codegen" (number, plan, emit)
-- @
--
-- Named lambdas ('Lambda' with 'Just' tag) hoist to shared @$name@ bindings
-- (see @namedLambda@, @namedLambdaRow@, @applyNamed2@ in @JShark.Api@).
module JShark.Core
  ( Expr (..)
  , FnBody (..)
  , LamInfo (..)
  , noLamInfo
  , Value (..)
  , Arg (..)
  , ClosedExpr
  , ClosedEffect
  , Effect (..)
  , evaluate
  , tryEvaluate
  , EvalFailure (..)
  , evaluateNumber
  , evaluateBigInt
  , packUint8
  , uint8Elems
  , pureProgram
  , effectfulProgram
  , pureAST
  , pureASTWith
  , effectfulAST
  , effectfulASTWith
  , OutputStyle (..)
  , JS
  , renderJS
  , escapeJsString
  , structuralEq
  , structuralNEq
  )
where

import JShark.Api.Types
import JShark.Compiler.Codegen
  ( OutputStyle (..)
  , effectfulAST
  , effectfulASTWith
  , effectfulProgram
  , pureAST
  , pureASTWith
  , pureProgram
  )
import JShark.Compiler.Emit (JS, escapeJsString, renderJS)
import JShark.Compiler.Evaluate
  ( EvalFailure (..)
  , evaluate
  , evaluateBigInt
  , evaluateNumber
  , packUint8
  , tryEvaluate
  , uint8Elems
  )
