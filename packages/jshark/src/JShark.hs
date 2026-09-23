-- | JShark compiler facade: PHOAS terms to JavaScript.
--
-- User-facing syntax lives in 'JShark.Api.Types' and 'JShark.Api'. This module
-- re-exports the compile pipeline and the two entry points
-- ('pureProgram', 'effectfulProgram').
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
-- (see 'JShark.Api.namedLambda', 'namedLambdaRow', 'applyNamed2').
module JShark
  ( Expr (..)
  , FnBody (..)
  , LamInfo (..)
  , noLamInfo
  , Value (..)
  , GroupBy
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
  , JS
  , renderJS
  , escapeJsString
  , structuralEq
  , structuralNEq
  )
where

import JShark.Api.Types
import JShark.Compiler.Codegen (effectfulProgram, pureProgram)
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
