{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

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
  ( Expr
      ( Literal
      , Concat
      , Plus
      , Times
      , Minus
      , Negate
      , FracDiv
      , Rem
      , BitAnd
      , BitOr
      , BitXor
      , Shl
      , Shr
      , UShr
      , And
      , Or
      , Eq
      , NEq
      , GTh
      , LTh
      , GTEq
      , LTEq
      , Let
      , LetRec
      , Lambda
      , Apply
      , Show
      , TypeOf
      , Var
      , If
      , OptionCase
      , ResultOk
      , ResultErr
      , ResultCase
      , Index
      , U8Index
      , Error
      , Std
      , FnLit
      , UnsafeNullable
      , FrozenLit
      , GetField
      )
  , FnBody (..)
  , LamInfo (..)
  , noLamInfo
  , Value (..)
  , GroupBy
  , Arg (..)
  , ClosedExpr
  , ClosedEffect
  , Effect
    ( Lift
    , FFI
    , UnsafeObject
    , UnsafeObjectGet
    , UnsafeObjectAssign
    , CallMethod
    , Bind
    , ThenE
    , BindRec
    , LambdaE
    , ApplyE
    , IfE
    , While
    , ForRange
    , U8Set
    , U8Fill
    , OptionCaseE
    , ResultCaseE
    , StringCaseE
    , Throw
    , Try
    , ObjectLit
    , DeleteProp
    , ArrayLit
    )
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
import JShark.Compiler.Emit (JS, renderJS)
import JShark.Compiler.Evaluate
  ( EvalFailure (..)
  , escapeJsString
  , evaluate
  , evaluateBigInt
  , evaluateNumber
  , packUint8
  , tryEvaluate
  , uint8Elems
  )
