{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- | JShark compiler facade: PHOAS terms to JavaScript.
--
-- User-facing syntax lives in 'JShark.Api.Types' and 'JShark.Api'. This module
-- re-exports the compile pipeline and the two entry points
-- ('pureProgram', 'effectfulProgram').
--
-- == Pipeline (read top to bottom)
--
-- @
-- ClosedExpr / ClosedEffect           -- 'JShark.Api.Types'
--       |
--       +--> Evaluate                 -- 'JShark.Compiler.Evaluate' (tests, REPL)
--       |
--       v
-- Lower                               -- 'JShark.Compiler.Lower' (PHOAS -> first-order Ir)
--       |
--       v
-- Ir optimize                         -- 'JShark.Compiler.Ir' (one optimizer: folds + elim)
--       |
--       v
-- Flat (pack + SoA bulk opts)         -- 'JShark.Compiler.Flat'
--       |
--       v
-- Codegen.Flat -> JS                  -- 'JShark.Compiler.Codegen.Flat' (pure + effectful)
--
-- Hoist    ('JShark.Compiler.Hoist')          -- named @$tag@ registration
--           ('JShark.Compiler.Hoist.Canonical') -- dedup by alpha-renamed source
-- Codegen.Core ('JShark.Compiler.Codegen.Core') -- 'CG' state, prep, IIFE wrapper
-- Codegen.Stmt ('JShark.Compiler.Codegen.Stmt') -- shared statement renderers
-- @
--
-- Named lambdas ('Lambda' with 'Just' tag) hoist to shared @$name@ bindings via
-- 'JShark.Compiler.Hoist.registerHoistedTag' (see 'JShark.Api.namedLambda',
-- 'namedLambdaRow', 'applyNamed2').
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
  , evaluateNumber
  , evaluateBigInt
  , packUint8
  , uint8Elems
  , optimizedExprSize
  , optimizedEffectSize
  , pureAST
  , pureASTWith
  , effectfulAST
  , effectfulASTWith
  , effectfulASTFromSoA
  , irEffectFromClosed
  , flatPrepareCore
  , flatPrepareFromIr
  , profileFlatOptFromIr
  , profileIrOptFromClosed
  , profileIrOptFromIr
  , profileLowerFromClosed
  , flatSoaNodeCount
  , irExprFromClosed
  , irOptimizedEffectFromClosed
  , irOptimizedExprFromClosed
  , pureProgram
  , effectfulProgram
  , renderJS
  , escapeJsString
  , structuralEq
  , structuralNEq
  , Builtin (ValueEq)
  , builtinSrc
  )
where

import JShark.Api.Types
import JShark.Compiler.Codegen.Core
  ( flatPrepareCore
  , flatPrepareFromIr
  , flatSoaNodeCount
  , profileFlatOptFromIr
  , profileIrOptFromClosed
  , profileIrOptFromIr
  , profileLowerFromClosed
  , renderIIFE
  )
import JShark.Compiler.Codegen.Flat
  ( effectfulAST
  , effectfulASTFromSoA
  , effectfulASTWith
  , flatEffectfulCodegen
  , flatPureCodegen
  , pureAST
  , pureASTWith
  )
import JShark.Compiler.Emit (JS, renderJS)
import JShark.Compiler.Evaluate
  ( escapeJsString
  , evaluate
  , evaluateBigInt
  , evaluateNumber
  , packUint8
  , uint8Elems
  )
import JShark.Compiler.JsShim (Builtin (ValueEq), builtinSrc)
import JShark.Compiler.Lower
  ( irEffectFromClosed
  , irExprFromClosed
  , irOptimizedEffectFromClosed
  , irOptimizedExprFromClosed
  , optimizedEffectSize
  , optimizedExprSize
  )

pureProgram :: ClosedExpr u -> JS
pureProgram e = uncurry renderIIFE (flatPureCodegen e)

effectfulProgram :: ClosedEffect u -> JS
effectfulProgram e = uncurry renderIIFE (flatEffectfulCodegen e)
