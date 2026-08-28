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
-- ClosedExpr / ClosedEffect          -- 'JShark.Api.Types'
--       |
--       v
-- Flatten  ('JShark.Compiler.Flatten')        -- tree normalize before lower/opt
--       |
--       v
-- Lower    ('JShark.Compiler.Lower')          -- PHOAS -> first-order IR
--       |
--       v
-- Optimize ('JShark.Compiler.Optimize')      -- PHOAS + IR passes
--       |
--       +-- pure:  Codegen.Phoas     -- direct PHOAS -> JS ('pureAST')
--       |
--       +-- effect: Flat -> SoA -> Codegen.Flat
--                 ('JShark.Compiler.Flat', 'JShark.Compiler.FlatSoA', 'JShark.Compiler.Codegen.Flat')
--
-- Evaluate ('JShark.Compiler.Evaluate')      -- reference interpreter (tests, REPL)
-- Hoist    ('JShark.Compiler.Hoist')         -- named @$tag@ registration
--           ('JShark.Compiler.Hoist.Canonical') -- dedup by alpha-renamed source
-- Codegen.Core ('JShark.Compiler.Codegen.Core') -- 'CG' state, prep, IIFE wrapper
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
      , Hvm2Kernel
      )
  , FnBody (..)
  , LamInfo (..)
  , noLamInfo
  , Value (..)
  , GroupBy
  , Arg (..)
  , Hvm2KernelEntry (..)
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
  , evaluateCached
  , packUint8
  , uint8Elems
  , optimize
  , optimizeWith
  , optimizeEffect
  , optimizeEffectFromIr
  , phoasNodeCountFromIr
  , optimizeEffectIr
  , nodeCountExpr
  , nodeCountEff
  , closedEffectNodes
  , closedExprNodes
  , lowerOptEffectIr
  , optIrLargeThreshold
  , optimizedExprSize
  , optimizedEffectSize
  , pureAST
  , pureASTWith
  , effectfulAST
  , effectfulASTWith
  , effectfulASTFromFlat
  , effectfulASTFromSoA
  , effectfulASTIr
  , irEffectFromClosed
  , flatPrepareCore
  , flatPrepareFromIr
  , profileFlatOptFromIr
  , profileIrOptFromClosed
  , profileIrOptFromIr
  , profileLowerFromClosed
  , flatSoaNodeCount
  , flatSoaParallelThreshold
  , irExprFromClosed
  , irOptimizedEffectFromClosed
  , irOptimizedExprFromClosed
  , collectHvm2Kernels
  , pureProgram
  , effectfulProgram
  , printComputation
  , renderJS
  , renderJSCompact
  , escapeJsString
  , structuralEq
  , structuralNEq
  , Builtin (ValueEq)
  , builtinSrc
  )
where

import qualified Data.IntMap.Strict as IM
import GHC.IO.Unsafe (unsafePerformIO)
import JShark.Api.Types
import JShark.Compiler.Codegen.Core
  ( flatPrepareCore
  , flatPrepareFromIr
  , flatSoaNodeCount
  , flatSoaParallelThreshold
  , preparePureProgram
  , printComputation
  , profileFlatOptFromIr
  , profileIrOptFromClosed
  , profileIrOptFromIr
  , profileLowerFromClosed
  , renderIIFE
  )
import JShark.Compiler.Codegen.Flat
  ( effectfulAST
  , effectfulASTFromFlat
  , effectfulASTFromSoA
  , effectfulASTIr
  , effectfulASTWith
  , flatEffectfulCodegen
  )
import JShark.Compiler.Codegen.Phoas (pureAST, pureAST', pureASTWith)
import JShark.Compiler.Emit (JS, renderJS, renderJSCompact)
import JShark.Compiler.Evaluate
  ( escapeJsString
  , evaluate
  , evaluateBigInt
  , evaluateCached
  , evaluateNumber
  , packUint8
  , uint8Elems
  )
import JShark.Compiler.JsShim (Builtin (ValueEq), builtinSrc)
import JShark.Compiler.Lower
  ( irEffectFromClosed
  , irExprFromClosed
  , lowerOptEffectIr
  )
import JShark.Compiler.Optimize
  ( closedEffectNodes
  , closedExprNodes
  , collectHvm2Kernels
  , irOptimizedEffectFromClosed
  , irOptimizedExprFromClosed
  , nodeCountEff
  , nodeCountExpr
  , optIrLargeThreshold
  , optimize
  , optimizeEffect
  , optimizeEffectFromIr
  , optimizeEffectIr
  , optimizeWith
  , optimizedEffectSize
  , optimizedExprSize
  , phoasNodeCountFromIr
  )

pureProgram :: ClosedExpr u -> JS
pureProgram e =
  let
    !(s0, expr) = unsafePerformIO (preparePureProgram e)
   in
    uncurry renderIIFE (pureAST' s0 IM.empty expr)

effectfulProgram :: ClosedEffect u -> JS
effectfulProgram e = uncurry renderIIFE (flatEffectfulCodegen e)
