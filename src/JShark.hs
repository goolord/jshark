{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | JShark compiler facade: PHOAS terms to JavaScript.
--
-- User-facing syntax lives in 'JShark.Types' and 'JShark.Api'. This module
-- re-exports the compile pipeline and the two entry points
-- ('pureProgram', 'effectfulProgram').
--
-- == Pipeline (read top to bottom)
--
-- @
-- ClosedExpr / ClosedEffect          -- 'JShark.Types'
--       |
--       v
-- Flatten  ('JShark.Flatten')        -- tree normalize before lower/opt
--       |
--       v
-- Lower    ('JShark.Lower')          -- PHOAS -> first-order IR
--       |
--       v
-- Optimize ('JShark.Optimize')      -- PHOAS + IR passes
--       |
--       +-- pure:  Codegen.Phoas     -- direct PHOAS -> JS ('pureAST')
--       |
--       +-- effect: Flat -> SoA -> Codegen.Flat
--                 ('JShark.Flat', 'JShark.FlatSoA', 'JShark.Codegen.Flat')
--
-- Evaluate ('JShark.Evaluate')      -- reference interpreter (tests, REPL)
-- Hoist    ('JShark.Hoist')         -- named @$tag@ helper registration
--           ('JShark.Hoist.Canonical') -- dedup by alpha-renamed source
-- Codegen.Core ('JShark.Codegen.Core') -- 'CG' state, prep, IIFE wrapper
-- @
--
-- Named lambdas ('Lambda' with 'Just' tag) hoist to shared helpers via
-- 'JShark.Hoist.registerHoistedTag' (see 'JShark.Api.namedLambda',
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
  , optimize
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
  , effectfulAST
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
  )
where

import qualified Data.IntMap.Strict as IM
import GHC.IO.Unsafe (unsafePerformIO)
import JShark.Codegen.Core
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
import JShark.Codegen.Flat
  ( effectfulAST
  , effectfulASTFromFlat
  , effectfulASTFromSoA
  , effectfulASTIr
  , flatEffectfulCodegen
  )
import JShark.Codegen.Phoas (pureAST, pureAST')
import JShark.Emit (JS, renderJS, renderJSCompact)
import JShark.Evaluate
  ( escapeJsString
  , evaluate
  , evaluateBigInt
  , evaluateCached
  , evaluateNumber
  )
import JShark.Lower
  ( irEffectFromClosed
  , irExprFromClosed
  , lowerOptEffectIr
  )
import JShark.Optimize
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
  , optimizedEffectSize
  , optimizedExprSize
  , phoasNodeCountFromIr
  )
import JShark.Types

pureProgram :: ClosedExpr u -> JS
pureProgram e =
  let
    !(s0, expr) = unsafePerformIO (preparePureProgram e)
   in
    uncurry renderIIFE (pureAST' s0 IM.empty expr)

effectfulProgram :: ClosedEffect u -> JS
effectfulProgram e = uncurry renderIIFE (flatEffectfulCodegen e)
