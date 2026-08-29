{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier -Wno-orphans #-}

-- | PHOAS optimizer and IR preparation.
module JShark.Compiler.Optimize
  ( optimize
  , optimizeWith
  , optimizeEffect
  , optimizeEffectFromIr
  , optimizeEffectIr
  , phoasNodeCountFromIr
  , nodeCountExpr
  , nodeCountEff
  , closedEffectNodes
  , closedExprNodes
  , optIrLargeThreshold
  , optimizedExprSize
  , optimizedEffectSize
  , irOptimizedEffectFromClosed
  , irOptimizedExprFromClosed
  , collectHvm2Kernels
  , bindProbeTag
  , letProbeTag
  )
where

import Data.Bits (xor, (.&.), (.|.))
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import JShark.Api.Prim
  ( MathBinary (..)
  , MathUnary (..)
  , isPureFixed
  , matchMathBinary
  , matchMathUnary
  )
import JShark.Api.Rec
import JShark.Api.Types
import JShark.Compiler.Binder
  ( Stamp (..)
  , nestedDummy
  , peelBoolEffect
  , peelOption
  , peelResult
  , peelString
  )
import JShark.Compiler.Evaluate (isCheapValue, valueEq)
import JShark.Compiler.Flatten
  ( PhoasDummy (..)
  , fieldsPure
  , flattenEff
  , flattenExpr
  , foldGetField
  , rebindEff
  , rebindExpr
  , rebindExpr2
  )
import qualified JShark.Compiler.Ir as Ir
import JShark.Compiler.JsNum (jsBit2, jsRem, jsShl, jsShr, jsUShr)
import JShark.Compiler.Lower
  ( allocFnTags
  , evalFnBody
  , rebindFn
  , lowerEffectAt
  , lowerExprAt
  , lowerOptEffectIr
  , reifyEffect
  )
import JShark.Compiler.Metadata (Metadata (..), optSmall, optStep)
import JShark.Compiler.Optimize.Analysis
  ( bindProbeTag
  , cheapExpr
  , letProbeTag
  , nodeCountEff
  , nodeCountExpr
  , pureExpr
  )
import JShark.Compiler.Optimize.Elim (elimBindFrom, elimLetFrom)
import JShark.Compiler.Optimize.Fold
  ( foldAnd
  , foldArrLen
  , foldBig
  , foldBigNeg
  , foldConcat
  , foldFixedBinary
  , foldFixedUnary
  , foldFromBigInt
  , foldFrozenEq
  , foldIndex
  , foldNum1
  , foldNum2
  , foldOr
  , foldOrd
  , foldOrdNeq
  , foldParseBigInt
  , foldShow
  , foldToBigInt
  , foldTypeOf
  )
import JShark.Compiler.Optimize.Hvm2 (collectHvm2Kernels)

instance PhoasDummy Stamp where
  phoasDummy = nestedDummy
  isPureExpr_ = pureExpr

closedEffectNodes :: ClosedEffect u -> Int
closedEffectNodes e = snd (lowerOptEffectIr e)
{-# NOINLINE closedEffectNodes #-}

closedExprNodes :: ClosedExpr u -> Int
closedExprNodes (e :: ClosedExpr u) =
  let
    (_, ir) = lowerExprAt (-2) (flattenExpr (optimize e))
   in
    Ir.irSize (Ir.metaIrExpr ir)
{-# NOINLINE closedExprNodes #-}

-- | PHOAS 'optEffect' is quadratic on long bind chains; IR opt for huge ASTs.
optIrLargeThreshold :: Int
optIrLargeThreshold = 0

keepExprCont ::
  (?keepLets :: Bool) =>
  Int
  -> Int
  -> Expr Stamp v
  -> Metadata
  -> (Stamp u -> Expr Stamp v)
  -> Stamp u
  -> Expr Stamp v
keepExprCont t tag body _ f
  | nodeCountExpr body <= optSmall = reoptExpr t f
  | otherwise = rebindExpr tag body

keepEffCont ::
  (?keepLets :: Bool) =>
  Int
  -> Int
  -> Effect Stamp v
  -> Metadata
  -> (Stamp u -> Effect Stamp v)
  -> Stamp u
  -> Effect Stamp v
keepEffCont t tag body _ f
  | nodeCountEff body <= optSmall = reoptEff t f
  | otherwise = rebindEff tag body

keepExprCont2 ::
  Int
  -> Int
  -> Int
  -> Expr Stamp v
  -> Metadata
  -> (Stamp a -> Stamp b -> Expr Stamp v)
  -> Stamp a
  -> Stamp b
  -> Expr Stamp v
keepExprCont2 _ tA tB body _ _ a b = rebindExpr2 tA tB body a b

reoptExpr ::
  (?keepLets :: Bool) =>
  Int -> (Stamp u -> Expr Stamp v) -> Stamp u -> Expr Stamp v
reoptExpr t f b = let (_, e, _) = optExpr t (flattenExpr (f b)) in e

reoptEff ::
  (?keepLets :: Bool) =>
  Int -> (Stamp u -> Effect Stamp v) -> Stamp u -> Effect Stamp v
reoptEff t f b = let (_, e, _) = optEffect t (flattenEff (f b)) in e

irOptimizedExprFromClosed :: ClosedExpr u -> Ir.IrExpr u
irOptimizedExprFromClosed (e :: ClosedExpr u) =
  let
    ?keepLets = False
   in
    let
      (!_, !ir) = lowerExprAt (-2) (flattenExpr (optimize e))
      (!_, !irOpt, !_) = Ir.optIrExpr (-2) ir
     in
      irOpt
{-# NOINLINE irOptimizedExprFromClosed #-}

irOptimizedEffectFromClosed :: ClosedEffect u -> Ir.IrEffect u
irOptimizedEffectFromClosed e = fst (lowerOptEffectIr e)
{-# NOINLINE irOptimizedEffectFromClosed #-}

optimizedExprSize :: ClosedExpr u -> Int
optimizedExprSize (e :: ClosedExpr u) =
  let
    ?keepLets = False
   in
    let
      (!_, !ir) = lowerExprAt (-2) (flattenExpr (e :: Expr Stamp u))
      (!_, !_, !md) = Ir.optIrExpr (-2) ir
     in
      Ir.irSize md

optimizedEffectSize :: ClosedEffect u -> Int
optimizedEffectSize e = snd (lowerOptEffectIr e)

optimize :: ClosedExpr u -> Expr Stamp u
optimize = optimizeWith False

optimizeWith :: Bool -> ClosedExpr u -> Expr Stamp u
optimizeWith keepLets (e :: ClosedExpr u) =
  let
    ?keepLets = keepLets
   in
    let
      (_, final, _) = optExpr (-2) (e :: Expr Stamp u)
     in
      flattenExpr final
{-# NOINLINE optimizeWith #-}

optimizeEffectIr :: Effect Stamp u -> Effect Stamp u
optimizeEffectIr e =
  let
    ?keepLets = False
   in
    let
      (!_, !ir) = lowerEffectAt (-2) (flattenEff e)
      (!_, !irOpt, !_) = Ir.optIrEffect (-2) ir
     in
      flattenEff (reifyEffect irOpt)
{-# NOINLINE optimizeEffectIr #-}

optimizeEffectFromIr :: Ir.IrEffect u -> Effect Stamp u
optimizeEffectFromIr ir = flattenEff (reifyEffect ir)
{-# NOINLINE optimizeEffectFromIr #-}

phoasNodeCountFromIr :: Ir.IrEffect u -> Int
phoasNodeCountFromIr ir = nodeCountEff (optimizeEffectFromIr ir)
{-# NOINLINE phoasNodeCountFromIr #-}

optimizeEffect :: ClosedEffect u -> Effect Stamp u
optimizeEffect e = optimizeEffectFromIr (fst (lowerOptEffectIr e))
{-# NOINLINE optimizeEffect #-}

optUnder ::
  (?keepLets :: Bool) =>
  Int -> (Stamp u -> Expr Stamp v) -> (Int, Int, Expr Stamp v, Metadata)
optUnder t0 f =
  let
    tag = t0
    (t1, body, md) = optExpr (t0 - optStep) (f (Stamp tag))
   in
    (t1, tag, body, md)

optUnderE ::
  (?keepLets :: Bool) =>
  Int -> (Stamp u -> Effect Stamp v) -> (Int, Int, Effect Stamp v, Metadata)
optUnderE t0 f =
  let
    tag = t0
    (t1, body, md) = optEffect (t0 - optStep) (f (Stamp tag))
   in
    (t1, tag, body, md)

optUnder2 ::
  (?keepLets :: Bool) =>
  Int
  -> (Stamp a -> Stamp b -> Expr Stamp v)
  -> (Int, Int, Int, Expr Stamp v, Metadata)
optUnder2 t0 f =
  let
    tA = t0
    tB = t0 - optStep
    (t1, body, md) = optExpr (t0 - 2 * optStep) (f (Stamp tA) (Stamp tB))
   in
    (t1, tA, tB, body, md)

optArgs ::
  (?keepLets :: Bool) =>
  Int -> Rec (Arg Stamp) us -> (Int, Rec (Arg Stamp) us, Metadata)
optArgs t0 RecNil = (t0, RecNil, mempty)
optArgs t0 (RecCons x xs) =
  let
    (t1, x', mdX) = optArg t0 x
    (t2, xs', mdXS) = optArgs t1 xs
   in
    (t2, RecCons x' xs', mdX <> mdXS)

optArg ::
  (?keepLets :: Bool) => Int -> Arg Stamp u -> (Int, Arg Stamp u, Metadata)
optArg t (ArgExpr e) =
  let
    (t', e', md) = optExpr t e
   in
    (t', ArgExpr e', md)
optArg t (ArgEffect e) =
  let
    (t', e', md) = optEffect t e
   in
    (t', ArgEffect e', md)

optFixed ::
  (?keepLets :: Bool) =>
  Int
  -> FixedOp a b c u
  -> FixedArgs Stamp a b c
  -> (Int, Expr Stamp u, Metadata)
optFixed t0 op args = case (op, args) of
  (n, ArgsU x)
    | Just (MathUnary n') <- matchMathUnary n ->
        let
          (t1, x', mdX) = optExpr t0 x
          res = foldFixedUnary n' x'
          md = Metadata 1 True (cheapExpr res) <> mdX
         in
          (t1, res, md)
  (n, ArgsB x y)
    | Just (MathBinary n') <- matchMathBinary n ->
        let
          (t1, x', mdX) = optExpr t0 x
          (t2, y', mdY) = optExpr t1 y
          res = foldFixedBinary n' x' y'
          md = Metadata 1 True (cheapExpr res) <> mdX <> mdY
         in
          (t2, res, md)
  (FixArrLen, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldArrLen x'
      md = Metadata 1 True (cheapExpr res) <> mdX
     in
      (t1, res, md)
  (FixToBigInt, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldToBigInt x'
      md = Metadata 1 True (cheapExpr res) <> mdX
     in
      (t1, res, md)
  (FixFromBigInt, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldFromBigInt x'
      md = Metadata 1 True (cheapExpr res) <> mdX
     in
      (t1, res, md)
  (FixParseBigInt, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldParseBigInt x'
      md = Metadata 1 True (cheapExpr res) <> mdX
     in
      (t1, res, md)
  (n, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = expr1 n x'
      md = Metadata 1 (isPureFixed n) (cheapExpr res) <> mdX
     in
      (t1, res, md)
  (n, ArgsB x y) ->
    let
      (t1, x', mdX) = optExpr t0 x
      (t2, y', mdY) = optExpr t1 y
      res = expr2 n x' y'
      md = Metadata 1 (isPureFixed n) (cheapExpr res) <> mdX <> mdY
     in
      (t2, res, md)
  (n, ArgsT x y z) ->
    let
      (t1, x', mdX) = optExpr t0 x
      (t2, y', mdY) = optExpr t1 y
      (t3, z', mdZ) = optExpr t2 z
      res = expr3 n x' y' z'
      md = Metadata 1 True (cheapExpr res) <> mdX <> mdY <> mdZ
     in
      (t3, res, md)

optLet ::
  (?keepLets :: Bool) =>
  Int
  -> Expr Stamp u
  -> (Stamp u -> Expr Stamp v)
  -> (Int, Expr Stamp v, Metadata)
optLet t0 x f =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, tag, body, mdBody) = optUnder t1 f
   in
    elimLetFrom optExpr t2 x' mdX f tag body mdBody

optBind ::
  (?keepLets :: Bool) =>
  Int
  -> Effect Stamp u
  -> (Stamp u -> Effect Stamp v)
  -> (Int, Effect Stamp v, Metadata)
optBind t0 x f =
  let
    (t1, x', mdX) = optEffect t0 x
    (t2, tag, body, mdBody) = optUnderE t1 f
   in
    elimBindFrom optEffect t2 x' mdX f tag body mdBody

optBin ::
  (?keepLets :: Bool) =>
  Int
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> (Int, Expr Stamp 'Bool, Metadata)
optBin t0 k x y =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, y', mdY) = optExpr t1 y
   in
    (t2, k x' y', Metadata 1 True False <> mdX <> mdY)

optBinNum ::
  (?keepLets :: Bool) =>
  Int
  -> (Double -> Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
  -> (Int, Expr Stamp 'Number, Metadata)
optBinNum t0 f k x y =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, y', mdY) = optExpr t1 y
    res = foldNum2 f k x' y'
   in
    (t2, res, Metadata 1 True (cheapExpr res) <> mdX <> mdY)

optUnNum ::
  (?keepLets :: Bool) =>
  Int
  -> (Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> (Int, Expr Stamp 'Number, Metadata)
optUnNum t0 f k x =
  let
    (t1, x', mdX) = optExpr t0 x
    res = foldNum1 f k x'
   in
    (t1, res, Metadata 1 True (cheapExpr res) <> mdX)

optUnderFn ::
  (?keepLets :: Bool) =>
  Int
  -> FnBody Stamp us v
  -> (Int, [Int], Expr Stamp v, Metadata, FnBody Stamp us v)
optUnderFn t0 body =
  let
    (tags, tEnd) = allocFnTags t0 body
    expr = evalFnBody body tags
    (t1, expr', md) = optExpr tEnd expr
   in
    (t1, tags, expr', md, body)

keepFnCont ::
  [Int] -> Expr Stamp v -> FnBody Stamp us v -> FnBody Stamp us v
keepFnCont tags expr' body = rebindFn tags expr' body

optExpr ::
  (?keepLets :: Bool) => Int -> Expr Stamp u -> (Int, Expr Stamp u, Metadata)
optExpr t0 expr = case expr of
  Literal v -> (t0, Literal v, Metadata 1 True (isCheapValue v))
  Var (Embed e) -> optExpr t0 (flattenExpr e)
  Var (EmbedEff (Lift e)) -> optExpr t0 (flattenExpr e)
  Var (EmbedEff e) ->
    let
      (t1, e', md) = optEffect t0 e
     in
      case e' of
        Lift x -> (t1, x, md)
        _ -> (t1, Var (EmbedEff e'), md)
  Var (Stamp i) -> (t0, Var (Stamp i), Metadata 1 True False)
  Let x f -> optLet t0 x f
  LetRec r b ->
    let
      tag = t0
      (t1, r', mdR) = optExpr (t0 - optStep) (r (Stamp tag))
      (t2, b', mdB) = optExpr t1 (b (Stamp tag))
      res = LetRec (keepExprCont t2 tag r' mdR r) (keepExprCont t2 tag b' mdB b)
      md = Metadata 1 True False <> mdR <> mdB
     in
      (t2, res, md)
  Lambda hoist f ->
    let
      (t1, tag, body, mdBody) = optUnder t0 f
      res = Lambda hoist (keepExprCont t1 tag body mdBody f)
      md = Metadata 1 True False <> mdBody
     in
      (t1, res, md)
  Apply f x ->
    let
      (t1, f', mdF) = optExpr t0 f
      (t2, x', mdX) = optExpr t1 x
     in
      case f' of
        fn@(Lambda LamInfo {lamTag = Just _} _) ->
          (t2, Apply fn x', Metadata 1 True False <> mdF <> mdX)
        Lambda LamInfo {lamTag = Nothing} g -> optLet t2 x' g
        _ -> (t2, Apply f' x', Metadata 1 True False <> mdF <> mdX)
  If c t e ->
    let
      (t1, c', mdC) = optExpr t0 c
     in
      case c' of
        Literal (ValueBool True) -> optExpr t1 t
        Literal (ValueBool False) -> optExpr t1 e
        _ ->
          let
            (t2, t', mdT) = optExpr t1 t
            (t3, e', mdE) = optExpr t2 e
            md = Metadata 1 True False <> mdC <> mdT <> mdE
           in
            (t3, If c' t' e', md)
  OptionCase o n s ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case peelOption o' of
        Just Nothing -> optExpr t1 n
        Just (Just x) ->
          let
            (t2, tag, body, mdBody) = optUnder t1 s
           in
            elimLetFrom optExpr t2 x mdO s tag body mdBody
        Nothing ->
          let
            (t2, n', mdN) = optExpr t1 n
            (t3, tag, body, mdBody) = optUnder t2 s
            md = Metadata 1 True False <> mdO <> mdN <> mdBody
           in
            (t3, OptionCase o' n' (keepExprCont t3 tag body mdBody s), md)
  ResultOk x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      (t1, ResultOk x', Metadata 1 True False <> mdX)
  ResultErr x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      (t1, ResultErr x', Metadata 1 True False <> mdX)
  ResultCase o e s ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case peelResult o' of
        Just (Left x) ->
          let
            (t2, tag, body, mdBody) = optUnder t1 e
           in
            elimLetFrom optExpr t2 x mdO e tag body mdBody
        Just (Right x) ->
          let
            (t2, tag, body, mdBody) = optUnder t1 s
           in
            elimLetFrom optExpr t2 x mdO s tag body mdBody
        Nothing ->
          let
            (t2, tE, e', mdE) = optUnder t1 e
            (t3, tS, s', mdS) = optUnder t2 s
            md = Metadata 1 True False <> mdO <> mdE <> mdS
           in
            ( t3
            , ResultCase o' (keepExprCont t3 tE e' mdE e) (keepExprCont t3 tS s' mdS s)
            , md
            )
  Index arr idx ->
    let
      (t1, arr', mdA) = optExpr t0 arr
      (t2, idx', mdI) = optExpr t1 idx
      res = foldIndex arr' idx'
      md = Metadata 1 True (cheapExpr res) <> mdA <> mdI
     in
      (t2, res, md)
  U8Index buf idx ->
    let
      (t1, buf', mdB) = optExpr t0 buf
      (t2, idx', mdI) = optExpr t1 idx
      md = Metadata 1 True False <> mdB <> mdI
     in
      (t2, U8Index buf' idx', md)
  Error x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      (t1, Error x', Metadata 1 True False <> mdX)
  Std s -> optStd t0 s
  FnLit body ->
    let
      (t1, tags, expr', mdExpr, body0) = optUnderFn t0 body
      res = FnLit (keepFnCont tags expr' body0)
      md = Metadata 1 True False <> mdExpr
     in
      (t1, res, md)
  UnsafeNullable x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      (t1, UnsafeNullable x', Metadata 1 True (cheapExpr x') <> mdX)
  FrozenLit fs ->
    let
      (t1, fs', mdFS) = mapAccumField t0 fs
     in
      (t1, FrozenLit fs', Metadata 1 (fieldsPure fs') False <> mdFS)
  GetField @k o ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case foldGetField @k o' of
        Just e -> optExpr t1 e
        Nothing -> (t1, GetField @k o', Metadata 1 True False <> mdO)
  Hvm2Kernel name k ->
    (t0, Hvm2Kernel name k, Metadata 1 True False)

optMapped ::
  (?keepLets :: Bool) =>
  ( Expr Stamp ('Array u)
    -> (Stamp u -> Expr Stamp b)
    -> Expr Stamp c
  )
  -> Int
  -> Expr Stamp ('Array u)
  -> (Stamp u -> Expr Stamp b)
  -> (Int, Expr Stamp c, Metadata)
optMapped k t0 x f =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, tag, body, mdBody) = optUnder t1 f
    md = Metadata 1 True False <> mdX <> mdBody
   in
    (t2, k x' (keepExprCont t2 tag body mdBody f), md)

optReduced ::
  (?keepLets :: Bool) =>
  ( Expr Stamp ('Array u)
    -> Expr Stamp v
    -> (Stamp v -> Stamp u -> Expr Stamp v)
    -> Expr Stamp v
  )
  -> Int
  -> Expr Stamp ('Array u)
  -> Expr Stamp v
  -> (Stamp v -> Stamp u -> Expr Stamp v)
  -> (Int, Expr Stamp v, Metadata)
optReduced k t0 x z f =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, z', mdZ) = optExpr t1 z
    (t3, tA, tB, body, mdBody) = optUnder2 t2 f
    md = Metadata 1 True False <> mdX <> mdZ <> mdBody
   in
    (t3, k x' z' (keepExprCont2 t3 tA tB body mdBody f), md)

optToSorted ::
  (?keepLets :: Bool) =>
  ( Expr Stamp ('Array u)
    -> (Stamp u -> Stamp u -> Expr Stamp 'Number)
    -> Expr Stamp ('Array u)
  )
  -> Int
  -> Expr Stamp ('Array u)
  -> (Stamp u -> Stamp u -> Expr Stamp 'Number)
  -> (Int, Expr Stamp ('Array u), Metadata)
optToSorted k t0 x f =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, tA, tB, body, mdBody) = optUnder2 t1 f
    md = Metadata 1 True False <> mdX <> mdBody
   in
    (t2, k x' (keepExprCont2 t2 tA tB body mdBody f), md)

optStd ::
  (?keepLets :: Bool) => Int -> Std Stamp u -> (Int, Expr Stamp u, Metadata)
optStd t0 s = case s of
  Fixed op args -> optFixed t0 op args
  Method m -> optMethod t0 m
  Kernel k -> optKernel t0 k

optKernel ::
  (?keepLets :: Bool) => Int -> Kernel Stamp u -> (Int, Expr Stamp u, Metadata)
optKernel t0 k = case k of
  KPlus x y -> optBinNum t0 (+) Plus x y
  KTimes x y -> optBinNum t0 (*) Times x y
  KMinus x y -> optBinNum t0 (-) Minus x y
  KFracDiv x y -> optBinNum t0 (/) FracDiv x y
  KRem x y -> optBinNum t0 jsRem Rem x y
  KBitAnd x y -> optBinNum t0 (jsBit2 (.&.)) BitAnd x y
  KBitOr x y -> optBinNum t0 (jsBit2 (.|.)) BitOr x y
  KBitXor x y -> optBinNum t0 (jsBit2 xor) BitXor x y
  KShl x y -> optBinNum t0 jsShl Shl x y
  KShr x y -> optBinNum t0 jsShr Shr x y
  KUShr x y -> optBinNum t0 jsUShr UShr x y
  KNegate x -> optUnNum t0 negate Negate x
  KBig op x y ->
    let
      (t1, x', mdX) = optExpr t0 x
      (t2, y', mdY) = optExpr t1 y
      res = foldBig op x' y'
      md = Metadata 1 True (cheapExpr res) <> mdX <> mdY
     in
      (t2, res, md)
  KBigNeg x ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldBigNeg x'
      md = Metadata 1 True (cheapExpr res) <> mdX
     in
      (t1, res, md)
  KConcat x y ->
    let
      (t1, x', mdX) = optExpr t0 x
      (t2, y', mdY) = optExpr t1 y
      res = foldConcat x' y'
      md = Metadata 1 True (cheapExpr res) <> mdX <> mdY
     in
      (t2, res, md)
  KShow x ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldShow x'
      md = Metadata 1 True (cheapExpr res) <> mdX
     in
      (t1, res, md)
  KTypeOf x ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldTypeOf x'
      md = Metadata 1 True (cheapExpr res) <> mdX
     in
      (t1, res, md)
  KAnd x y ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      case x' of
        Literal (ValueBool False) -> (t1, Literal (ValueBool False), Metadata 1 True True <> mdX)
        Literal (ValueBool True) -> optExpr t1 y
        _ ->
          let
            (t2, y', mdY) = optExpr t1 y
            res = foldAnd x' y'
            md = Metadata 1 True (cheapExpr res) <> mdX <> mdY
           in
            (t2, res, md)
  KOr x y ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      case x' of
        Literal (ValueBool True) -> (t1, Literal (ValueBool True), Metadata 1 True True <> mdX)
        Literal (ValueBool False) -> optExpr t1 y
        _ ->
          let
            (t2, y', mdY) = optExpr t1 y
            res = foldOr x' y'
            md = Metadata 1 True (cheapExpr res) <> mdX <> mdY
           in
            (t2, res, md)
  KEq structural x y ->
    optBin t0 (foldFrozenEq valueEq (\a b -> Std (Kernel (KEq structural a b)))) x y
  KNEq structural x y ->
    optBin
      t0
      ( foldFrozenEq
          (\a b -> not (valueEq a b))
          (\a b -> Std (Kernel (KNEq structural a b)))
      )
      x
      y
  KGTh x y -> optBin t0 (foldOrd GT GTh) x y
  KLTh x y -> optBin t0 (foldOrd LT LTh) x y
  KGTEq x y -> optBin t0 (foldOrdNeq LT GTEq) x y
  KLTEq x y -> optBin t0 (foldOrdNeq GT LTEq) x y

optMethod ::
  (?keepLets :: Bool) => Int -> Method Stamp u -> (Int, Expr Stamp u, Metadata)
optMethod t0 m = case m of
  MethMap x f -> optMapped (\a g -> Std (Method (MethMap a g))) t0 x f
  MethFilter x f -> optMapped (\a g -> Std (Method (MethFilter a g))) t0 x f
  MethReduce x z f -> optReduced (\a b g -> Std (Method (MethReduce a b g))) t0 x z f
  MethReduceRight x z f -> optReduced (\a b g -> Std (Method (MethReduceRight a b g))) t0 x z f
  MethToSorted x f -> optToSorted (\a g -> Std (Method (MethToSorted a g))) t0 x f
  MethFrom n f ->
    let
      (t1, n', mdN) = optExpr t0 n
      (t2, tag, body, mdBody) = optUnder t1 f
      md = Metadata 1 True False <> mdN <> mdBody
     in
      (t2, Std (Method (MethFrom n' (keepExprCont t2 tag body mdBody f))), md)

optEffect ::
  (?keepLets :: Bool) => Int -> Effect Stamp u -> (Int, Effect Stamp u, Metadata)
optEffect t0 eff = case eff of
  Lift x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      case x' of
        Var (EmbedEff e) -> optEffect t1 e
        _ -> (t1, Lift x', Metadata 1 True (cheapExpr x') <> mdX)
  FFI n args ->
    let
      (t1, args', md) = optArgs t0 args
     in
      (t1, FFI n args', Metadata 1 False False <> md)
  UnsafeObject o -> (t0, UnsafeObject o, Metadata 1 False False)
  UnsafeObjectGet x s ->
    let
      (t1, x', mdX) = optEffect t0 x
     in
      (t1, UnsafeObjectGet x' s, Metadata 1 False False <> mdX)
  UnsafeObjectAssign x y ->
    let
      (t1, x', mdX) = optEffect t0 x
      (t2, y', mdY) = optEffect t1 y
     in
      (t2, UnsafeObjectAssign x' y', Metadata 1 False False <> mdX <> mdY)
  CallMethod x n args ->
    let
      (t1, x', mdX) = optEffect t0 x
      (t2, args', mdA) = optArgs t1 args
     in
      (t2, CallMethod x' n args', Metadata 1 False False <> mdX <> mdA)
  Bind x f -> optBind t0 x f
  ThenE x y ->
    let
      (t1, x', mdX) = optEffect t0 x
      (t2, y', mdY) = optEffect t1 y
     in
      (t2, ThenE x' y', Metadata 1 (mdIsPure mdX && mdIsPure mdY) False <> mdX <> mdY)
  BindRec r b ->
    let
      tag = t0
      (t1, r', mdR) = optEffect (t0 - optStep) (r (Stamp tag))
      (t2, b', mdB) = optEffect t1 (b (Stamp tag))
      res = BindRec (keepEffCont t2 tag r' mdR r) (keepEffCont t2 tag b' mdB b)
      md = Metadata 1 False False <> mdR <> mdB
     in
      (t2, res, md)
  LambdaE f ->
    let
      (t1, tag, body, mdBody) = optUnderE t0 f
      res = LambdaE (keepEffCont t1 tag body mdBody f)
      md = Metadata 1 True False <> mdBody
     in
      (t1, res, md)
  ApplyE f x ->
    let
      (t1, f', mdF) = optEffect t0 f
      (t2, x', mdX) = optEffect t1 x
     in
      case f' of
        LambdaE g -> optBind t2 x' g
        _ -> (t2, ApplyE f' x', Metadata 1 False False <> mdF <> mdX)
  IfE c t e ->
    let
      (t1, c', mdC) = optEffect t0 c
     in
      case peelBoolEffect c' of
        Just True -> optEffect t1 t
        Just False -> optEffect t1 e
        Nothing ->
          let
            (t2, t', mdT) = optEffect t1 t
            (t3, e', mdE) = optEffect t2 e
            md =
              Metadata 1 (mdIsPure mdC && mdIsPure mdT && mdIsPure mdE) False
                <> mdC
                <> mdT
                <> mdE
           in
            (t3, IfE c' t' e', md)
  While c b ->
    let
      (t1, c', mdC) = optEffect t0 c
     in
      case peelBoolEffect c' of
        Just False -> (t1, Lift (Literal ValueUnit), Metadata 1 True True <> mdC)
        _ ->
          let
            (t2, b', mdB) = optEffect t1 b
            md = Metadata 1 False False <> mdC <> mdB
           in
            (t2, While c' b', md)
  ForRange s e b ->
    let
      (t1, s', mdS) = optExpr t0 s
      (t2, e', mdE) = optExpr t1 e
      (t3, tag, body, mdBody) = optUnderE t2 b
      md = Metadata 1 False False <> mdS <> mdE <> mdBody
     in
      (t3, ForRange s' e' (keepEffCont t3 tag body mdBody b), md)
  U8Set b i v ->
    let
      (t1, b', mdB) = optExpr t0 b
      (t2, i', mdI) = optExpr t1 i
      (t3, v', mdV) = optExpr t2 v
      md = Metadata 1 False False <> mdB <> mdI <> mdV
     in
      (t3, U8Set b' i' v', md)
  U8Fill b v ->
    let
      (t1, b', mdB) = optExpr t0 b
      (t2, v', mdV) = optExpr t1 v
      md = Metadata 1 False False <> mdB <> mdV
     in
      (t2, U8Fill b' v', md)
  OptionCaseE o n s ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case peelOption o' of
        Just Nothing -> optEffect t1 n
        Just (Just x) ->
          let
            (t2, tag, body, mdBody) = optUnderE t1 s
           in
            elimBindFrom optEffect t2 (Lift x) mdO s tag body mdBody
        Nothing ->
          let
            (t2, n', mdN) = optEffect t1 n
            (t3, tag, body, mdBody) = optUnderE t2 s
            md =
              Metadata 1 (mdIsPure mdO && mdIsPure mdN && mdIsPure mdBody) False
                <> mdO
                <> mdN
                <> mdBody
           in
            (t3, OptionCaseE o' n' (keepEffCont t3 tag body mdBody s), md)
  ResultCaseE o e s ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case peelResult o' of
        Just (Left x) ->
          let
            (t2, tag, body, mdBody) = optUnderE t1 e
           in
            elimBindFrom optEffect t2 (Lift x) mdO e tag body mdBody
        Just (Right x) ->
          let
            (t2, tag, body, mdBody) = optUnderE t1 s
           in
            elimBindFrom optEffect t2 (Lift x) mdO s tag body mdBody
        Nothing ->
          let
            (t2, tE, e', mdE) = optUnderE t1 e
            (t3, tS, s', mdS) = optUnderE t2 s
            md =
              Metadata 1 (mdIsPure mdO && mdIsPure mdE && mdIsPure mdS) False
                <> mdO
                <> mdE
                <> mdS
           in
            ( t3
            , ResultCaseE o' (keepEffCont t3 tE e' mdE e) (keepEffCont t3 tS s' mdS s)
            , md
            )
  StringCaseE o arms d ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case peelString o' of
        Just k -> optEffect t1 (fromMaybe d (lookup k arms))
        Nothing ->
          let
            (t2, arms', mdArms) = mapAccumArms t1 arms
            (t3, d', mdD) = optEffect t2 d
            md = Metadata 1 False False <> mdO <> mdArms <> mdD
           in
            (t3, StringCaseE o' arms' d', md)
  Throw x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      (t1, Throw x', Metadata 1 False False <> mdX)
  Try a k ->
    let
      (t1, a', mdA) = optEffect t0 a
      (t2, tag, body, mdBody) = optUnderE t1 k
      md = Metadata 1 False False <> mdA <> mdBody
     in
      (t2, Try a' (keepEffCont t2 tag body mdBody k), md)
  ObjectLit fs ->
    let
      (t1, fs', mdFS) = mapAccumField t0 fs
     in
      (t1, ObjectLit fs', Metadata 1 False False <> mdFS)
  DeleteProp o k ->
    let
      (t1, o', mdO) = optEffect t0 o
      (t2, k', mdK) = optExpr t1 k
      md = Metadata 1 False False <> mdO <> mdK
     in
      (t2, DeleteProp o' k', md)
  ArrayLit es ->
    let
      (t1, es', mdEs) = mapAccumEffs t0 es
     in
      (t1, ArrayLit es', Metadata 1 False False <> mdEs)

mapAccumField ::
  forall r.
  (?keepLets :: Bool) =>
  Int -> [FieldLit Stamp r] -> (Int, [FieldLit Stamp r], Metadata)
mapAccumField t0 fs =
  let
    (t1, res) = mapAccumL step t0 fs
   in
    (t1, map fst res, mconcat (map snd res))
 where
  step ::
    (?keepLets :: Bool) =>
    Int -> FieldLit Stamp r -> (Int, (FieldLit Stamp r, Metadata))
  step t = \case
    FieldLit @k e ->
      let (t', e', md) = optExpr t e in (t', (FieldLit @k e', md))
    FieldLitEffect @k e ->
      let (t', e', md) = optEffect t e in (t', (FieldLitEffect @k e', md))
    FieldLitExtra @k e ->
      let (t', e', md) = optExpr t e in (t', (FieldLitExtra @k e', md))
    FieldLitExtraEffect @k e ->
      let (t', e', md) = optEffect t e in (t', (FieldLitExtraEffect @k e', md))

mapAccumEffs ::
  (?keepLets :: Bool) =>
  Int -> [Effect Stamp u] -> (Int, [Effect Stamp u], Metadata)
mapAccumEffs t0 es =
  let
    (t1, res) = mapAccumL step t0 es
   in
    (t1, map fst res, mconcat (map snd res))
 where
  step t e = let (t', e', md) = optEffect t e in (t', (e', md))

mapAccumArms ::
  (?keepLets :: Bool) =>
  Int -> [(Text, Effect Stamp u)] -> (Int, [(Text, Effect Stamp u)], Metadata)
mapAccumArms t0 arms =
  let
    (t1, res) = mapAccumL step t0 arms
   in
    (t1, map fst res, mconcat (map snd res))
 where
  step t (k, e) = let (t', e', md) = optEffect t e in (t', ((k, e'), md))

-- | Sequencing without a binder ('ThenE' / discarded bind).
