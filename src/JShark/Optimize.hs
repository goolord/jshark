{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds -Wno-pattern-namespace-specifier -Wno-unused-imports -Wno-orphans #-}

-- | PHOAS optimizer and IR preparation wrappers.
module JShark.Optimize
  ( optimize
  , optimizeEffect
  , optimizeEffectFromIr
  , optimizeEffectIr
  , optimizeEffectTree
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
  , evalFnBody
  , fnArity
  , reoptExpr
  , reoptEff
  , bindProbeTag
  , letProbeTag
  )
where

import Control.Monad (forM_)
import Data.Bits (xor, (.&.), (.|.))
import Data.Char (isDigit)
import qualified Data.Char as Char
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', mapAccumL, nub, sortBy)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Monoid (All (..), Any (..), Sum (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, type (:~:) (Refl))
import GHC.TypeLits (KnownSymbol, sameSymbol, symbolVal)
import JShark.Binder
  ( Stamp (..)
  , nestedDummy
  , nestedDummyId
  , peelBoolEffect
  , peelOption
  , peelResult
  , peelString
  , stampId
  , pattern Name
  )
import JShark.Emit
  ( JS
  , dquotes
  , jsString
  , jsText
  )
import JShark.Evaluate
  ( bigOpJS
  , eqFoldableValue
  , escapeJsString
  , isFiniteDouble
  , isOrderableValue
  , jsShow
  , parseBigIntString
  , tryEvalBigBin
  , typeOfValue
  , valueCompare
  , valueEq
  )
import JShark.Flatten
  ( PhoasDummy (..)
  , fieldsPure
  , flattenEff
  , flattenExpr
  , foldEff
  , foldExpr
  , foldGetField
  , inlineEff
  , inlineExpr
  , isPureExpr
  , occursVarInEff
  , occursVarInExpr
  , rebindEff
  , rebindExpr
  , rebindExpr2
  )
import qualified JShark.Ir as Ir
import JShark.JsNum (jsBit2, jsRem, jsShl, jsShr, jsUShr)
import JShark.Lower
  ( allocFnTags
  , evalFnBody
  , fnDepthStamp
  , lowerEffectAt
  , lowerExprAt
  , lowerOptEffectIr
  , rebindFn
  , reifyEffect
  )
import JShark.Metadata (Metadata (..), optStep)
import JShark.Prim
  ( MathBinary (..)
  , MathUnary (..)
  , isPureFixed
  , matchMathBinary
  , matchMathUnary
  )
import qualified JShark.Prim as Prim
import JShark.Rec
import JShark.Types
import Unsafe.Coerce (unsafeCoerce)

instance PhoasDummy Stamp where
  phoasDummy = nestedDummy
  isPureExpr_ = pureExpr
  isPureEffect_ = pureEffect

countLazyExpr :: Int -> Expr Stamp u -> Int
countLazyExpr t e = if occursVarInExpr t e then 2 else 0
{-# NOINLINE countLazyExpr #-}

countLazyEffect :: Int -> Effect Stamp u -> Int
countLazyEffect t e = if occursVarInEff t e then 2 else 0
{-# NOINLINE countLazyEffect #-}

countExpr :: Int -> Expr Stamp u -> Int
countExpr t e = case e of
  Var (Stamp i) -> if i == t then 1 else 0
  Var (Embed e') -> countExpr t e'
  Var (EmbedEff e') -> countEffect t e'
  _ ->
    getSum
      ( foldExpr
          nestedDummy
          (Sum . countExpr t)
          (Sum . countLazyExpr t)
          (Sum . countEffect t)
          e
      )

countEffect :: Int -> Effect Stamp u -> Int
countEffect t e =
  getSum
    ( foldEff
        nestedDummy
        (Sum . countExpr t)
        (Sum . countEffect t)
        (Sum . countLazyEffect t)
        e
    )

effectBindUses :: Int -> Effect Stamp u -> Int
effectBindUses tag e =
  let
    n = countEffect tag e
   in
    if n == 0 && occursVarInEff tag e then 2 else n

bindProbeTag :: Int -> Effect Stamp u -> (Int, Bool)
bindProbeTag probeTag tagged =
  (probeTag, occursVarInEff probeTag tagged)

letProbeTag :: Int -> Expr Stamp u -> (Int, Int)
letProbeTag probeTag tagged =
  (probeTag, elimExprUses probeTag tagged mempty)

elimExprUses :: Int -> Expr Stamp v -> Metadata -> Int
elimExprUses tag body _ =
  let
    n = countExpr tag body
   in
    if n == 0 && occursVarInExpr tag body then 1 else n

elimEffUses :: Int -> Effect Stamp v -> Metadata -> Int
elimEffUses tag body _ = effectBindUses tag body

nodeCountExpr :: Expr Stamp u -> Int
nodeCountExpr expr = case expr of
  Var (Embed e') -> nodeCountExpr e'
  Var (EmbedEff e') -> nodeCountEff e'
  e ->
    1
      + getSum
        ( foldExpr
            nestedDummy
            (Sum . nodeCountExpr)
            (Sum . nodeCountExpr)
            (Sum . nodeCountEff)
            e
        )

nodeCountEff :: Effect Stamp u -> Int
nodeCountEff e =
  1
    + getSum
      ( foldEff
          nestedDummy
          (Sum . nodeCountExpr)
          (Sum . nodeCountEff)
          (Sum . nodeCountEff)
          e
      )

closedEffectNodes :: ClosedEffect u -> Int
closedEffectNodes e = snd (lowerOptEffectIr e)
{-# NOINLINE closedEffectNodes #-}

closedExprNodes :: ClosedExpr u -> Int
closedExprNodes (e :: ClosedExpr u) =
  let
    (_, ir) = lowerExprAt (-2) (flattenExpr (optimize e))
   in
    Ir.irMetaSize (Ir.metaIrExpr ir)
{-# NOINLINE closedExprNodes #-}

cheapExpr :: Expr Stamp u -> Bool
cheapExpr = \case
  Literal v -> isCheapValue v
  Var (Embed e') -> cheapExpr e'
  Var (EmbedEff e') -> cheapEffect e'
  Var _ -> True
  e ->
    let
      here = case e of
        UnsafeNullable {} -> True
        GetField {} -> True
        _ -> False
     in
      here
        && getAll
          ( foldExpr
              nestedDummy
              (All . cheapExpr)
              (const mempty)
              (All . cheapEffect)
              e
          )

cheapEffect :: Effect Stamp u -> Bool
cheapEffect e =
  let
    here = case e of
      Lift {} -> True
      _ -> False
   in
    here
      && getAll
        ( foldEff
            nestedDummy
            (All . cheapExpr)
            (All . cheapEffect)
            (const mempty)
            e
        )

pureExpr :: Expr Stamp u -> Bool
pureExpr = \case
  Literal _ -> True
  Var (Embed e') -> pureExpr e'
  Var (EmbedEff e') -> pureEffect e'
  Var _ -> True
  e ->
    let
      here = case e of
        Std (Fixed op _) -> Prim.isPureFixed op
        _ -> True
     in
      here
        && getAll
          ( foldExpr
              nestedDummy
              (All . pureExpr)
              (const mempty)
              (All . pureEffect)
              e
          )

isAliasBind :: Effect Stamp u -> Bool
isAliasBind (Lift (Var (EmbedEff e))) = isAliasBind e
isAliasBind (Lift (Var _)) = True
isAliasBind (Lift (UnsafeNullable (Var _))) = True
isAliasBind _ = False

pureEffect :: Effect Stamp u -> Bool
pureEffect e =
  let
    here = case e of
      FFI {} -> False
      UnsafeObjectGet {} -> False
      UnsafeObjectAssign {} -> False
      CallMethod {} -> False
      ApplyE {} -> False
      While {} -> False
      ForRange {} -> False
      U8Set {} -> False
      U8Fill {} -> False
      Throw {} -> False
      Try {} -> False
      DeleteProp {} -> False
      _ -> True
   in
    here
      && getAll
        ( foldEff
            nestedDummy
            (All . pureExpr)
            (All . pureEffect)
            (const mempty)
            e
        )

isPureEffect :: Effect Stamp u -> Bool
isPureEffect = pureEffect

-- Re-opt only small trees. A second walk of a @bindRec@ / do-chain
-- paint body is what hung todo-mvc and breakout.
optSmall :: Int
optSmall = 16

-- | PHOAS 'optEffect' is quadratic on long bind chains; IR opt for huge ASTs.
optIrLargeThreshold :: Int
optIrLargeThreshold = 0

fnArity :: FnBody Stamp us r -> Int
fnArity = fnDepthStamp

keepExprCont ::
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

reoptExpr :: Int -> (Stamp u -> Expr Stamp v) -> Stamp u -> Expr Stamp v
reoptExpr t f b = let (_, e, _) = optExpr t (flattenExpr (f b)) in e

reoptEff :: Int -> (Stamp u -> Effect Stamp v) -> Stamp u -> Effect Stamp v
reoptEff t f b = let (_, e, _) = optEffect t (flattenEff (f b)) in e

reoptExpr2 ::
  Int
  -> (Stamp u -> Stamp w -> Expr Stamp v)
  -> Stamp u
  -> Stamp w
  -> Expr Stamp v
reoptExpr2 t f a b = let (_, e, _) = optExpr t (flattenExpr (f a b)) in e

irOptimizedExprFromClosed :: ClosedExpr u -> Ir.IrExpr u
irOptimizedExprFromClosed (e :: ClosedExpr u) =
  let
    (!_, !ir) = lowerExprAt (-2) (flattenExpr (optimize e))
    (!_, !irOpt, !_) = Ir.optIrExpr (-2) ir
   in
    irOpt
{-# NOINLINE irOptimizedExprFromClosed #-}

irOptimizedEffectFromClosed :: ClosedEffect u -> Ir.IrEffect u
irOptimizedEffectFromClosed e = fst (lowerOptEffectIr e)
{-# NOINLINE irOptimizedEffectFromClosed #-}

collectHvm2Kernels :: Expr f u -> [Hvm2KernelEntry]
collectHvm2Kernels expr = collectAny (unsafeCoerce expr :: Expr Stamp u)
 where
  collectAny :: Expr Stamp v -> [Hvm2KernelEntry]
  collectAny = \case
    Hvm2Kernel name k -> [Hvm2KernelEntry name k]
    Literal _ -> []
    Var _ -> []
    Let x g -> collectAny x <> collectAny (g nestedDummy)
    LetRec r b ->
      collectAny (r nestedDummy) <> collectAny (b nestedDummy)
    Lambda _ g -> collectAny (g nestedDummy)
    Apply f x -> collectAny f <> collectAny x
    If c t eF -> collectAny c <> collectAny t <> collectAny eF
    OptionCase o n s ->
      collectAny o <> collectAny n <> collectAny (s nestedDummy)
    ResultOk x -> collectAny x
    ResultErr x -> collectAny x
    ResultCase o er ok ->
      collectAny o
        <> collectAny (er nestedDummy)
        <> collectAny (ok nestedDummy)
    Index x i -> collectAny x <> collectAny i
    U8Index x i -> collectAny x <> collectAny i
    Error x -> collectAny x
    Std s -> collectStdHvm2 s
    FnLit body -> collectFnBodyHvm2 body
    UnsafeNullable x -> collectAny x
    FrozenLit fs -> concatMap collectFieldLitHvm2 fs
    GetField o -> collectAny o
  collectStdHvm2 = \case
    Fixed _ args -> collectFixedArgsHvm2 args
    Method m -> collectMethodHvm2 m
    Kernel k -> collectKernelHvm2 k
  collectFixedArgsHvm2 = \case
    ArgsU x -> collectAny x
    ArgsB x y -> collectAny x <> collectAny y
    ArgsT x y z -> collectAny x <> collectAny y <> collectAny z
  collectMethodHvm2 = \case
    MethMap x f -> collectAny x <> collectAny (f nestedDummy)
    MethFilter x f -> collectAny x <> collectAny (f nestedDummy)
    MethReduce x z _ -> collectAny x <> collectAny z
    MethReduceRight x z _ -> collectAny x <> collectAny z
    MethToSorted x _ -> collectAny x
    MethFrom n f -> collectAny n <> collectAny (f nestedDummy)
  collectKernelHvm2 = \case
    KPlus x y -> collectAny x <> collectAny y
    KTimes x y -> collectAny x <> collectAny y
    KMinus x y -> collectAny x <> collectAny y
    KNegate x -> collectAny x
    KFracDiv x y -> collectAny x <> collectAny y
    KRem x y -> collectAny x <> collectAny y
    KBitAnd x y -> collectAny x <> collectAny y
    KBitOr x y -> collectAny x <> collectAny y
    KBitXor x y -> collectAny x <> collectAny y
    KShl x y -> collectAny x <> collectAny y
    KShr x y -> collectAny x <> collectAny y
    KUShr x y -> collectAny x <> collectAny y
    KBig _ x y -> collectAny x <> collectAny y
    KBigNeg x -> collectAny x
    KConcat x y -> collectAny x <> collectAny y
    KShow x -> collectAny x
    KTypeOf x -> collectAny x
    KAnd x y -> collectAny x <> collectAny y
    KOr x y -> collectAny x <> collectAny y
    KEq _ x y -> collectAny x <> collectAny y
    KNEq _ x y -> collectAny x <> collectAny y
    KGTh x y -> collectAny x <> collectAny y
    KLTh x y -> collectAny x <> collectAny y
    KGTEq x y -> collectAny x <> collectAny y
    KLTEq x y -> collectAny x <> collectAny y
  collectFnBodyHvm2 :: FnBody Stamp us r -> [Hvm2KernelEntry]
  collectFnBodyHvm2 = \case
    JfNil e -> collectAny e
    JfCons k -> collectFnBodyHvm2 (k nestedDummy)
  collectFieldLitHvm2 = \case
    FieldLit e -> collectAny e
    FieldLitEffect e -> collectEffectAny e
    FieldLitExtra e -> collectAny e
    FieldLitExtraEffect e -> collectEffectAny e
  collectEffectAny :: Effect Stamp v -> [Hvm2KernelEntry]
  collectEffectAny = \case
    Lift x -> collectAny x
    FFI _ args -> collectRecArgs args
    Bind x f -> collectEffectAny x <> collectEffectAny (f nestedDummy)
    ThenE x y -> collectEffectAny x <> collectEffectAny y
    BindRec r b ->
      collectEffectAny (r nestedDummy) <> collectEffectAny (b nestedDummy)
    LambdaE f -> collectEffectAny (f nestedDummy)
    ApplyE f x -> collectEffectAny f <> collectEffectAny x
    IfE c u v -> collectEffectAny c <> collectEffectAny u <> collectEffectAny v
    While c b -> collectEffectAny c <> collectEffectAny b
    ForRange s e b ->
      collectAny s <> collectAny e <> collectEffectAny (b nestedDummy)
    U8Set b i v -> collectAny b <> collectAny i <> collectAny v
    U8Fill b v -> collectAny b <> collectAny v
    OptionCaseE o n s ->
      collectAny o <> collectEffectAny n <> collectEffectAny (s nestedDummy)
    ResultCaseE o er ok ->
      collectAny o
        <> collectEffectAny (er nestedDummy)
        <> collectEffectAny (ok nestedDummy)
    StringCaseE o arms d ->
      collectAny o
        <> concatMap (collectEffectAny . snd) arms
        <> collectEffectAny d
    Throw x -> collectAny x
    Try a k -> collectEffectAny a <> collectEffectAny (k nestedDummy)
    ObjectLit fs -> concatMap collectFieldLitHvm2 fs
    DeleteProp o k -> collectEffectAny o <> collectAny k
    ArrayLit es -> concatMap collectEffectAny es
    UnsafeObject {} -> []
    UnsafeObjectGet x _ -> collectEffectAny x
    UnsafeObjectAssign x y -> collectEffectAny x <> collectEffectAny y
    CallMethod x _ args -> collectEffectAny x <> collectRecArgs args
  collectRecArgs :: Rec (Arg Stamp) us -> [Hvm2KernelEntry]
  collectRecArgs = \case
    RecNil -> []
    RecCons a rest -> collectArgAny a <> collectRecArgs rest
  collectArgAny :: Arg Stamp v -> [Hvm2KernelEntry]
  collectArgAny = \case
    ArgExpr e -> collectAny (unsafeCoerce e :: Expr Stamp v)
    ArgEffect e -> collectEffectAny (unsafeCoerce e :: Effect Stamp v)

hvm2ExportRef :: Text -> JS
hvm2ExportRef name =
  let
    key = dquotes (jsString (escapeJsString (T.unpack name)))
    err =
      dquotes
        (jsString (escapeJsString ("HVM2 kernel not loaded: " ++ T.unpack name)))
   in
    "((function(){var f=globalThis.__jsharkHvm2?.exports?.["
      <> key
      <> "];if(typeof f!==\"function\")return function(){throw new Error("
      <> err
      <> ")};"
      <> "function toI64(x){var buf=new ArrayBuffer(8);"
      <> "var f64=new Float64Array(buf);var i64=new BigInt64Array(buf);"
      <> "f64[0]=+x;return i64[0];}"
      <> "function fromOut(r){return typeof r===\"bigint\"?Number(r):r;}"
      <> "if(f.length>=2){return function(a){return function(b){"
      <> "return fromOut(f(toI64(a),toI64(b)));};};}"
      <> "return function(a){return fromOut(f(toI64(a)));};})())"

optimizedExprSize :: ClosedExpr u -> Int
optimizedExprSize (e :: ClosedExpr u) =
  let
    (!_, !ir) = lowerExprAt (-2) (flattenExpr (e :: Expr Stamp u))
    (!_, !_, !md) = Ir.optIrExpr (-2) ir
   in
    Ir.irMetaSize md

optimizedEffectSize :: ClosedEffect u -> Int
optimizedEffectSize e = snd (lowerOptEffectIr e)

optimize :: ClosedExpr u -> Expr Stamp u
optimize (e :: ClosedExpr u) =
  let
    (_, final, _) = optExpr (-2) (e :: Expr Stamp u)
   in
    flattenExpr final
{-# NOINLINE optimize #-}

optimizeEffectIr :: Effect Stamp u -> Effect Stamp u
optimizeEffectIr e =
  let
    (!_, !ir) = lowerEffectAt (-2) (flattenEff e)
    (!_, !irOpt, !_) = Ir.optIrEffect (-2) ir
   in
    flattenEff (reifyEffect irOpt)
{-# NOINLINE optimizeEffectIr #-}

optimizeEffectTree :: ClosedEffect u -> Effect Stamp u
optimizeEffectTree e =
  flattenEff (reifyEffect (fst (lowerOptEffectIr e)))
{-# NOINLINE optimizeEffectTree #-}

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
  Int -> (Stamp u -> Expr Stamp v) -> (Int, Int, Expr Stamp v, Metadata)
optUnder t0 f =
  let
    tag = t0
    (t1, body, md) = optExpr (t0 - optStep) (f (Stamp tag))
   in
    (t1, tag, body, md)

optUnderE ::
  Int -> (Stamp u -> Effect Stamp v) -> (Int, Int, Effect Stamp v, Metadata)
optUnderE t0 f =
  let
    tag = t0
    (t1, body, md) = optEffect (t0 - optStep) (f (Stamp tag))
   in
    (t1, tag, body, md)

optUnder2 ::
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

isCheapValue :: Value u -> Bool
isCheapValue = \case
  ValueNumber {} -> True
  ValueBigInt {} -> True
  ValueString {} -> True
  ValueBool {} -> True
  ValueUnit -> True
  ValueOption Nothing -> True
  ValueOption (Just v) -> isCheapValue v
  ValueResult (Left v) -> isCheapValue v
  ValueResult (Right v) -> isCheapValue v
  ValueRegex {} -> False
  ValueUint8Array {} -> False
  ValueArray {} -> False
  ValueFunction {} -> False
  ValueFrozen {} -> False

isCheap :: Expr Stamp u -> Bool
isCheap = cheapExpr

isCheapFieldLit :: FieldLit Stamp r -> Bool
isCheapFieldLit = \case
  FieldLit e -> isCheap e
  FieldLitExtra e -> isCheap e
  FieldLitEffect {} -> False
  FieldLitExtraEffect {} -> False

isCheapEffect :: Effect Stamp u -> Bool
isCheapEffect = cheapEffect

optArgs :: Int -> Rec (Arg Stamp) us -> (Int, Rec (Arg Stamp) us, Metadata)
optArgs t0 RecNil = (t0, RecNil, mempty)
optArgs t0 (RecCons x xs) =
  let
    (t1, x', mdX) = optArg t0 x
    (t2, xs', mdXS) = optArgs t1 xs
   in
    (t2, RecCons x' xs', mdX <> mdXS)

optArg :: Int -> Arg Stamp u -> (Int, Arg Stamp u, Metadata)
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

foldNum1 ::
  (Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
foldNum1 f k = \case
  Literal (ValueNumber a) -> Literal (ValueNumber (f a))
  x -> k x

foldNum2 ::
  (Double -> Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
foldNum2 f k x y = case (x, y) of
  (Literal (ValueNumber a), Literal (ValueNumber b)) -> Literal (ValueNumber (f a b))
  _ -> k x y

foldConcat :: Expr Stamp 'String -> Expr Stamp 'String -> Expr Stamp 'String
foldConcat x y = case (x, y) of
  (Literal (ValueString a), Literal (ValueString b)) -> Literal (ValueString (a <> b))
  _ -> Concat x y

foldAnd :: Expr Stamp 'Bool -> Expr Stamp 'Bool -> Expr Stamp 'Bool
foldAnd x y = case (x, y) of
  (Literal (ValueBool False), _) -> Literal (ValueBool False)
  (Literal (ValueBool True), y') -> y'
  (_, Literal (ValueBool True)) -> x
  (x', Literal (ValueBool False)) | isPureExpr x' -> Literal (ValueBool False)
  _ -> Std (Kernel (KAnd x y))

foldOr :: Expr Stamp 'Bool -> Expr Stamp 'Bool -> Expr Stamp 'Bool
foldOr x y = case (x, y) of
  (Literal (ValueBool True), _) -> Literal (ValueBool True)
  (Literal (ValueBool False), y') -> y'
  (_, Literal (ValueBool False)) -> x
  (x', Literal (ValueBool True)) | isPureExpr x' -> Literal (ValueBool True)
  _ -> Std (Kernel (KOr x y))

foldCmp ::
  (Value u -> Value u -> Bool)
  -> (Value u -> Bool)
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldCmp cmp ok k x y = case (x, y) of
  (Literal a, Literal b) | ok a && ok b -> Literal (ValueBool (cmp a b))
  _ -> k x y

foldEq :: Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool
foldEq = foldFrozenEq valueEq structuralEq

foldNEq :: Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool
foldNEq = foldFrozenEq (\a b -> not (valueEq a b)) structuralNEq

foldFrozenEq ::
  (forall a. Value a -> Value a -> Bool)
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldFrozenEq cmp k x y = case (x, y) of
  (Literal a, Literal b)
    | eqFoldableValue a && eqFoldableValue b ->
        Literal (ValueBool (cmp a b))
  (FrozenLit as, FrozenLit bs)
    | Just as' <- peelFrozen as
    , Just bs' <- peelFrozen bs ->
        Literal (ValueBool (cmp (ValueFrozen as') (ValueFrozen bs')))
  _ -> k x y

peelFrozen :: [FieldLit Stamp r] -> Maybe [FieldLit Value r]
peelFrozen = traverse $ \case
  FieldLit @k e -> case e of
    Literal v -> Just (FieldLit @k (Literal v))
    _ -> Nothing
  FieldLitExtra @k e -> case e of
    Literal v -> Just (FieldLitExtra @k (Literal v))
    _ -> Nothing
  FieldLitEffect {} -> Nothing
  FieldLitExtraEffect {} -> Nothing

foldOrd ::
  Ordering
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldOrd ord = foldCmp (\a b -> valueCompare a b == ord) isOrderableValue

foldOrdNeq ::
  Ordering
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldOrdNeq ord = foldCmp (\a b -> valueCompare a b /= ord) isOrderableValue

foldShow :: Expr Stamp u -> Expr Stamp 'String
foldShow x = case x of
  Literal (ValueFunction _) -> Show x
  Literal v -> Literal (ValueString (jsShow v))
  _ -> Show x

foldTypeOf :: Expr Stamp u -> Expr Stamp 'String
foldTypeOf x = case x of
  Literal v -> Literal (ValueString (typeOfValue v))
  _ -> TypeOf x

foldIndex :: Expr Stamp ('Array u) -> Expr Stamp 'Number -> Expr Stamp u
foldIndex arr idx = case (arr, idx) of
  (Index {}, _) -> Index arr idx
  (Literal (ValueArray vs), Literal (ValueNumber d))
    | isFiniteDouble d
    , let
        i = truncate d :: Int
    , i >= 0 && i < length vs ->
        Literal (vs !! i)
  _ -> Index arr idx

foldFixedUnary ::
  FixedOp Number 'Unit 'Unit Number -> Expr Stamp 'Number -> Expr Stamp 'Number
foldFixedUnary n x = case x of
  Literal (ValueNumber a)
    | Just r <- Prim.exactMathUnary n a -> Literal (ValueNumber r)
  _ -> expr1 n x

foldFixedBinary ::
  FixedOp 'Number 'Number 'Unit 'Number
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
foldFixedBinary n x y = case (x, y) of
  (Literal (ValueNumber a), Literal (ValueNumber b))
    | Just r <- Prim.exactMathBinary n a b -> Literal (ValueNumber r)
  _ -> expr2 n x y

foldArrLen :: Expr Stamp ('Array u) -> Expr Stamp 'Number
foldArrLen x = case x of
  Literal (ValueArray vs) ->
    Literal (ValueNumber (fromIntegral (Prelude.length vs)))
  _ -> expr1 FixArrLen x

foldToBigInt :: Expr Stamp 'Number -> Expr Stamp 'BigInt
foldToBigInt x = case x of
  Literal (ValueNumber d)
    | isFiniteDouble d
    , let
        n = truncate d
    , d == fromInteger n ->
        Literal (ValueBigInt n)
  _ -> expr1 FixToBigInt x

foldFromBigInt :: Expr Stamp 'BigInt -> Expr Stamp 'Number
foldFromBigInt x = case x of
  Literal (ValueBigInt n) -> Literal (ValueNumber (fromInteger n))
  _ -> expr1 FixFromBigInt x

foldParseBigInt :: Expr Stamp 'String -> Expr Stamp 'BigInt
foldParseBigInt x = case x of
  Literal (ValueString s)
    | Just n <- parseBigIntString (T.unpack s) ->
        Literal (ValueBigInt n)
  _ -> expr1 FixParseBigInt x

foldBig ::
  BigBinOp
  -> Expr Stamp 'BigInt
  -> Expr Stamp 'BigInt
  -> Expr Stamp 'BigInt
foldBig op x y = case (x, y) of
  (Literal (ValueBigInt a), Literal (ValueBigInt b))
    | Just r <- tryEvalBigBin op a b ->
        Literal (ValueBigInt r)
  _ -> Std (Kernel (KBig op x y))

foldBigNeg :: Expr Stamp 'BigInt -> Expr Stamp 'BigInt
foldBigNeg x = case x of
  Literal (ValueBigInt n) -> Literal (ValueBigInt (negate n))
  _ -> Std (Kernel (KBigNeg x))

optFixed ::
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
          md = Metadata 1 True (isCheap res) <> mdX
         in
          (t1, res, md)
  (n, ArgsB x y)
    | Just (MathBinary n') <- matchMathBinary n ->
        let
          (t1, x', mdX) = optExpr t0 x
          (t2, y', mdY) = optExpr t1 y
          res = foldFixedBinary n' x' y'
          md = Metadata 1 True (isCheap res) <> mdX <> mdY
         in
          (t2, res, md)
  (FixArrLen, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldArrLen x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  (FixToBigInt, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldToBigInt x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  (FixFromBigInt, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldFromBigInt x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  (FixParseBigInt, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldParseBigInt x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  (n, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = expr1 n x'
      md = Metadata 1 (isPureFixed n) (isCheap res) <> mdX
     in
      (t1, res, md)
  (n, ArgsB x y) ->
    let
      (t1, x', mdX) = optExpr t0 x
      (t2, y', mdY) = optExpr t1 y
      res = expr2 n x' y'
      md = Metadata 1 (isPureFixed n) (isCheap res) <> mdX <> mdY
     in
      (t2, res, md)
  (n, ArgsT x y z) ->
    let
      (t1, x', mdX) = optExpr t0 x
      (t2, y', mdY) = optExpr t1 y
      (t3, z', mdZ) = optExpr t2 z
      res = expr3 n x' y' z'
      md = Metadata 1 True (isCheap res) <> mdX <> mdY <> mdZ
     in
      (t3, res, md)

optLet ::
  Int
  -> Expr Stamp u
  -> (Stamp u -> Expr Stamp v)
  -> (Int, Expr Stamp v, Metadata)
optLet t0 x f =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, tag, body, mdBody) = optUnder t1 f
   in
    elimLetFrom t2 x' mdX f tag body mdBody

-- Count uses on the already-optimized body. Large tails keep that
-- body (rename-only reopen). Small @f@ may still be applied once more
-- so nested lets / optionCase peel fold.
data ElimOps src body = ElimOps
  { elimCount :: Int -> body -> Metadata -> Int
  , elimPure :: Metadata -> Bool
  , elimCheap :: Metadata -> Bool
  , elimSize :: Metadata -> Int
  , elimRebuild :: body -> body
  , elimSplice :: Int -> (Int, body, Metadata)
  , elimDropUnused :: Metadata -> Bool
  , elimOccurs :: Int -> body -> Bool
  }

elimFrom ::
  ElimOps src body
  -> Int
  -> Metadata
  -> Int
  -> body
  -> Metadata
  -> (Int, body, Metadata)
elimFrom ops t mdX tag body mdBody =
  let
    uses = elimCount ops tag body mdBody
    kept = elimRebuild ops body
    inlined
      | elimSize ops mdBody > optSmall = (t, kept, mdBody)
      | otherwise = elimSplice ops t
   in
    case uses of
      0
        | elimPure ops mdX
        , elimDropUnused ops mdX
        , not (elimOccurs ops tag body) ->
            (t, body, mdBody)
      0 -> (t, kept, mdBody)
      1 -> inlined
      _ | elimCheap ops mdX -> inlined
      _ -> (t, kept, mdBody)

elimLetFrom ::
  Int
  -> Expr Stamp u
  -> Metadata
  -> (Stamp u -> Expr Stamp v)
  -> Int
  -> Expr Stamp v
  -> Metadata
  -> (Int, Expr Stamp v, Metadata)
elimLetFrom t x mdX f tag body mdBody =
  elimFrom
    ElimOps
      { elimCount = elimExprUses
      , elimPure = mdIsPure
      , elimCheap = mdIsCheap
      , elimSize = \_ -> nodeCountExpr body
      , elimRebuild = Let x . rebindExpr tag
      , elimSplice = \t' -> optExpr t' (inlineExpr f x)
      , elimDropUnused = const True
      , elimOccurs = occursVarInExpr
      }
    t
    mdX
    tag
    body
    mdBody

optBind ::
  Int
  -> Effect Stamp u
  -> (Stamp u -> Effect Stamp v)
  -> (Int, Effect Stamp v, Metadata)
optBind t0 x f =
  let
    (t1, x', mdX) = optEffect t0 x
    (t2, tag, body, mdBody) = optUnderE t1 f
   in
    elimBindFrom t2 x' mdX f tag body mdBody

elimBindFrom ::
  Int
  -> Effect Stamp u
  -> Metadata
  -> (Stamp u -> Effect Stamp v)
  -> Int
  -> Effect Stamp v
  -> Metadata
  -> (Int, Effect Stamp v, Metadata)
elimBindFrom t x mdX f tag body mdBody =
  elimFrom
    ElimOps
      { elimCount = elimEffUses
      , elimPure = const (isPureEffect x)
      , elimCheap = mdIsCheap
      , elimSize = \_ -> nodeCountEff body
      , elimRebuild = Bind x . rebindEff tag
      , elimSplice = \t' -> optEffect t' (inlineEff f x)
      , elimDropUnused = \_ -> not (isAliasBind x)
      , elimOccurs = occursVarInEff
      }
    t
    mdX
    tag
    body
    mdBody

optBin ::
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
    (t2, res, Metadata 1 True (isCheap res) <> mdX <> mdY)

optUnNum ::
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
    (t1, res, Metadata 1 True (isCheap res) <> mdX)

optUnderFn ::
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
keepFnCont tags expr' _body = rebindFn tags expr'

optExpr :: Int -> Expr Stamp u -> (Int, Expr Stamp u, Metadata)
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
        fn@(Lambda (Just _) _) ->
          (t2, Apply fn x', Metadata 1 True False <> mdF <> mdX)
        Lambda Nothing g -> optLet t2 x' g
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
            elimLetFrom t2 x mdO s tag body mdBody
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
            elimLetFrom t2 x mdO e tag body mdBody
        Just (Right x) ->
          let
            (t2, tag, body, mdBody) = optUnder t1 s
           in
            elimLetFrom t2 x mdO s tag body mdBody
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
      md = Metadata 1 True (isCheap res) <> mdA <> mdI
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
      (t1, UnsafeNullable x', Metadata 1 True (isCheap x') <> mdX)
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

optStd :: Int -> Std Stamp u -> (Int, Expr Stamp u, Metadata)
optStd t0 s = case s of
  Fixed op args -> optFixed t0 op args
  Method m -> optMethod t0 m
  Kernel k -> optKernel t0 k

optKernel :: Int -> Kernel Stamp u -> (Int, Expr Stamp u, Metadata)
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
      md = Metadata 1 True (isCheap res) <> mdX <> mdY
     in
      (t2, res, md)
  KBigNeg x ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldBigNeg x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  KConcat x y ->
    let
      (t1, x', mdX) = optExpr t0 x
      (t2, y', mdY) = optExpr t1 y
      res = foldConcat x' y'
      md = Metadata 1 True (isCheap res) <> mdX <> mdY
     in
      (t2, res, md)
  KShow x ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldShow x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  KTypeOf x ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldTypeOf x'
      md = Metadata 1 True (isCheap res) <> mdX
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
            md = Metadata 1 True (isCheap res) <> mdX <> mdY
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
            md = Metadata 1 True (isCheap res) <> mdX <> mdY
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

optMethod :: Int -> Method Stamp u -> (Int, Expr Stamp u, Metadata)
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

optEffect :: Int -> Effect Stamp u -> (Int, Effect Stamp u, Metadata)
optEffect t0 eff = case eff of
  Lift x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      case x' of
        Var (EmbedEff e) -> optEffect t1 e
        _ -> (t1, Lift x', Metadata 1 True (isCheap x') <> mdX)
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
            elimBindFrom t2 (Lift x) mdO s tag body mdBody
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
            elimBindFrom t2 (Lift x) mdO e tag body mdBody
        Just (Right x) ->
          let
            (t2, tag, body, mdBody) = optUnderE t1 s
           in
            elimBindFrom t2 (Lift x) mdO s tag body mdBody
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
  forall r. Int -> [FieldLit Stamp r] -> (Int, [FieldLit Stamp r], Metadata)
mapAccumField t0 fs =
  let
    (t1, res) = mapAccumL step t0 fs
   in
    (t1, map fst res, mconcat (map snd res))
 where
  step :: Int -> FieldLit Stamp r -> (Int, (FieldLit Stamp r, Metadata))
  step t = \case
    FieldLit @k e ->
      let (t', e', md) = optExpr t e in (t', (FieldLit @k e', md))
    FieldLitEffect @k e ->
      let (t', e', md) = optEffect t e in (t', (FieldLitEffect @k e', md))
    FieldLitExtra @k e ->
      let (t', e', md) = optExpr t e in (t', (FieldLitExtra @k e', md))
    FieldLitExtraEffect @k e ->
      let (t', e', md) = optEffect t e in (t', (FieldLitExtraEffect @k e', md))

mapAccumEffs :: Int -> [Effect Stamp u] -> (Int, [Effect Stamp u], Metadata)
mapAccumEffs t0 es =
  let
    (t1, res) = mapAccumL step t0 es
   in
    (t1, map fst res, mconcat (map snd res))
 where
  step t e = let (t', e', md) = optEffect t e in (t', (e', md))

mapAccumArms ::
  Int -> [(Text, Effect Stamp u)] -> (Int, [(Text, Effect Stamp u)], Metadata)
mapAccumArms t0 arms =
  let
    (t1, res) = mapAccumL step t0 arms
   in
    (t1, map fst res, mconcat (map snd res))
 where
  step t (k, e) = let (t', e', md) = optEffect t e in (t', ((k, e'), md))

-- | Sequencing without a binder ('ThenE' / discarded bind).
