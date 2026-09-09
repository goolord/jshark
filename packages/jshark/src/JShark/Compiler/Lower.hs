{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier -Wno-missing-signatures #-}

-- | Lower PHOAS 'Expr'/'Effect' to first-order IR. Both entry points now
-- share the single untyped 'Ir.IrNode'.
module JShark.Compiler.Lower
  ( lowerExprAt
  , lowerEffectAt
  , lowerEffectClosed
  , optEffectClosed
  , lowerOptEffectIr
  , lowerOptEffectIrWith
  , lowerOptExprIr
  , irEffectFromClosed
  , irExprFromClosed
  , lowerOptEffectAt
  , closedEffectNodes
  , closedExprNodes
  , optimizedExprSize
  , optimizedEffectSize
  , irOptimizedEffectFromClosed
  , irOptimizedExprFromClosed
  )
where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import JShark.Api.Rec
import JShark.Api.Types
import JShark.Compiler.Binder
  ( Stamp
  , pattern Name
  , pattern Stamp
  )
import JShark.Compiler.Ir (optStep)
import qualified JShark.Compiler.Ir as Ir

fieldKeyText :: forall k. KnownSymbol k => Text
fieldKeyText = T.pack (symbolVal (Proxy @k))

lowerExprAt :: Int -> Expr Stamp u -> (Int, Ir.IrNode)
lowerExprAt !t0 expr = case expr of
  Literal v -> (t0, Ir.IrLiteral v)
  Var (Stamp i) -> (t0, Ir.IrVar i)
  Let hint x g ->
    let
      tag = t0
      tUnder = t0 - optStep
      (_, x') = lowerExprAt tUnder x
      (t2, body') = lowerExprAt tUnder (g (Name tag))
     in
      (t2, Ir.IrLet tag hint x' body')
  LetRec r b ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, r') = lowerExprAt tUnder (r (Name tag))
      (t2, b') = lowerExprAt t1 (b (Name tag))
     in
      (t2, Ir.IrLetRec tag r' b')
  Lambda hoist g ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, body') = lowerExprAt tUnder (g (Name tag))
     in
      (t1, Ir.IrLambda tag hoist body')
  Apply f x ->
    let
      (t1, f') = lowerExprAt t0 f
      (t2, x') = lowerExprAt t1 x
     in
      (t2, Ir.IrApply f' x')
  If c t e ->
    let
      (t1, c') = lowerExprAt t0 c
      (t2, t') = lowerExprAt t1 t
      (t3, e') = lowerExprAt t2 e
     in
      (t3, Ir.IrIf c' t' e')
  OptionCase o n s ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, o') = lowerExprAt tUnder o
      (t2, n') = lowerExprAt t1 n
      (t3, s') = lowerExprAt t2 (s (Name tag))
     in
      (t3, Ir.IrOptionCase o' n' tag s')
  ResultOk x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrResultOk x')
  ResultErr x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrResultErr x')
  ResultCase o er ok ->
    let
      tagE = t0
      t1 = t0 - optStep
      tagO = t1
      tUnder = t1 - optStep
      (t2, o') = lowerExprAt tUnder o
      (t3, er') = lowerExprAt t2 (er (Name tagE))
      (t4, ok') = lowerExprAt t3 (ok (Name tagO))
     in
      (t4, Ir.IrResultCase o' tagE er' tagO ok')
  Index arr idx ->
    let
      (t1, arr') = lowerExprAt t0 arr
      (t2, idx') = lowerExprAt t1 idx
     in
      (t2, Ir.IrIndex arr' idx')
  U8Index buf idx ->
    let
      (t1, buf') = lowerExprAt t0 buf
      (t2, idx') = lowerExprAt t1 idx
     in
      (t2, Ir.IrU8Index buf' idx')
  Error msg ->
    let
      (t1, msg') = lowerExprAt t0 msg
     in
      (t1, Ir.IrError msg')
  Std (Fixed op args) ->
    let
      (t1, args') = lowerFixedArgsAt t0 args
     in
      (t1, Ir.IrFixed (Ir.SomeFixedOp op) args')
  Std (Kernel k) ->
    let
      (t1, k') = lowerKernelAt t0 k
     in
      (t1, k')
  Std (Method m) ->
    let
      (t1, m') = lowerMethodAt t0 m
     in
      (t1, m')
  FnLit body ->
    let
      (t1, tags, names, body') = lowerFnBodyAt t0 body
     in
      (t1, Ir.IrFnLit tags names body')
  UnsafeNullable x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrUnsafeNullable x')
  FrozenLit fs ->
    let
      (t1, fs') = lowerFieldLitsAt t0 fs
     in
      (t1, Ir.IrFrozenLit fs')
  GetField @k o ->
    let
      (t1, o') = lowerExprAt t0 o
     in
      (t1, Ir.IrGetField (fieldKeyText @k) o')
  Hvm2Kernel name _ ->
    (t0, Ir.IrHvm2Ref name)

lowerEffectAt :: Int -> Effect Stamp u -> (Int, Ir.IrNode)
lowerEffectAt !t0 eff = case eff of
  Lift x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrLift x')
  FFI n args ->
    let
      (t1, args') = lowerArgListAt t0 args
     in
      (t1, Ir.IrFFI n args')
  UnsafeObject o -> (t0, Ir.IrUnsafeObject o)
  UnsafeObjectGet x s ->
    let
      (t1, x') = lowerEffectAt t0 x
     in
      (t1, Ir.IrUnsafeObjectGet x' s)
  UnsafeObjectAssign x y ->
    let
      (t1, x') = lowerEffectAt t0 x
      (t2, y') = lowerEffectAt t1 y
     in
      (t2, Ir.IrUnsafeObjectAssign x' y')
  CallMethod x n args ->
    let
      (t1, x') = lowerEffectAt t0 x
      (t2, args') = lowerArgListAt t1 args
     in
      (t2, Ir.IrCallMethod x' n args')
  Bind hint x f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (_, x') = lowerEffectAt tUnder x
      (t2, body') = lowerEffectAt tUnder (f (Name tag))
     in
      (t2, Ir.IrBind tag hint x' body')
  ThenE x y ->
    let
      (t1, x') = lowerEffectAt t0 x
      (t2, y') = lowerEffectAt t1 y
     in
      (t2, Ir.IrThenE x' y')
  BindRec rhs body ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, r') = lowerEffectAt tUnder (rhs (Name tag))
      (t2, b') = lowerEffectAt t1 (body (Name tag))
     in
      (t2, Ir.IrBindRec tag r' b')
  LambdaE f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, body') = lowerEffectAt tUnder (f (Name tag))
     in
      (t1, Ir.IrLambdaE tag body')
  ApplyE f x ->
    let
      (t1, f') = lowerEffectAt t0 f
      (t2, x') = lowerEffectAt t1 x
     in
      (t2, Ir.IrApplyE f' x')
  IfE c t e ->
    let
      (t1, c') = lowerEffectAt t0 c
      (t2, t') = lowerEffectAt t1 t
      (t3, e') = lowerEffectAt t2 e
     in
      (t3, Ir.IrIfE c' t' e')
  While c b ->
    let
      (t1, c') = lowerEffectAt t0 c
      (t2, b') = lowerEffectAt t1 b
     in
      (t2, Ir.IrWhile c' b')
  ForRange s e f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, s') = lowerExprAt tUnder s
      (t2, e') = lowerExprAt t1 e
      (t3, body') = lowerEffectAt t2 (f (Name tag))
     in
      (t3, Ir.IrForRange s' e' tag body')
  U8Set b i v ->
    let
      (t1, b') = lowerExprAt t0 b
      (t2, i') = lowerExprAt t1 i
      (t3, v') = lowerExprAt t2 v
     in
      (t3, Ir.IrU8Set b' i' v')
  U8Fill b v ->
    let
      (t1, b') = lowerExprAt t0 b
      (t2, v') = lowerExprAt t1 v
     in
      (t2, Ir.IrU8Fill b' v')
  OptionCaseE o n s ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, o') = lowerExprAt tUnder o
      (t2, n') = lowerEffectAt t1 n
      (t3, s') = lowerEffectAt t2 (s (Name tag))
     in
      (t3, Ir.IrOptionCaseE o' n' tag s')
  ResultCaseE o er ok ->
    let
      tagE = t0
      t1 = t0 - optStep
      tagO = t1
      tUnder = t1 - optStep
      (t2, o') = lowerExprAt tUnder o
      (t3, er') = lowerEffectAt t2 (er (Name tagE))
      (t4, ok') = lowerEffectAt t3 (ok (Name tagO))
     in
      (t4, Ir.IrResultCaseE o' tagE er' tagO ok')
  StringCaseE s arms d ->
    let
      (t1, s') = lowerExprAt t0 s
      (t2, arms') = lowerEffectArmsAt t1 arms
      (t3, d') = lowerEffectAt t2 d
     in
      (t3, Ir.IrStringCaseE s' arms' d')
  Throw x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrThrow x')
  Try a k ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, a') = lowerEffectAt tUnder a
      (t2, k') = lowerEffectAt t1 (k (Name tag))
     in
      (t2, Ir.IrTry a' tag k')
  ObjectLit fs ->
    let
      (t1, fs') = lowerFieldLitsAt t0 fs
     in
      (t1, Ir.IrObjectLit fs')
  DeleteProp o k ->
    let
      (t1, o') = lowerEffectAt t0 o
      (t2, k') = lowerExprAt t1 k
     in
      (t2, Ir.IrDeleteProp o' k')
  ArrayLit es ->
    let
      (t1, es') = lowerEffectsAt t0 es
     in
      (t1, Ir.IrArrayLit es')

lowerArgListAt :: Int -> Rec (Arg Stamp) us -> (Int, [Ir.IrNode])
lowerArgListAt !t0 args = case args of
  RecNil -> (t0, [])
  RecCons (ArgExpr e) rest ->
    let
      (t1, x') = lowerExprAt t0 e
      (t2, xs') = lowerArgListAt t1 rest
     in
      (t2, x' : xs')
  RecCons (ArgEffect e) rest ->
    let
      (t1, x') = lowerEffectAt t0 e
      (t2, xs') = lowerArgListAt t1 rest
     in
      (t2, x' : xs')

lowerFixedArgsAt :: Int -> FixedArgs Stamp a b c -> (Int, [Ir.IrNode])
lowerFixedArgsAt !t0 args = case args of
  ArgsU x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, [x'])
  ArgsB x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, [x', y'])
  ArgsT x y z ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
      (t3, z') = lowerExprAt t2 z
     in
      (t3, [x', y', z'])

lowerKernelAt :: Int -> Kernel Stamp u -> (Int, Ir.IrNode)
lowerKernelAt !t0 k = case k of
  KConcat x y -> lower2 Ir.KConcat x y
  KPlus x y -> lower2 Ir.KPlus x y
  KTimes x y -> lower2 Ir.KTimes x y
  KMinus x y -> lower2 Ir.KMinus x y
  KNegate x -> lower1 Ir.KNegate x
  KFracDiv x y -> lower2 Ir.KFracDiv x y
  KRem x y -> lower2 Ir.KRem x y
  KBitAnd x y -> lower2 Ir.KBitAnd x y
  KBitOr x y -> lower2 Ir.KBitOr x y
  KBitXor x y -> lower2 Ir.KBitXor x y
  KShl x y -> lower2 Ir.KShl x y
  KShr x y -> lower2 Ir.KShr x y
  KUShr x y -> lower2 Ir.KUShr x y
  KBig op x y -> lower2 (Ir.KBig op) x y
  KBigNeg x -> lower1 Ir.KBigNeg x
  KAnd x y -> lower2 Ir.KAnd x y
  KOr x y -> lower2 Ir.KOr x y
  KEq s x y -> lower2 (Ir.KEq s) x y
  KNEq s x y -> lower2 (Ir.KNEq s) x y
  KGTh x y -> lower2 Ir.KGTh x y
  KLTh x y -> lower2 Ir.KLTh x y
  KGTEq x y -> lower2 Ir.KGTEq x y
  KLTEq x y -> lower2 Ir.KLTEq x y
  KShow x -> lower1 Ir.KShow x
  KTypeOf x -> lower1 Ir.KTypeOf x
 where
  lower1 ::
    (Ir.IrNode -> Ir.IrNode) -> Expr Stamp a -> (Int, Ir.IrNode)
  lower1 kon x =
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, kon x')
  lower2 ::
    (Ir.IrNode -> Ir.IrNode -> Ir.IrNode)
    -> Expr Stamp a
    -> Expr Stamp b
    -> (Int, Ir.IrNode)
  lower2 kon x y =
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, kon x' y')

lowerMethodAt :: Int -> Method Stamp u -> (Int, Ir.IrNode)
lowerMethodAt !t0 m = case m of
  MethMap arr f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, arr') = lowerExprAt tUnder arr
      (t2, body') = lowerExprAt t1 (f (Name tag))
     in
      (t2, Ir.IrMethMap arr' tag body')
  MethFilter arr f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, arr') = lowerExprAt tUnder arr
      (t2, body') = lowerExprAt t1 (f (Name tag))
     in
      (t2, Ir.IrMethFilter arr' tag body')
  MethReduce arr z f ->
    let
      tagA = t0
      tagB = t0 - optStep
      tUnder = t0 - 2 * optStep
      (t1, arr') = lowerExprAt tUnder arr
      (t2, z') = lowerExprAt t1 z
      (t3, body') =
        lowerExprAt t2 (f (Name tagA) (Name tagB))
     in
      (t3, Ir.IrMethReduce arr' z' tagA tagB body')
  MethReduceRight arr z f ->
    let
      tagA = t0
      tagB = t0 - optStep
      tUnder = t0 - 2 * optStep
      (t1, arr') = lowerExprAt tUnder arr
      (t2, z') = lowerExprAt t1 z
      (t3, body') =
        lowerExprAt t2 (f (Name tagA) (Name tagB))
     in
      (t3, Ir.IrMethReduceRight arr' z' tagA tagB body')
  MethToSorted arr f ->
    let
      tagA = t0
      tagB = t0 - optStep
      tUnder = t0 - 2 * optStep
      (t1, arr') = lowerExprAt tUnder arr
      (t2, body') =
        lowerExprAt t1 (f (Name tagA) (Name tagB))
     in
      (t2, Ir.IrMethToSorted arr' tagA tagB body')
  MethFrom n f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, n') = lowerExprAt tUnder n
      (t2, body') = lowerExprAt t1 (f (Name tag))
     in
      (t2, Ir.IrMethFrom n' tag body')

lowerFieldLitAt :: Int -> FieldLit Stamp r -> (Int, Ir.IrField)
lowerFieldLitAt !t0 fl = case fl of
  FieldLit @k e ->
    let
      (t1, e') = lowerExprAt t0 e
     in
      (t1, Ir.IrField (fieldKeyText @k) e')
  FieldLitEffect @k e ->
    let
      (t1, e') = lowerEffectAt t0 e
     in
      (t1, Ir.IrFieldEff (fieldKeyText @k) e')
  FieldLitExtra @k e ->
    let
      (t1, e') = lowerExprAt t0 e
     in
      (t1, Ir.IrFieldExtra (fieldKeyText @k) e')
  FieldLitExtraEffect @k e ->
    let
      (t1, e') = lowerEffectAt t0 e
     in
      (t1, Ir.IrFieldExtraEff (fieldKeyText @k) e')

lowerFieldLitsAt :: Int -> [FieldLit Stamp r] -> (Int, [Ir.IrField])
lowerFieldLitsAt !t0 fs = goFieldLits t0 fs []
 where
  goFieldLits !t [] acc = (t, reverse acc)
  goFieldLits !t (fl : rest) acc =
    let
      (t1, fl') = lowerFieldLitAt t fl
     in
      goFieldLits t1 rest (fl' : acc)

lowerEffectsAt :: Int -> [Effect Stamp u] -> (Int, [Ir.IrNode])
lowerEffectsAt !t0 es = goEffects t0 es []
 where
  goEffects !t [] acc = (t, reverse acc)
  goEffects !t (e : rest) acc =
    let
      (t1, e') = lowerEffectAt t e
     in
      goEffects t1 rest (e' : acc)

lowerEffectArmsAt ::
  Int -> [(Text, Effect Stamp u)] -> (Int, [(Text, Ir.IrNode)])
lowerEffectArmsAt !t0 arms = goArms t0 arms []
 where
  goArms !t [] acc = (t, reverse acc)
  goArms !t ((k, e) : rest) acc =
    let
      (t1, e') = lowerEffectAt t e
     in
      goArms t1 rest ((k, e') : acc)

fnDepthStamp :: FnBody Stamp us r -> Int
fnDepthStamp = \case
  JfNil _ -> 0
  JfCons _ k -> 1 + fnDepthStamp (k (Stamp minBound))

allocFnTags :: Int -> FnBody Stamp us r -> ([Int], Int)
allocFnTags t0 body =
  let
    n = fnDepthStamp body
    tags = take n [t0, t0 - optStep ..]
    tEnd = t0 - n * optStep
   in
    (tags, tEnd)

lowerFnBodyAt ::
  Int -> FnBody Stamp us r -> (Int, [Int], [Maybe Text], Ir.IrNode)
lowerFnBodyAt !t0 body =
  let
    (tags, tEnd) = allocFnTags t0 body
    (names, body') = lowerFnBodyTags tags body
   in
    (tEnd, tags, names, body')

lowerFnBodyTags :: [Int] -> FnBody Stamp us r -> ([(Maybe Text)], Ir.IrNode)
lowerFnBodyTags _ (JfNil e) = ([], lowerExpr e)
lowerFnBodyTags (t : ts) (JfCons pn k) =
  let
    (restNames, body') = lowerFnBodyTags ts (k (Name t))
   in
    (pn : restNames, body')
lowerFnBodyTags [] (JfCons {}) =
  error "JShark.lowerFnBodyTags: arity mismatch"

lowerExpr :: Expr Stamp u -> Ir.IrNode
lowerExpr e =
  let
    (!_, !ir) = lowerExprAt (-2) e
   in
    ir

lowerEffectClosed :: ClosedEffect u -> Ir.IrNode
lowerEffectClosed (e :: ClosedEffect u) =
  let
    (!_, !ir) = lowerEffectAt (-2) e
   in
    ir
{-# NOINLINE lowerEffectClosed #-}

optEffectClosed :: Ir.IrNode -> Ir.IrNode
optEffectClosed ir =
  let
    ?keepLets = False
   in
    let
      (!_, !irOpt, !_) = Ir.optIr (-2) ir
     in
      irOpt
{-# NOINLINE optEffectClosed #-}

lowerOptEffectIr :: ClosedEffect u -> (Ir.IrNode, Int)
lowerOptEffectIr = lowerOptEffectIrWith False

lowerOptExprIr :: Bool -> ClosedExpr u -> (Ir.IrNode, Int)
lowerOptExprIr keepLets (e :: ClosedExpr u) =
  let
    ?keepLets = keepLets
   in
    let
      (!_, !ir) = lowerExprAt (-2) (e :: Expr Stamp u)
      (!_, !irOpt, !mdOpt) = Ir.optIr (-2) ir
      !nodes = Ir.irSize mdOpt
     in
      Ir.metaIr irOpt `seq` (irOpt, nodes)
{-# NOINLINE lowerOptExprIr #-}

lowerOptEffectAt ::
  (?keepLets :: Bool) => Int -> Effect Stamp u -> (Int, Ir.IrNode, Ir.IrMeta)
lowerOptEffectAt !t0 eff =
  let
    (t1, ir) = lowerEffectAt t0 eff
    (t2, ir', md) = Ir.optIr t1 ir
   in
    (t2, ir', md)
{-# NOINLINE lowerOptEffectAt #-}

lowerOptEffectIrWith :: Bool -> ClosedEffect u -> (Ir.IrNode, Int)
lowerOptEffectIrWith keepLets e =
  let
    ?keepLets = keepLets
   in
    let
      (!_, !irOpt, !mdOpt) = lowerOptEffectAt (-2) e
      !nodes = Ir.irSize mdOpt
     in
      Ir.metaIr irOpt `seq` (irOpt, nodes)
{-# NOINLINE lowerOptEffectIrWith #-}

irEffectFromClosed :: ClosedEffect u -> Ir.IrNode
irEffectFromClosed e = fst (lowerOptEffectIr e)
{-# NOINLINE irEffectFromClosed #-}

irExprFromClosed :: ClosedExpr u -> Ir.IrNode
irExprFromClosed (e :: ClosedExpr u) =
  let
    ?keepLets = False
   in
    let
      (!_, !ir) = lowerExprAt (-2) (e :: Expr Stamp u)
      (!_, !irOpt, !_) = Ir.optIr (-2) ir
     in
      irOpt
{-# NOINLINE irExprFromClosed #-}

-- | Nodes after IR optimize of a closed effect.
closedEffectNodes :: ClosedEffect u -> Int
closedEffectNodes e = snd (lowerOptEffectIr e)
{-# NOINLINE closedEffectNodes #-}

-- | Nodes after IR optimize of a closed expression.
closedExprNodes :: ClosedExpr u -> Int
closedExprNodes e = snd (lowerOptExprIr False e)
{-# NOINLINE closedExprNodes #-}

optimizedExprSize :: ClosedExpr u -> Int
optimizedExprSize = closedExprNodes

optimizedEffectSize :: ClosedEffect u -> Int
optimizedEffectSize = closedEffectNodes

irOptimizedEffectFromClosed :: ClosedEffect u -> Ir.IrNode
irOptimizedEffectFromClosed e = fst (lowerOptEffectIr e)
{-# NOINLINE irOptimizedEffectFromClosed #-}

irOptimizedExprFromClosed :: ClosedExpr u -> Ir.IrNode
irOptimizedExprFromClosed e = fst (lowerOptExprIr False e)
{-# NOINLINE irOptimizedExprFromClosed #-}
