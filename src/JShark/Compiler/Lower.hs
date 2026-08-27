{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds -Wno-pattern-namespace-specifier -Wno-unused-imports -Wno-missing-signatures #-}

-- | Lower PHOAS 'Expr'/'Effect' to first-order IR.
module JShark.Compiler.Lower
  ( lowerArg
  , lowerArgAt
  , lowerExpr
  , lowerExprAt
  , lowerEffect
  , lowerEffectAt
  , lowerOptExprAt
  , lowerOptEffectAt
  , lowerEffectClosed
  , optEffectClosed
  , lowerOptEffectIr
  , reifyExpr
  , reifyEffect
  , reifyArg
  , irFnTags
  , irEffectFromClosed
  , irExprFromClosed
  , allocFnTags
  , evalFnBody
  , rebindFn
  , fnDepthStamp
  )
where

import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, type (:~:) (Refl))
import GHC.TypeLits (KnownSymbol, sameSymbol, symbolVal)
import JShark.Api.Rec
import JShark.Api.Types
import JShark.Compiler.Binder
  ( Stamp (..)
  , nestedDummy
  , pattern Name
  )
import JShark.Compiler.Flatten
  ( flattenEff
  , flattenExpr
  , rebindEff
  , rebindExpr
  , rebindExpr2
  , renameEff
  , renameExpr
  )
import qualified JShark.Compiler.Ir as Ir
import JShark.Compiler.Metadata (Metadata (..), optStep)
import Unsafe.Coerce (unsafeCoerce)

lowerArg :: Arg Stamp u -> Ir.IrArg u
lowerArg = \case
  ArgExpr e -> Ir.IrArgExpr (lowerExpr e)
  ArgEffect e -> Ir.IrArgEffect (lowerEffect e)

lowerArgAt :: Int -> Arg Stamp u -> (Int, Ir.IrArg u)
lowerArgAt !t0 a = case a of
  ArgExpr e ->
    let
      (t1, e') = lowerExprAt t0 e
     in
      (t1, Ir.IrArgExpr e')
  ArgEffect e ->
    let
      (t1, e') = lowerEffectAt t0 e
     in
      (t1, Ir.IrArgEffect e')

lowerRecArgsAt ::
  Int -> Rec (Arg Stamp) us -> (Int, Rec (Ir.IrArg) us)
lowerRecArgsAt !t0 args = case args of
  RecNil -> (t0, RecNil)
  RecCons x xs ->
    let
      (t1, x') = lowerArgAt t0 x
      (t2, xs') = lowerRecArgsAt t1 xs
     in
      (t2, RecCons x' xs')

lowerArgsAt ::
  Int -> Rec (Arg Stamp) us -> (Int, Rec (Ir.IrArg) us)
lowerArgsAt = lowerRecArgsAt

lowerFixedArgsAt ::
  Int -> FixedArgs Stamp a b c -> (Int, Ir.IrFixedArgs a b c)
lowerFixedArgsAt !t0 a = case a of
  ArgsU x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrArgsU x')
  ArgsB x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.IrArgsB x' y')
  ArgsT x y z ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
      (t3, z') = lowerExprAt t2 z
     in
      (t3, Ir.IrArgsT x' y' z')

lowerKernelKAt :: Int -> Kernel Stamp u -> (Int, Ir.IrKernel u)
lowerKernelKAt !t0 k = case k of
  KPlus x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KPlus x' y')
  KTimes x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KTimes x' y')
  KMinus x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KMinus x' y')
  KNegate x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.KNegate x')
  KFracDiv x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KFracDiv x' y')
  KRem x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KRem x' y')
  KBitAnd x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KBitAnd x' y')
  KBitOr x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KBitOr x' y')
  KBitXor x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KBitXor x' y')
  KShl x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KShl x' y')
  KShr x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KShr x' y')
  KUShr x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KUShr x' y')
  KBig op x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KBig op x' y')
  KBigNeg x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.KBigNeg x')
  KConcat x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KConcat x' y')
  KShow x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.KShow x')
  KTypeOf x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.KTypeOf x')
  KAnd x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KAnd x' y')
  KOr x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KOr x' y')
  KEq s x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KEq s x' y')
  KNEq s x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KNEq s x' y')
  KGTh x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KGTh x' y')
  KLTh x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KLTh x' y')
  KGTEq x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KGTEq x' y')
  KLTEq x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KLTEq x' y')

reifyFixedArgs ::
  Ir.IrFixedArgs a b c -> FixedArgs Stamp a b c
reifyFixedArgs = \case
  Ir.IrArgsU x -> ArgsU (reifyExpr x)
  Ir.IrArgsB x y -> ArgsB (reifyExpr x) (reifyExpr y)
  Ir.IrArgsT x y z -> ArgsT (reifyExpr x) (reifyExpr y) (reifyExpr z)

lowerFixedArgs ::
  FixedArgs Stamp a b c -> Ir.IrFixedArgs a b c
lowerFixedArgs args = snd (lowerFixedArgsAt (-2) args)

lowerKernelK :: Kernel Stamp u -> Ir.IrKernel u
lowerKernelK k = snd (lowerKernelKAt (-2) k)

reifyKernelK :: Ir.IrKernel u -> Kernel Stamp u
reifyKernelK = \case
  Ir.KPlus x y -> KPlus (reifyExpr x) (reifyExpr y)
  Ir.KTimes x y -> KTimes (reifyExpr x) (reifyExpr y)
  Ir.KMinus x y -> KMinus (reifyExpr x) (reifyExpr y)
  Ir.KNegate x -> KNegate (reifyExpr x)
  Ir.KFracDiv x y -> KFracDiv (reifyExpr x) (reifyExpr y)
  Ir.KRem x y -> KRem (reifyExpr x) (reifyExpr y)
  Ir.KBitAnd x y -> KBitAnd (reifyExpr x) (reifyExpr y)
  Ir.KBitOr x y -> KBitOr (reifyExpr x) (reifyExpr y)
  Ir.KBitXor x y -> KBitXor (reifyExpr x) (reifyExpr y)
  Ir.KShl x y -> KShl (reifyExpr x) (reifyExpr y)
  Ir.KShr x y -> KShr (reifyExpr x) (reifyExpr y)
  Ir.KUShr x y -> KUShr (reifyExpr x) (reifyExpr y)
  Ir.KBig op x y -> KBig op (reifyExpr x) (reifyExpr y)
  Ir.KBigNeg x -> KBigNeg (reifyExpr x)
  Ir.KConcat x y -> KConcat (reifyExpr x) (reifyExpr y)
  Ir.KShow x -> KShow (reifyExpr x)
  Ir.KTypeOf x -> KTypeOf (reifyExpr x)
  Ir.KAnd x y -> KAnd (reifyExpr x) (reifyExpr y)
  Ir.KOr x y -> KOr (reifyExpr x) (reifyExpr y)
  Ir.KEq s x y -> KEq s (reifyExpr x) (reifyExpr y)
  Ir.KNEq s x y -> KNEq s (reifyExpr x) (reifyExpr y)
  Ir.KGTh x y -> KGTh (reifyExpr x) (reifyExpr y)
  Ir.KLTh x y -> KLTh (reifyExpr x) (reifyExpr y)
  Ir.KGTEq x y -> KGTEq (reifyExpr x) (reifyExpr y)
  Ir.KLTEq x y -> KLTEq (reifyExpr x) (reifyExpr y)

lowerStdMethodAt :: Int -> Method Stamp u -> (Int, Ir.IrMethod u)
lowerStdMethodAt !t0 m = case m of
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

reifyStdMethod :: Ir.IrMethod u -> Method Stamp u
reifyStdMethod = \case
  Ir.IrMethMap arr tag body ->
    MethMap (reifyExpr arr) (\s -> rebindExpr tag (reifyExpr body) s)
  Ir.IrMethFilter arr tag body ->
    MethFilter (reifyExpr arr) (\s -> rebindExpr tag (reifyExpr body) s)
  Ir.IrMethReduce arr z tagA tagB body ->
    MethReduce
      (reifyExpr arr)
      (reifyExpr z)
      (\a b -> rebindExpr2 tagA tagB (reifyExpr body) a b)
  Ir.IrMethReduceRight arr z tagA tagB body ->
    MethReduceRight
      (reifyExpr arr)
      (reifyExpr z)
      (\a b -> rebindExpr2 tagA tagB (reifyExpr body) a b)
  Ir.IrMethToSorted arr tagA tagB body ->
    MethToSorted
      (reifyExpr arr)
      (\a b -> rebindExpr2 tagA tagB (reifyExpr body) a b)
  Ir.IrMethFrom n tag body ->
    MethFrom (reifyExpr n) (\s -> rebindExpr tag (reifyExpr body) s)

lowerFieldLitAt :: Int -> FieldLit Stamp r -> (Int, Ir.IrFieldLit r)
lowerFieldLitAt !t0 fl = case fl of
  FieldLit @k e ->
    let
      (t1, e') = lowerExprAt t0 e
     in
      (t1, Ir.IrFieldLit @k e')
  FieldLitEffect @k e ->
    let
      (t1, e') = lowerEffectAt t0 e
     in
      (t1, Ir.IrFieldLitEffect @k e')
  FieldLitExtra @k e ->
    let
      (t1, e') = lowerExprAt t0 e
     in
      (t1, Ir.IrFieldLitExtra @k e')
  FieldLitExtraEffect @k e ->
    let
      (t1, e') = lowerEffectAt t0 e
     in
      (t1, Ir.IrFieldLitExtraEffect @k e')

lowerFieldLitsAt ::
  Int -> [FieldLit Stamp r] -> (Int, [Ir.IrFieldLit r])
lowerFieldLitsAt !t0 fs = goFieldLits t0 fs []
 where
  goFieldLits !t [] acc = (t, reverse acc)
  goFieldLits !t (fl : rest) acc =
    let
      (t1, fl') = lowerFieldLitAt t fl
     in
      goFieldLits t1 rest (fl' : acc)

lowerEffectsAt :: Int -> [Effect Stamp u] -> (Int, [Ir.IrEffect u])
lowerEffectsAt !t0 es = goEffects t0 es []
 where
  goEffects !t [] acc = (t, reverse acc)
  goEffects !t (e : rest) acc =
    let
      (t1, e') = lowerEffectAt t e
     in
      goEffects t1 rest (e' : acc)

lowerEffectArmsAt ::
  Int -> [(Text, Effect Stamp u)] -> (Int, [(Text, Ir.IrEffect u)])
lowerEffectArmsAt !t0 arms = goArms t0 arms []
 where
  goArms !t [] acc = (t, reverse acc)
  goArms !t ((k, e) : rest) acc =
    let
      (t1, e') = lowerEffectAt t e
     in
      goArms t1 rest ((k, e') : acc)

reifyFieldLitExtra ::
  forall u k r. (KnownSymbol k, Typeable u) => Ir.IrExpr u -> FieldLit Stamp r
reifyFieldLitExtra e = FieldLitExtra @k (reifyExpr e)

reifyFieldLitExtraEffect ::
  forall u k r. (KnownSymbol k, Typeable u) => Ir.IrEffect u -> FieldLit Stamp r
reifyFieldLitExtraEffect e = FieldLitExtraEffect @k (reifyEffect e)

reifyFieldLit :: forall r. Ir.IrFieldLit r -> FieldLit Stamp r
reifyFieldLit fl =
  unsafeCoerce $
    case fl of
      Ir.IrFieldLit @k e ->
        FieldLit @k (reifyExpr (unsafeCoerce e))
      Ir.IrFieldLitEffect @k e ->
        FieldLitEffect @k (reifyEffect (unsafeCoerce e))
      Ir.IrFieldLitExtra @k (e :: Ir.IrExpr u) ->
        reifyFieldLitExtra @u @k @r e
      Ir.IrFieldLitExtraEffect @k (e :: Ir.IrEffect u) ->
        reifyFieldLitExtraEffect @u @k @r e

irFnTags :: Ir.IrFnBody us r -> [Int]
irFnTags = \case
  Ir.IrJfNil _ -> []
  Ir.IrJfCons t k -> t : irFnTags k

irFnBodyExpr :: Ir.IrFnBody us r -> Ir.IrExpr r
irFnBodyExpr = \case
  Ir.IrJfNil e -> e
  Ir.IrJfCons _ k -> irFnBodyExpr k

fnDepthStamp :: FnBody Stamp us r -> Int
fnDepthStamp = \case
  JfNil _ -> 0
  JfCons k -> 1 + fnDepthStamp (k (Stamp minBound))

allocFnTags :: Int -> FnBody Stamp us r -> ([Int], Int)
allocFnTags t0 body =
  let
    n = fnDepthStamp body
    tags = take n [t0, t0 - optStep ..]
    tEnd = t0 - n * optStep
   in
    (tags, tEnd)

evalFnBody :: forall us r. FnBody Stamp us r -> [Int] -> Expr Stamp r
evalFnBody body tags =
  unsafeCoerce (evalAny (unsafeCoerce body) tags :: Expr Stamp r)
 where
  evalAny (JfNil e) [] = e
  evalAny (JfCons k) (t : ts) = evalAny (unsafeCoerce (k (Name t))) ts
  evalAny _ _ = error "JShark.evalFnBody: arity mismatch"

rebindFn :: [Int] -> Expr Stamp v -> FnBody Stamp us v
rebindFn tags expr = unsafeCoerce (rebindGo tags expr)
 where
  rebindGo [] e = JfNil e
  rebindGo (t : ts) e = unsafeCoerce (JfCons $ \s -> rebindGo ts (rebindExpr t e s))

lowerFnBody :: FnBody Stamp us r -> Ir.IrFnBody us r
lowerFnBody body = snd (lowerFnBodyAt (-2) body)

lowerFnBodyAt :: Int -> FnBody Stamp us r -> (Int, Ir.IrFnBody us r)
lowerFnBodyAt !t0 body =
  let
    (tags, tEnd) = allocFnTags t0 body
   in
    (tEnd, lowerFnBodyTags tags body)

lowerFnBodyTags :: [Int] -> FnBody Stamp us r -> Ir.IrFnBody us r
lowerFnBodyTags tags b = case b of
  JfNil e -> Ir.IrJfNil (lowerExpr e)
  JfCons k ->
    case tags of
      t : ts -> Ir.IrJfCons t (lowerFnBodyTags ts (k (Name t)))
      _ -> error "JShark.lowerFnBodyTags: arity mismatch"

reifyFnBody :: Ir.IrFnBody us r -> FnBody Stamp us r
reifyFnBody ir =
  rebindFn (irFnTags ir) (reifyExpr (irFnBodyExpr ir))

lowerExpr :: Expr Stamp u -> Ir.IrExpr u
lowerExpr e =
  let
    (!_, !ir) = lowerExprAt (-2) e
   in
    ir

lowerExprAt :: Int -> Expr Stamp u -> (Int, Ir.IrExpr u)
lowerExprAt !t0 expr = case expr of
  Literal v -> (t0, Ir.IrLiteral v)
  Var (Stamp i) -> (t0, Ir.IrVar i)
  Var (Embed e) ->
    let
      (t1, e') = lowerExprAt t0 (flattenExpr e)
     in
      (t1, e')
  Var (EmbedEff e) ->
    let
      (t1, e') = lowerEffectAt t0 (flattenEff e)
     in
      (t1, Ir.IrEmbedEff e')
  Let x g ->
    let
      tag = t0
      tUnder = t0 - optStep
      (_, x') = lowerExprAt tUnder x
      (t2, body') = lowerExprAt tUnder (g (Name tag))
     in
      (t2, Ir.IrLet tag x' body')
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
      (t1, Ir.IrFixed op args')
  Std (Kernel k) ->
    let
      (t1, k') = lowerKernelKAt t0 k
     in
      (t1, Ir.IrKernelK k')
  Std (Method m) ->
    let
      (t1, m') = lowerStdMethodAt t0 m
     in
      (t1, Ir.IrMethod m')
  FnLit body ->
    let
      (t1, body') = lowerFnBodyAt t0 body
     in
      (t1, Ir.IrFnLit body')
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
      (t1, Ir.IrGetField @k o')
  Hvm2Kernel name _ ->
    (t0, Ir.IrHvm2Ref name)

reifyExpr :: Ir.IrExpr u -> Expr Stamp u
reifyExpr = \case
  Ir.IrLiteral v -> Literal v
  Ir.IrVar i -> Var (Name i)
  Ir.IrEmbedEff e -> Var (EmbedEff (reifyEffect e))
  Ir.IrLet tag x body ->
    Let (reifyExpr x) (\s -> rebindExpr tag (reifyExpr body) s)
  Ir.IrLetRec tag r b ->
    LetRec
      (\s -> rebindExpr tag (reifyExpr r) s)
      (\s -> rebindExpr tag (reifyExpr b) s)
  Ir.IrLambda tag hoist body ->
    Lambda hoist (\s -> rebindExpr tag (reifyExpr body) s)
  Ir.IrApply f x -> Apply (reifyExpr f) (reifyExpr x)
  Ir.IrIf c t e -> If (reifyExpr c) (reifyExpr t) (reifyExpr e)
  Ir.IrOptionCase o n tag s ->
    OptionCase (reifyExpr o) (reifyExpr n) (\x -> rebindExpr tag (reifyExpr s) x)
  Ir.IrResultOk x -> ResultOk (reifyExpr x)
  Ir.IrResultErr x -> ResultErr (reifyExpr x)
  Ir.IrResultCase o tagE er tagO ok ->
    ResultCase
      (reifyExpr o)
      (\x -> rebindExpr tagE (reifyExpr er) x)
      (\x -> rebindExpr tagO (reifyExpr ok) x)
  Ir.IrIndex arr idx -> Index (reifyExpr arr) (reifyExpr idx)
  Ir.IrU8Index buf idx -> U8Index (reifyExpr buf) (reifyExpr idx)
  Ir.IrError msg -> Error (reifyExpr msg)
  Ir.IrFixed op args -> Std (Fixed op (reifyFixedArgs args))
  Ir.IrKernelK k -> Std (Kernel (reifyKernelK k))
  Ir.IrMethod m -> Std (Method (reifyStdMethod m))
  Ir.IrFnLit body -> FnLit (reifyFnBody body)
  Ir.IrUnsafeNullable x -> UnsafeNullable (reifyExpr x)
  Ir.IrFrozenLit fs -> FrozenLit (map reifyFieldLit fs)
  Ir.IrGetField @k o -> GetField @k (reifyExpr o)
  Ir.IrHvm2Ref name ->
    error ("JShark.reifyExpr: IrHvm2Ref " <> T.unpack name)

lowerEffect :: Effect Stamp u -> Ir.IrEffect u
lowerEffect e =
  let
    (!_, !ir) = lowerEffectAt (-2) e
   in
    ir

lowerEffectAt :: Int -> Effect Stamp u -> (Int, Ir.IrEffect u)
lowerEffectAt !t0 eff = case eff of
  Lift x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrLift x')
  FFI n args ->
    let
      (t1, args') = lowerArgsAt t0 args
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
      (t2, args') = lowerArgsAt t1 args
     in
      (t2, Ir.IrCallMethod x' n args')
  Bind x f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (_, x') = lowerEffectAt tUnder x
      (t2, body') = lowerEffectAt tUnder (f (Name tag))
     in
      (t2, Ir.IrBind tag x' body')
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

lowerOptExprAt :: Int -> Expr Stamp u -> (Int, Ir.IrExpr u, Ir.IrMeta)
lowerOptExprAt !t0 expr =
  let
    (t1, ir) = lowerExprAt t0 expr
    (t2, ir', md) = Ir.optIrExpr t1 ir
   in
    (t2, ir', md)

lowerOptArgAt :: Int -> Arg Stamp u -> (Int, Ir.IrArg u, Ir.IrMeta)
lowerOptArgAt !t0 a = case a of
  ArgExpr e ->
    let
      (t1, e', md) = lowerOptExprAt t0 e
     in
      (t1, Ir.IrArgExpr e', md)
  ArgEffect e ->
    let
      (t1, e', md) = lowerOptEffectAt t0 e
     in
      (t1, Ir.IrArgEffect e', md)

lowerOptArgsAt ::
  Int -> Rec (Arg Stamp) us -> (Int, Rec (Ir.IrArg) us, Ir.IrMeta)
lowerOptArgsAt !t0 args = case args of
  RecNil -> (t0, RecNil, mempty)
  RecCons x xs ->
    let
      (t1, x', mdX) = lowerOptArgAt t0 x
      (t2, xs', mdXs) = lowerOptArgsAt t1 xs
     in
      (t2, RecCons x' xs', Ir.nodeMeta mdX mdXs)

lowerOptFieldLitAt ::
  Int -> FieldLit Stamp r -> (Int, Ir.IrFieldLit r, Ir.IrMeta)
lowerOptFieldLitAt !t0 fl = case fl of
  FieldLit @k e ->
    let
      (t1, e', md) = lowerOptExprAt t0 e
     in
      (t1, Ir.IrFieldLit @k e', md)
  FieldLitEffect @k e ->
    let
      (t1, e', md) = lowerOptEffectAt t0 e
     in
      (t1, Ir.IrFieldLitEffect @k e', md)
  FieldLitExtra @k e ->
    let
      (t1, e', md) = lowerOptExprAt t0 e
     in
      (t1, Ir.IrFieldLitExtra @k e', md)
  FieldLitExtraEffect @k e ->
    let
      (t1, e', md) = lowerOptEffectAt t0 e
     in
      (t1, Ir.IrFieldLitExtraEffect @k e', md)

lowerOptFieldLitsAt ::
  Int -> [FieldLit Stamp r] -> (Int, [Ir.IrFieldLit r], Ir.IrMeta)
lowerOptFieldLitsAt !t0 fs =
  foldr
    ( \fl (!t, acc, !md) ->
        let
          (t', fl', md') = lowerOptFieldLitAt t fl
         in
          (t', fl' : acc, md' <> md)
    )
    (t0, [], mempty)
    fs

lowerOptEffectsAt ::
  Int -> [Effect Stamp u] -> (Int, [Ir.IrEffect u], Ir.IrMeta)
lowerOptEffectsAt !t0 es =
  foldr
    ( \e (!t, acc, !md) ->
        let
          (t', e', md') = lowerOptEffectAt t e
         in
          (t', e' : acc, md' <> md)
    )
    (t0, [], mempty)
    es

lowerOptEffectArmsAt ::
  Int -> [(Text, Effect Stamp u)] -> (Int, [(Text, Ir.IrEffect u)], Ir.IrMeta)
lowerOptEffectArmsAt !t0 arms =
  foldr
    ( \(k, e) (!t, acc, !md) ->
        let
          (t', e', md') = lowerOptEffectAt t e
         in
          (t', (k, e') : acc, md' <> md)
    )
    (t0, [], mempty)
    arms

lowerOptEffectAt :: Int -> Effect Stamp u -> (Int, Ir.IrEffect u, Ir.IrMeta)
lowerOptEffectAt !t0 eff = case eff of
  Lift x ->
    let
      (t1, x', md) = lowerOptExprAt t0 x
     in
      (t1, Ir.IrLift x', md)
  FFI n args ->
    let
      (t1, args', md) = lowerOptArgsAt t0 args
     in
      (t1, Ir.IrFFI n args', Ir.effectMd md)
  UnsafeObject o -> (t0, Ir.IrUnsafeObject o, Ir.IrMeta 1 IM.empty False False)
  UnsafeObjectGet x s ->
    let
      (t1, x', md) = lowerOptEffectAt t0 x
     in
      (t1, Ir.IrUnsafeObjectGet x' s, Ir.effectMd md)
  UnsafeObjectAssign x y ->
    let
      (t1, x', mdX) = lowerOptEffectAt t0 x
      (t2, y', mdY) = lowerOptEffectAt t1 y
     in
      (t2, Ir.IrUnsafeObjectAssign x' y', Ir.effectMd (Ir.nodeMeta mdX mdY))
  CallMethod x n args ->
    let
      (t1, x', mdX) = lowerOptEffectAt t0 x
      (t2, args', mdA) = lowerOptArgsAt t1 args
     in
      (t2, Ir.IrCallMethod x' n args', Ir.effectMd (Ir.nodeMeta mdX mdA))
  Bind x f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (_, x', mdX) = lowerOptEffectAt tUnder x
      (t2, body', mdBody) = lowerOptEffectAt tUnder (f (Name tag))
      (e', md') = Ir.elimIrBind mdX tag x' body' mdBody
     in
      (t2, e', md')
  ThenE x y ->
    let
      (t1, x', mdX) = lowerOptEffectAt t0 x
      (t2, y', mdY) = lowerOptEffectAt t1 y
     in
      (t2, Ir.IrThenE x' y', Ir.nodeMeta mdX mdY)
  BindRec rhs body ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, r', mdR) = lowerOptEffectAt tUnder (rhs (Name tag))
      (t2, b', mdB) = lowerOptEffectAt t1 (body (Name tag))
     in
      (t2, Ir.IrBindRec tag r' b', Ir.bindMeta tag (Ir.nodeMeta mdR mdB))
  LambdaE f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, body', md) = lowerOptEffectAt tUnder (f (Name tag))
     in
      (t1, Ir.IrLambdaE tag body', Ir.bindMeta tag md)
  ApplyE f x ->
    let
      (t1, f', mdF) = lowerOptEffectAt t0 f
      (t2, x', mdX) = lowerOptEffectAt t1 x
     in
      (t2, Ir.IrApplyE f' x', Ir.effectMd (Ir.nodeMeta mdF mdX))
  IfE c t e ->
    let
      (t1, c', mdC) = lowerOptEffectAt t0 c
      (t2, t', mdT) = lowerOptEffectAt t1 t
      (t3, e', mdE) = lowerOptEffectAt t2 e
     in
      (t3, Ir.IrIfE c' t' e', Ir.nodeMeta mdC (Ir.nodeMeta mdT mdE))
  While c b ->
    let
      (t1, c', mdC) = lowerOptEffectAt t0 c
      (t2, b', mdB) = lowerOptEffectAt t1 b
     in
      (t2, Ir.IrWhile c' b', Ir.effectMd (Ir.nodeMeta mdC mdB))
  ForRange s e f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, s', mdS) = lowerOptExprAt tUnder s
      (t2, e', mdE) = lowerOptExprAt t1 e
      (t3, body', mdB) = lowerOptEffectAt t2 (f (Name tag))
     in
      ( t3
      , Ir.IrForRange s' e' tag body'
      , Ir.effectMd (Ir.nodeMeta mdS (Ir.nodeMeta mdE (Ir.bindMeta tag mdB)))
      )
  U8Set b i v ->
    let
      (t1, b', mdB) = lowerOptExprAt t0 b
      (t2, i', mdI) = lowerOptExprAt t1 i
      (t3, v', mdV) = lowerOptExprAt t2 v
     in
      (t3, Ir.IrU8Set b' i' v', Ir.effectMd (Ir.nodeMeta mdB (Ir.nodeMeta mdI mdV)))
  U8Fill b v ->
    let
      (t1, b', mdB) = lowerOptExprAt t0 b
      (t2, v', mdV) = lowerOptExprAt t1 v
     in
      (t2, Ir.IrU8Fill b' v', Ir.effectMd (Ir.nodeMeta mdB mdV))
  OptionCaseE o n s ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, o', mdO) = lowerOptExprAt tUnder o
      (t2, n', mdN) = lowerOptEffectAt t1 n
      (t3, s', mdS) = lowerOptEffectAt t2 (s (Name tag))
     in
      ( t3
      , Ir.IrOptionCaseE o' n' tag s'
      , Ir.nodeMeta mdO (Ir.nodeMeta mdN (Ir.bindMeta tag mdS))
      )
  ResultCaseE o er ok ->
    let
      tagE = t0
      t1 = t0 - optStep
      tagO = t1
      tUnder = t1 - optStep
      (t2, o', mdO) = lowerOptExprAt tUnder o
      (t3, er', mdE) = lowerOptEffectAt t2 (er (Name tagE))
      (t4, ok', mdS) = lowerOptEffectAt t3 (ok (Name tagO))
     in
      ( t4
      , Ir.IrResultCaseE o' tagE er' tagO ok'
      , Ir.nodeMeta mdO (Ir.nodeMeta (Ir.bindMeta tagE mdE) (Ir.bindMeta tagO mdS))
      )
  StringCaseE s arms d ->
    let
      (t1, s', mdS) = lowerOptExprAt t0 s
      (t2, arms', mdA) = lowerOptEffectArmsAt t1 arms
      (t3, d', mdD) = lowerOptEffectAt t2 d
     in
      (t3, Ir.IrStringCaseE s' arms' d', Ir.nodeMeta mdS (Ir.nodeMeta mdA mdD))
  Throw x ->
    let
      (t1, x', md) = lowerOptExprAt t0 x
     in
      (t1, Ir.IrThrow x', Ir.effectMd md)
  Try a k ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, a', mdA) = lowerOptEffectAt tUnder a
      (t2, k', mdK) = lowerOptEffectAt t1 (k (Name tag))
     in
      (t2, Ir.IrTry a' tag k', Ir.nodeMeta mdA (Ir.bindMeta tag mdK))
  ObjectLit fs ->
    let
      (t1, fs', md) = lowerOptFieldLitsAt t0 fs
     in
      (t1, Ir.IrObjectLit fs', md)
  DeleteProp o k ->
    let
      (t1, o', mdO) = lowerOptEffectAt t0 o
      (t2, k', mdK) = lowerOptExprAt t1 k
     in
      (t2, Ir.IrDeleteProp o' k', Ir.effectMd (Ir.nodeMeta mdO mdK))
  ArrayLit es ->
    let
      (t1, es', md) = lowerOptEffectsAt t0 es
     in
      (t1, Ir.IrArrayLit es', md)

{-# NOINLINE lowerOptExprAt #-}

{-# NOINLINE lowerOptEffectAt #-}

reifyEffect :: Ir.IrEffect u -> Effect Stamp u
reifyEffect = \case
  Ir.IrLift x -> Lift (reifyExpr x)
  Ir.IrFFI n args -> FFI n (mapRec reifyArg args)
  Ir.IrUnsafeObject o -> UnsafeObject o
  Ir.IrUnsafeObjectGet x s -> UnsafeObjectGet (reifyEffect x) s
  Ir.IrUnsafeObjectAssign x y -> UnsafeObjectAssign (reifyEffect x) (reifyEffect y)
  Ir.IrCallMethod x n args -> CallMethod (reifyEffect x) n (mapRec reifyArg args)
  Ir.IrBind tag x body ->
    Bind (reifyEffect x) (\s -> rebindEff tag (reifyEffect body) s)
  Ir.IrThenE x y -> ThenE (reifyEffect x) (reifyEffect y)
  Ir.IrBindRec tag r b ->
    BindRec
      (\s -> rebindEff tag (reifyEffect r) s)
      (\s -> rebindEff tag (reifyEffect b) s)
  Ir.IrLambdaE tag body ->
    LambdaE (\s -> rebindEff tag (reifyEffect body) s)
  Ir.IrApplyE f x -> ApplyE (reifyEffect f) (reifyEffect x)
  Ir.IrIfE c t e -> IfE (reifyEffect c) (reifyEffect t) (reifyEffect e)
  Ir.IrWhile c b -> While (reifyEffect c) (reifyEffect b)
  Ir.IrForRange s e tag body ->
    ForRange (reifyExpr s) (reifyExpr e) (\i -> rebindEff tag (reifyEffect body) i)
  Ir.IrU8Set b i v -> U8Set (reifyExpr b) (reifyExpr i) (reifyExpr v)
  Ir.IrU8Fill b v -> U8Fill (reifyExpr b) (reifyExpr v)
  Ir.IrOptionCaseE o n tag s ->
    OptionCaseE
      (reifyExpr o)
      (reifyEffect n)
      (\x -> rebindEff tag (reifyEffect s) x)
  Ir.IrResultCaseE o tagE er tagO ok ->
    ResultCaseE
      (reifyExpr o)
      (\x -> rebindEff tagE (reifyEffect er) x)
      (\x -> rebindEff tagO (reifyEffect ok) x)
  Ir.IrStringCaseE s arms d ->
    StringCaseE (reifyExpr s) (map (fmap reifyEffect) arms) (reifyEffect d)
  Ir.IrThrow x -> Throw (reifyExpr x)
  Ir.IrTry a tag k ->
    Try (reifyEffect a) (\s -> rebindEff tag (reifyEffect k) s)
  Ir.IrObjectLit fs -> ObjectLit (map reifyFieldLit fs)
  Ir.IrDeleteProp o k -> DeleteProp (reifyEffect o) (reifyExpr k)
  Ir.IrArrayLit es -> ArrayLit (map reifyEffect es)

reifyArg :: Ir.IrArg u -> Arg Stamp u
reifyArg = \case
  Ir.IrArgExpr e -> ArgExpr (reifyExpr e)
  Ir.IrArgEffect e -> ArgEffect (reifyEffect e)

lowerEffectClosed :: ClosedEffect u -> Ir.IrEffect u
lowerEffectClosed (e :: ClosedEffect u) =
  let
    (!_, !ir) = lowerEffectAt (-2) (flattenEff e)
   in
    ir
{-# NOINLINE lowerEffectClosed #-}

optEffectClosed :: Ir.IrEffect u -> Ir.IrEffect u
optEffectClosed ir =
  let
    (!_, !irOpt, !_) = Ir.optIrEffect (-2) ir
   in
    irOpt
{-# NOINLINE optEffectClosed #-}

lowerOptEffectIr :: ClosedEffect u -> (Ir.IrEffect u, Int)
lowerOptEffectIr e =
  let
    (!_, !irOpt, !mdOpt) = lowerOptEffectAt (-2) (flattenEff e)
    !nodes = Ir.irMetaSize mdOpt
   in
    Ir.metaIrEffect irOpt `seq` (irOpt, nodes)
{-# NOINLINE lowerOptEffectIr #-}

irEffectFromClosed :: ClosedEffect u -> Ir.IrEffect u
irEffectFromClosed e = fst (lowerOptEffectIr e)
{-# NOINLINE irEffectFromClosed #-}

irExprFromClosed :: ClosedExpr u -> Ir.IrExpr u
irExprFromClosed (e :: ClosedExpr u) =
  let
    (!_, !ir) = lowerExprAt (-2) (flattenExpr (e :: Expr Stamp u))
    (!_, !irOpt, !_) = Ir.optIrExpr (-2) ir
   in
    irOpt
{-# NOINLINE irExprFromClosed #-}
