{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

-- | Collect HVM2 kernel references from PHOAS trees.
module JShark.Compiler.Optimize.Hvm2 (collectHvm2Kernels) where

import JShark.Api.Rec
import JShark.Api.Types
import JShark.Compiler.Flatten (PhoasDummy (..))

collectHvm2Kernels :: forall f u. PhoasDummy f => Expr f u -> [Hvm2KernelEntry]
collectHvm2Kernels = collectAny

collectAny :: PhoasDummy f => Expr f v -> [Hvm2KernelEntry]
collectAny = \case
  Hvm2Kernel name k -> [Hvm2KernelEntry name k]
  Literal _ -> []
  Var _ -> []
  Let x g -> collectAny x <> collectAny (g phoasDummy)
  LetRec r b ->
    collectAny (r phoasDummy) <> collectAny (b phoasDummy)
  Lambda _ g -> collectAny (g phoasDummy)
  Apply f x -> collectAny f <> collectAny x
  If c t eF -> collectAny c <> collectAny t <> collectAny eF
  OptionCase o n s ->
    collectAny o <> collectAny n <> collectAny (s phoasDummy)
  ResultOk x -> collectAny x
  ResultErr x -> collectAny x
  ResultCase o er ok ->
    collectAny o
      <> collectAny (er phoasDummy)
      <> collectAny (ok phoasDummy)
  Index x i -> collectAny x <> collectAny i
  U8Index x i -> collectAny x <> collectAny i
  Error x -> collectAny x
  Std s -> collectStdHvm2 s
  FnLit body -> collectFnBodyHvm2 body
  UnsafeNullable x -> collectAny x
  FrozenLit fs -> concatMap collectFieldLitHvm2 fs
  GetField o -> collectAny o

collectStdHvm2 :: PhoasDummy f => Std f u -> [Hvm2KernelEntry]
collectStdHvm2 = \case
  Fixed _ args -> collectFixedArgsHvm2 args
  Method m -> collectMethodHvm2 m
  Kernel k -> collectKernelHvm2 k

collectFixedArgsHvm2 ::
  PhoasDummy f => FixedArgs f a b c -> [Hvm2KernelEntry]
collectFixedArgsHvm2 = \case
  ArgsU x -> collectAny x
  ArgsB x y -> collectAny x <> collectAny y
  ArgsT x y z -> collectAny x <> collectAny y <> collectAny z

collectMethodHvm2 :: PhoasDummy f => Method f u -> [Hvm2KernelEntry]
collectMethodHvm2 = \case
  MethMap x f -> collectAny x <> collectAny (f phoasDummy)
  MethFilter x f -> collectAny x <> collectAny (f phoasDummy)
  MethReduce x z _ -> collectAny x <> collectAny z
  MethReduceRight x z _ -> collectAny x <> collectAny z
  MethToSorted x _ -> collectAny x
  MethFrom n f -> collectAny n <> collectAny (f phoasDummy)

collectKernelHvm2 :: PhoasDummy f => Kernel f u -> [Hvm2KernelEntry]
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

collectFnBodyHvm2 :: PhoasDummy f => FnBody f us r -> [Hvm2KernelEntry]
collectFnBodyHvm2 = \case
  JfNil e -> collectAny e
  JfCons _ k -> collectFnBodyHvm2 (k phoasDummy)

collectFieldLitHvm2 :: PhoasDummy f => FieldLit f r -> [Hvm2KernelEntry]
collectFieldLitHvm2 = \case
  FieldLit e -> collectAny e
  FieldLitEffect e -> collectEffectAny e
  FieldLitExtra e -> collectAny e
  FieldLitExtraEffect e -> collectEffectAny e

collectEffectAny :: PhoasDummy f => Effect f v -> [Hvm2KernelEntry]
collectEffectAny = \case
  Lift x -> collectAny x
  FFI _ args -> collectRecArgs args
  Bind x f -> collectEffectAny x <> collectEffectAny (f phoasDummy)
  ThenE x y -> collectEffectAny x <> collectEffectAny y
  BindRec r b ->
    collectEffectAny (r phoasDummy) <> collectEffectAny (b phoasDummy)
  LambdaE f -> collectEffectAny (f phoasDummy)
  ApplyE f x -> collectEffectAny f <> collectEffectAny x
  IfE c u v -> collectEffectAny c <> collectEffectAny u <> collectEffectAny v
  While c b -> collectEffectAny c <> collectEffectAny b
  ForRange s e b ->
    collectAny s <> collectAny e <> collectEffectAny (b phoasDummy)
  U8Set b i v -> collectAny b <> collectAny i <> collectAny v
  U8Fill b v -> collectAny b <> collectAny v
  OptionCaseE o n s ->
    collectAny o <> collectEffectAny n <> collectEffectAny (s phoasDummy)
  ResultCaseE o er ok ->
    collectAny o
      <> collectEffectAny (er phoasDummy)
      <> collectEffectAny (ok phoasDummy)
  StringCaseE o arms d ->
    collectAny o
      <> concatMap (collectEffectAny . snd) arms
      <> collectEffectAny d
  Throw x -> collectAny x
  Try a k -> collectEffectAny a <> collectEffectAny (k phoasDummy)
  ObjectLit fs -> concatMap collectFieldLitHvm2 fs
  DeleteProp o k -> collectEffectAny o <> collectAny k
  ArrayLit es -> concatMap collectEffectAny es
  UnsafeObject {} -> []
  UnsafeObjectGet x _ -> collectEffectAny x
  UnsafeObjectAssign x y -> collectEffectAny x <> collectEffectAny y
  CallMethod x _ args -> collectEffectAny x <> collectRecArgs args

collectRecArgs :: PhoasDummy f => Rec (Arg f) us -> [Hvm2KernelEntry]
collectRecArgs = \case
  RecNil -> []
  RecCons a rest -> collectArgAny a <> collectRecArgs rest

collectArgAny :: PhoasDummy f => Arg f v -> [Hvm2KernelEntry]
collectArgAny = \case
  ArgExpr e -> collectAny e
  ArgEffect e -> collectEffectAny e
