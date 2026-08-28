{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | PHOAS tree normalization before lower/opt.
module JShark.Compiler.Flatten
  ( flattenExpr
  , flattenEff
  , inlineExpr
  , inlineEff
  , foldGetField
  , fieldsPure
  , foldExpr
  , foldEff
  , occursVarInExpr
  , occursVarInEff
  , rebindEff
  , rebindExpr
  , rebindExpr2
  , PhoasDummy (..)
  )
where

import Data.Monoid (Any (..))
import Data.Proxy (Proxy (..))
import Data.Typeable (type (:~:) (Refl))
import GHC.TypeLits (KnownSymbol, sameSymbol)
import JShark.Api.Rec
import JShark.Api.Types
import JShark.Compiler.Binder
  ( Stamp (..)
  , nestedDummy
  , nestedDummyId
  , stampId
  , strictFoldMap
  )
import JShark.Compiler.Evaluate (foldFixed, mapFixedArgs)

occursVarInExpr :: Int -> Expr Stamp u -> Bool
occursVarInExpr t expr = case expr of
  Var (Stamp i) -> i == t
  Var (Embed e') -> occursVarInExpr t e'
  Var (EmbedEff e') -> occursVarInEff t e'
  e ->
    getAny $
      foldExpr
        nestedDummy
        (Any . occursVarInExpr t)
        (Any . occursVarInExpr t)
        (Any . occursVarInEff t)
        e

occursVarInEff :: Int -> Effect Stamp u -> Bool
occursVarInEff t e =
  getAny
    ( foldEff
        nestedDummy
        (Any . occursVarInExpr t)
        (Any . occursVarInEff t)
        (Any . occursVarInEff t)
        e
    )

mapFnBody ::
  forall f.
  (forall v. Expr f v -> Expr f v)
  -> forall us r.
  FnBody f us r
  -> FnBody f us r
mapFnBody ge = \case
  JfNil e -> JfNil (ge e)
  JfCons k -> JfCons (\x -> mapFnBody ge (k x))

foldFnBody ::
  forall f us r m.
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> FnBody f us r
  -> m
foldFnBody dummy le body =
  le (evalFnBodyWith dummy body)

evalFnBodyWith :: forall f us r. (forall v. f v) -> FnBody f us r -> Expr f r
evalFnBodyWith dummy = \case
  JfNil e -> e
  JfCons k -> evalFnBodyWith dummy (k dummy)

mapFieldLit ::
  (forall u. Expr f u -> Expr f u)
  -> (forall u. Effect f u -> Effect f u)
  -> FieldLit f r
  -> FieldLit f r
mapFieldLit ge _ (FieldLit @k e) = FieldLit @k (ge e)
mapFieldLit _ gf (FieldLitEffect @k e) = FieldLitEffect @k (gf e)
mapFieldLit ge _ (FieldLitExtra @k e) = FieldLitExtra @k (ge e)
mapFieldLit _ gf (FieldLitExtraEffect @k e) = FieldLitExtraEffect @k (gf e)

mapArg ::
  (forall v. Expr f v -> Expr f v)
  -> (forall v. Effect f v -> Effect f v)
  -> Arg f u
  -> Arg f u
mapArg ge _ (ArgExpr e) = ArgExpr (ge e)
mapArg _ gf (ArgEffect e) = ArgEffect (gf e)

-- | Rebuild by rewriting immediate children. 'Var' / 'Literal' are leaves.
mapExpr ::
  (forall v. Expr f v -> Expr f v)
  -> (forall v. Effect f v -> Effect f v)
  -> Expr f u
  -> Expr f u
mapExpr ge gf expr = case expr of
  Literal x -> Literal x
  Var s -> Var s
  Let x g -> Let (ge x) (ge . g)
  LetRec rhs body -> LetRec (ge . rhs) (ge . body)
  Lambda hoist g -> Lambda hoist (ge . g)
  Apply f x -> Apply (ge f) (ge x)
  If c u v -> If (ge c) (ge u) (ge v)
  OptionCase o n s -> OptionCase (ge o) (ge n) (ge . s)
  ResultOk x -> ResultOk (ge x)
  ResultErr x -> ResultErr (ge x)
  ResultCase o e s -> ResultCase (ge o) (ge . e) (ge . s)
  Index x i -> Index (ge x) (ge i)
  U8Index x i -> U8Index (ge x) (ge i)
  Error x -> Error (ge x)
  Std s -> Std (mapStd ge s)
  FnLit body -> FnLit (mapFnBody ge body)
  UnsafeNullable x -> UnsafeNullable (ge x)
  FrozenLit fs -> FrozenLit (map (mapFieldLit ge gf) fs)
  GetField @k o -> GetField @k (ge o)
  Hvm2Kernel name k -> Hvm2Kernel name k

mapStd ::
  (forall v. Expr f v -> Expr f v)
  -> Std f u
  -> Std f u
mapStd ge s = case s of
  Fixed op args -> Fixed op (mapFixedArgs ge args)
  Method m -> Method (mapMethod ge m)
  Kernel k -> Kernel (mapKernel ge k)

mapKernel ::
  (forall v. Expr f v -> Expr f v)
  -> Kernel f u
  -> Kernel f u
mapKernel ge k = case k of
  KPlus x y -> KPlus (ge x) (ge y)
  KTimes x y -> KTimes (ge x) (ge y)
  KMinus x y -> KMinus (ge x) (ge y)
  KNegate x -> KNegate (ge x)
  KFracDiv x y -> KFracDiv (ge x) (ge y)
  KRem x y -> KRem (ge x) (ge y)
  KBitAnd x y -> KBitAnd (ge x) (ge y)
  KBitOr x y -> KBitOr (ge x) (ge y)
  KBitXor x y -> KBitXor (ge x) (ge y)
  KShl x y -> KShl (ge x) (ge y)
  KShr x y -> KShr (ge x) (ge y)
  KUShr x y -> KUShr (ge x) (ge y)
  KBig op x y -> KBig op (ge x) (ge y)
  KBigNeg x -> KBigNeg (ge x)
  KConcat x y -> KConcat (ge x) (ge y)
  KShow x -> KShow (ge x)
  KTypeOf x -> KTypeOf (ge x)
  KAnd x y -> KAnd (ge x) (ge y)
  KOr x y -> KOr (ge x) (ge y)
  KEq structural x y -> KEq structural (ge x) (ge y)
  KNEq structural x y -> KNEq structural (ge x) (ge y)
  KGTh x y -> KGTh (ge x) (ge y)
  KLTh x y -> KLTh (ge x) (ge y)
  KGTEq x y -> KGTEq (ge x) (ge y)
  KLTEq x y -> KLTEq (ge x) (ge y)

mapMethod ::
  (forall v. Expr f v -> Expr f v)
  -> Method f u
  -> Method f u
mapMethod ge m = case m of
  MethMap x f -> MethMap (ge x) (ge . f)
  MethFilter x f -> MethFilter (ge x) (ge . f)
  MethReduce x z f -> MethReduce (ge x) (ge z) (\a b -> ge (f a b))
  MethReduceRight x z f -> MethReduceRight (ge x) (ge z) (\a b -> ge (f a b))
  MethToSorted x f -> MethToSorted (ge x) (\a b -> ge (f a b))
  MethFrom n f -> MethFrom (ge n) (ge . f)

mapEff ::
  (forall v. Expr f v -> Expr f v)
  -> (forall v. Effect f v -> Effect f v)
  -> Effect f u
  -> Effect f u
mapEff ge gf eff = case eff of
  Lift x -> Lift (ge x)
  FFI n args -> FFI n (mapRec (mapArg ge gf) args)
  UnsafeObject o -> UnsafeObject o
  UnsafeObjectGet x s -> UnsafeObjectGet (gf x) s
  UnsafeObjectAssign x y -> UnsafeObjectAssign (gf x) (gf y)
  CallMethod x n args -> CallMethod (gf x) n (mapRec (mapArg ge gf) args)
  Bind x f -> Bind (gf x) (gf . f)
  ThenE x y -> ThenE (gf x) (gf y)
  BindRec rhs body -> BindRec (gf . rhs) (gf . body)
  LambdaE f -> LambdaE (gf . f)
  ApplyE f x -> ApplyE (gf f) (gf x)
  IfE c u v -> IfE (gf c) (gf u) (gf v)
  While c b -> While (gf c) (gf b)
  ForRange s e b -> ForRange (ge s) (ge e) (gf . b)
  U8Set b i v -> U8Set (ge b) (ge i) (ge v)
  U8Fill b v -> U8Fill (ge b) (ge v)
  OptionCaseE o n s -> OptionCaseE (ge o) (gf n) (gf . s)
  ResultCaseE o e s -> ResultCaseE (ge o) (gf . e) (gf . s)
  StringCaseE o arms d ->
    StringCaseE (ge o) (map (fmap gf) arms) (gf d)
  Throw x -> Throw (ge x)
  Try a k -> Try (gf a) (gf . k)
  ObjectLit fs -> ObjectLit (map (mapFieldLit ge gf) fs)
  DeleteProp o k -> DeleteProp (gf o) (ge k)
  ArrayLit es -> ArrayLit (map gf es)

-- | Immediate children. Lazy positions (&&/|| RHS, lambda, ?: arms)
-- use @le@. Binders are applied to @dummy@.
-- 'Expr' has no lazy 'Effect' child; see 'foldEff' for @lf@.
foldExpr ::
  forall f m u.
  Monoid m =>
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> (forall v. Expr f v -> m)
  -> (forall v. Effect f v -> m)
  -> Expr f u
  -> m
foldExpr dummy se le sf expr = case expr of
  Literal {} -> mempty
  Var {} -> mempty
  Let x g -> se x <> se (g dummy)
  LetRec r b -> le (r dummy) <> se (b dummy)
  Lambda _ g -> le (g dummy)
  Apply f x -> se f <> se x
  If c u v -> se c <> le u <> le v
  OptionCase o n s -> se o <> le n <> le (s dummy)
  ResultOk x -> se x
  ResultErr x -> se x
  ResultCase o e s -> se o <> le (e dummy) <> le (s dummy)
  Index x i -> se x <> se i
  U8Index x i -> se x <> se i
  Error x -> se x
  Std s -> foldStd dummy se le s
  FnLit body -> foldFnBody dummy le body
  UnsafeNullable x -> se x
  FrozenLit fs -> strictFoldMap (foldFieldLit se sf) fs
  GetField o -> se o
  Hvm2Kernel {} -> mempty

foldStd ::
  forall f m u.
  Monoid m =>
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> (forall v. Expr f v -> m)
  -> Std f u
  -> m
foldStd dummy se le s = case s of
  Fixed op args -> foldFixed dummy se op args
  Method m -> foldMethod dummy se le m
  Kernel k -> foldKernel se le k

foldKernel ::
  forall f m u.
  Monoid m =>
  (forall v. Expr f v -> m)
  -> (forall v. Expr f v -> m)
  -> Kernel f u
  -> m
foldKernel se le k = case k of
  KPlus x y -> se x <> se y
  KTimes x y -> se x <> se y
  KMinus x y -> se x <> se y
  KNegate x -> se x
  KFracDiv x y -> se x <> se y
  KRem x y -> se x <> se y
  KBitAnd x y -> se x <> se y
  KBitOr x y -> se x <> se y
  KBitXor x y -> se x <> se y
  KShl x y -> se x <> se y
  KShr x y -> se x <> se y
  KUShr x y -> se x <> se y
  KBig _ x y -> se x <> se y
  KBigNeg x -> se x
  KConcat x y -> se x <> se y
  KShow x -> se x
  KTypeOf x -> se x
  KAnd x y -> se x <> le y
  KOr x y -> se x <> le y
  KEq _ x y -> se x <> se y
  KNEq _ x y -> se x <> se y
  KGTh x y -> se x <> se y
  KLTh x y -> se x <> se y
  KGTEq x y -> se x <> se y
  KLTEq x y -> se x <> se y

foldMethod ::
  forall f m u.
  Monoid m =>
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> (forall v. Expr f v -> m)
  -> Method f u
  -> m
foldMethod dummy se le m = case m of
  MethMap x f -> se x <> le (f dummy)
  MethFilter x f -> se x <> le (f dummy)
  MethReduce x z f -> se x <> se z <> le (f dummy dummy)
  MethReduceRight x z f -> se x <> se z <> le (f dummy dummy)
  MethToSorted x f -> se x <> le (f dummy dummy)
  MethFrom n f -> se n <> le (f dummy)

foldFieldLit ::
  (forall v. Expr f v -> m)
  -> (forall v. Effect f v -> m)
  -> FieldLit f r
  -> m
foldFieldLit se _ (FieldLit e) = se e
foldFieldLit _ sf (FieldLitEffect e) = sf e
foldFieldLit se _ (FieldLitExtra e) = se e
foldFieldLit _ sf (FieldLitExtraEffect e) = sf e

foldEff ::
  forall f m u.
  Monoid m =>
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> (forall v. Effect f v -> m)
  -> (forall v. Effect f v -> m)
  -> Effect f u
  -> m
foldEff dummy se sf lf eff = case eff of
  Lift x -> se x
  FFI _ args -> recFold (\n a -> n <> foldArg a) mempty args
  UnsafeObject {} -> mempty
  UnsafeObjectGet x _ -> sf x
  UnsafeObjectAssign x y -> sf x <> sf y
  CallMethod x _ args -> sf x <> recFold (\n a -> n <> foldArg a) mempty args
  Bind x f -> sf x <> sf (f dummy)
  ThenE x y -> sf x <> sf y
  BindRec r b -> lf (r dummy) <> sf (b dummy)
  LambdaE f -> lf (f dummy)
  ApplyE f x -> sf f <> sf x
  IfE c u v -> sf c <> lf u <> lf v
  While c b -> lf c <> lf b
  ForRange s e b -> se s <> se e <> lf (b dummy)
  U8Set b i v -> se b <> se i <> se v
  U8Fill b v -> se b <> se v
  OptionCaseE o n s -> se o <> lf n <> lf (s dummy)
  ResultCaseE o e s -> se o <> lf (e dummy) <> lf (s dummy)
  StringCaseE o arms d ->
    se o <> strictFoldMap (lf . snd) arms <> lf d
  Throw x -> se x
  Try a k -> sf a <> lf (k dummy)
  ObjectLit fs -> strictFoldMap (foldFieldLit se sf) fs
  DeleteProp o k -> sf o <> se k
  ArrayLit es -> strictFoldMap sf es
 where
  foldArg :: forall x. Arg f x -> m
  foldArg (ArgExpr e) = se e
  foldArg (ArgEffect e) = sf e

lookupField ::
  forall k r f. KnownSymbol k => [FieldLit f r] -> Maybe (Expr f (Field r k))
lookupField = findLit . reverse
 where
  findLit [] = Nothing
  findLit (FieldLit @k' e : rest) =
    case sameSymbol (Proxy @k) (Proxy @k') of
      Just Refl -> Just e
      Nothing -> findLit rest
  findLit (_ : rest) = findLit rest

fieldsPure :: PhoasDummy f => [FieldLit f r] -> Bool
fieldsPure = all $ \case
  FieldLit e -> isPureExpr_ e
  FieldLitExtra e -> isPureExpr_ e
  FieldLitEffect {} -> False
  FieldLitExtraEffect {} -> False

-- | Last-wins, and only when every sibling is observationally pure
-- (so projecting @.b@ cannot DCE @JSON.stringify@ in @.a@).
projectFrozenField ::
  forall k r f.
  (KnownSymbol k, PhoasDummy f) => [FieldLit f r] -> Maybe (Expr f (Field r k))
projectFrozenField fs
  | fieldsPure fs = lookupField @k fs
  | otherwise = Nothing

foldGetField ::
  forall k r f.
  (KnownSymbol k, PhoasDummy f) =>
  Expr f ('Object r) -> Maybe (Expr f (Field r k))
foldGetField = \case
  FrozenLit fs -> projectFrozenField @k fs
  If (Literal (ValueBool True)) t _ -> foldGetField @k t
  If (Literal (ValueBool False)) _ e -> foldGetField @k e
  _ -> Nothing

-- | Remove 'Embed' nodes from the tree.
flattenExpr :: Expr Stamp u -> Expr Stamp u
flattenExpr = \case
  Var (Embed x) -> flattenExpr x
  Var (EmbedEff (Lift x)) -> flattenExpr x
  Var (EmbedEff x) -> Var (EmbedEff (flattenEff x))
  e -> mapExpr flattenExpr flattenEff e

flattenEff :: Effect Stamp u -> Effect Stamp u
flattenEff = \case
  Lift (Var (EmbedEff x)) -> flattenEff x
  e -> mapEff flattenExpr flattenEff e

-- | Replace 'Stamp' @old@ with @new@. Phantom in the universe, so this
-- does not need a cast. Used after the one 'optUnder' apply of @f@.
-- The occurrence check answers "is there anything to do here", so it
-- belongs at the top of a rename only. Repeating it at every node of the
-- descent re-reads each subtree once per ancestor, which turns one
-- rename into work proportional to size times depth.
renameExpr :: Int -> Int -> Expr Stamp u -> Expr Stamp u
renameExpr old new e
  | old == new = e
  | not (occursVarInExpr old e) = e
  | otherwise = renameExprGo old new e

renameExprGo :: Int -> Int -> Expr Stamp u -> Expr Stamp u
renameExprGo old new = \case
  Var (Embed e') -> renameExprGo old new (flattenExpr e')
  Var (EmbedEff (Lift e')) -> renameExprGo old new (flattenExpr e')
  Var (EmbedEff e') -> Var (EmbedEff (renameEffGo old new e'))
  Var (Stamp t) | t == old -> Var (Stamp new)
  Var s -> Var s
  e -> mapExpr (renameExprGo old new) (renameEffGo old new) e

renameEff :: Int -> Int -> Effect Stamp u -> Effect Stamp u
renameEff old new e
  | old == new = e
  | not (occursVarInEff old e) = e
  | otherwise = renameEffGo old new e

renameEffGo :: Int -> Int -> Effect Stamp u -> Effect Stamp u
renameEffGo old new = \case
  Lift (Var (EmbedEff e')) -> renameEffGo old new (flattenEff e')
  e -> mapEff (renameExprGo old new) (renameEffGo old new) e

inlineExpr :: (Stamp u -> Expr Stamp v) -> Expr Stamp u -> Expr Stamp v
inlineExpr f x = flattenExpr (f (Embed x))

inlineEff :: (Stamp u -> Effect Stamp v) -> Effect Stamp u -> Effect Stamp v
inlineEff f x = flattenEff (f (EmbedEff x))

rebindExpr :: Int -> Expr Stamp v -> Stamp u -> Expr Stamp v
rebindExpr tag body s
  | i == tag || i == nestedDummyId = body
  | otherwise = renameExpr tag i body
 where
  i = stampId s

rebindEff :: Int -> Effect Stamp v -> Stamp u -> Effect Stamp v
rebindEff tag body s
  | i == tag || i == nestedDummyId = body
  | otherwise = renameEff tag i body
 where
  i = stampId s

rebindExpr2 :: Int -> Int -> Expr Stamp v -> Stamp a -> Stamp b -> Expr Stamp v
rebindExpr2 tA tB body a b = rebindExpr tA (rebindExpr tB body b) a

class PhoasDummy f where
  phoasDummy :: f u
  isPureExpr_ :: Expr f u -> Bool

instance PhoasDummy Value where
  phoasDummy = error "JShark.phoasDummy: Value binder"
  isPureExpr_ _ = True
