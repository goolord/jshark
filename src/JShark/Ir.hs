{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeOperators #-}

-- | First-order IR for optimize + codegen. PHOAS 'Expr'/'Effect' in
-- 'JShark.Types' stay the user-facing syntax; closed terms lower here once.
module JShark.Ir
  ( IrExpr (..)
  , IrEffect (..)
  , IrFnBody (..)
  , IrArg (..)
  , IrFieldLit (..)
  , IrFixedArgs (..)
  , IrKernel (..)
  , IrMethod (..)
  , IrMeta (..)
  , metaIrExpr
  , metaIrEffect
  , irMetaSize
  , irMetaFree
  , irMetaPure
  , irMetaCheap
  , optIrExpr
  , optIrEffect
  , substIrExpr
  , substIrEffect
  , irNestedDummyId
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Kind (Type)
import Data.Monoid (Any (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol)
import Unsafe.Coerce (unsafeCoerce)
import JShark.Prim (isPureFixed)
import JShark.Rec (Rec (..))
import JShark.Types
  ( BigBinOp
  , Comparable
  , FFIForm (..)
  , Field
  , FixedOp
  , Universe (..)
  , Value (..)
  )
import Prelude hiding (Bool)
import qualified Prelude as P

irNestedDummyId :: Int
irNestedDummyId = minBound

optSmall :: Int
optSmall = 16

optStep :: Int
optStep = 2

data IrMeta = IrMeta
  { irSize :: {-# UNPACK #-} !Int
  , irFree :: !(IntMap Int)
  , irPure :: !P.Bool
  , irCheap :: !P.Bool
  }

instance Semigroup IrMeta where
  IrMeta s1 f1 p1 c1 <> IrMeta s2 f2 p2 c2 =
    IrMeta (s1 + s2) (IM.unionWith (+) f1 f2) (p1 && p2) (c1 && c2)

instance Monoid IrMeta where
  mempty = IrMeta 0 IM.empty True True

irMetaSize :: IrMeta -> Int
irMetaSize = irSize

irMetaFree :: IrMeta -> IntMap Int
irMetaFree = irFree

irMetaPure :: IrMeta -> P.Bool
irMetaPure = irPure

irMetaCheap :: IrMeta -> P.Bool
irMetaCheap = irCheap

-- | Force impure on optimized metadata (empty 'RecNil' args are otherwise pure).
effectMd :: IrMeta -> IrMeta
effectMd !md = md <> IrMeta 0 IM.empty False False

isCheapValueIr :: Value u -> P.Bool
isCheapValueIr = \case
  ValueNumber {} -> True
  ValueBigInt {} -> True
  ValueString {} -> True
  ValueBool {} -> True
  ValueUnit -> True
  ValueOption Nothing -> True
  ValueOption (Just v) -> isCheapValueIr v
  ValueResult (Left v) -> isCheapValueIr v
  ValueResult (Right v) -> isCheapValueIr v
  ValueRegex {} -> False
  ValueUint8Array {} -> False
  ValueArray {} -> False
  ValueFunction {} -> False
  ValueFrozen {} -> False

data IrMethod :: Universe -> Type where
  IrMethMap ::
    IrExpr ('Array a) -> !Int -> IrExpr b -> IrMethod ('Array b)
  IrMethFilter ::
    IrExpr ('Array a) -> !Int -> IrExpr 'Bool -> IrMethod ('Array a)
  IrMethReduce ::
    IrExpr ('Array a)
    -> IrExpr b
    -> !Int
    -> !Int
    -> IrExpr b
    -> IrMethod b
  IrMethReduceRight ::
    IrExpr ('Array a)
    -> IrExpr b
    -> !Int
    -> !Int
    -> IrExpr b
    -> IrMethod b
  IrMethToSorted ::
    IrExpr ('Array a) -> !Int -> !Int -> IrExpr 'Number -> IrMethod ('Array a)
  IrMethFrom :: IrExpr 'Number -> !Int -> IrExpr a -> IrMethod ('Array a)

-- | Kernel mirror with 'IrExpr' children ('Kernel' in 'Types' hardcodes 'Expr f').
data IrKernel :: Universe -> Type where
  KConcat ::
    IrExpr 'String -> IrExpr 'String -> IrKernel 'String
  KPlus ::
    IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number
  KTimes ::
    IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number
  KMinus ::
    IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number
  KNegate :: IrExpr 'Number -> IrKernel 'Number
  KFracDiv ::
    IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number
  KRem ::
    IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number
  KBitAnd ::
    IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number
  KBitOr ::
    IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number
  KBitXor ::
    IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number
  KShl ::
    IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number
  KShr ::
    IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number
  KUShr ::
    IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number
  KBig ::
    BigBinOp -> IrExpr 'BigInt -> IrExpr 'BigInt -> IrKernel 'BigInt
  KBigNeg :: IrExpr 'BigInt -> IrKernel 'BigInt
  KAnd ::
    IrExpr 'Bool -> IrExpr 'Bool -> IrKernel 'Bool
  KOr ::
    IrExpr 'Bool -> IrExpr 'Bool -> IrKernel 'Bool
  KEq :: P.Bool -> IrExpr a -> IrExpr a -> IrKernel 'Bool
  KNEq :: P.Bool -> IrExpr a -> IrExpr a -> IrKernel 'Bool
  KGTh ::
    Comparable a => IrExpr a -> IrExpr a -> IrKernel 'Bool
  KLTh ::
    Comparable a => IrExpr a -> IrExpr a -> IrKernel 'Bool
  KGTEq ::
    Comparable a => IrExpr a -> IrExpr a -> IrKernel 'Bool
  KLTEq ::
    Comparable a => IrExpr a -> IrExpr a -> IrKernel 'Bool
  KShow :: IrExpr u -> IrKernel 'String
  KTypeOf :: IrExpr u -> IrKernel 'String

data IrFixedArgs a b c where
  IrArgsU :: IrExpr a -> IrFixedArgs a 'Unit 'Unit
  IrArgsB :: IrExpr a -> IrExpr b -> IrFixedArgs a b 'Unit
  IrArgsT :: IrExpr a -> IrExpr b -> IrExpr c -> IrFixedArgs a b c

data IrExpr :: Universe -> Type where
  IrLiteral :: Value u -> IrExpr u
  IrLet :: !Int -> IrExpr u -> IrExpr v -> IrExpr v
  IrLetRec :: !Int -> IrExpr u -> IrExpr v -> IrExpr v
  IrLambda :: !Int -> IrExpr v -> IrExpr ('Function u v)
  IrApply :: IrExpr ('Function u v) -> IrExpr u -> IrExpr v
  IrVar :: !Int -> IrExpr u
  IrEmbedEff :: IrEffect u -> IrExpr u
  IrIf :: IrExpr 'Bool -> IrExpr u -> IrExpr u -> IrExpr u
  IrOptionCase :: IrExpr ('Option u) -> IrExpr v -> !Int -> IrExpr v -> IrExpr v
  IrResultOk :: IrExpr a -> IrExpr ('Result e a)
  IrResultErr :: IrExpr e -> IrExpr ('Result e a)
  IrResultCase ::
    IrExpr ('Result e a) -> !Int -> IrExpr v -> !Int -> IrExpr v -> IrExpr v
  IrIndex :: IrExpr ('Array u) -> IrExpr 'Number -> IrExpr u
  IrU8Index :: IrExpr 'Uint8Array -> IrExpr 'Number -> IrExpr 'Number
  IrError :: IrExpr 'String -> IrExpr u
  IrFixed ::
    FixedOp a b c u -> IrFixedArgs a b c -> IrExpr u
  IrKernelK :: IrKernel u -> IrExpr u
  IrMethod :: IrMethod u -> IrExpr u
  IrFnLit :: IrFnBody us r -> IrExpr ('Fn us r)
  IrUnsafeNullable :: IrExpr u -> IrExpr ('Option u)
  IrFrozenLit :: [IrFieldLit r] -> IrExpr ('Object r)
  IrGetField :: forall k r. KnownSymbol k => IrExpr ('Object r) -> IrExpr (Field r k)
  IrHvm2Ref :: Text -> IrExpr u

data IrFnBody :: [Universe] -> Universe -> Type where
  IrJfNil :: IrExpr r -> IrFnBody '[] r
  IrJfCons :: !Int -> IrFnBody us r -> IrFnBody (u ': us) r

data IrFieldLit r where
  IrFieldLit :: KnownSymbol k => IrExpr (Field r k) -> IrFieldLit r
  IrFieldLitEffect :: KnownSymbol k => IrEffect (Field r k) -> IrFieldLit r
  IrFieldLitExtra :: (KnownSymbol k, Typeable u) => IrExpr u -> IrFieldLit r
  IrFieldLitExtraEffect :: (KnownSymbol k, Typeable u) => IrEffect u -> IrFieldLit r

data IrArg u where
  IrArgExpr :: IrExpr u -> IrArg u
  IrArgEffect :: IrEffect u -> IrArg u

data IrEffect :: Universe -> Type where
  IrLift :: IrExpr u -> IrEffect u
  IrFFI :: FFIForm -> Rec (IrArg) us -> IrEffect u
  IrUnsafeObject :: Text -> IrEffect ('MutableObject x)
  IrUnsafeObjectGet :: IrEffect object -> Text -> IrEffect u
  IrUnsafeObjectAssign :: IrEffect object -> IrEffect assignment -> IrEffect u
  IrCallMethod :: IrEffect object -> Text -> Rec (IrArg) us -> IrEffect u
  IrBind :: !Int -> IrEffect u -> IrEffect v -> IrEffect v
  IrThenE :: IrEffect u -> IrEffect v -> IrEffect v
  IrBindRec :: !Int -> IrEffect u -> IrEffect v -> IrEffect v
  IrLambdaE :: !Int -> IrEffect v -> IrEffect ('Function u v)
  IrApplyE :: IrEffect ('Function u v) -> IrEffect u -> IrEffect v
  IrIfE :: IrEffect 'Bool -> IrEffect u -> IrEffect u -> IrEffect u
  IrWhile :: IrEffect 'Bool -> IrEffect 'Unit -> IrEffect 'Unit
  IrForRange ::
    IrExpr 'Number -> IrExpr 'Number -> !Int -> IrEffect 'Unit -> IrEffect 'Unit
  IrU8Set :: IrExpr 'Uint8Array -> IrExpr 'Number -> IrExpr 'Number -> IrEffect 'Unit
  IrU8Fill :: IrExpr 'Uint8Array -> IrExpr 'Number -> IrEffect 'Unit
  IrOptionCaseE ::
    IrExpr ('Option u) -> IrEffect v -> !Int -> IrEffect v -> IrEffect v
  IrResultCaseE ::
    IrExpr ('Result e a) -> !Int -> IrEffect v -> !Int -> IrEffect v -> IrEffect v
  IrStringCaseE :: IrExpr 'String -> [(Text, IrEffect v)] -> IrEffect v -> IrEffect v
  IrThrow :: IrExpr 'String -> IrEffect v
  IrTry :: IrEffect u -> !Int -> IrEffect u -> IrEffect u
  IrObjectLit :: [IrFieldLit r] -> IrEffect ('MutableObject r)
  IrDeleteProp :: IrEffect object -> IrExpr 'String -> IrEffect 'Bool
  IrArrayLit :: [IrEffect u] -> IrEffect ('Array u)

-- | Structural metadata. Every child contributes, including the lazy
-- ones: a free variable that occurs only inside a lambda body, a @?:@
-- arm, or an FFI argument is still a use, and 'substIrExpr' keys its
-- skip test off 'irFree'.
metaIrExpr :: IrExpr u -> IrMeta
metaIrExpr !e = case e of
  IrLiteral v -> IrMeta 1 IM.empty True (isCheapValueIr v)
  IrVar i -> IrMeta 1 (IM.singleton i 1) True True
  IrEmbedEff x -> metaIrEffect x
  _ ->
    let
      !md = here <> foldIrExpr metaIrExpr metaIrExpr metaIrEffect e
     in
      md
 where
  here = case e of
    IrFixed op _ -> IrMeta 1 IM.empty (isPureFixed op) True
    _ -> IrMeta 1 IM.empty True False

metaIrEffect :: IrEffect u -> IrMeta
metaIrEffect !e =
  let
    !md = here <> foldIrEff metaIrExpr metaIrEffect metaIrEffect e
   in
    md
 where
  here = case e of
    IrFFI {} -> impure
    IrUnsafeObject {} -> impure
    IrUnsafeObjectGet {} -> impure
    IrUnsafeObjectAssign {} -> impure
    IrCallMethod {} -> impure
    IrApplyE {} -> impure
    IrWhile {} -> impure
    IrForRange {} -> impure
    IrU8Set {} -> impure
    IrThrow {} -> impure
    IrTry {} -> impure
    IrDeleteProp {} -> impure
    _ -> IrMeta 1 IM.empty True False
  impure = IrMeta 1 IM.empty False False

-- | Occurrence test for 'substIrExpr' / 'substIrEffect'. Short-circuits
-- on the first hit and, unlike a free-variable map, allocates nothing.
occursIrExpr :: Int -> IrExpr u -> P.Bool
occursIrExpr !t = \case
  IrVar i -> i == t
  IrEmbedEff e -> occursIrEffect t e
  e ->
    getAny
      ( foldIrExpr
          (Any . occursIrExpr t)
          (Any . occursIrExpr t)
          (Any . occursIrEffect t)
          e
      )

occursIrEffect :: Int -> IrEffect u -> P.Bool
occursIrEffect !t !e =
  getAny
    ( foldIrEff
        (Any . occursIrExpr t)
        (Any . occursIrEffect t)
        (Any . occursIrEffect t)
        e
    )

-- | Does the tag occur in a position that is not evaluated exactly once
-- where it stands: a lambda body, a @?:@ arm, an @&&@ right operand, a
-- loop body? Inlining there either skips work the program asked for or
-- repeats it, so those uses keep their binding.
lazyOccursIrExpr :: Int -> IrExpr u -> P.Bool
lazyOccursIrExpr !t = \case
  IrEmbedEff e -> lazyOccursIrEffect t e
  e ->
    getAny
      ( foldIrExpr
          (Any . lazyOccursIrExpr t)
          (Any . occursIrExpr t)
          (Any . lazyOccursIrEffect t)
          e
      )

lazyOccursIrEffect :: Int -> IrEffect u -> P.Bool
lazyOccursIrEffect !t = \case
  -- Re-evaluated once per iteration, so neither part is a "once" slot.
  IrWhile c b -> occursIrEffect t c P.|| occursIrEffect t b
  e ->
    getAny
      ( foldIrEff
          (Any . lazyOccursIrExpr t)
          (Any . lazyOccursIrEffect t)
          (Any . occursIrEffect t)
          e
      )

foldIrExpr ::
  Monoid m =>
  (forall v. IrExpr v -> m)
  -> (forall v. IrExpr v -> m)
  -> (forall v. IrEffect v -> m)
  -> IrExpr u
  -> m
foldIrExpr se le sf = \case
  IrLiteral {} -> mempty
  IrVar {} -> mempty
  IrEmbedEff e -> sf e
  IrLet _ x g -> se x <> se g
  IrLetRec _ r b -> se r <> se b
  IrLambda _ g -> le g
  IrApply f x -> se f <> se x
  IrIf c t e -> se c <> le t <> le e
  IrOptionCase o n _ s -> se o <> le n <> le s
  IrResultOk x -> se x
  IrResultErr x -> se x
  IrResultCase o _ e _ s -> se o <> le e <> le s
  IrIndex x i -> se x <> se i
  IrU8Index x i -> se x <> se i
  IrError x -> se x
  IrFixed _ args -> foldIrFixedArgs se args
  IrKernelK k -> foldIrKernel se le k
  IrMethod m -> foldIrMethod se le m
  IrFnLit b -> foldIrFnBody le b
  IrUnsafeNullable x -> se x
  IrFrozenLit fs -> foldMap (foldIrFieldLit se sf) fs
  IrGetField o -> se o
  IrHvm2Ref {} -> mempty

foldIrKernel ::
  Monoid m =>
  (forall v. IrExpr v -> m)
  -> (forall v. IrExpr v -> m)
  -> IrKernel u
  -> m
foldIrKernel se le = \case
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

foldIrMethod ::
  Monoid m =>
  (forall v. IrExpr v -> m)
  -> (forall v. IrExpr v -> m)
  -> IrMethod u
  -> m
foldIrMethod se le = \case
  IrMethMap x _ g -> se x <> le g
  IrMethFilter x _ g -> se x <> le g
  IrMethReduce x z _ _ g -> se x <> se z <> le g
  IrMethReduceRight x z _ _ g -> se x <> se z <> le g
  IrMethToSorted x _ _ g -> se x <> le g
  IrMethFrom n _ g -> se n <> le g

foldIrFixedArgs ::
  Monoid m =>
  (forall v. IrExpr v -> m)
  -> IrFixedArgs a b c
  -> m
foldIrFixedArgs se = \case
  IrArgsU x -> se x
  IrArgsB x y -> se x <> se y
  IrArgsT x y z -> se x <> se y <> se z

foldIrFnBody ::
  Monoid m =>
  (forall v. IrExpr v -> m)
  -> IrFnBody us r
  -> m
foldIrFnBody le = \case
  IrJfNil e -> le e
  IrJfCons _ (IrJfNil e) -> le e
  IrJfCons _ k -> foldIrFnBody le k

foldIrFieldLit ::
  (forall v. IrExpr v -> m)
  -> (forall v. IrEffect v -> m)
  -> IrFieldLit r
  -> m
foldIrFieldLit se sf = \case
  IrFieldLit e -> se e
  IrFieldLitEffect e -> sf e
  IrFieldLitExtra e -> se e
  IrFieldLitExtraEffect e -> sf e

foldIrEff ::
  Monoid m =>
  (forall v. IrExpr v -> m)
  -> (forall v. IrEffect v -> m)
  -> (forall v. IrEffect v -> m)
  -> IrEffect u
  -> m
foldIrEff se sf lf = \case
  IrLift x -> se x
  IrFFI _ args -> recFoldIrArg se sf args
  IrUnsafeObject {} -> mempty
  IrUnsafeObjectGet x _ -> sf x
  IrUnsafeObjectAssign x y -> sf x <> sf y
  IrCallMethod x _ args -> sf x <> recFoldIrArg se sf args
  IrBind _ x g -> sf x <> sf g
  IrThenE x y -> sf x <> sf y
  IrBindRec _ r b -> sf r <> sf b
  IrLambdaE _ g -> lf g
  IrApplyE f x -> sf f <> sf x
  IrIfE c t e -> sf c <> lf t <> lf e
  IrWhile c b -> sf c <> lf b
  IrForRange s e _ b -> se s <> se e <> lf b
  IrU8Set b i v -> se b <> se i <> se v
  IrU8Fill b v -> se b <> se v
  IrOptionCaseE o n _ s -> se o <> sf n <> sf s
  IrResultCaseE o _ e _ s -> se o <> sf e <> sf s
  IrStringCaseE s arms d -> se s <> foldMap (\(_, e) -> sf e) arms <> sf d
  IrThrow x -> se x
  IrTry a _ k -> sf a <> sf k
  IrObjectLit fs -> foldMap (foldIrFieldLit se sf) fs
  IrDeleteProp o k -> sf o <> se k
  IrArrayLit es -> foldMap sf es

recFoldIrArg ::
  Monoid m =>
  (forall v. IrExpr v -> m)
  -> (forall v. IrEffect v -> m)
  -> Rec (IrArg) us
  -> m
recFoldIrArg se sf = \case
  RecNil -> mempty
  RecCons (IrArgExpr x) xs -> se x <> recFoldIrArg se sf xs
  RecCons (IrArgEffect x) xs -> sf x <> recFoldIrArg se sf xs

mapIrExpr ::
  (forall v. IrExpr v -> IrExpr v)
  -> (forall v. IrEffect v -> IrEffect v)
  -> IrExpr u
  -> IrExpr u
mapIrExpr ge gf = \case
  IrLiteral v -> IrLiteral v
  IrVar i -> IrVar i
  IrEmbedEff e -> IrEmbedEff (gf e)
  IrLet tag x g -> IrLet tag (ge x) (ge g)
  IrLetRec tag r b -> IrLetRec tag (ge r) (ge b)
  IrLambda tag g -> IrLambda tag (ge g)
  IrApply f x -> IrApply (ge f) (ge x)
  IrIf c t e -> IrIf (ge c) (ge t) (ge e)
  IrOptionCase o n tag g -> IrOptionCase (ge o) (ge n) tag (ge g)
  IrResultOk x -> IrResultOk (ge x)
  IrResultErr x -> IrResultErr (ge x)
  IrResultCase o tagE e tagO s ->
    IrResultCase (ge o) tagE (ge e) tagO (ge s)
  IrIndex x i -> IrIndex (ge x) (ge i)
  IrU8Index x i -> IrU8Index (ge x) (ge i)
  IrError x -> IrError (ge x)
  IrFixed op args -> IrFixed op (mapIrFixedArgs ge args)
  IrKernelK k -> IrKernelK (mapIrKernel ge k)
  IrMethod m -> IrMethod (mapIrMethod ge m)
  IrFnLit b -> IrFnLit (mapIrFnBody ge b)
  IrUnsafeNullable x -> IrUnsafeNullable (ge x)
  IrFrozenLit fs -> IrFrozenLit (map (mapIrFieldLit ge gf) fs)
  IrGetField @k o -> IrGetField @k (ge o)
  IrHvm2Ref name -> IrHvm2Ref name

mapIrKernel ::
  (forall v. IrExpr v -> IrExpr v)
  -> IrKernel u
  -> IrKernel u
mapIrKernel ge = \case
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
  KEq s x y -> KEq s (ge x) (ge y)
  KNEq s x y -> KNEq s (ge x) (ge y)
  KGTh x y -> KGTh (ge x) (ge y)
  KLTh x y -> KLTh (ge x) (ge y)
  KGTEq x y -> KGTEq (ge x) (ge y)
  KLTEq x y -> KLTEq (ge x) (ge y)

mapIrMethod ::
  (forall v. IrExpr v -> IrExpr v)
  -> IrMethod u
  -> IrMethod u
mapIrMethod ge = \case
  IrMethMap x tag g -> IrMethMap (ge x) tag (ge g)
  IrMethFilter x tag g -> IrMethFilter (ge x) tag (ge g)
  IrMethReduce x z tagA tagB g ->
    IrMethReduce (ge x) (ge z) tagA tagB (ge g)
  IrMethReduceRight x z tagA tagB g ->
    IrMethReduceRight (ge x) (ge z) tagA tagB (ge g)
  IrMethToSorted x tagA tagB g ->
    IrMethToSorted (ge x) tagA tagB (ge g)
  IrMethFrom n tag g -> IrMethFrom (ge n) tag (ge g)

mapIrFixedArgs ::
  (forall v. IrExpr v -> IrExpr v)
  -> IrFixedArgs a b c
  -> IrFixedArgs a b c
mapIrFixedArgs ge = \case
  IrArgsU x -> IrArgsU (ge x)
  IrArgsB x y -> IrArgsB (ge x) (ge y)
  IrArgsT x y z -> IrArgsT (ge x) (ge y) (ge z)

mapIrFnBody ::
  (forall v. IrExpr v -> IrExpr v)
  -> IrFnBody us r
  -> IrFnBody us r
mapIrFnBody ge = \case
  IrJfNil e -> IrJfNil (ge e)
  IrJfCons tag k -> IrJfCons tag (mapIrFnBody ge k)

mapIrFieldLit ::
  (forall v. IrExpr v -> IrExpr v)
  -> (forall v. IrEffect v -> IrEffect v)
  -> IrFieldLit r
  -> IrFieldLit r
mapIrFieldLit ge gf = \case
  IrFieldLit @k e -> IrFieldLit @k (ge e)
  IrFieldLitEffect @k e -> IrFieldLitEffect @k (gf e)
  IrFieldLitExtra @k e -> IrFieldLitExtra @k (ge e)
  IrFieldLitExtraEffect @k e -> IrFieldLitExtraEffect @k (gf e)

mapIrEff ::
  (forall v. IrExpr v -> IrExpr v)
  -> (forall v. IrEffect v -> IrEffect v)
  -> IrEffect u
  -> IrEffect u
mapIrEff ge gf = \case
  IrLift x -> IrLift (ge x)
  IrFFI n args -> IrFFI n (mapIrArgs gf ge args)
  IrUnsafeObject o -> IrUnsafeObject o
  IrUnsafeObjectGet x s -> IrUnsafeObjectGet (gf x) s
  IrUnsafeObjectAssign x y -> IrUnsafeObjectAssign (gf x) (gf y)
  IrCallMethod x n args -> IrCallMethod (gf x) n (mapIrArgs gf ge args)
  IrBind tag x g -> IrBind tag (gf x) (gf g)
  IrThenE x y -> IrThenE (gf x) (gf y)
  IrBindRec tag r b -> IrBindRec tag (gf r) (gf b)
  IrLambdaE tag g -> IrLambdaE tag (gf g)
  IrApplyE f x -> IrApplyE (gf f) (gf x)
  IrIfE c t e -> IrIfE (gf c) (gf t) (gf e)
  IrWhile c b -> IrWhile (gf c) (gf b)
  IrForRange s e tag b -> IrForRange (ge s) (ge e) tag (gf b)
  IrU8Set b i v -> IrU8Set (ge b) (ge i) (ge v)
  IrU8Fill b v -> IrU8Fill (ge b) (ge v)
  IrOptionCaseE o n tag s -> IrOptionCaseE (ge o) (gf n) tag (gf s)
  IrResultCaseE o tagE e tagO s ->
    IrResultCaseE (ge o) tagE (gf e) tagO (gf s)
  IrStringCaseE s arms d ->
    IrStringCaseE (ge s) (map (\(k, v) -> (k, gf v)) arms) (gf d)
  IrThrow x -> IrThrow (ge x)
  IrTry a tag k -> IrTry (gf a) tag (gf k)
  IrObjectLit fs -> IrObjectLit (map (mapIrFieldLit ge gf) fs)
  IrDeleteProp o k -> IrDeleteProp (gf o) (ge k)
  IrArrayLit es -> IrArrayLit (map gf es)

mapIrArgs ::
  (forall v. IrEffect v -> IrEffect v)
  -> (forall v. IrExpr v -> IrExpr v)
  -> Rec (IrArg) us
  -> Rec (IrArg) us
mapIrArgs gf ge = \case
  RecNil -> RecNil
  RecCons (IrArgExpr x) xs -> RecCons (IrArgExpr (ge x)) (mapIrArgs gf ge xs)
  RecCons (IrArgEffect x) xs -> RecCons (IrArgEffect (gf x)) (mapIrArgs gf ge xs)

substIrExpr :: Int -> Int -> IrExpr u -> IrExpr u
substIrExpr !old !new !e
  | old == new = e
  | not (occursIrExpr old e) = e
  | otherwise = case e of
      IrVar i | i == old -> IrVar new
      _ -> mapIrExpr (substIrExpr old new) (substIrEffect old new) e

substIrEffect :: Int -> Int -> IrEffect u -> IrEffect u
substIrEffect !old !new !e
  | old == new = e
  | not (occursIrEffect old e) = e
  | otherwise = mapIrEff (substIrExpr old new) (substIrEffect old new) e

-- | Replace the binder with the bound term itself. A variable-to-variable
-- bound term is a plain rename; anything else has to be spliced in, and
-- treating it as a rename would drop the binding while leaving the uses
-- pointing at a tag nothing binds.
inlineIrExpr :: Int -> IrExpr u -> IrExpr v -> IrExpr v
inlineIrExpr !tag !bound !body = case bound of
  IrVar i -> substIrExpr tag i body
  _ -> replaceIrVarExpr tag bound body

replaceIrVarExpr :: Int -> IrExpr u -> IrExpr v -> IrExpr v
replaceIrVarExpr !tag bound = \case
  IrVar i | i == tag -> unsafeCoerce bound
  e -> mapIrExpr (replaceIrVarExpr tag bound) (replaceIrVarEff tag bound) e

replaceIrVarEff :: Int -> IrExpr u -> IrEffect v -> IrEffect v
replaceIrVarEff !tag bound =
  mapIrEff (replaceIrVarExpr tag bound) (replaceIrVarEff tag bound)

inlineIrEffect :: Int -> IrEffect u -> IrEffect v -> IrEffect v
inlineIrEffect !tag !bound !body = case bound of
  IrLift (IrVar i) -> substIrEffect tag i body
  _
    | not (occursIrEffect tag body) -> body
    | otherwise ->
        mapIrEff (inlineIrExprInEff tag bound) (inlineIrEffect tag bound) body

inlineIrEffectInExpr :: Int -> IrEffect u -> IrEffect v -> IrEffect v
inlineIrEffectInExpr !tag bound = \case
  IrLift e -> IrLift (inlineIrExprInEff tag bound e)
  e -> inlineIrEffect tag bound e

inlineIrExprInEff :: Int -> IrEffect u -> IrExpr v -> IrExpr v
inlineIrExprInEff !tag bound = \case
  IrVar i | i == tag -> unsafeCoerce (inlineEffAsExpr bound)
  e -> mapIrExpr (inlineIrExprInEff tag bound) (inlineIrEffectInExpr tag bound) e
 where
  inlineEffAsExpr :: IrEffect u -> IrExpr u
  inlineEffAsExpr = \case
    IrLift e -> e
    eff -> IrEmbedEff eff

isAliasIrEffect :: IrEffect u -> P.Bool
isAliasIrEffect = \case
  IrLift (IrVar _) -> True
  IrLift (IrUnsafeNullable (IrVar _)) -> True
  _ -> False

elimIrLet ::
  IrMeta
  -> Int
  -> IrExpr u
  -> IrExpr v
  -> IrMeta
  -> (IrExpr v, IrMeta)
elimIrLet !mdX !tag !x !body !mdBody =
  let
    uses = IM.findWithDefault 0 tag (irFree mdBody)
    closed = bindMeta tag mdBody
    spliced = closed <> mdX
    once = irCheap mdX P.|| not (lazyOccursIrExpr tag body)
   in
    case uses of
      0 | irPure mdX -> (body, closed)
      0 -> (IrLet tag x body, nodeMeta mdX closed)
      1 | irSize mdBody <= optSmall, once ->
        (inlineIrExpr tag x body, spliced)
      _ | irCheap mdX, irSize mdBody <= optSmall ->
        (inlineIrExpr tag x body, spliced)
      _ -> (IrLet tag x body, nodeMeta mdX closed)

elimIrBind ::
  IrMeta
  -> Int
  -> IrEffect u
  -> IrEffect v
  -> IrMeta
  -> (IrEffect v, IrMeta)
elimIrBind !mdX !tag !x !body !mdBody =
  let
    uses = IM.findWithDefault 0 tag (irFree mdBody)
    closed = bindMeta tag mdBody
    spliced = closed <> mdX
    once =
      isAliasIrEffect x
        P.|| irCheap mdX
        P.|| not (lazyOccursIrEffect tag body)
   in
    case uses of
      0 | irPure mdX, not (isAliasIrEffect x) -> (body, closed)
      0 -> (IrThenE x body, nodeMeta mdX closed)
      1 | irSize mdBody <= optSmall, once ->
        (inlineIrEffect tag x body, spliced)
      _ | irCheap mdX, irSize mdBody <= optSmall ->
        (inlineIrEffect tag x body, spliced)
      _ -> (IrBind tag x body, nodeMeta mdX closed)

nodeMeta :: IrMeta -> IrMeta -> IrMeta
nodeMeta !mdX !mdY =
  IrMeta 1 IM.empty (irPure mdX && irPure mdY) False <> mdX <> mdY

-- | Close a binder: its tag is no longer free above this node. Without
-- this the free map grows to every tag in the subtree, and the union in
-- '<>' then costs the whole program at every node.
bindMeta :: Int -> IrMeta -> IrMeta
bindMeta !tag !md = md {irFree = IM.delete tag (irFree md)}

optIrExpr :: Int -> IrExpr u -> (Int, IrExpr u, IrMeta)
optIrExpr !t0 = \case
  IrLiteral v -> (t0, IrLiteral v, litMeta v)
  IrVar i -> (t0, IrVar i, varMeta i)
  IrLet tag x body ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, body', mdBody) = optIrExpr t1 body
     in
      let (e', md') = elimIrLet mdX tag x' body' mdBody
       in (t2, e', md')
  -- A JS call evaluates its argument before the body runs, so an applied
  -- lambda is a let and gets the same inlining decision.
  IrApply (IrLambda tag g) x ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, g', mdG) = optIrExpr t1 g
     in
      let (e', md') = elimIrLet mdX tag x' g' mdG
       in (t2, e', md')
  IrFixed op args ->
    let (t1, args', md) = optIrFixedArgs t0 args
     in (t1, IrFixed op args', md)
  IrKernelK k ->
    let (t1, k', md) = optIrKernel t0 k
     in (t1, IrKernelK k', md)
  IrMethod m ->
    let (t1, m', md) = optIrMethod t0 m
     in (t1, IrMethod m', md)
  IrHvm2Ref name ->
    (t0, IrHvm2Ref name, IrMeta 1 IM.empty True True)
  e ->
    let (t1, e', md) = optIrExprChildren t0 e
     in (t1, e', md)

litMeta :: Value u -> IrMeta
litMeta v = IrMeta 1 IM.empty True (isCheapValueIr v)

varMeta :: Int -> IrMeta
varMeta !i = IrMeta 1 (IM.singleton i 1) True True

optIrExprChildren :: Int -> IrExpr u -> (Int, IrExpr u, IrMeta)
optIrExprChildren !t0 = \case
  IrEmbedEff e ->
    let (t1, e', md) = optIrEffect t0 e
     in (t1, IrEmbedEff e', md)
  IrLetRec tag r b ->
    let
      (t1, r', mdR) = optIrExpr t0 r
      (t2, b', mdB) = optIrExpr t1 b
     in
      (t2, IrLetRec tag r' b', bindMeta tag (nodeMeta mdR mdB))
  IrLambda tag g ->
    let (t1, g', md) = optIrExpr t0 g
     in (t1, IrLambda tag g', bindMeta tag md)
  IrApply f x ->
    let
      (t1, f', mdF) = optIrExpr t0 f
      (t2, x', mdX) = optIrExpr t1 x
     in
      (t2, IrApply f' x', nodeMeta mdF mdX)
  IrIf c t e ->
    let
      (t1, c', mdC) = optIrExpr t0 c
      (t2, t', mdT) = optIrExpr t1 t
      (t3, e', mdE) = optIrExpr t2 e
     in
      ( t3
      , IrIf c' t' e'
      , nodeMeta mdC (nodeMeta mdT mdE)
      )
  IrOptionCase o n tag s ->
    let
      (t1, o', mdO) = optIrExpr t0 o
      (t2, n', mdN) = optIrExpr t1 n
      (t3, s', mdS) = optIrExpr t2 s
     in
      ( t3
      , IrOptionCase o' n' tag s'
      , nodeMeta mdO (nodeMeta mdN (bindMeta tag mdS))
      )
  IrResultOk x ->
    let (t1, x', md) = optIrExpr t0 x
     in (t1, IrResultOk x', md)
  IrResultErr x ->
    let (t1, x', md) = optIrExpr t0 x
     in (t1, IrResultErr x', md)
  IrResultCase o tagE e tagO s ->
    let
      (t1, o', mdO) = optIrExpr t0 o
      (t2, e', mdE) = optIrExpr t1 e
      (t3, s', mdS) = optIrExpr t2 s
     in
      ( t3
      , IrResultCase o' tagE e' tagO s'
      , nodeMeta mdO (nodeMeta (bindMeta tagE mdE) (bindMeta tagO mdS))
      )
  IrIndex x i ->
    binOptIr t0 IrIndex x i
  IrU8Index x i ->
    binOptIr t0 IrU8Index x i
  IrError x ->
    let (t1, x', md) = optIrExpr t0 x
     in (t1, IrError x', md)
  IrFnLit b ->
    let (t1, b', md) = optIrFnBody t0 b
     in (t1, IrFnLit b', md)
  IrUnsafeNullable x ->
    let (t1, x', md) = optIrExpr t0 x
     in (t1, IrUnsafeNullable x', md)
  IrFrozenLit fs ->
    let (t1, fs', md) = mapAccumIrFieldLit t0 fs
     in (t1, IrFrozenLit fs', md)
  IrGetField @k o ->
    let (t1, o', md) = optIrExpr t0 o
     in (t1, IrGetField @k o', md)
  _ -> error "JShark.Ir.optIrExprChildren: unhandled constructor"

binOptIr ::
  Int
  -> (IrExpr a -> IrExpr b -> IrExpr c)
  -> IrExpr a
  -> IrExpr b
  -> (Int, IrExpr c, IrMeta)
binOptIr !t0 k x y =
  let
    (t1, x', mdX) = optIrExpr t0 x
    (t2, y', mdY) = optIrExpr t1 y
   in
    (t2, k x' y', nodeMeta mdX mdY)

optIrFixedArgs ::
  Int -> IrFixedArgs a b c -> (Int, IrFixedArgs a b c, IrMeta)
optIrFixedArgs !t0 = \case
  IrArgsU x ->
    let (t1, x', md) = optIrExpr t0 x
     in (t1, IrArgsU x', md)
  IrArgsB x y ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, y', mdY) = optIrExpr t1 y
     in
      (t2, IrArgsB x' y', nodeMeta mdX mdY)
  IrArgsT x y z ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, y', mdY) = optIrExpr t1 y
      (t3, z', mdZ) = optIrExpr t2 z
     in
      (t3, IrArgsT x' y' z', nodeMeta mdX (nodeMeta mdY mdZ))

optIrKernel :: Int -> IrKernel u -> (Int, IrKernel u, IrMeta)
optIrKernel !t0 = \case
  KPlus x y -> binOptKernel t0 KPlus x y
  KTimes x y -> binOptKernel t0 KTimes x y
  KMinus x y -> binOptKernel t0 KMinus x y
  KFracDiv x y -> binOptKernel t0 KFracDiv x y
  KRem x y -> binOptKernel t0 KRem x y
  KBitAnd x y -> binOptKernel t0 KBitAnd x y
  KBitOr x y -> binOptKernel t0 KBitOr x y
  KBitXor x y -> binOptKernel t0 KBitXor x y
  KShl x y -> binOptKernel t0 KShl x y
  KShr x y -> binOptKernel t0 KShr x y
  KUShr x y -> binOptKernel t0 KUShr x y
  KBig op x y -> binOptKernel t0 (KBig op) x y
  KBigNeg x ->
    let (t1, x', md) = optIrExpr t0 x
     in (t1, KBigNeg x', md)
  KConcat x y -> binOptKernel t0 KConcat x y
  KShow x ->
    let (t1, x', md) = optIrExpr t0 x
     in (t1, KShow x', md)
  KTypeOf x ->
    let (t1, x', md) = optIrExpr t0 x
     in (t1, KTypeOf x', md)
  KNegate x ->
    let (t1, x', md) = optIrExpr t0 x
     in (t1, KNegate x', md)
  KAnd x y -> binOptKernel t0 KAnd x y
  KOr x y -> binOptKernel t0 KOr x y
  KEq s x y -> binOptKernel t0 (KEq s) x y
  KNEq s x y -> binOptKernel t0 (KNEq s) x y
  KGTh x y -> binOptKernel t0 KGTh x y
  KLTh x y -> binOptKernel t0 KLTh x y
  KGTEq x y -> binOptKernel t0 KGTEq x y
  KLTEq x y -> binOptKernel t0 KLTEq x y

binOptKernel ::
  Int
  -> (IrExpr a -> IrExpr b -> IrKernel c)
  -> IrExpr a
  -> IrExpr b
  -> (Int, IrKernel c, IrMeta)
binOptKernel t0 k x y =
  let
    (t1, x', mdX) = optIrExpr t0 x
    (t2, y', mdY) = optIrExpr t1 y
   in
    (t2, k x' y', nodeMeta mdX mdY)

optIrMethod :: Int -> IrMethod u -> (Int, IrMethod u, IrMeta)
optIrMethod !t0 = \case
  IrMethMap x tag g ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, g', mdG) = optIrExpr t1 g
     in
      (t2, IrMethMap x' tag g', nodeMeta mdX (bindMeta tag mdG))
  IrMethFilter x tag g ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, g', mdG) = optIrExpr t1 g
     in
      (t2, IrMethFilter x' tag g', nodeMeta mdX (bindMeta tag mdG))
  IrMethReduce x z tagA tagB g ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, z', mdZ) = optIrExpr t1 z
      (t3, g', mdG) = optIrExpr t2 g
     in
      ( t3
      , IrMethReduce x' z' tagA tagB g'
      , nodeMeta mdX (nodeMeta mdZ (bindMeta tagA (bindMeta tagB mdG)))
      )
  IrMethReduceRight x z tagA tagB g ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, z', mdZ) = optIrExpr t1 z
      (t3, g', mdG) = optIrExpr t2 g
     in
      ( t3
      , IrMethReduceRight x' z' tagA tagB g'
      , nodeMeta mdX (nodeMeta mdZ (bindMeta tagA (bindMeta tagB mdG)))
      )
  IrMethToSorted x tagA tagB g ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, g', mdG) = optIrExpr t1 g
     in
      ( t2
      , IrMethToSorted x' tagA tagB g'
      , nodeMeta mdX (bindMeta tagA (bindMeta tagB mdG))
      )
  IrMethFrom n tag g ->
    let
      (t1, n', mdN) = optIrExpr t0 n
      (t2, g', mdG) = optIrExpr t1 g
     in
      (t2, IrMethFrom n' tag g', nodeMeta mdN (bindMeta tag mdG))

optIrFnBody :: Int -> IrFnBody us r -> (Int, IrFnBody us r, IrMeta)
optIrFnBody !t0 = \case
  IrJfNil e ->
    let (t1, e', md) = optIrExpr t0 e
     in (t1, IrJfNil e', md)
  IrJfCons tag k ->
    let (t1, k', md) = optIrFnBody (t0 - optStep) k
     in (t1, IrJfCons tag k', bindMeta tag md)

mapAccumIrFieldLit ::
  Int -> [IrFieldLit r] -> (Int, [IrFieldLit r], IrMeta)
mapAccumIrFieldLit !t0 fs =
  foldr
    ( \fl (!t, acc, !md) ->
        let (t', fl', md') = step t fl
         in (t', fl' : acc, md' <> md)
    )
    (t0, [], mempty)
    fs
 where
  step :: Int -> IrFieldLit r -> (Int, IrFieldLit r, IrMeta)
  step !t = \case
    IrFieldLit @k e ->
      let (t', e', md) = optIrExpr t e
       in (t', IrFieldLit @k e', md)
    IrFieldLitEffect @k e ->
      let (t', e', md) = optIrEffect t e
       in (t', IrFieldLitEffect @k e', md)
    IrFieldLitExtra @k e ->
      let (t', e', md) = optIrExpr t e
       in (t', IrFieldLitExtra @k e', md)
    IrFieldLitExtraEffect @k e ->
      let (t', e', md) = optIrEffect t e
       in (t', IrFieldLitExtraEffect @k e', md)

optIrEffect :: Int -> IrEffect u -> (Int, IrEffect u, IrMeta)
optIrEffect !t0 = \case
  IrLift x ->
    let (t1, x', md) = optIrExpr t0 x
     in (t1, IrLift x', md)
  IrBind tag x body ->
    let
      (t1, x', mdX) = optIrEffect t0 x
      (t2, body', mdBody) = optIrEffect t1 body
     in
      let (e', md') = elimIrBind mdX tag x' body' mdBody
       in (t2, e', md')
  IrThenE x y ->
    let
      (t1, x', mdX) = optIrEffect t0 x
      (t2, y', mdY) = optIrEffect t1 y
     in
      (t2, IrThenE x' y', nodeMeta mdX mdY)
  IrFFI n args ->
    let (t1, args', md) = optIrArgs t0 args
     in (t1, IrFFI n args', effectMd md)
  IrUnsafeObject o -> (t0, IrUnsafeObject o, IrMeta 1 IM.empty False False)
  IrUnsafeObjectGet x s ->
    let (t1, x', md) = optIrEffect t0 x
     in (t1, IrUnsafeObjectGet x' s, effectMd md)
  IrUnsafeObjectAssign x y ->
    let
      (t1, x', mdX) = optIrEffect t0 x
      (t2, y', mdY) = optIrEffect t1 y
     in
      (t2, IrUnsafeObjectAssign x' y', effectMd (nodeMeta mdX mdY))
  IrCallMethod x n args ->
    let
      (t1, x', mdX) = optIrEffect t0 x
      (t2, args', mdA) = optIrArgs t1 args
     in
      (t2, IrCallMethod x' n args', effectMd (nodeMeta mdX mdA))
  IrBindRec tag r b ->
    let
      (t1, r', mdR) = optIrEffect (t0 - optStep) r
      (t2, b', mdB) = optIrEffect t1 b
     in
      (t2, IrBindRec tag r' b', bindMeta tag (nodeMeta mdR mdB))
  IrLambdaE tag g ->
    let (t1, g', md) = optIrEffect (t0 - optStep) g
     in (t1, IrLambdaE tag g', bindMeta tag md)
  IrApplyE f x ->
    let
      (t1, f', mdF) = optIrEffect t0 f
      (t2, x', mdX) = optIrEffect t1 x
     in
      (t2, IrApplyE f' x', effectMd (nodeMeta mdF mdX))
  IrIfE c t e ->
    let
      (t1, c', mdC) = optIrEffect t0 c
      (t2, t', mdT) = optIrEffect t1 t
      (t3, e', mdE) = optIrEffect t2 e
     in
      (t3, IrIfE c' t' e', nodeMeta mdC (nodeMeta mdT mdE))
  IrWhile c b ->
    let
      (t1, c', mdC) = optIrEffect t0 c
      (t2, b', mdB) = optIrEffect t1 b
     in
      (t2, IrWhile c' b', effectMd (nodeMeta mdC mdB))
  IrForRange s e tag b ->
    let
      (t1, s', mdS) = optIrExpr t0 s
      (t2, e', mdE) = optIrExpr t1 e
      (t3, b', mdB) = optIrEffect (t2 - optStep) b
     in
      (t3, IrForRange s' e' tag b', nodeMeta mdS (nodeMeta mdE (bindMeta tag mdB)))
  IrU8Set b i v ->
    let
      (t1, b', mdB) = optIrExpr t0 b
      (t2, i', mdI) = optIrExpr t1 i
      (t3, v', mdV) = optIrExpr t2 v
     in
      (t3, IrU8Set b' i' v', nodeMeta mdB (nodeMeta mdI mdV))
  IrU8Fill b v ->
    let
      (t1, b', mdB) = optIrExpr t0 b
      (t2, v', mdV) = optIrExpr t1 v
     in
      (t2, IrU8Fill b' v', nodeMeta mdB mdV)
  IrOptionCaseE o n tag s ->
    let
      (t1, o', mdO) = optIrExpr t0 o
      (t2, n', mdN) = optIrEffect t1 n
      (t3, s', mdS) = optIrEffect (t2 - optStep) s
     in
      (t3, IrOptionCaseE o' n' tag s', nodeMeta mdO (nodeMeta mdN (bindMeta tag mdS)))
  IrResultCaseE o tagE e tagO s ->
    let
      (t1, o', mdO) = optIrExpr t0 o
      (t2, e', mdE) = optIrEffect (t1 - optStep) e
      (t3, s', mdS) = optIrEffect (t2 - optStep) s
     in
      ( t3
      , IrResultCaseE o' tagE e' tagO s'
      , nodeMeta mdO (nodeMeta (bindMeta tagE mdE) (bindMeta tagO mdS))
      )
  IrStringCaseE s arms d ->
    let
      (t1, s', mdS) = optIrExpr t0 s
      (t2, arms', mdA) = mapAccumIrEffect t1 arms
      (t3, d', mdD) = optIrEffect t2 d
     in
      (t3, IrStringCaseE s' arms' d', nodeMeta mdS (nodeMeta mdA mdD))
  IrThrow x ->
    let (t1, x', md) = optIrExpr t0 x
     in (t1, IrThrow x', effectMd md)
  IrTry a tag k ->
    let
      (t1, a', mdA) = optIrEffect t0 a
      (t2, k', mdK) = optIrEffect (t1 - optStep) k
     in
      (t2, IrTry a' tag k', nodeMeta mdA (bindMeta tag mdK))
  IrObjectLit fs ->
    let (t1, fs', md) = mapAccumIrFieldLit t0 fs
     in (t1, IrObjectLit fs', md)
  IrDeleteProp o k ->
    let
      (t1, o', mdO) = optIrEffect t0 o
      (t2, k', mdK) = optIrExpr t1 k
     in
      (t2, IrDeleteProp o' k', effectMd (nodeMeta mdO mdK))
  IrArrayLit es ->
    let (t1, es', md) = mapAccumIrEffects t0 es
     in (t1, IrArrayLit es', md)

mapAccumIrEffect ::
  Int -> [(Text, IrEffect u)] -> (Int, [(Text, IrEffect u)], IrMeta)
mapAccumIrEffect !t0 arms =
  foldr
    ( \(k, e) (!t, acc, !md) ->
        let (t', e', md') = optIrEffect t e
         in (t', (k, e') : acc, md' <> md)
    )
    (t0, [], mempty)
    arms

mapAccumIrEffects :: Int -> [IrEffect u] -> (Int, [IrEffect u], IrMeta)
mapAccumIrEffects !t0 es =
  foldr
    ( \e (!t, acc, !md) ->
        let (t', e', md') = optIrEffect t e
         in (t', e' : acc, md' <> md)
    )
    (t0, [], mempty)
    es

optIrArgs :: Int -> Rec (IrArg) us -> (Int, Rec (IrArg) us, IrMeta)
optIrArgs !t0 = \case
  RecNil -> (t0, RecNil, mempty)
  RecCons (IrArgExpr x) xs ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, xs', mdXs) = optIrArgs t1 xs
     in
      (t2, RecCons (IrArgExpr x') xs', nodeMeta mdX mdXs)
  RecCons (IrArgEffect x) xs ->
    let
      (t1, x', mdX) = optIrEffect t0 x
      (t2, xs', mdXs) = optIrArgs t1 xs
     in
      (t2, RecCons (IrArgEffect x') xs', nodeMeta mdX mdXs)
