{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | First-order IR for optimize + codegen. PHOAS 'Expr'/'Effect' in
-- 'JShark.Api.Types' stay the user-facing syntax; closed terms lower here once.
module JShark.Compiler.Ir
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
  , optIrExpr
  , optIrEffect
  , elimIrBind
  , effectMd
  , nodeMeta
  , bindMeta
  )
where

import Data.Bits (xor, (.&.), (.|.))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Kind (Type)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Monoid (Any (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, (:~:) (Refl))
import GHC.TypeLits (KnownSymbol, sameSymbol)
import JShark.Api.Prim
  ( MathBinary (..)
  , MathUnary (..)
  , exactMathBinary
  , exactMathUnary
  , isFiniteDouble
  , isPureFixed
  , matchMathBinary
  , matchMathUnary
  )
import JShark.Api.Rec (Rec (..))
import JShark.Api.Types
  ( BigBinOp
  , Comparable
  , Expr (Literal)
  , FieldLit (..)
  , FFIForm (..)
  , Field
  , FixedOp (..)
  , LamInfo (..)
  , Universe (..)
  , Value (..)
  )
import JShark.Compiler.Binder (strictFoldMap)
import JShark.Compiler.Evaluate
  ( eqFoldableValue
  , isCheapValue
  , isOrderableValue
  , jsShow
  , parseBigIntString
  , tryEvalBigBin
  , typeOfValue
  , valueCompare
  , valueEq
  )
import JShark.Compiler.JsNum (jsBit2, jsRem, jsShl, jsShr, jsUShr)
import JShark.Compiler.Metadata (optSmall, optStep)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (Bool, lookup)
import qualified Prelude as P

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

-- | Force impure on optimized metadata (empty 'RecNil' args are otherwise pure).
effectMd :: IrMeta -> IrMeta
effectMd !md = md <> IrMeta 0 IM.empty False False

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
  IrLambda :: !Int -> !LamInfo -> IrExpr v -> IrExpr ('Function u v)
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
  IrGetField ::
    forall k r. KnownSymbol k => IrExpr ('Object r) -> IrExpr (Field r k)
  IrHvm2Ref :: Text -> IrExpr u

data IrFnBody :: [Universe] -> Universe -> Type where
  IrJfNil :: IrExpr r -> IrFnBody '[] r
  IrJfCons :: !Int -> !(Maybe Text) -> IrFnBody us r -> IrFnBody (u ': us) r

data IrFieldLit r where
  IrFieldLit :: KnownSymbol k => IrExpr (Field r k) -> IrFieldLit r
  IrFieldLitEffect :: KnownSymbol k => IrEffect (Field r k) -> IrFieldLit r
  IrFieldLitExtra :: (KnownSymbol k, Typeable u) => IrExpr u -> IrFieldLit r
  IrFieldLitExtraEffect ::
    (KnownSymbol k, Typeable u) => IrEffect u -> IrFieldLit r

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
  IrBind :: !Int -> Maybe Text -> IrEffect u -> IrEffect v -> IrEffect v
  IrThenE :: IrEffect u -> IrEffect v -> IrEffect v
  IrBindRec :: !Int -> IrEffect u -> IrEffect v -> IrEffect v
  IrLambdaE :: !Int -> IrEffect v -> IrEffect ('Function u v)
  IrApplyE :: IrEffect ('Function u v) -> IrEffect u -> IrEffect v
  IrIfE :: IrEffect 'Bool -> IrEffect u -> IrEffect u -> IrEffect u
  IrWhile :: IrEffect 'Bool -> IrEffect 'Unit -> IrEffect 'Unit
  IrForRange ::
    IrExpr 'Number -> IrExpr 'Number -> !Int -> IrEffect 'Unit -> IrEffect 'Unit
  IrU8Set ::
    IrExpr 'Uint8Array -> IrExpr 'Number -> IrExpr 'Number -> IrEffect 'Unit
  IrU8Fill :: IrExpr 'Uint8Array -> IrExpr 'Number -> IrEffect 'Unit
  IrOptionCaseE ::
    IrExpr ('Option u) -> IrEffect v -> !Int -> IrEffect v -> IrEffect v
  IrResultCaseE ::
    IrExpr ('Result e a) -> !Int -> IrEffect v -> !Int -> IrEffect v -> IrEffect v
  IrStringCaseE ::
    IrExpr 'String -> [(Text, IrEffect v)] -> IrEffect v -> IrEffect v
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
  IrLiteral v -> IrMeta 1 IM.empty True (isCheapValue v)
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
occursIrExpr !t expr = case expr of
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
lazyOccursIrExpr !t expr = case expr of
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
lazyOccursIrEffect !t eff = case eff of
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
foldIrExpr se le sf expr = case expr of
  IrLiteral {} -> mempty
  IrVar {} -> mempty
  IrEmbedEff e -> sf e
  IrLet _ x g -> se x <> se g
  IrLetRec _ r b -> se r <> se b
  IrLambda _ _ g -> le g
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
  IrFrozenLit fs -> strictFoldMap (foldIrFieldLit se sf) fs
  IrGetField o -> se o
  IrHvm2Ref {} -> mempty

foldIrKernel ::
  Monoid m =>
  (forall v. IrExpr v -> m)
  -> (forall v. IrExpr v -> m)
  -> IrKernel u
  -> m
foldIrKernel se le k = case k of
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
foldIrMethod se le m = case m of
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
foldIrFixedArgs se a = case a of
  IrArgsU x -> se x
  IrArgsB x y -> se x <> se y
  IrArgsT x y z -> se x <> se y <> se z

foldIrFnBody ::
  Monoid m =>
  (forall v. IrExpr v -> m)
  -> IrFnBody us r
  -> m
foldIrFnBody le b = case b of
  IrJfNil e -> le e
  IrJfCons _ _ (IrJfNil e) -> le e
  IrJfCons _ _ k -> foldIrFnBody le k

foldIrFieldLit ::
  (forall v. IrExpr v -> m)
  -> (forall v. IrEffect v -> m)
  -> IrFieldLit r
  -> m
foldIrFieldLit se sf fl = case fl of
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
foldIrEff se sf lf eff = case eff of
  IrLift x -> se x
  IrFFI _ args -> recFoldIrArg se sf args
  IrUnsafeObject {} -> mempty
  IrUnsafeObjectGet x _ -> sf x
  IrUnsafeObjectAssign x y -> sf x <> sf y
  IrCallMethod x _ args -> sf x <> recFoldIrArg se sf args
  IrBind _ _ x g -> sf x <> sf g
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
  IrStringCaseE s arms d ->
    se s <> strictFoldMap (\(_, e) -> sf e) arms <> sf d
  IrThrow x -> se x
  IrTry a _ k -> sf a <> sf k
  IrObjectLit fs -> strictFoldMap (foldIrFieldLit se sf) fs
  IrDeleteProp o k -> sf o <> se k
  IrArrayLit es -> strictFoldMap sf es

recFoldIrArg ::
  forall m us.
  Monoid m =>
  (forall v. IrExpr v -> m)
  -> (forall v. IrEffect v -> m)
  -> Rec (IrArg) us
  -> m
recFoldIrArg se sf args = go mempty args
 where
  go :: forall vs. m -> Rec IrArg vs -> m
  go !acc RecNil = acc
  go !acc (RecCons (IrArgExpr x) xs) = go (acc <> se x) xs
  go !acc (RecCons (IrArgEffect x) xs) = go (acc <> sf x) xs

mapIrExpr ::
  (forall v. IrExpr v -> IrExpr v)
  -> (forall v. IrEffect v -> IrEffect v)
  -> IrExpr u
  -> IrExpr u
mapIrExpr ge gf expr = case expr of
  IrLiteral v -> IrLiteral v
  IrVar i -> IrVar i
  IrEmbedEff e -> IrEmbedEff (gf e)
  IrLet tag x g -> IrLet tag (ge x) (ge g)
  IrLetRec tag r b -> IrLetRec tag (ge r) (ge b)
  IrLambda tag hoist g -> IrLambda tag hoist (ge g)
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
mapIrKernel ge k = case k of
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
mapIrMethod ge m = case m of
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
mapIrFixedArgs ge a = case a of
  IrArgsU x -> IrArgsU (ge x)
  IrArgsB x y -> IrArgsB (ge x) (ge y)
  IrArgsT x y z -> IrArgsT (ge x) (ge y) (ge z)

mapIrFnBody ::
  (forall v. IrExpr v -> IrExpr v)
  -> IrFnBody us r
  -> IrFnBody us r
mapIrFnBody ge b = case b of
  IrJfNil e -> IrJfNil (ge e)
  IrJfCons tag pn k -> IrJfCons tag pn (mapIrFnBody ge k)

mapIrFieldLit ::
  (forall v. IrExpr v -> IrExpr v)
  -> (forall v. IrEffect v -> IrEffect v)
  -> IrFieldLit r
  -> IrFieldLit r
mapIrFieldLit ge gf fl = case fl of
  IrFieldLit @k e -> IrFieldLit @k (ge e)
  IrFieldLitEffect @k e -> IrFieldLitEffect @k (gf e)
  IrFieldLitExtra @k e -> IrFieldLitExtra @k (ge e)
  IrFieldLitExtraEffect @k e -> IrFieldLitExtraEffect @k (gf e)

mapIrEff ::
  (forall v. IrExpr v -> IrExpr v)
  -> (forall v. IrEffect v -> IrEffect v)
  -> IrEffect u
  -> IrEffect u
mapIrEff ge gf eff = case eff of
  IrLift x -> IrLift (ge x)
  IrFFI n args -> IrFFI n (mapIrArgs gf ge args)
  IrUnsafeObject o -> IrUnsafeObject o
  IrUnsafeObjectGet x s -> IrUnsafeObjectGet (gf x) s
  IrUnsafeObjectAssign x y -> IrUnsafeObjectAssign (gf x) (gf y)
  IrCallMethod x n args -> IrCallMethod (gf x) n (mapIrArgs gf ge args)
  IrBind tag hint x g -> IrBind tag hint (gf x) (gf g)
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
mapIrArgs gf ge args = case args of
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
replaceIrVarExpr !tag bound expr = case expr of
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
inlineIrEffectInExpr !tag bound expr = case expr of
  IrLift e -> IrLift (inlineIrExprInEff tag bound e)
  e -> inlineIrEffect tag bound e

inlineIrExprInEff :: Int -> IrEffect u -> IrExpr v -> IrExpr v
inlineIrExprInEff !tag bound expr = case expr of
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
  (?keepLets :: P.Bool) =>
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
    preserve =
      not (isIrLambda x) && not (isIdentityIrExpr tag body)
   in
    case uses of
      0 | irPure mdX -> (body, closed)
      0 -> (IrLet tag x body, nodeMeta mdX closed)
      1
        | ?keepLets && preserve ->
            (IrLet tag x body, nodeMeta mdX closed)
      1
        | irSize mdBody <= optSmall
        , once ->
            (inlineIrExpr tag x body, spliced)
      _
        | irCheap mdX
        , irSize mdBody <= optSmall ->
            (inlineIrExpr tag x body, spliced)
      _ -> (IrLet tag x body, nodeMeta mdX closed)

isIrLambda :: IrExpr u -> P.Bool
isIrLambda = \case
  IrLambda {} -> True
  _ -> False

isIrLambdaE :: IrEffect u -> P.Bool
isIrLambdaE = \case
  IrLambdaE {} -> True
  _ -> False

isIdentityIrExpr :: Int -> IrExpr u -> P.Bool
isIdentityIrExpr tag = \case
  IrVar i -> i == tag
  _ -> False

isIdentityIrEffect :: Int -> IrEffect u -> P.Bool
isIdentityIrEffect tag = \case
  IrLift x -> isIdentityIrExpr tag x
  _ -> False

elimIrBind ::
  (?keepLets :: P.Bool) =>
  IrMeta
  -> Maybe Text
  -> Int
  -> IrEffect u
  -> IrEffect v
  -> IrMeta
  -> (IrEffect v, IrMeta)
elimIrBind !mdX !hint !tag !x !body !mdBody =
  let
    uses = IM.findWithDefault 0 tag (irFree mdBody)
    closed = bindMeta tag mdBody
    spliced = closed <> mdX
    once =
      isAliasIrEffect x
        P.|| irCheap mdX
        P.|| not (lazyOccursIrEffect tag body)
    preserve =
      not (isIrLambdaE x)
        && not (isIdentityIrEffect tag body)
        && not (isAliasIrEffect x)
   in
    case uses of
      0 | irPure mdX, not (isAliasIrEffect x) -> (body, closed)
      0 -> (IrThenE x body, nodeMeta mdX closed)
      1
        | ?keepLets && preserve ->
            (IrBind tag hint x body, nodeMeta mdX closed)
      1
        | irSize mdBody <= optSmall
        , once ->
            (inlineIrEffect tag x body, spliced)
      _
        | irCheap mdX
        , irSize mdBody <= optSmall ->
            (inlineIrEffect tag x body, spliced)
      _ -> (IrBind tag hint x body, nodeMeta mdX closed)

nodeMeta :: IrMeta -> IrMeta -> IrMeta
nodeMeta !mdX !mdY =
  IrMeta 1 IM.empty (irPure mdX && irPure mdY) False <> mdX <> mdY

-- | Close a binder: its tag is no longer free above this node. Without
-- this the free map grows to every tag in the subtree, and the union in
-- '<>' then costs the whole program at every node.
bindMeta :: Int -> IrMeta -> IrMeta
bindMeta !tag !md = md {irFree = IM.delete tag (irFree md)}

optIrExpr :: (?keepLets :: P.Bool) => Int -> IrExpr u -> (Int, IrExpr u, IrMeta)
optIrExpr !t0 expr = case expr of
  IrLiteral v -> (t0, IrLiteral v, litMeta v)
  IrVar i -> (t0, IrVar i, varMeta i)
  IrLet tag x body ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, body', mdBody) = optIrExpr t1 body
     in
      let
        (e', md') = elimIrLet mdX tag x' body' mdBody
       in
        (t2, e', md')
  -- Named hoists (@Just@ tag) always stay as calls so codegen can emit one
  -- shared helper (e.g. @$groupBy@). Literal partial application would
  -- beta into an untagged inner lambda and duplicate the body at each site.
  IrApply (IrLambda bindTag info@LamInfo {lamTag = Just _} g) x ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, g', mdG) = optIrExpr t1 g
     in
      ( t2
      , IrApply (IrLambda bindTag info g') x'
      , nodeMeta mdX mdG
      )
  IrApply (IrLambda tag LamInfo {lamTag = Nothing} g) x ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, g', mdG) = optIrExpr t1 g
     in
      let
        (e', md') = elimIrLet mdX tag x' g' mdG
       in
        (t2, e', md')
  IrFixed op args ->
    let
      (t1, e', md) = optIrFixedF t0 op args
     in
      (t1, e', md)
  IrKernelK k ->
    let
      (t1, e', md) = optIrKernelF t0 k
     in
      (t1, e', md)
  IrMethod m ->
    let
      (t1, m', md) = optIrMethod t0 m
     in
      (t1, IrMethod m', md)
  IrHvm2Ref name ->
    (t0, IrHvm2Ref name, IrMeta 1 IM.empty True True)
  e ->
    let
      (t1, e', md) = optIrExprChildren t0 e
     in
      (t1, e', md)

litMeta :: Value u -> IrMeta
litMeta v = IrMeta 1 IM.empty True (isCheapValue v)

varMeta :: Int -> IrMeta
varMeta !i = IrMeta 1 (IM.singleton i 1) True True

optIrExprChildren ::
  (?keepLets :: P.Bool) => Int -> IrExpr u -> (Int, IrExpr u, IrMeta)
optIrExprChildren !t0 expr = case expr of
  IrEmbedEff e ->
    let
      (t1, e', md) = optIrEffect t0 e
     in
      (t1, IrEmbedEff e', md)
  IrLetRec tag r b ->
    let
      (t1, r', mdR) = optIrExpr t0 r
      (t2, b', mdB) = optIrExpr t1 b
     in
      (t2, IrLetRec tag r' b', bindMeta tag (nodeMeta mdR mdB))
  IrLambda tag hoist g ->
    let
      (t1, g', md) = optIrExpr t0 g
     in
      (t1, IrLambda tag hoist g', bindMeta tag md)
  IrApply f x ->
    let
      (t1, f', mdF) = optIrExpr t0 f
      (t2, x', mdX) = optIrExpr t1 x
     in
      (t2, IrApply f' x', nodeMeta mdF mdX)
  IrIf c t e ->
    let
      (t1, c', mdC) = optIrExpr t0 c
     in
      case c' of
        IrLiteral (ValueBool P.True) -> optIrExpr t1 t
        IrLiteral (ValueBool P.False) -> optIrExpr t1 e
        _ ->
          let
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
     in
      case peelIrOption o' of
        Just Nothing -> optIrExpr t1 n
        Just (Just v) ->
          let
            (t2, s', mdS) = optIrExpr t1 s
            (e', md') = elimIrLet (litMeta v) tag (IrLiteral v) s' mdS
           in
            (t2, e', md')
        Nothing ->
          let
            (t2, n', mdN) = optIrExpr t1 n
            (t3, s', mdS) = optIrExpr t2 s
           in
            ( t3
            , IrOptionCase o' n' tag s'
            , nodeMeta mdO (nodeMeta mdN (bindMeta tag mdS))
            )
  IrResultOk x ->
    let
      (t1, x', md) = optIrExpr t0 x
     in
      (t1, IrResultOk x', md)
  IrResultErr x ->
    let
      (t1, x', md) = optIrExpr t0 x
     in
      (t1, IrResultErr x', md)
  IrResultCase o tagE e tagO s ->
    let
      (t1, o', mdO) = optIrExpr t0 o
     in
      case peelIrResult o' of
        Just (Left x) ->
          let
            (t2, e'', mdE) = optIrExpr t1 e
            (res, md') = elimIrLet (boundIrMeta x) tagE x e'' mdE
           in
            (t2, res, md')
        Just (Right x) ->
          let
            (t2, s', mdS) = optIrExpr t1 s
            (res, md') = elimIrLet (boundIrMeta x) tagO x s' mdS
           in
            (t2, res, md')
        Nothing ->
          let
            (t2, e', mdE) = optIrExpr t1 e
            (t3, s', mdS) = optIrExpr t2 s
           in
            ( t3
            , IrResultCase o' tagE e' tagO s'
            , nodeMeta mdO (nodeMeta (bindMeta tagE mdE) (bindMeta tagO mdS))
            )
  IrIndex x i ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, i', mdI) = optIrExpr t1 i
     in
      case (x', i') of
        (IrIndex {}, _) -> (t2, IrIndex x' i', nodeMeta mdX mdI)
        (IrLiteral (ValueArray vs), IrLiteral (ValueNumber d))
          | isFiniteDouble d
          , let
              n = truncate d :: Int
          , n >= 0 && n < length vs ->
              let
                v = vs !! n
               in
                (t2, IrLiteral v, litMeta v <> mdX <> mdI)
        _ -> (t2, IrIndex x' i', nodeMeta mdX mdI)
  IrU8Index x i ->
    binOptIr t0 IrU8Index x i
  IrError x ->
    let
      (t1, x', md) = optIrExpr t0 x
     in
      (t1, IrError x', md)
  IrFnLit b ->
    let
      (t1, b', md) = optIrFnBody t0 b
     in
      (t1, IrFnLit b', md)
  IrUnsafeNullable x ->
    let
      (t1, x', md) = optIrExpr t0 x
     in
      (t1, IrUnsafeNullable x', md)
  IrFrozenLit fs ->
    let
      (t1, fs', md) = mapAccumIrFieldLit t0 fs
     in
      (t1, IrFrozenLit fs', md)
  IrGetField @k o ->
    let
      (t1, o', mdO) = optIrExpr t0 o
     in
      case o' of
        -- Project only when every sibling field is pure, so projecting
        -- @.b@ cannot DCE an effectful @.a@.
        IrFrozenLit fs
          | irPure mdO
          , Just fld <- lookupIrField @k fs ->
              optIrExpr t1 fld
        _ -> (t1, IrGetField @k o', mdO)
  _ -> error "JShark.Compiler.Ir.optIrExprChildren: unhandled constructor"

binOptIr ::
  (?keepLets :: P.Bool) =>
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

-- | Optimize fixed-op args; a fold can escape to an 'IrLiteral'. The
-- node contributes @1 / pure-is-@isPureFixed@ / cheap@, matching
-- 'metaIrExpr'.
optIrFixedF ::
  (?keepLets :: P.Bool) =>
  Int -> FixedOp a b c u -> IrFixedArgs a b c -> (Int, IrExpr u, IrMeta)
optIrFixedF !t0 op args = case (op, args) of
  (n, IrArgsU x)
    | Just (MathUnary n') <- matchMathUnary n ->
        let
          (t1, x', mdX) = optIrExpr t0 x
          res = case x' of
            IrLiteral (ValueNumber a)
              | Just r <- exactMathUnary n' a -> IrLiteral (ValueNumber r)
            _ -> IrFixed n' (IrArgsU x')
         in
          (t1, res, fixedFoldMd n' res <> mdX)
  (n, IrArgsB x y)
    | Just (MathBinary n') <- matchMathBinary n ->
        let
          (t1, x', mdX) = optIrExpr t0 x
          (t2, y', mdY) = optIrExpr t1 y
          res = case (x', y') of
            (IrLiteral (ValueNumber a), IrLiteral (ValueNumber b))
              | Just r <- exactMathBinary n' a b -> IrLiteral (ValueNumber r)
            _ -> IrFixed n' (IrArgsB x' y')
         in
          (t2, res, fixedFoldMd n' res <> nodeMeta mdX mdY)
  (FixArrLen, IrArgsU x) ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      res = case x' of
        IrLiteral (ValueArray vs) ->
          IrLiteral (ValueNumber (fromIntegral (length vs)))
        _ -> IrFixed op (IrArgsU x')
     in
      (t1, res, fixedFoldMd op res <> mdX)
  (FixToBigInt, IrArgsU x) ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      res = case x' of
        IrLiteral (ValueNumber d)
          | isFiniteDouble d
          , let
              n = truncate d
          , d == fromInteger n ->
              IrLiteral (ValueBigInt n)
        _ -> IrFixed op (IrArgsU x')
     in
      (t1, res, fixedFoldMd op res <> mdX)
  (FixFromBigInt, IrArgsU x) ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      res = case x' of
        IrLiteral (ValueBigInt n) -> IrLiteral (ValueNumber (fromInteger n))
        _ -> IrFixed op (IrArgsU x')
     in
      (t1, res, fixedFoldMd op res <> mdX)
  (FixParseBigInt, IrArgsU x) ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      res = case x' of
        IrLiteral (ValueString s)
          | Just n <- parseBigIntString (T.unpack s) ->
              IrLiteral (ValueBigInt n)
        _ -> IrFixed op (IrArgsU x')
     in
      (t1, res, fixedFoldMd op res <> mdX)
  (_, IrArgsU x) ->
    let
      (t1, x', mdX) = optIrExpr t0 x
     in
      (t1, IrFixed op (IrArgsU x'), fixedKeepMd op <> mdX)
  (_, IrArgsB x y) ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, y', mdY) = optIrExpr t1 y
     in
      (t2, IrFixed op (IrArgsB x' y'), fixedKeepMd op <> nodeMeta mdX mdY)
  (_, IrArgsT x y z) ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, y', mdY) = optIrExpr t1 y
      (t3, z', mdZ) = optIrExpr t2 z
     in
      ( t3
      , IrFixed op (IrArgsT x' y' z')
      , fixedKeepMd op <> nodeMeta mdX (nodeMeta mdY mdZ)
      )

fixedFoldMd :: FixedOp a b c u -> IrExpr u -> IrMeta
fixedFoldMd n res = case res of
  IrLiteral v -> litMeta v
  _ -> fixedKeepMd n

fixedKeepMd :: FixedOp a b c u -> IrMeta
fixedKeepMd n = IrMeta 1 IM.empty (isPureFixed n) True

-- | Optimize a kernel node; a fold can escape the kernel entirely
-- (two literals under 'KPlus' become one 'IrLiteral').
optIrKernelF ::
  (?keepLets :: P.Bool) => Int -> IrKernel u -> (Int, IrExpr u, IrMeta)
optIrKernelF !t0 k = case k of
  KPlus x y -> num2OptIr t0 (+) KPlus x y
  KTimes x y -> num2OptIr t0 (*) KTimes x y
  KMinus x y -> num2OptIr t0 (-) KMinus x y
  KFracDiv x y -> num2OptIr t0 (/) KFracDiv x y
  KRem x y -> num2OptIr t0 jsRem KRem x y
  KBitAnd x y -> num2OptIr t0 (jsBit2 (.&.)) KBitAnd x y
  KBitOr x y -> num2OptIr t0 (jsBit2 (.|.)) KBitOr x y
  KBitXor x y -> num2OptIr t0 (jsBit2 xor) KBitXor x y
  KShl x y -> num2OptIr t0 jsShl KShl x y
  KShr x y -> num2OptIr t0 jsShr KShr x y
  KUShr x y -> num2OptIr t0 jsUShr KUShr x y
  KNegate x -> num1OptIr t0 negate KNegate x
  KBig op x y ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, y', mdY) = optIrExpr t1 y
      res = case (x', y') of
        (IrLiteral (ValueBigInt a), IrLiteral (ValueBigInt b))
          | Just r <- tryEvalBigBin op a b -> IrLiteral (ValueBigInt r)
        _ -> IrKernelK (KBig op x' y')
     in
      (t2, res, kernelFoldMd res <> nodeMeta mdX mdY)
  KBigNeg x ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      res = case x' of
        IrLiteral (ValueBigInt n) -> IrLiteral (ValueBigInt (negate n))
        _ -> IrKernelK (KBigNeg x')
     in
      (t1, res, kernelFoldMd res <> mdX)
  KConcat x y ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      (t2, y', mdY) = optIrExpr t1 y
      res = case (x', y') of
        (IrLiteral (ValueString a), IrLiteral (ValueString b)) ->
          IrLiteral (ValueString (a <> b))
        _ -> IrKernelK (KConcat x' y')
     in
      (t2, res, kernelFoldMd res <> nodeMeta mdX mdY)
  KShow x ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      -- Function values have no compile-time show.
      res = case x' of
        IrLiteral (ValueFunction _) -> IrKernelK (KShow x')
        IrLiteral v -> IrLiteral (ValueString (jsShow v))
        _ -> IrKernelK (KShow x')
     in
      (t1, res, kernelFoldMd res <> mdX)
  KTypeOf x ->
    let
      (t1, x', mdX) = optIrExpr t0 x
      res = case x' of
        IrLiteral v -> IrLiteral (ValueString (typeOfValue v))
        _ -> IrKernelK (KTypeOf x')
     in
      (t1, res, kernelFoldMd res <> mdX)
  KAnd x y ->
    let
      (t1, x', mdX) = optIrExpr t0 x
     in
      case x' of
        IrLiteral (ValueBool P.False) ->
          (t1, IrLiteral (ValueBool P.False), litMeta (ValueBool P.False) <> mdX)
        IrLiteral (ValueBool P.True) -> optIrExpr t1 y
        _ ->
          let
            (t2, y', mdY) = optIrExpr t1 y
           in
            case y' of
              -- @x && true@ is @x@ (kept with its own metadata).
              IrLiteral (ValueBool P.True) -> (t2, x', mdX)
              IrLiteral (ValueBool P.False)
                -- @x && false@ is @false@ only when @x@ is pure; the
                -- JS @&&@ never evaluates the RHS, but an impure @x@
                -- must keep its effect.
                | irPure mdX ->
                    (t2, IrLiteral (ValueBool P.False), litMeta (ValueBool P.False) <> mdX)
              _ ->
                (t2, IrKernelK (KAnd x' y'), kernelFoldMdK <> nodeMeta mdX mdY)
  KOr x y ->
    let
      (t1, x', mdX) = optIrExpr t0 x
     in
      case x' of
        IrLiteral (ValueBool P.True) ->
          (t1, IrLiteral (ValueBool P.True), litMeta (ValueBool P.True) <> mdX)
        IrLiteral (ValueBool P.False) -> optIrExpr t1 y
        _ ->
          let
            (t2, y', mdY) = optIrExpr t1 y
           in
            case y' of
              IrLiteral (ValueBool P.False) -> (t2, x', mdX)
              IrLiteral (ValueBool P.True)
                | irPure mdX ->
                    (t2, IrLiteral (ValueBool P.True), litMeta (ValueBool P.True) <> mdX)
              _ ->
                (t2, IrKernelK (KOr x' y'), kernelFoldMdK <> nodeMeta mdX mdY)
  KEq s x y -> eqNeqOptIr t0 (KEq s) valueEq x y
  KNEq s x y -> eqNeqOptIr t0 (KNEq s) (\a b -> P.not (valueEq a b)) x y
  KGTh x y -> ordOptIr t0 KGTh (== GT) x y
  KLTh x y -> ordOptIr t0 KLTh (== LT) x y
  KGTEq x y -> ordOptIr t0 KGTEq (/= LT) x y
  KLTEq x y -> ordOptIr t0 KLTEq (/= GT) x y

kernelFoldMd :: IrExpr u -> IrMeta
kernelFoldMd res = case res of
  IrLiteral v -> litMeta v
  _ -> kernelFoldMdK

kernelFoldMdK :: IrMeta
kernelFoldMdK = IrMeta 1 IM.empty True False

num2OptIr ::
  (?keepLets :: P.Bool) =>
  Int
  -> (Double -> Double -> Double)
  -> (IrExpr 'Number -> IrExpr 'Number -> IrKernel 'Number)
  -> IrExpr 'Number
  -> IrExpr 'Number
  -> (Int, IrExpr 'Number, IrMeta)
num2OptIr !t0 f kon x y =
  let
    (t1, x', mdX) = optIrExpr t0 x
    (t2, y', mdY) = optIrExpr t1 y
    res = case (x', y') of
      (IrLiteral (ValueNumber a), IrLiteral (ValueNumber b)) ->
        IrLiteral (ValueNumber (f a b))
      _ -> IrKernelK (kon x' y')
   in
    (t2, res, kernelFoldMd res <> nodeMeta mdX mdY)

num1OptIr ::
  (?keepLets :: P.Bool) =>
  Int
  -> (Double -> Double)
  -> (IrExpr 'Number -> IrKernel 'Number)
  -> IrExpr 'Number
  -> (Int, IrExpr 'Number, IrMeta)
num1OptIr !t0 f kon x =
  let
    (t1, x', mdX) = optIrExpr t0 x
    res = case x' of
      IrLiteral (ValueNumber a) -> IrLiteral (ValueNumber (f a))
      _ -> IrKernelK (kon x')
   in
    (t1, res, kernelFoldMd res <> mdX)

eqNeqOptIr ::
  (?keepLets :: P.Bool) =>
  Int
  -> (IrExpr u -> IrExpr u -> IrKernel 'Bool)
  -> (Value u -> Value u -> P.Bool)
  -> IrExpr u
  -> IrExpr u
  -> (Int, IrExpr 'Bool, IrMeta)
eqNeqOptIr !t0 kon cmp x y =
  let
    (t1, x', mdX) = optIrExpr t0 x
    (t2, y', mdY) = optIrExpr t1 y
    res = case (x', y') of
      (IrLiteral a, IrLiteral b)
        | eqFoldableValue a && eqFoldableValue b ->
            IrLiteral (ValueBool (cmp a b))
      (IrFrozenLit as, IrFrozenLit bs)
        | Just as' <- peelIrFrozen as
        , Just bs' <- peelIrFrozen bs ->
            IrLiteral (ValueBool (cmp (ValueFrozen as') (ValueFrozen bs')))
      _ -> IrKernelK (kon x' y')
   in
    (t2, res, kernelFoldMd res <> nodeMeta mdX mdY)

ordOptIr ::
  (?keepLets :: P.Bool) =>
  Int
  -> (IrExpr u -> IrExpr u -> IrKernel 'Bool)
  -> (Ordering -> P.Bool)
  -> IrExpr u
  -> IrExpr u
  -> (Int, IrExpr 'Bool, IrMeta)
ordOptIr !t0 kon cmp x y =
  let
    (t1, x', mdX) = optIrExpr t0 x
    (t2, y', mdY) = optIrExpr t1 y
    res = case (x', y') of
      (IrLiteral a, IrLiteral b)
        | isOrderableValue a && isOrderableValue b ->
            IrLiteral (ValueBool (cmp (valueCompare a b)))
      _ -> IrKernelK (kon x' y')
   in
    (t2, res, kernelFoldMd res <> nodeMeta mdX mdY)

-- | Known-constructor scrutinees for 'IrOptionCase' \/ 'IrOptionCaseE'.
peelIrOption :: IrExpr ('Option u) -> Maybe (Maybe (Value u))
peelIrOption = \case
  IrLiteral (ValueOption Nothing) -> Just Nothing
  IrLiteral (ValueOption (Just v)) -> Just (Just v)
  -- Host literals are never JS null; FFI / vars stay unpeeled so
  -- 'Storage.getItem' keeps its @=== null@ check.
  IrUnsafeNullable (IrLiteral v) -> Just (Just v)
  _ -> Nothing

-- | Known-constructor scrutinees for 'IrResultCase' \/ 'IrResultCaseE'.
peelIrResult :: IrExpr ('Result e a) -> Maybe (Either (IrExpr e) (IrExpr a))
peelIrResult = \case
  IrLiteral (ValueResult (Left v)) -> Just (Left (IrLiteral v))
  IrLiteral (ValueResult (Right v)) -> Just (Right (IrLiteral v))
  IrResultOk x -> Just (Right x)
  IrResultErr x -> Just (Left x)
  _ -> Nothing

-- | Metadata of a case-bound payload.
boundIrMeta :: IrExpr u -> IrMeta
boundIrMeta x = case x of
  IrLiteral v -> litMeta v
  _ -> metaIrExpr x

peelIrFrozen :: [IrFieldLit r] -> Maybe [FieldLit Value r]
peelIrFrozen = traverse $ \case
  IrFieldLit @k (IrLiteral v) -> Just (FieldLit @k (Literal v))
  IrFieldLit _ -> Nothing
  IrFieldLitExtra @k (IrLiteral v) -> Just (FieldLitExtra @k (Literal v))
  IrFieldLitExtra _ -> Nothing
  IrFieldLitEffect {} -> Nothing
  IrFieldLitExtraEffect {} -> Nothing

-- | Last-wins field lookup, mirroring 'JShark.Compiler.Flatten.lookupField'.
lookupIrField ::
  forall k r. KnownSymbol k => [IrFieldLit r] -> Maybe (IrExpr (Field r k))
lookupIrField = go . reverse
 where
  go [] = Nothing
  go (IrFieldLit @k' e : rest) = case sameSymbol (Proxy @k) (Proxy @k') of
    Just Refl -> Just e
    Nothing -> go rest
  go (_ : rest) = go rest



optIrMethod ::
  (?keepLets :: P.Bool) => Int -> IrMethod u -> (Int, IrMethod u, IrMeta)
optIrMethod !t0 m = case m of
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

optIrFnBody ::
  (?keepLets :: P.Bool) => Int -> IrFnBody us r -> (Int, IrFnBody us r, IrMeta)
optIrFnBody !t0 b = case b of
  IrJfNil e ->
    let
      (t1, e', md) = optIrExpr t0 e
     in
      (t1, IrJfNil e', md)
  IrJfCons tag pn k ->
    let
      (t1, k', md) = optIrFnBody (t0 - optStep) k
     in
      (t1, IrJfCons tag pn k', bindMeta tag md)

mapAccumIrFieldLit ::
  (?keepLets :: P.Bool) =>
  Int -> [IrFieldLit r] -> (Int, [IrFieldLit r], IrMeta)
mapAccumIrFieldLit !t0 fs =
  foldr
    ( \fl (!t, acc, !md) ->
        let
          (t', fl', md') = step t fl
         in
          (t', fl' : acc, md' <> md)
    )
    (t0, [], mempty)
    fs
 where
  step ::
    (?keepLets :: P.Bool) => Int -> IrFieldLit r -> (Int, IrFieldLit r, IrMeta)
  step !t = \case
    IrFieldLit @k e ->
      let
        (t', e', md) = optIrExpr t e
       in
        (t', IrFieldLit @k e', md)
    IrFieldLitEffect @k e ->
      let
        (t', e', md) = optIrEffect t e
       in
        (t', IrFieldLitEffect @k e', md)
    IrFieldLitExtra @k e ->
      let
        (t', e', md) = optIrExpr t e
       in
        (t', IrFieldLitExtra @k e', md)
    IrFieldLitExtraEffect @k e ->
      let
        (t', e', md) = optIrEffect t e
       in
        (t', IrFieldLitExtraEffect @k e', md)

optIrEffect ::
  (?keepLets :: P.Bool) => Int -> IrEffect u -> (Int, IrEffect u, IrMeta)
optIrEffect !t0 eff = case eff of
  IrLift x ->
    let
      (t1, x', md) = optIrExpr t0 x
     in
      (t1, IrLift x', md)
  IrBind tag hint x body ->
    let
      (t1, x', mdX) = optIrEffect t0 x
      (t2, body', mdBody) = optIrEffect t1 body
     in
      let
        (e', md') = elimIrBind mdX hint tag x' body' mdBody
       in
        (t2, e', md')
  IrThenE x y ->
    let
      (t1, x', mdX) = optIrEffect t0 x
      (t2, y', mdY) = optIrEffect t1 y
     in
      (t2, IrThenE x' y', nodeMeta mdX mdY)
  IrFFI n args ->
    let
      (t1, args', md) = optIrArgs t0 args
     in
      (t1, IrFFI n args', effectMd md)
  IrUnsafeObject o -> (t0, IrUnsafeObject o, IrMeta 1 IM.empty False False)
  IrUnsafeObjectGet x s ->
    let
      (t1, x', md) = optIrEffect t0 x
     in
      (t1, IrUnsafeObjectGet x' s, effectMd md)
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
    let
      (t1, g', md) = optIrEffect (t0 - optStep) g
     in
      (t1, IrLambdaE tag g', bindMeta tag md)
  IrApplyE f x ->
    let
      (t1, f', mdF) = optIrEffect t0 f
     in
      case f' of
        IrLambdaE tag g ->
          let
            (t2, x', mdX) = optIrEffect t1 x
            (t3, g', mdG) = optIrEffect t2 g
            (e', md') = elimIrBind mdX Nothing tag x' g' mdG
           in
            (t3, e', md')
        _ ->
          let
            (t2, x', mdX) = optIrEffect t1 x
           in
            (t2, IrApplyE f' x', effectMd (nodeMeta mdF mdX))
  IrIfE c t e ->
    let
      (t1, c', mdC) = optIrEffect t0 c
     in
      case c' of
        IrLift (IrLiteral (ValueBool P.True)) -> optIrEffect t1 t
        IrLift (IrLiteral (ValueBool P.False)) -> optIrEffect t1 e
        _ ->
          let
            (t2, t', mdT) = optIrEffect t1 t
            (t3, e', mdE) = optIrEffect t2 e
           in
            (t3, IrIfE c' t' e', nodeMeta mdC (nodeMeta mdT mdE))
  IrWhile c b ->
    let
      (t1, c', mdC) = optIrEffect t0 c
     in
      case c' of
        IrLift (IrLiteral (ValueBool P.False)) ->
          (t1, IrLift (IrLiteral ValueUnit), litMeta ValueUnit <> mdC)
        _ ->
          let
            (t2, b', mdB) = optIrEffect t1 b
           in
            (t2, IrWhile c' b', effectMd (nodeMeta mdC mdB))
  IrForRange s e tag b ->
    let
      (t1, s', mdS) = optIrExpr t0 s
      (t2, e', mdE) = optIrExpr t1 e
      (t3, b', mdB) = optIrEffect (t2 - optStep) b
     in
      ( t3
      , IrForRange s' e' tag b'
      , effectMd (nodeMeta mdS (nodeMeta mdE (bindMeta tag mdB)))
      )
  IrU8Set b i v ->
    let
      (t1, b', mdB) = optIrExpr t0 b
      (t2, i', mdI) = optIrExpr t1 i
      (t3, v', mdV) = optIrExpr t2 v
     in
      (t3, IrU8Set b' i' v', effectMd (nodeMeta mdB (nodeMeta mdI mdV)))
  IrU8Fill b v ->
    let
      (t1, b', mdB) = optIrExpr t0 b
      (t2, v', mdV) = optIrExpr t1 v
     in
      (t2, IrU8Fill b' v', effectMd (nodeMeta mdB mdV))
  IrOptionCaseE o n tag s ->
    let
      (t1, o', mdO) = optIrExpr t0 o
     in
      case peelIrOption o' of
        Just Nothing -> optIrEffect t1 n
        Just (Just v) ->
          let
            (t2, s', mdS) = optIrEffect t1 s
            (e', md') = elimIrBind (litMeta v) Nothing tag (IrLift (IrLiteral v)) s' mdS
           in
            (t2, e', md')
        Nothing ->
          let
            (t2, n', mdN) = optIrEffect t1 n
            (t3, s', mdS) = optIrEffect t2 s
           in
            ( t3
            , IrOptionCaseE o' n' tag s'
            , nodeMeta mdO (nodeMeta mdN (bindMeta tag mdS))
            )
  IrResultCaseE o tagE e tagO s ->
    let
      (t1, o', mdO) = optIrExpr t0 o
     in
      case peelIrResult o' of
        Just (Left x) ->
          let
            (t2, e', mdE) = optIrEffect t1 e
            (res, md') = elimIrBind (boundIrMeta x) Nothing tagE (IrLift x) e' mdE
           in
            (t2, res, md')
        Just (Right x) ->
          let
            (t2, s', mdS) = optIrEffect t1 s
            (res, md') = elimIrBind (boundIrMeta x) Nothing tagO (IrLift x) s' mdS
           in
            (t2, res, md')
        Nothing ->
          let
            (t2, e', mdE) = optIrEffect t1 e
            (t3, s', mdS) = optIrEffect t2 s
           in
            ( t3
            , IrResultCaseE o' tagE e' tagO s'
            , nodeMeta mdO (nodeMeta (bindMeta tagE mdE) (bindMeta tagO mdS))
            )
  IrStringCaseE s arms d ->
    let
      (t1, s', mdS) = optIrExpr t0 s
     in
      case s' of
        IrLiteral (ValueString k) -> optIrEffect t1 (fromMaybe d (lookup k arms))
        _ ->
          let
            (t2, arms', mdA) = mapAccumIrEffect t1 arms
            (t3, d', mdD) = optIrEffect t2 d
           in
            (t3, IrStringCaseE s' arms' d', nodeMeta mdS (nodeMeta mdA mdD))
  IrThrow x ->
    let
      (t1, x', md) = optIrExpr t0 x
     in
      (t1, IrThrow x', effectMd md)
  IrTry a tag k ->
    let
      (t1, a', mdA) = optIrEffect t0 a
      (t2, k', mdK) = optIrEffect (t1 - optStep) k
     in
      (t2, IrTry a' tag k', nodeMeta mdA (bindMeta tag mdK))
  IrObjectLit fs ->
    let
      (t1, fs', md) = mapAccumIrFieldLit t0 fs
     in
      (t1, IrObjectLit fs', md)
  IrDeleteProp o k ->
    let
      (t1, o', mdO) = optIrEffect t0 o
      (t2, k', mdK) = optIrExpr t1 k
     in
      (t2, IrDeleteProp o' k', effectMd (nodeMeta mdO mdK))
  IrArrayLit es ->
    let
      (t1, es', md) = mapAccumIrEffects t0 es
     in
      (t1, IrArrayLit es', md)

mapAccumIrEffect ::
  (?keepLets :: P.Bool) =>
  Int -> [(Text, IrEffect u)] -> (Int, [(Text, IrEffect u)], IrMeta)
mapAccumIrEffect !t0 arms =
  foldr
    ( \(k, e) (!t, acc, !md) ->
        let
          (t', e', md') = optIrEffect t e
         in
          (t', (k, e') : acc, md' <> md)
    )
    (t0, [], mempty)
    arms

mapAccumIrEffects ::
  (?keepLets :: P.Bool) => Int -> [IrEffect u] -> (Int, [IrEffect u], IrMeta)
mapAccumIrEffects !t0 es =
  foldr
    ( \e (!t, acc, !md) ->
        let
          (t', e', md') = optIrEffect t e
         in
          (t', e' : acc, md' <> md)
    )
    (t0, [], mempty)
    es

optIrArgs ::
  (?keepLets :: P.Bool) => Int -> Rec (IrArg) us -> (Int, Rec (IrArg) us, IrMeta)
optIrArgs !t0 args = case args of
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
