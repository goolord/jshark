{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

-- | First-order IR for optimize + codegen. PHOAS 'Expr'/'Effect' in
-- 'JShark.Api.Types' stay the user-facing syntax; closed terms lower here once.
--
-- A single untyped 'IrNode' carries both expression and effect subtrees. The
-- @IrExpr@\/@IrEffect@ type split is gone: post-lowering the universe indices
-- do no checking, so they carried no information that the flat IR did not
-- already reify (opcode + @Text@ names + value payloads). Every constructor
-- below mirrors one 'FlatNode' opcode so pack and emit are mechanical.
--
-- Expression\/effect *position* is still a property of a subtree, determined
-- by its top constructor; the type bridge nodes ('IrLift' \/ the old
-- 'IrEmbedEff') reduce to pack-time wrapper rows.
--
-- Internal to the JShark compiler; this module is exposed for tests and
-- tooling and its API may change between 0.x releases.
module JShark.Compiler.Ir
  ( IrMeta (..)
  , IrNode (..)
  , IrField (..)
  , irFieldName
  , irFieldChild
  , irNodeChildren
  , SomeFixedOp (..)
  , metaIr
  , optIr
  , occursIr
  , lazyOccursIr
  , effectMd
  , optStep
  , optSmall
  )
where

import Data.Bits (xor, (.&.), (.|.))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (symbolVal)
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
import JShark.Api.Types
  ( BigBinOp
  , Expr (Literal)
  , FFIForm (..)
  , FieldLit (..)
  , FixedOp (..)
  , LamInfo (..)
  , Value (..)
  )
import JShark.Compiler.Binder (strictFoldMap)
import JShark.Compiler.Evaluate
  ( eqFoldableValue
  , isCheapValue
  , isOrderableValue
  , jsShow
  , keepLastByKey
  , parseBigIntString
  , tryEvalBigBin
  , typeOfValue
  )
import JShark.Compiler.JsNum (jsBit2, jsRem, jsShl, jsShr, jsUShr)
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

-- | Optimizer tag spacing and the small-body inline threshold (kept from
-- the deleted 'JShark.Compiler.Metadata').
optStep :: Int
optStep = 2

optSmall :: Int
optSmall = 16

-- | Force impure on optimized metadata (empty arg lists are otherwise pure).
effectMd :: IrMeta -> IrMeta
effectMd !md = md <> IrMeta 0 IM.empty False False

-- | One field of a frozen or mutable object literal. Name is the JS key;
-- the child is the field value (effect-kinded for the @Eff@ forms).
data IrField
  = IrField !Text !IrNode
  | IrFieldEff !Text !IrNode
  | IrFieldExtra !Text !IrNode
  | IrFieldExtraEff !Text !IrNode

irFieldName :: IrField -> Text
irFieldName = \case
  IrField t _ -> t
  IrFieldEff t _ -> t
  IrFieldExtra t _ -> t
  IrFieldExtraEff t _ -> t

irFieldChild :: IrField -> IrNode
irFieldChild = \case
  IrField _ c -> c
  IrFieldEff _ c -> c
  IrFieldExtra _ c -> c
  IrFieldExtraEff _ c -> c

-- | Immediate children of a node, in evaluation order. The canonical
-- list-based traversal: lint-style passes use it directly; the
-- perf-sensitive optimizer walks constructors by hand (short-circuiting,
-- allocation-free). A new constructor must extend this case (the
-- exhaustiveness check enforces it).
irNodeChildren :: IrNode -> [IrNode]
irNodeChildren = \case
  IrLiteral {} -> []
  IrVar {} -> []
  IrLet _ _ x b -> [x, b]
  IrLetRec _ r b -> [r, b]
  IrLambda _ _ b -> [b]
  IrApply f x -> [f, x]
  IrIf c t eF -> [c, t, eF]
  IrOptionCase o n _ s -> [o, n, s]
  IrResultOk x -> [x]
  IrResultErr x -> [x]
  IrResultCase o _ er _ ok -> [o, er, ok]
  IrIndex x i -> [x, i]
  IrU8Index x i -> [x, i]
  IrError x -> [x]
  IrFixed _ args -> args
  IrFnLit _ _ b -> [b]
  IrUnsafeNullable x -> [x]
  IrFrozenLit fs -> map irFieldChild fs
  IrGetField _ o -> [o]
  KConcat x y -> [x, y]
  KPlus x y -> [x, y]
  KTimes x y -> [x, y]
  KMinus x y -> [x, y]
  KNegate x -> [x]
  KFracDiv x y -> [x, y]
  KRem x y -> [x, y]
  KBitAnd x y -> [x, y]
  KBitOr x y -> [x, y]
  KBitXor x y -> [x, y]
  KShl x y -> [x, y]
  KShr x y -> [x, y]
  KUShr x y -> [x, y]
  KBig _ x y -> [x, y]
  KBigNeg x -> [x]
  KAnd x y -> [x, y]
  KOr x y -> [x, y]
  KEq _ x y -> [x, y]
  KNEq _ x y -> [x, y]
  KGTh x y -> [x, y]
  KLTh x y -> [x, y]
  KGTEq x y -> [x, y]
  KLTEq x y -> [x, y]
  KShow x -> [x]
  KTypeOf x -> [x]
  IrMethMap x _ g -> [x, g]
  IrMethFilter x _ g -> [x, g]
  IrMethReduce x z _ _ g -> [x, z, g]
  IrMethReduceRight x z _ _ g -> [x, z, g]
  IrMethToSorted x _ _ g -> [x, g]
  IrMethFrom n _ g -> [n, g]
  IrLift x -> [x]
  IrFFI _ args -> args
  IrUnsafeObject {} -> []
  IrUnsafeObjectGet x _ -> [x]
  IrUnsafeObjectAssign x y -> [x, y]
  IrCallMethod x _ args -> x : args
  IrBind _ _ x b -> [x, b]
  IrThenE x y -> [x, y]
  IrBindRec _ r b -> [r, b]
  IrLambdaE _ b -> [b]
  IrApplyE f x -> [f, x]
  IrIfE c t eF -> [c, t, eF]
  IrWhile c b -> [c, b]
  IrForRange s e _ b -> [s, e, b]
  IrU8Set b i v -> [b, i, v]
  IrU8Fill b v -> [b, v]
  IrOptionCaseE o n _ s -> [o, n, s]
  IrResultCaseE o _ er _ ok -> [o, er, ok]
  IrStringCaseE s arms d -> s : map snd arms ++ [d]
  IrThrow x -> [x]
  IrTry a _ k -> [a, k]
  IrObjectLit fs -> map irFieldChild fs
  IrDeleteProp o k -> [o, k]
  IrArrayLit es -> es

-- | A 'Value' with its universe hidden. Opt-time literal folds and pack both
-- consume values constructor-wise, so the index is never needed.
data SomeIrValue where
  SomeIrValue :: Value u -> SomeIrValue

-- | A 'FixedOp' with its universes hidden (the flat IR keeps the GADT; the
-- untyped tree only needs it as an opaque opcode payload).
data SomeFixedOp where
  SomeFixedOp :: FixedOp a b c u -> SomeFixedOp

data IrNode
  = forall u. IrLiteral (Value u)
  | IrVar !Int
  | IrLet !Int !(Maybe Text) !IrNode !IrNode
  | IrLetRec !Int !IrNode !IrNode
  | IrLambda !Int !LamInfo !IrNode
  | IrApply !IrNode !IrNode
  | IrIf !IrNode !IrNode !IrNode
  | IrOptionCase !IrNode !IrNode !Int !IrNode
  | IrResultOk !IrNode
  | IrResultErr !IrNode
  | IrResultCase !IrNode !Int !IrNode !Int !IrNode
  | IrIndex !IrNode !IrNode
  | IrU8Index !IrNode !IrNode
  | IrError !IrNode
  | IrFixed !SomeFixedOp ![IrNode]
  | IrFnLit ![Int] ![Maybe Text] !IrNode
  | IrUnsafeNullable !IrNode
  | IrFrozenLit ![IrField]
  | IrGetField !Text !IrNode
  | KConcat !IrNode !IrNode
  | KPlus !IrNode !IrNode
  | KTimes !IrNode !IrNode
  | KMinus !IrNode !IrNode
  | KNegate !IrNode
  | KFracDiv !IrNode !IrNode
  | KRem !IrNode !IrNode
  | KBitAnd !IrNode !IrNode
  | KBitOr !IrNode !IrNode
  | KBitXor !IrNode !IrNode
  | KShl !IrNode !IrNode
  | KShr !IrNode !IrNode
  | KUShr !IrNode !IrNode
  | KBig !BigBinOp !IrNode !IrNode
  | KBigNeg !IrNode
  | KAnd !IrNode !IrNode
  | KOr !IrNode !IrNode
  | KEq !P.Bool !IrNode !IrNode
  | KNEq !P.Bool !IrNode !IrNode
  | KGTh !IrNode !IrNode
  | KLTh !IrNode !IrNode
  | KGTEq !IrNode !IrNode
  | KLTEq !IrNode !IrNode
  | KShow !IrNode
  | KTypeOf !IrNode
  | IrMethMap !IrNode !Int !IrNode
  | IrMethFilter !IrNode !Int !IrNode
  | IrMethReduce !IrNode !IrNode !Int !Int !IrNode
  | IrMethReduceRight !IrNode !IrNode !Int !Int !IrNode
  | IrMethToSorted !IrNode !Int !Int !IrNode
  | IrMethFrom !IrNode !Int !IrNode
  | IrLift !IrNode
  | IrFFI !FFIForm ![IrNode]
  | IrUnsafeObject !Text
  | IrUnsafeObjectGet !IrNode !Text
  | IrUnsafeObjectAssign !IrNode !IrNode
  | IrCallMethod !IrNode !Text ![IrNode]
  | IrBind !Int !(Maybe Text) !IrNode !IrNode
  | IrThenE !IrNode !IrNode
  | IrBindRec !Int !IrNode !IrNode
  | IrLambdaE !Int !IrNode
  | IrApplyE !IrNode !IrNode
  | IrIfE !IrNode !IrNode !IrNode
  | IrWhile !IrNode !IrNode
  | IrForRange !IrNode !IrNode !Int !IrNode
  | IrU8Set !IrNode !IrNode !IrNode
  | IrU8Fill !IrNode !IrNode
  | IrOptionCaseE !IrNode !IrNode !Int !IrNode
  | IrResultCaseE !IrNode !Int !IrNode !Int !IrNode
  | IrStringCaseE !IrNode ![(Text, IrNode)] !IrNode
  | IrThrow !IrNode
  | IrTry !IrNode !Int !IrNode
  | IrObjectLit ![IrField]
  | IrDeleteProp !IrNode !IrNode
  | IrArrayLit ![IrNode]

-- | Structural metadata. Every child contributes, including the lazy
-- ones: a free variable that occurs only inside a lambda body, a @?:@
-- arm, or an FFI argument is still a use, and substitution keys its
-- skip test off 'irFree'.
metaIr :: IrNode -> IrMeta
metaIr !node = case node of
  IrLiteral v -> IrMeta 1 IM.empty True (isCheapValue v)
  IrVar i -> IrMeta 1 (IM.singleton i 1) True True
  _ -> here node <> childMeta node
 where
  here = \case
    -- Stdlib calls (Math.*, checkedIndex, …) are not 'cheap': a 2-use
    -- @let x = Math.sin(1) in x + x@ keeps its binding instead of
    -- duplicating the call, matching the former PHOAS policy.
    IrFixed (SomeFixedOp op) _ -> IrMeta 1 IM.empty (isPureFixed op) P.False
    IrFFI {} -> impure
    IrUnsafeObject {} -> impure
    IrUnsafeObjectGet {} -> impure
    IrUnsafeObjectAssign {} -> impure
    IrCallMethod {} -> impure
    IrApplyE {} -> impure
    IrWhile {} -> impure
    IrForRange {} -> impure
    IrU8Set {} -> impure
    IrU8Fill {} -> impure
    IrThrow {} -> impure
    IrTry {} -> impure
    IrDeleteProp {} -> impure
    IrError {} -> impure
    _ -> IrMeta 1 IM.empty True False
  impure = IrMeta 1 IM.empty False False

-- | Metadata of the immediate children (list children included), so every
-- child, lazy or not, contributes once. 'irNodeChildren' already encodes the
-- per-constructor child layout, so this is a plain fold over it.
childMeta :: IrNode -> IrMeta
childMeta = strictFoldMap metaIr . irNodeChildren

-- | Does @t@ occur anywhere in @node@? Unlike a free-variable map this
-- short-circuits on the first hit and allocates nothing.
occursIr :: Int -> IrNode -> P.Bool
occursIr !t node = case node of
  IrVar i -> i == t
  _ -> anyOccurs t node

anyOccurs :: Int -> IrNode -> P.Bool
anyOccurs !t = \case
  IrLiteral _ -> False
  IrVar {} -> False
  IrLet _ _ x g -> occursIr t x P.|| occursIr t g
  IrLetRec _ r b -> occursIr t r P.|| occursIr t b
  IrLambda _ _ g -> occursIr t g
  IrApply f x -> occursIr t f P.|| occursIr t x
  IrIf c x e -> occursIr t c P.|| occursIr t x P.|| occursIr t e
  IrOptionCase o n _ s -> occursIr t o P.|| occursIr t n P.|| occursIr t s
  IrResultOk x -> occursIr t x
  IrResultErr x -> occursIr t x
  IrResultCase o _ er _ ok -> occursIr t o P.|| occursIr t er P.|| occursIr t ok
  IrIndex x i -> occursIr t x P.|| occursIr t i
  IrU8Index x i -> occursIr t x P.|| occursIr t i
  IrError x -> occursIr t x
  IrFixed _ args -> any (occursIr t) args
  IrFnLit _ _ b -> occursIr t b
  IrUnsafeNullable x -> occursIr t x
  IrFrozenLit fs -> any (occursIr t . irFieldChild) fs
  IrGetField _ o -> occursIr t o
  KConcat x y -> occursIr t x P.|| occursIr t y
  KPlus x y -> occursIr t x P.|| occursIr t y
  KTimes x y -> occursIr t x P.|| occursIr t y
  KMinus x y -> occursIr t x P.|| occursIr t y
  KNegate x -> occursIr t x
  KFracDiv x y -> occursIr t x P.|| occursIr t y
  KRem x y -> occursIr t x P.|| occursIr t y
  KBitAnd x y -> occursIr t x P.|| occursIr t y
  KBitOr x y -> occursIr t x P.|| occursIr t y
  KBitXor x y -> occursIr t x P.|| occursIr t y
  KShl x y -> occursIr t x P.|| occursIr t y
  KShr x y -> occursIr t x P.|| occursIr t y
  KUShr x y -> occursIr t x P.|| occursIr t y
  KBig _ x y -> occursIr t x P.|| occursIr t y
  KBigNeg x -> occursIr t x
  KAnd x y -> occursIr t x P.|| occursIr t y
  KOr x y -> occursIr t x P.|| occursIr t y
  KEq _ x y -> occursIr t x P.|| occursIr t y
  KNEq _ x y -> occursIr t x P.|| occursIr t y
  KGTh x y -> occursIr t x P.|| occursIr t y
  KLTh x y -> occursIr t x P.|| occursIr t y
  KGTEq x y -> occursIr t x P.|| occursIr t y
  KLTEq x y -> occursIr t x P.|| occursIr t y
  KShow x -> occursIr t x
  KTypeOf x -> occursIr t x
  IrMethMap x _ g -> occursIr t x P.|| occursIr t g
  IrMethFilter x _ g -> occursIr t x P.|| occursIr t g
  IrMethReduce x z _ _ g -> occursIr t x P.|| occursIr t z P.|| occursIr t g
  IrMethReduceRight x z _ _ g -> occursIr t x P.|| occursIr t z P.|| occursIr t g
  IrMethToSorted x _ _ g -> occursIr t x P.|| occursIr t g
  IrMethFrom n _ g -> occursIr t n P.|| occursIr t g
  IrLift x -> occursIr t x
  IrFFI _ args -> any (occursIr t) args
  IrUnsafeObject {} -> False
  IrUnsafeObjectGet x _ -> occursIr t x
  IrUnsafeObjectAssign x y -> occursIr t x P.|| occursIr t y
  IrCallMethod x _ args -> occursIr t x P.|| any (occursIr t) args
  IrBind _ _ x g -> occursIr t x P.|| occursIr t g
  IrThenE x y -> occursIr t x P.|| occursIr t y
  IrBindRec _ r b -> occursIr t r P.|| occursIr t b
  IrLambdaE _ g -> occursIr t g
  IrApplyE f x -> occursIr t f P.|| occursIr t x
  IrIfE c x e -> occursIr t c P.|| occursIr t x P.|| occursIr t e
  IrWhile c b -> occursIr t c P.|| occursIr t b
  IrForRange s e _ b -> occursIr t s P.|| occursIr t e P.|| occursIr t b
  IrU8Set b i v -> occursIr t b P.|| occursIr t i P.|| occursIr t v
  IrU8Fill b v -> occursIr t b P.|| occursIr t v
  IrOptionCaseE o n _ s -> occursIr t o P.|| occursIr t n P.|| occursIr t s
  IrResultCaseE o _ er _ ok -> occursIr t o P.|| occursIr t er P.|| occursIr t ok
  IrStringCaseE s arms d ->
    occursIr t s P.|| any (occursIr t . snd) arms P.|| occursIr t d
  IrThrow x -> occursIr t x
  IrTry a _ k -> occursIr t a P.|| occursIr t k
  IrObjectLit fs -> any (occursIr t . irFieldChild) fs
  IrDeleteProp o k -> occursIr t o P.|| occursIr t k
  IrArrayLit es -> any (occursIr t) es

-- | Does the tag occur in a position that is not evaluated exactly once
-- where it stands: a lambda body, a @?:@ arm, an @&&@ right operand, a
-- loop body? Inlining there either skips work the program asked for or
-- repeats it, so those uses keep their binding.
lazyOccursIr :: Int -> IrNode -> P.Bool
lazyOccursIr !t node = case node of
  -- Re-evaluated once per iteration, so neither part is a "once" slot.
  IrWhile c b -> occursIr t c P.|| occursIr t b
  _ -> lazyChildren t node

lazyChildren :: Int -> IrNode -> P.Bool
lazyChildren !t = \case
  IrLiteral _ -> False
  IrVar {} -> False
  -- Strict positions recurse lazily; lazy positions count any use.
  IrLet _ _ x g -> lazyOccursIr t x P.|| lazyOccursIr t g
  IrLetRec _ r b -> lazyOccursIr t r P.|| lazyOccursIr t b
  IrLambda _ _ g -> occursIr t g
  IrApply f x -> lazyOccursIr t f P.|| lazyOccursIr t x
  IrIf c x e -> lazyOccursIr t c P.|| occursIr t x P.|| occursIr t e
  IrOptionCase o n _ s -> lazyOccursIr t o P.|| occursIr t n P.|| occursIr t s
  IrResultOk x -> lazyOccursIr t x
  IrResultErr x -> lazyOccursIr t x
  IrResultCase o _ er _ ok -> lazyOccursIr t o P.|| occursIr t er P.|| occursIr t ok
  IrIndex x i -> lazyOccursIr t x P.|| lazyOccursIr t i
  IrU8Index x i -> lazyOccursIr t x P.|| lazyOccursIr t i
  IrError x -> lazyOccursIr t x
  IrFixed _ args -> any (lazyOccursIr t) args
  IrFnLit _ _ b -> occursIr t b
  IrUnsafeNullable x -> lazyOccursIr t x
  IrFrozenLit fs -> any (lazyOccursIr t . irFieldChild) fs
  IrGetField _ o -> lazyOccursIr t o
  KConcat x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KPlus x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KTimes x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KMinus x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KNegate x -> lazyOccursIr t x
  KFracDiv x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KRem x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KBitAnd x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KBitOr x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KBitXor x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KShl x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KShr x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KUShr x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KBig _ x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KBigNeg x -> lazyOccursIr t x
  -- @x && y@ and @x || y@ short-circuit: only the left operand is strict.
  KAnd x y -> lazyOccursIr t x P.|| occursIr t y
  KOr x y -> lazyOccursIr t x P.|| occursIr t y
  KEq _ x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KNEq _ x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KGTh x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KLTh x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KGTEq x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KLTEq x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  KShow x -> lazyOccursIr t x
  KTypeOf x -> lazyOccursIr t x
  IrMethMap x _ g -> lazyOccursIr t x P.|| occursIr t g
  IrMethFilter x _ g -> lazyOccursIr t x P.|| occursIr t g
  IrMethReduce x z _ _ g -> lazyOccursIr t x P.|| lazyOccursIr t z P.|| occursIr t g
  IrMethReduceRight x z _ _ g -> lazyOccursIr t x P.|| lazyOccursIr t z P.|| occursIr t g
  IrMethToSorted x _ _ g -> lazyOccursIr t x P.|| occursIr t g
  IrMethFrom n _ g -> lazyOccursIr t n P.|| occursIr t g
  IrLift x -> lazyOccursIr t x
  IrFFI _ args -> any (lazyOccursIr t) args
  IrUnsafeObject {} -> False
  IrUnsafeObjectGet x _ -> lazyOccursIr t x
  IrUnsafeObjectAssign x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  IrCallMethod x _ args -> lazyOccursIr t x P.|| any (lazyOccursIr t) args
  IrBind _ _ x g -> lazyOccursIr t x P.|| lazyOccursIr t g
  IrThenE x y -> lazyOccursIr t x P.|| lazyOccursIr t y
  IrBindRec _ r b -> lazyOccursIr t r P.|| lazyOccursIr t b
  IrLambdaE _ g -> occursIr t g
  IrApplyE f x -> lazyOccursIr t f P.|| lazyOccursIr t x
  IrIfE c x e -> lazyOccursIr t c P.|| occursIr t x P.|| occursIr t e
  IrWhile c b -> occursIr t c P.|| occursIr t b
  IrForRange s e _ b -> lazyOccursIr t s P.|| lazyOccursIr t e P.|| occursIr t b
  IrU8Set b i v -> lazyOccursIr t b P.|| lazyOccursIr t i P.|| lazyOccursIr t v
  IrU8Fill b v -> lazyOccursIr t b P.|| lazyOccursIr t v
  IrOptionCaseE o n _ s -> lazyOccursIr t o P.|| occursIr t n P.|| occursIr t s
  IrResultCaseE o _ er _ ok -> lazyOccursIr t o P.|| occursIr t er P.|| occursIr t ok
  IrStringCaseE s arms d ->
    lazyOccursIr t s P.|| any (occursIr t . snd) arms P.|| occursIr t d
  IrThrow x -> lazyOccursIr t x
  IrTry a _ k -> lazyOccursIr t a P.|| occursIr t k
  IrObjectLit fs -> any (lazyOccursIr t . irFieldChild) fs
  IrDeleteProp o k -> lazyOccursIr t o P.|| lazyOccursIr t k
  IrArrayLit es -> any (lazyOccursIr t) es

-- | Rebuild @node@ with @f@ applied to every immediate child. Binder tags
-- and names are left untouched.
rebuildIr :: (IrNode -> IrNode) -> IrNode -> IrNode
rebuildIr f node = case node of
  IrLiteral _ -> node
  IrVar {} -> node
  IrLet t h x g -> IrLet t h (f x) (f g)
  IrLetRec t r b -> IrLetRec t (f r) (f b)
  IrLambda t info g -> IrLambda t info (f g)
  IrApply x y -> IrApply (f x) (f y)
  IrIf c x e -> IrIf (f c) (f x) (f e)
  IrOptionCase o n t s -> IrOptionCase (f o) (f n) t (f s)
  IrResultOk x -> IrResultOk (f x)
  IrResultErr x -> IrResultErr (f x)
  IrResultCase o te er to ok ->
    IrResultCase (f o) te (f er) to (f ok)
  IrIndex x i -> IrIndex (f x) (f i)
  IrU8Index x i -> IrU8Index (f x) (f i)
  IrError x -> IrError (f x)
  IrFixed op args -> IrFixed op (map f args)
  IrFnLit ts ns b -> IrFnLit ts ns (f b)
  IrUnsafeNullable x -> IrUnsafeNullable (f x)
  IrFrozenLit fs -> IrFrozenLit (map (rebuildField f) fs)
  IrGetField k o -> IrGetField k (f o)
  KConcat x y -> KConcat (f x) (f y)
  KPlus x y -> KPlus (f x) (f y)
  KTimes x y -> KTimes (f x) (f y)
  KMinus x y -> KMinus (f x) (f y)
  KNegate x -> KNegate (f x)
  KFracDiv x y -> KFracDiv (f x) (f y)
  KRem x y -> KRem (f x) (f y)
  KBitAnd x y -> KBitAnd (f x) (f y)
  KBitOr x y -> KBitOr (f x) (f y)
  KBitXor x y -> KBitXor (f x) (f y)
  KShl x y -> KShl (f x) (f y)
  KShr x y -> KShr (f x) (f y)
  KUShr x y -> KUShr (f x) (f y)
  KBig op x y -> KBig op (f x) (f y)
  KBigNeg x -> KBigNeg (f x)
  KAnd x y -> KAnd (f x) (f y)
  KOr x y -> KOr (f x) (f y)
  KEq s x y -> KEq s (f x) (f y)
  KNEq s x y -> KNEq s (f x) (f y)
  KGTh x y -> KGTh (f x) (f y)
  KLTh x y -> KLTh (f x) (f y)
  KGTEq x y -> KGTEq (f x) (f y)
  KLTEq x y -> KLTEq (f x) (f y)
  KShow x -> KShow (f x)
  KTypeOf x -> KTypeOf (f x)
  IrMethMap a t b -> IrMethMap (f a) t (f b)
  IrMethFilter a t b -> IrMethFilter (f a) t (f b)
  IrMethReduce a z ta tb b -> IrMethReduce (f a) (f z) ta tb (f b)
  IrMethReduceRight a z ta tb b -> IrMethReduceRight (f a) (f z) ta tb (f b)
  IrMethToSorted a ta tb b -> IrMethToSorted (f a) ta tb (f b)
  IrMethFrom n t b -> IrMethFrom (f n) t (f b)
  IrLift x -> IrLift (f x)
  IrFFI form args -> IrFFI form (map f args)
  IrUnsafeObject {} -> node
  IrUnsafeObjectGet x s -> IrUnsafeObjectGet (f x) s
  IrUnsafeObjectAssign x y -> IrUnsafeObjectAssign (f x) (f y)
  IrCallMethod x s args -> IrCallMethod (f x) s (map f args)
  IrBind t h x g -> IrBind t h (f x) (f g)
  IrThenE x y -> IrThenE (f x) (f y)
  IrBindRec t r b -> IrBindRec t (f r) (f b)
  IrLambdaE t g -> IrLambdaE t (f g)
  IrApplyE x y -> IrApplyE (f x) (f y)
  IrIfE c x e -> IrIfE (f c) (f x) (f e)
  IrWhile c b -> IrWhile (f c) (f b)
  IrForRange s e t b -> IrForRange (f s) (f e) t (f b)
  IrU8Set b i v -> IrU8Set (f b) (f i) (f v)
  IrU8Fill b v -> IrU8Fill (f b) (f v)
  IrOptionCaseE o n t s -> IrOptionCaseE (f o) (f n) t (f s)
  IrResultCaseE o te er to ok -> IrResultCaseE (f o) te (f er) to (f ok)
  IrStringCaseE s arms d ->
    IrStringCaseE (f s) (map (\(k, v) -> (k, f v)) arms) (f d)
  IrThrow x -> IrThrow (f x)
  IrTry a t k -> IrTry (f a) t (f k)
  IrObjectLit fs -> IrObjectLit (map (rebuildField f) fs)
  IrDeleteProp o k -> IrDeleteProp (f o) (f k)
  IrArrayLit es -> IrArrayLit (map f es)
 where
  rebuildField g = \case
    IrField t c -> IrField t (g c)
    IrFieldEff t c -> IrFieldEff t (g c)
    IrFieldExtra t c -> IrFieldExtra t (g c)
    IrFieldExtraEff t c -> IrFieldExtraEff t (g c)

-- | Rename tag @old@ to @new@ across @node@ (alias inlining).
substIr :: Int -> Int -> IrNode -> IrNode
substIr !old !new !node
  | old == new = node
  | not (occursIr old node) = node
  | otherwise = case node of
      IrVar i | i == old -> IrVar new
      _ -> rebuildIr (substIr old new) node

-- | Replace the binder with the bound term itself. A variable-to-variable
-- bound term is a plain rename; anything else is spliced in.
inlineIr :: Int -> IrNode -> IrNode -> IrNode
inlineIr !tag !bound body = case bound of
  IrVar i -> substIr tag i body
  _ -> replaceIr tag bound body

replaceIr :: Int -> IrNode -> IrNode -> IrNode
replaceIr !tag bound node = case node of
  IrVar i | i == tag -> bound
  _ -> rebuildIr (replaceIr tag bound) node

isIrLambda :: IrNode -> P.Bool
isIrLambda = \case
  IrLambda {} -> True
  _ -> False

isIrLambdaE :: IrNode -> P.Bool
isIrLambdaE = \case
  IrLambdaE {} -> True
  _ -> False

isIdentityIr :: Int -> IrNode -> P.Bool
isIdentityIr tag = \case
  IrVar i -> i == tag
  _ -> False

isAliasEffect :: IrNode -> P.Bool
isAliasEffect = \case
  IrLift (IrVar _) -> True
  IrLift (IrUnsafeNullable (IrVar _)) -> True
  _ -> False

isIdentityEffect :: Int -> IrNode -> P.Bool
isIdentityEffect tag = \case
  IrLift x -> isIdentityIr tag x
  _ -> False

-- | Shared let\/bind eliminator. 'IrLet' and 'IrBind' differ in how a kept
-- binding is rebuilt, how a dead one is rebuilt (@IrThenE@ drops the
-- binder), and which right-hand sides count as aliases.
elimBinder ::
  (?keepLets :: P.Bool) =>
  (Int -> Maybe Text -> IrNode -> IrNode -> IrNode)
  -- rebuild a kept binding
  -> (Int -> Maybe Text -> IrNode -> IrNode -> IrNode)
  -- rebuild a dead binding
  -> (IrNode -> P.Bool)
  -- extra guard on the pure dead-drop
  -> (IrNode -> P.Bool)
  -- extra @once@ condition
  -> (IrNode -> Int -> IrNode -> P.Bool)
  -- @preserve@ predicate
  -> IrMeta
  -> Maybe Text
  -> Int
  -> IrNode
  -> IrNode
  -> IrMeta
  -> (IrNode, IrMeta)
elimBinder ctorKeep ctorDead dropPure extraOnce preserve !mdX !hint !tag !x !body !mdBody =
  let
    uses = IM.findWithDefault 0 tag (irFree mdBody)
    closed = bindMeta tag mdBody
    spliced = closed <> mdX
    -- A single use may be inlined only when moving the bound term to that
    -- use is safe. Pure terms are referentially transparent, so their
    -- position does not matter. An impure term may only move as an alias
    -- (a rename); splicing a real effect past other effects would reorder
    -- evaluation. The cheap/once estimates only refine the pure case.
    once =
      extraOnce x
        P.|| (irPure mdX P.&& (irCheap mdX P.|| not (lazyOccursIr tag body)))
    keep = preserve x tag body
   in
    case uses of
      0
        | irPure mdX
        , not (dropPure x) ->
            (body, closed)
      0 -> (ctorDead tag hint x body, nodeMeta mdX closed)
      1
        | ?keepLets && keep ->
            (ctorKeep tag hint x body, nodeMeta mdX closed)
      1
        | irSize mdBody <= optSmall
        , once ->
            (inlineIr tag x body, spliced)
      _
        | irCheap mdX
        , irSize mdBody <= optSmall ->
            (inlineIr tag x body, spliced)
      _ -> (ctorKeep tag hint x body, nodeMeta mdX closed)

elimIrLet ::
  (?keepLets :: P.Bool) =>
  IrMeta
  -> Maybe Text
  -> Int
  -> IrNode
  -> IrNode
  -> IrMeta
  -> (IrNode, IrMeta)
elimIrLet =
  elimBinder
    IrLet
    IrLet
    (const False)
    (const False)
    (\x' t b -> not (isIrLambda x') && not (isIdentityIr t b))

elimIrBind ::
  (?keepLets :: P.Bool) =>
  IrMeta
  -> Maybe Text
  -> Int
  -> IrNode
  -> IrNode
  -> IrMeta
  -> (IrNode, IrMeta)
elimIrBind =
  elimBinder
    IrBind
    (\_ _ x b -> IrThenE x b)
    isAliasEffect
    isAliasEffect
    ( \x' t b ->
        not (isIrLambdaE x') && not (isIdentityEffect t b) && not (isAliasEffect x')
    )

nodeMeta :: IrMeta -> IrMeta -> IrMeta
nodeMeta !mdX !mdY =
  IrMeta 1 IM.empty (irPure mdX && irPure mdY) False <> mdX <> mdY

-- | Close a binder: its tag is no longer free above this node. Without
-- this the free map grows to every tag in the subtree, and the union in
-- '<>' then costs the whole program at every node.
bindMeta :: Int -> IrMeta -> IrMeta
bindMeta !tag !md = md {irFree = IM.delete tag (irFree md)}

litMeta :: Value u -> IrMeta
litMeta v = IrMeta 1 IM.empty True (isCheapValue v)

varMeta :: Int -> IrMeta
varMeta !i = IrMeta 1 (IM.singleton i 1) True True

optIr :: (?keepLets :: P.Bool) => Int -> IrNode -> (Int, IrNode, IrMeta)
optIr !t0 node = case node of
  IrLiteral v -> (t0, IrLiteral v, litMeta v)
  IrVar i -> (t0, IrVar i, varMeta i)
  IrLet tag hint x body ->
    let
      (t1, x', mdX) = optIr t0 x
      (t2, body', mdBody) = optIr t1 body
     in
      let
        (e', md') = elimIrLet mdX hint tag x' body' mdBody
       in
        (t2, e', md')
  -- Named hoists (@Just@ tag) always stay as calls so codegen can emit one
  -- shared helper (e.g. @$groupBy@). Literal partial application would
  -- beta into an untagged inner lambda and duplicate the body at each site.
  IrApply (IrLambda bindTag info@LamInfo {lamTag = Just _} g) x ->
    let
      (t1, x', mdX) = optIr t0 x
      (t2, g', mdG) = optIr t1 g
     in
      ( t2
      , IrApply (IrLambda bindTag info g') x'
      , nodeMeta mdX mdG
      )
  IrApply (IrLambda tag LamInfo {lamTag = Nothing} g) x ->
    let
      (t1, x', mdX) = optIr t0 x
      (t2, g', mdG) = optIr t1 g
     in
      let
        (e', md') = elimIrLet mdX Nothing tag x' g' mdG
       in
        (t2, e', md')
  IrBind tag hint x body ->
    let
      (t1, x', mdX) = optIr t0 x
      (t2, body', mdBody) = optIr t1 body
     in
      let
        (e', md') = elimIrBind mdX hint tag x' body' mdBody
       in
        (t2, e', md')
  n ->
    let
      (t1, e', md) = optIrNode t0 n
     in
      (t1, e', md)

optIrNode ::
  (?keepLets :: P.Bool) => Int -> IrNode -> (Int, IrNode, IrMeta)
optIrNode !t0 node = case node of
  IrLetRec tag r b ->
    let
      (t1, r', mdR) = optIr t0 r
      (t2, b', mdB) = optIr t1 b
     in
      (t2, IrLetRec tag r' b', bindMeta tag (nodeMeta mdR mdB))
  IrLambda tag hoist g ->
    let
      (t1, g', md) = optIr t0 g
     in
      (t1, IrLambda tag hoist g', bindMeta tag md)
  IrApply f x ->
    let
      (t1, f', mdF) = optIr t0 f
      (t2, x', mdX) = optIr t1 x
     in
      (t2, IrApply f' x', nodeMeta mdF mdX)
  IrIf c t e ->
    let
      (t1, c', mdC) = optIr t0 c
     in
      case c' of
        IrLiteral (ValueBool P.True) -> optIr t1 t
        IrLiteral (ValueBool P.False) -> optIr t1 e
        _ ->
          let
            (t2, t', mdT) = optIr t1 t
            (t3, e', mdE) = optIr t2 e
           in
            ( t3
            , IrIf c' t' e'
            , nodeMeta mdC (nodeMeta mdT mdE)
            )
  IrOptionCase o n tag s ->
    let
      (t1, o', mdO) = optIr t0 o
     in
      case peelIrOption o' of
        Just Nothing -> optIr t1 n
        Just (Just vn) ->
          let
            (t2, s', mdS) = optIr t1 s
            (e', md') = elimIrLet (boundIrMeta vn) Nothing tag vn s' mdS
           in
            (t2, e', md')
        Nothing ->
          let
            (t2, n', mdN) = optIr t1 n
            (t3, s', mdS) = optIr t2 s
           in
            ( t3
            , IrOptionCase o' n' tag s'
            , nodeMeta mdO (nodeMeta mdN (bindMeta tag mdS))
            )
  IrResultOk x ->
    let
      (t1, x', md) = optIr t0 x
     in
      (t1, IrResultOk x', md)
  IrResultErr x ->
    let
      (t1, x', md) = optIr t0 x
     in
      (t1, IrResultErr x', md)
  IrResultCase o tagE er tagO ok ->
    let
      (t1, o', mdO) = optIr t0 o
     in
      case peelIrResult o' of
        Just (Left x) ->
          let
            (t2, er', mdE) = optIr t1 er
            (res, md') = elimIrLet (boundIrMeta x) Nothing tagE x er' mdE
           in
            (t2, res, md')
        Just (Right x) ->
          let
            (t2, ok', mdS) = optIr t1 ok
            (res, md') = elimIrLet (boundIrMeta x) Nothing tagO x ok' mdS
           in
            (t2, res, md')
        Nothing ->
          let
            (t2, er', mdE) = optIr t1 er
            (t3, ok', mdS) = optIr t2 ok
           in
            ( t3
            , IrResultCase o' tagE er' tagO ok'
            , nodeMeta mdO (nodeMeta (bindMeta tagE mdE) (bindMeta tagO mdS))
            )
  IrIndex x i ->
    let
      (t1, x', mdX) = optIr t0 x
      (t2, i', mdI) = optIr t1 i
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
  IrU8Index x i -> binOptIr t0 IrU8Index x i
  IrError x ->
    let
      (t1, x', md) = optIr t0 x
     in
      (t1, IrError x', md)
  IrFixed sf args -> optIrFixedF t0 sf args
  IrFnLit tags names b -> optIrFnLit t0 tags names b
  IrUnsafeNullable x ->
    let
      (t1, x', md) = optIr t0 x
     in
      (t1, IrUnsafeNullable x', md)
  IrFrozenLit fs ->
    let
      (t1, fs', md) = mapAccumIrFields t0 fs
     in
      (t1, IrFrozenLit fs', md)
  IrGetField key o ->
    let
      (t1, o', mdO) = optIr t0 o
     in
      case o' of
        -- Project only when every sibling field is pure, so projecting
        -- @.b@ cannot DCE an effectful @.a@.
        IrFrozenLit fs
          | irPure mdO
          , Just fld <- lookupIrField key fs ->
              optIr t1 fld
        _ -> (t1, IrGetField key o', mdO)
  KConcat x y ->
    let
      (t1, x', mdX) = optIr t0 x
      (t2, y', mdY) = optIr t1 y
      res = case (x', y') of
        (IrLiteral (ValueString a), IrLiteral (ValueString b)) ->
          IrLiteral (ValueString (a <> b))
        _ -> KConcat x' y'
     in
      (t2, res, kernelFoldMd res <> nodeMeta mdX mdY)
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
      (t1, x', mdX) = optIr t0 x
      (t2, y', mdY) = optIr t1 y
      res = case (x', y') of
        (IrLiteral (ValueBigInt a), IrLiteral (ValueBigInt b))
          | Just r <- tryEvalBigBin op a b -> IrLiteral (ValueBigInt r)
        _ -> KBig op x' y'
     in
      (t2, res, kernelFoldMd res <> nodeMeta mdX mdY)
  KBigNeg x ->
    let
      (t1, x', mdX) = optIr t0 x
      res = case x' of
        IrLiteral (ValueBigInt n) -> IrLiteral (ValueBigInt (negate n))
        _ -> KBigNeg x'
     in
      (t1, res, kernelFoldMd res <> mdX)
  KShow x ->
    let
      (t1, x', mdX) = optIr t0 x
      -- Function values have no compile-time show.
      res = case x' of
        IrLiteral (ValueFunction _) -> KShow x'
        IrLiteral v -> IrLiteral (ValueString (jsShow v))
        _ -> KShow x'
     in
      (t1, res, kernelFoldMd res <> mdX)
  KTypeOf x ->
    let
      (t1, x', mdX) = optIr t0 x
      res = case x' of
        IrLiteral v -> IrLiteral (ValueString (typeOfValue v))
        _ -> KTypeOf x'
     in
      (t1, res, kernelFoldMd res <> mdX)
  KAnd x y ->
    let
      (t1, x', mdX) = optIr t0 x
     in
      case x' of
        IrLiteral (ValueBool P.False) ->
          (t1, IrLiteral (ValueBool P.False), litMeta (ValueBool P.False) <> mdX)
        IrLiteral (ValueBool P.True) -> optIr t1 y
        _ ->
          let
            (t2, y', mdY) = optIr t1 y
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
                (t2, KAnd x' y', kernelFoldMdK <> nodeMeta mdX mdY)
  KOr x y ->
    let
      (t1, x', mdX) = optIr t0 x
     in
      case x' of
        IrLiteral (ValueBool P.True) ->
          (t1, IrLiteral (ValueBool P.True), litMeta (ValueBool P.True) <> mdX)
        IrLiteral (ValueBool P.False) -> optIr t1 y
        _ ->
          let
            (t2, y', mdY) = optIr t1 y
           in
            case y' of
              IrLiteral (ValueBool P.False) -> (t2, x', mdX)
              IrLiteral (ValueBool P.True)
                | irPure mdX ->
                    (t2, IrLiteral (ValueBool P.True), litMeta (ValueBool P.True) <> mdX)
              _ ->
                (t2, KOr x' y', kernelFoldMdK <> nodeMeta mdX mdY)
  KEq s x y -> eqNeqOptIr t0 False (KEq s) x y
  KNEq s x y -> eqNeqOptIr t0 True (KNEq s) x y
  KGTh x y -> ordOptIr t0 (== GT) KGTh x y
  KLTh x y -> ordOptIr t0 (== LT) KLTh x y
  KGTEq x y -> ordOptIr t0 (/= LT) KGTEq x y
  KLTEq x y -> ordOptIr t0 (/= GT) KLTEq x y
  IrMethMap arr tag g ->
    let
      (t1, arr', mdA) = optIr t0 arr
      (t2, g', mdG) = optIr t1 g
     in
      (t2, IrMethMap arr' tag g', nodeMeta mdA (bindMeta tag mdG))
  IrMethFilter arr tag g ->
    let
      (t1, arr', mdA) = optIr t0 arr
      (t2, g', mdG) = optIr t1 g
     in
      (t2, IrMethFilter arr' tag g', nodeMeta mdA (bindMeta tag mdG))
  IrMethReduce arr z tagA tagB g ->
    let
      (t1, arr', mdA) = optIr t0 arr
      (t2, z', mdZ) = optIr t1 z
      (t3, g', mdG) = optIr t2 g
     in
      ( t3
      , IrMethReduce arr' z' tagA tagB g'
      , nodeMeta mdA (nodeMeta mdZ (bindMeta tagA (bindMeta tagB mdG)))
      )
  IrMethReduceRight arr z tagA tagB g ->
    let
      (t1, arr', mdA) = optIr t0 arr
      (t2, z', mdZ) = optIr t1 z
      (t3, g', mdG) = optIr t2 g
     in
      ( t3
      , IrMethReduceRight arr' z' tagA tagB g'
      , nodeMeta mdA (nodeMeta mdZ (bindMeta tagA (bindMeta tagB mdG)))
      )
  IrMethToSorted arr tagA tagB g ->
    let
      (t1, arr', mdA) = optIr t0 arr
      (t2, g', mdG) = optIr t1 g
     in
      ( t2
      , IrMethToSorted arr' tagA tagB g'
      , nodeMeta mdA (bindMeta tagA (bindMeta tagB mdG))
      )
  IrMethFrom n tag g ->
    let
      (t1, n', mdN) = optIr t0 n
      (t2, g', mdG) = optIr t1 g
     in
      (t2, IrMethFrom n' tag g', nodeMeta mdN (bindMeta tag mdG))
  IrLift x ->
    let
      (t1, x', md) = optIr t0 x
     in
      (t1, IrLift x', md)
  IrFFI form args ->
    let
      (t1, args', md) = optIrArgs t0 args
     in
      (t1, IrFFI form args', effectMd md)
  IrUnsafeObject o -> (t0, IrUnsafeObject o, IrMeta 1 IM.empty False False)
  IrUnsafeObjectGet x s ->
    let
      (t1, x', md) = optIr t0 x
     in
      (t1, IrUnsafeObjectGet x' s, effectMd md)
  IrUnsafeObjectAssign x y ->
    let
      (t1, x', mdX) = optIr t0 x
      (t2, y', mdY) = optIr t1 y
     in
      (t2, IrUnsafeObjectAssign x' y', effectMd (nodeMeta mdX mdY))
  IrCallMethod x method args ->
    let
      (t1, x', mdX) = optIr t0 x
      (t2, args', mdA) = optIrArgs t1 args
     in
      (t2, IrCallMethod x' method args', effectMd (nodeMeta mdX mdA))
  IrThenE x y ->
    let
      (t1, x', mdX) = optIr t0 x
      (t2, y', mdY) = optIr t1 y
     in
      (t2, IrThenE x' y', nodeMeta mdX mdY)
  IrBindRec tag r b ->
    let
      (t1, r', mdR) = optIr (t0 - optStep) r
      (t2, b', mdB) = optIr t1 b
     in
      (t2, IrBindRec tag r' b', bindMeta tag (nodeMeta mdR mdB))
  IrLambdaE tag g ->
    let
      (t1, g', md) = optIr (t0 - optStep) g
     in
      (t1, IrLambdaE tag g', bindMeta tag md)
  IrApplyE f x ->
    let
      (t1, f', mdF) = optIr t0 f
     in
      case f' of
        IrLambdaE tag g ->
          let
            (t2, x', mdX) = optIr t1 x
            (t3, g', mdG) = optIr t2 g
            (e', md') = elimIrBind mdX Nothing tag x' g' mdG
           in
            (t3, e', md')
        _ ->
          let
            (t2, x', mdX) = optIr t1 x
           in
            (t2, IrApplyE f' x', effectMd (nodeMeta mdF mdX))
  IrIfE c t e ->
    let
      (t1, c', mdC) = optIr t0 c
     in
      case c' of
        IrLift (IrLiteral (ValueBool P.True)) -> optIr t1 t
        IrLift (IrLiteral (ValueBool P.False)) -> optIr t1 e
        _ ->
          let
            (t2, t', mdT) = optIr t1 t
            (t3, e', mdE) = optIr t2 e
           in
            ( t3
            , IrIfE c' t' e'
            , nodeMeta mdC (nodeMeta mdT mdE)
            )
  IrWhile c b ->
    let
      (t1, c', mdC) = optIr t0 c
     in
      case c' of
        IrLift (IrLiteral (ValueBool P.False)) ->
          (t1, IrLift (IrLiteral ValueUnit), litMeta ValueUnit <> mdC)
        _ ->
          let
            (t2, b', mdB) = optIr t1 b
           in
            (t2, IrWhile c' b', effectMd (nodeMeta mdC mdB))
  IrForRange s e tag b ->
    let
      (t1, s', mdS) = optIr t0 s
      (t2, e', mdE) = optIr t1 e
      (t3, b', mdB) = optIr (t2 - optStep) b
     in
      ( t3
      , IrForRange s' e' tag b'
      , effectMd (nodeMeta mdS (nodeMeta mdE (bindMeta tag mdB)))
      )
  IrU8Set b i v ->
    let
      (t1, b', mdB) = optIr t0 b
      (t2, i', mdI) = optIr t1 i
      (t3, v', mdV) = optIr t2 v
     in
      (t3, IrU8Set b' i' v', effectMd (nodeMeta mdB (nodeMeta mdI mdV)))
  IrU8Fill b v ->
    let
      (t1, b', mdB) = optIr t0 b
      (t2, v', mdV) = optIr t1 v
     in
      (t2, IrU8Fill b' v', effectMd (nodeMeta mdB mdV))
  IrOptionCaseE o n tag s ->
    let
      (t1, o', mdO) = optIr t0 o
     in
      case peelIrOption o' of
        Just Nothing -> optIr t1 n
        Just (Just vn) ->
          let
            (t2, s', mdS) = optIr t1 s
            (e', md') =
              elimIrBind (boundIrMeta vn) Nothing tag (IrLift vn) s' mdS
           in
            (t2, e', md')
        Nothing ->
          let
            (t2, n', mdN) = optIr t1 n
            (t3, s', mdS) = optIr t2 s
           in
            ( t3
            , IrOptionCaseE o' n' tag s'
            , nodeMeta mdO (nodeMeta mdN (bindMeta tag mdS))
            )
  IrResultCaseE o tagE er tagO ok ->
    let
      (t1, o', mdO) = optIr t0 o
     in
      case peelIrResult o' of
        Just (Left x) ->
          let
            (t2, er', mdE) = optIr t1 er
            (res, md') = elimIrBind (boundIrMeta x) Nothing tagE (IrLift x) er' mdE
           in
            (t2, res, md')
        Just (Right x) ->
          let
            (t2, ok', mdS) = optIr t1 ok
            (res, md') = elimIrBind (boundIrMeta x) Nothing tagO (IrLift x) ok' mdS
           in
            (t2, res, md')
        Nothing ->
          let
            (t2, er', mdE) = optIr t1 er
            (t3, ok', mdS) = optIr t2 ok
           in
            ( t3
            , IrResultCaseE o' tagE er' tagO ok'
            , nodeMeta mdO (nodeMeta (bindMeta tagE mdE) (bindMeta tagO mdS))
            )
  IrStringCaseE s arms d ->
    let
      (t1, s', mdS) = optIr t0 s
     in
      case s' of
        IrLiteral (ValueString k) -> optIr t1 (fromMaybe d (lookup k arms))
        _ ->
          let
            (t2, arms', mdA) = mapAccumIrEffects t1 arms
            (t3, d', mdD) = optIr t2 d
           in
            (t3, IrStringCaseE s' arms' d', nodeMeta mdS (nodeMeta mdA mdD))
  IrThrow x ->
    let
      (t1, x', md) = optIr t0 x
     in
      (t1, IrThrow x', effectMd md)
  IrTry a tag k ->
    let
      (t1, a', mdA) = optIr t0 a
      (t2, k', mdK) = optIr (t1 - optStep) k
     in
      (t2, IrTry a' tag k', nodeMeta mdA (bindMeta tag mdK))
  IrObjectLit fs ->
    let
      (t1, fs', md) = mapAccumIrFields t0 fs
     in
      (t1, IrObjectLit fs', md)
  IrDeleteProp o k ->
    let
      (t1, o', mdO) = optIr t0 o
      (t2, k', mdK) = optIr t1 k
     in
      (t2, IrDeleteProp o' k', effectMd (nodeMeta mdO mdK))
  IrArrayLit es ->
    let
      (t1, es', md) = mapAccumIrNodeList t0 es
     in
      (t1, IrArrayLit es', md)
  _ -> error "JShark.Compiler.Ir.optIrNode: unhandled constructor"

binOptIr ::
  (?keepLets :: P.Bool) =>
  Int
  -> (IrNode -> IrNode -> IrNode)
  -> IrNode
  -> IrNode
  -> (Int, IrNode, IrMeta)
binOptIr !t0 k x y =
  let
    (t1, x', mdX) = optIr t0 x
    (t2, y', mdY) = optIr t1 y
   in
    (t2, k x' y', nodeMeta mdX mdY)

num2OptIr ::
  (?keepLets :: P.Bool) =>
  Int
  -> (Double -> Double -> Double)
  -> (IrNode -> IrNode -> IrNode)
  -> IrNode
  -> IrNode
  -> (Int, IrNode, IrMeta)
num2OptIr !t0 f kon x y =
  let
    (t1, x', mdX) = optIr t0 x
    (t2, y', mdY) = optIr t1 y
    res = case (x', y') of
      (IrLiteral (ValueNumber a), IrLiteral (ValueNumber b)) ->
        IrLiteral (ValueNumber (f a b))
      _ -> kon x' y'
   in
    (t2, res, kernelFoldMd res <> nodeMeta mdX mdY)

num1OptIr ::
  (?keepLets :: P.Bool) =>
  Int
  -> (Double -> Double)
  -> (IrNode -> IrNode)
  -> IrNode
  -> (Int, IrNode, IrMeta)
num1OptIr !t0 f kon x =
  let
    (t1, x', mdX) = optIr t0 x
    res = case x' of
      IrLiteral (ValueNumber a) -> IrLiteral (ValueNumber (f a))
      _ -> kon x'
   in
    (t1, res, kernelFoldMd res <> mdX)

-- | Constant-fold @==@\/@!=@ on same-family literal pairs. Families that do
-- not match never arise from the typed EDSL, so a 'Nothing' leaves the node
-- in place (the pre-merge code could not express such a node at all).
eqNeqOptIr ::
  (?keepLets :: P.Bool) =>
  Int
  -> P.Bool
  -> (IrNode -> IrNode -> IrNode)
  -> IrNode
  -> IrNode
  -> (Int, IrNode, IrMeta)
eqNeqOptIr !t0 neg kon x y =
  let
    (t1, x', mdX) = optIr t0 x
    (t2, y', mdY) = optIr t1 y
    fold b = IrLiteral (ValueBool (if neg then P.not b else b))
    res = case (x', y') of
      (IrLiteral a, IrLiteral b)
        | eqFoldableValue a && eqFoldableValue b
        , Just r <- sameFamilyEq a b ->
            fold r
      (IrFrozenLit as, IrFrozenLit bs)
        | Just ra <- mapM rfFromField as
        , Just rb <- mapM rfFromField bs
        , Just r <- recordEq ra rb ->
            fold r
      _ -> kon x' y'
   in
    (t2, res, kernelFoldMd res <> nodeMeta mdX mdY)

ordOptIr ::
  (?keepLets :: P.Bool) =>
  Int
  -> (Ordering -> P.Bool)
  -> (IrNode -> IrNode -> IrNode)
  -> IrNode
  -> IrNode
  -> (Int, IrNode, IrMeta)
ordOptIr !t0 cmp kon x y =
  let
    (t1, x', mdX) = optIr t0 x
    (t2, y', mdY) = optIr t1 y
    res = case (x', y') of
      (IrLiteral a, IrLiteral b)
        | isOrderableValue a && isOrderableValue b
        , Just o <- sameFamilyOrd a b ->
            IrLiteral (ValueBool (cmp o))
      _ -> kon x' y'
   in
    (t2, res, kernelFoldMd res <> nodeMeta mdX mdY)

-- | Value equality across two possibly-different hidden universes. Same
-- universe implies same constructor family, so family mismatch is 'Nothing'
-- (no fold); container equality recurses pairwise.
sameFamilyEq :: Value u -> Value v -> Maybe P.Bool
sameFamilyEq (ValueNumber a) (ValueNumber b) = Just (a == b)
sameFamilyEq (ValueBigInt a) (ValueBigInt b) = Just (a == b)
sameFamilyEq (ValueString a) (ValueString b) = Just (a == b)
sameFamilyEq (ValueBool a) (ValueBool b) = Just (a == b)
sameFamilyEq ValueUnit ValueUnit = Just True
sameFamilyEq (ValueArray as) (ValueArray bs) = listEq as bs
sameFamilyEq (ValueOption a) (ValueOption b) = case (a, b) of
  (Nothing, Nothing) -> Just True
  (Just x, Just y) -> sameFamilyEq x y
  _ -> Just False
sameFamilyEq (ValueResult a) (ValueResult b) = case (a, b) of
  (Left x, Left y) -> sameFamilyEq x y
  (Right x, Right y) -> sameFamilyEq x y
  _ -> Just False
sameFamilyEq (ValueRegex a) (ValueRegex b) = Just (a == b)
sameFamilyEq (ValueUint8Array a) (ValueUint8Array b) = Just (a == b)
sameFamilyEq (ValueFrozen as) (ValueFrozen bs) =
  recordEq (map valueField as) (map valueField bs)
sameFamilyEq (ValueFunction _) (ValueFunction _) =
  error "JShark.Compiler.Ir: functions cannot be compared for equality"
sameFamilyEq _ _ = Nothing

listEq :: [Value u] -> [Value v] -> Maybe P.Bool
listEq as bs
  | length as /= length bs = Just False
  | otherwise = go as bs
 where
  go [] [] = Just True
  go (x : xs) (y : ys) = case sameFamilyEq x y of
    Just True -> go xs ys
    Just False -> Just False
    Nothing -> Nothing
  go _ _ = Nothing

-- | Cross-type ordering for the orderable literal families.
sameFamilyOrd :: Value u -> Value v -> Maybe Ordering
sameFamilyOrd (ValueNumber a) (ValueNumber b) = Just (compare a b)
sameFamilyOrd (ValueBigInt a) (ValueBigInt b) = Just (compare a b)
sameFamilyOrd (ValueString a) (ValueString b) = Just (compare a b)
sameFamilyOrd (ValueBool a) (ValueBool b) = Just (compare a b)
sameFamilyOrd _ _ = Nothing

-- | A runtime record field: extra flag, JS name, hidden value.
data RF = RF !P.Bool !Text !SomeIrValue

-- | Peel a literal object field to its runtime form. Effect fields (and
-- non-literal children) block a compile-time equality fold.
rfFromField :: IrField -> Maybe RF
rfFromField = \case
  IrField k (IrLiteral v) -> Just (RF False k (SomeIrValue v))
  IrFieldExtra k (IrLiteral v) -> Just (RF True k (SomeIrValue v))
  IrFieldEff {} -> Nothing
  IrFieldExtraEff {} -> Nothing
  _ -> Nothing

-- | The same peel for a runtime 'FieldLit' inside a 'ValueFrozen' literal.
valueField :: FieldLit Value r -> RF
valueField = \case
  FieldLit @k (Literal v) ->
    RF False (T.pack (symbolVal (Proxy @k))) (SomeIrValue v)
  FieldLitExtra @k (Literal v) ->
    RF True (T.pack (symbolVal (Proxy @k))) (SomeIrValue v)
  _ -> error "JShark.Compiler.Ir.valueField: unfrozen frozen-literal field"

-- | Last-wins record equality mirroring 'Evaluate.frozenEq': duplicates of
-- the same name collapse to the last entry, then every kept field on one
-- side must match a same-name field on the other. Declared vs extra fields
-- never match; extras compare by name plus value family (type equality in
-- the typed world).
recordEq :: [RF] -> [RF] -> Maybe P.Bool
recordEq as bs =
  let
    las = lastWins as
    lbs = lastWins bs
   in
    if length las /= length lbs
      then Just False
      else Just (all (\fa -> any (rfEq fa) lbs) las)

rfEq :: RF -> RF -> P.Bool
rfEq (RF isA na va) (RF isB nb vb) =
  isA == isB && na == nb && maybe False id (valEq va vb)
 where
  valEq (SomeIrValue a) (SomeIrValue b) = sameFamilyEq a b

lastWins :: [RF] -> [RF]
lastWins = keepLastByKey (\(RF _ n _) -> n)

-- | Last-wins field lookup by name. Only plain declared fields project
-- (mirrors the typed 'GetField' rule; extras stay unprojectable).
lookupIrField :: Text -> [IrField] -> Maybe IrNode
lookupIrField key = go . reverse
 where
  go [] = Nothing
  go (IrField k e : rest)
    | k == key = Just e
    | otherwise = go rest
  go (_ : rest) = go rest

optIrFixedF ::
  (?keepLets :: P.Bool) =>
  Int -> SomeFixedOp -> [IrNode] -> (Int, IrNode, IrMeta)
optIrFixedF !t0 (SomeFixedOp op) args = case (op, args) of
  (n, [x])
    | Just (MathUnary n') <- matchMathUnary n ->
        let
          (t1, x', mdX) = optIr t0 x
          res = case x' of
            IrLiteral (ValueNumber a)
              | Just r <- exactMathUnary n' a -> IrLiteral (ValueNumber r)
            _ -> IrFixed (SomeFixedOp n') [x']
         in
          (t1, res, fixedFoldMd (SomeFixedOp n') res <> mdX)
  (n, [x, y])
    | Just (MathBinary n') <- matchMathBinary n ->
        let
          (t1, x', mdX) = optIr t0 x
          (t2, y', mdY) = optIr t1 y
          res = case (x', y') of
            (IrLiteral (ValueNumber a), IrLiteral (ValueNumber b))
              | Just r <- exactMathBinary n' a b -> IrLiteral (ValueNumber r)
            _ -> IrFixed (SomeFixedOp n') [x', y']
         in
          (t2, res, fixedFoldMd (SomeFixedOp n') res <> nodeMeta mdX mdY)
  (FixArrLen, [x]) ->
    let
      (t1, x', mdX) = optIr t0 x
      res = case x' of
        IrLiteral (ValueArray vs) ->
          IrLiteral (ValueNumber (fromIntegral (length vs)))
        _ -> IrFixed (SomeFixedOp FixArrLen) [x']
     in
      (t1, res, fixedFoldMd (SomeFixedOp FixArrLen) res <> mdX)
  (FixToBigInt, [x]) ->
    let
      (t1, x', mdX) = optIr t0 x
      res = case x' of
        IrLiteral (ValueNumber d)
          | isFiniteDouble d
          , let
              n = truncate d
          , d == fromInteger n ->
              IrLiteral (ValueBigInt n)
        _ -> IrFixed (SomeFixedOp FixToBigInt) [x']
     in
      (t1, res, fixedFoldMd (SomeFixedOp FixToBigInt) res <> mdX)
  (FixFromBigInt, [x]) ->
    let
      (t1, x', mdX) = optIr t0 x
      res = case x' of
        IrLiteral (ValueBigInt n) -> IrLiteral (ValueNumber (fromInteger n))
        _ -> IrFixed (SomeFixedOp FixFromBigInt) [x']
     in
      (t1, res, fixedFoldMd (SomeFixedOp FixFromBigInt) res <> mdX)
  (FixParseBigInt, [x]) ->
    let
      (t1, x', mdX) = optIr t0 x
      res = case x' of
        IrLiteral (ValueString s)
          | Just n <- parseBigIntString (T.unpack s) ->
              IrLiteral (ValueBigInt n)
        _ -> IrFixed (SomeFixedOp FixParseBigInt) [x']
     in
      (t1, res, fixedFoldMd (SomeFixedOp FixParseBigInt) res <> mdX)
  (_, [x]) ->
    let
      (t1, x', mdX) = optIr t0 x
     in
      (t1, IrFixed (SomeFixedOp op) [x'], fixedKeepMd op <> mdX)
  (_, [x, y]) ->
    let
      (t1, x', mdX) = optIr t0 x
      (t2, y', mdY) = optIr t1 y
     in
      (t2, IrFixed (SomeFixedOp op) [x', y'], fixedKeepMd op <> nodeMeta mdX mdY)
  (_, [x, y, z]) ->
    let
      (t1, x', mdX) = optIr t0 x
      (t2, y', mdY) = optIr t1 y
      (t3, z', mdZ) = optIr t2 z
     in
      ( t3
      , IrFixed (SomeFixedOp op) [x', y', z']
      , fixedKeepMd op <> nodeMeta mdX (nodeMeta mdY mdZ)
      )
  _ -> error "JShark.Compiler.Ir.optIrFixedF: unexpected fixed arity"

fixedFoldMd :: SomeFixedOp -> IrNode -> IrMeta
fixedFoldMd sf res = case res of
  IrLiteral v -> litMeta v
  _ -> case sf of
    SomeFixedOp n -> fixedKeepMd n

fixedKeepMd :: FixedOp a b c u -> IrMeta
fixedKeepMd n = IrMeta 1 IM.empty (isPureFixed n) P.False

kernelFoldMd :: IrNode -> IrMeta
kernelFoldMd res = case res of
  IrLiteral v -> litMeta v
  _ -> kernelFoldMdK

kernelFoldMdK :: IrMeta
kernelFoldMdK = IrMeta 1 IM.empty True False

-- | Known-constructor scrutinees for 'IrOptionCase' \/ 'IrOptionCaseE'.
peelIrOption :: IrNode -> Maybe (Maybe IrNode)
peelIrOption = \case
  IrLiteral (ValueOption Nothing) -> Just Nothing
  IrLiteral (ValueOption (Just v)) -> Just (Just (IrLiteral v))
  -- Host literals are never JS null; FFI / vars stay unpeeled so
  -- 'Storage.getItem' keeps its @=== null@ check.
  IrUnsafeNullable (IrLiteral v) -> Just (Just (IrLiteral v))
  _ -> Nothing

-- | Known-constructor scrutinees for 'IrResultCase' \/ 'IrResultCaseE'.
peelIrResult :: IrNode -> Maybe (Either IrNode IrNode)
peelIrResult = \case
  IrLiteral (ValueResult (Left v)) -> Just (Left (IrLiteral v))
  IrLiteral (ValueResult (Right v)) -> Just (Right (IrLiteral v))
  IrResultOk x -> Just (Right x)
  IrResultErr x -> Just (Left x)
  _ -> Nothing

-- | Metadata of a case-bound payload.
boundIrMeta :: IrNode -> IrMeta
boundIrMeta x = case x of
  IrLiteral v -> litMeta v
  _ -> metaIr x

optIrFnLit ::
  (?keepLets :: P.Bool) =>
  Int -> [Int] -> [Maybe Text] -> IrNode -> (Int, IrNode, IrMeta)
optIrFnLit t0 tags _names b0 =
  let
    (t1, mdB, b') = goBinders t0 tags b0
   in
    (t1, IrFnLit tags _names b', mdB)
 where
  goBinders t [] b = let (t1, b', md) = optIr t b in (t1, md, b')
  goBinders t (tag : rest) b =
    let
      (t1, md, b') = goBinders (t - optStep) rest b
     in
      (t1, bindMeta tag md, b')

mapAccumIrFields ::
  (?keepLets :: P.Bool) =>
  Int -> [IrField] -> (Int, [IrField], IrMeta)
mapAccumIrFields !t0 fs =
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
  step t fl = case fl of
    IrField k c -> let (t', c', m) = optIr t c in (t', IrField k c', m)
    IrFieldEff k c -> let (t', c', m) = optIr t c in (t', IrFieldEff k c', m)
    IrFieldExtra k c -> let (t', c', m) = optIr t c in (t', IrFieldExtra k c', m)
    IrFieldExtraEff k c -> let (t', c', m) = optIr t c in (t', IrFieldExtraEff k c', m)

mapAccumIrEffects ::
  (?keepLets :: P.Bool) =>
  Int -> [(Text, IrNode)] -> (Int, [(Text, IrNode)], IrMeta)
mapAccumIrEffects !t0 arms =
  foldr
    ( \(k, e) (!t, acc, !md) ->
        let
          (t', e', md') = optIr t e
         in
          (t', (k, e') : acc, md' <> md)
    )
    (t0, [], mempty)
    arms

optIrArgs ::
  (?keepLets :: P.Bool) => Int -> [IrNode] -> (Int, [IrNode], IrMeta)
optIrArgs = go
 where
  go t0 [] = (t0, [], mempty)
  go t0 (x : xs) =
    let
      (t1, x', mdX) = optIr t0 x
      (t2, xs', mdXs) = go t1 xs
     in
      (t2, x' : xs', nodeMeta mdX mdXs)

mapAccumIrNodeList ::
  (?keepLets :: P.Bool) => Int -> [IrNode] -> (Int, [IrNode], IrMeta)
mapAccumIrNodeList !t0 es =
  foldr
    ( \e (!t, acc, !md) ->
        let
          (t', e', md') = optIr t e
         in
          (t', e' : acc, md' <> md)
    )
    (t0, [], mempty)
    es
