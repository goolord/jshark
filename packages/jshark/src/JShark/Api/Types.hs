{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Suppresses missing sigs on compare pattern synonyms only; GHC 9.14 cannot
-- attach @Comparable@ to bidirectional @GTh@/@LTh@/@GTEq@/@LTEq@ (see 'mkGTh').
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

-- | Two PHOAS syntax trees for a typed subset of JavaScript
-- (Crockford's Good Parts kernel; binders as in Kmett's PHOAS).
--
-- * 'Expr' is pure. The kernel is the language: literals, operators,
--   @===@ / @!==@ (never @==@), @?:@, @typeof@, unary functions,
--   @const@ lets, object literals, @a[i]@. No @with@, @eval@, @new@,
--   or @this@. Combinators such as @zipWith@ are Haskell functions
--   that build this tree, not extra constructors.
-- * One 'Std' constructor holds every pure JS standard-library name
--   we expose ('Math.sin', @Array.prototype.map@, @JSON.stringify@, …).
-- * Haskell sums encoded as JS: 'Option' is @{some: Bool, value?}@;
--   'Result' is @{ok: Bool, value: …}@.
-- * 'Effect' is impure: statements, FFI, mutation, I/O, free-text names.
--
-- Binders are parametric (@f :: Universe -> Type@). A closed term is
-- an end @forall f. …@, the same quantification as Kmett's
-- @type End p = forall x. p x x@. The two trees meet at FFI via 'Arg'.
module JShark.Api.Types
  ( Universe (..)
  , Value (..)
  , Effect (..)
  , Arg (..)
  , Field
  , FieldLit (FieldLit, FieldLitEffect, FieldLitExtra, FieldLitExtraEffect)
  , fieldKey
  , FFIForm (..)
  , Expr (..)
  , FnBody (..)
  , Std (..)
  , Kernel (..)
  , data Plus
  , data Times
  , data Minus
  , data Negate
  , data FracDiv
  , data Rem
  , data BitAnd
  , data BitOr
  , data BitXor
  , data Shl
  , data Shr
  , data UShr
  , data And
  , data Or
  , data Eq
  , data NEq
  , data GTh
  , data LTh
  , data GTEq
  , data LTEq
  , data Concat
  , data Show
  , data TypeOf
  , FixedOp (..)
  , FixedArgs (..)
  , Method (..)
  , fixed1
  , fixed2
  , fixed3
  , expr1
  , expr2
  , expr3
  , GroupBy
  , ZipPair
  , ReduceWith
  , ClosedExpr
  , ClosedEffect
  , LamInfo (..)
  , noLamInfo
  , Comparable
  , KnownScalar
  , NumericU (..)
  , BigBinOp (..)
  , mkEq
  , mkNEq
  , structuralEq
  , structuralNEq
  , mkGTh
  , mkLTh
  , mkGTEq
  , mkLTEq
  , plusE
  , timesE
  , minusE
  , fracDivE
  , negateE
  , andE
  , orE
  , concatE
  , remE
  , bitAndE
  , bitOrE
  , bitXorE
  , shlE
  , shrE
  , ushrE
  , EffectSyntax (..)
  , toSyntax
  , toSyntax_
  , bindExpr
  , fromSyntax
  , seqSyntax
  , (>>)
  )
where

import Control.Monad (ap)
import Data.Array.Byte (ByteArray)
import Data.Bits (xor, (.&.), (.|.))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified GHC.Exts as Exts
import GHC.Stack (HasCallStack)
import GHC.TypeLits
  ( KnownSymbol
  , Symbol
  , symbolVal
  )
import JShark.Api.Caller (callerBinderHint)
import JShark.Api.Rec
import JShark.Compiler.JsNum (jsBit2, jsRem, jsShl, jsShr, jsUShr)
import Prelude hiding ((>>))
import qualified Prelude as P

-- | The kind of a JShark value: its surface JS type. Every 'Expr' and
-- 'Effect' node is indexed by a 'Universe'.
data Universe
  = -- | IEEE @Number@.
    Number
  | -- | Arbitrary-precision @BigInt@.
    BigInt
  | -- | UTF-16 @String@.
    String
  | -- | JS @undefined@ / @void@.
    Unit
  | -- | JS @Array@ of a uniform element universe.
    Array Universe
  | -- | Unary. Nest for n-ary JS functions.
    Function Universe Universe
  | -- | JS @function(a, b, …) { … }@ — not a curried @'Function@ chain.
    -- Parameter universes match 'JShark.Api.Params.RowUs' order ('fnLit' / 'toFn').
    Fn [Universe] Universe
  | -- | JS @null@ or a present value.
    Option Universe
  | -- | Haskell 'Either'; JS @{ok: Bool, value: …}@
    Result Universe Universe
  | -- | JS @RegExp@.
    Regex
  | -- | JS @boolean@.
    Bool
  | -- | JS @Uint8Array@. Host 'ByteArray' literals and
    -- 'JShark.Api.newByteArray' (the latter on 'Effect', because
    -- allocation has identity). Not a 'MutableObject' row —
    -- 'JShark.Object.set' must not typecheck. JS can write the object.
    Uint8Array
  | -- | JS @Map@. Phantom key/value universes; native @Map@, not a plain
    -- object. Not a 'MutableObject' row — 'JShark.Object.set' must not
    -- typecheck on it. No host 'Value' / 'evaluate' support: live handles
    -- are allocated on 'Effect' only (same as DOM refs).
    Map Universe Universe
  | -- | JS @Set@. Phantom element universe; effect-only handles.
    Set Universe
  | -- | Frozen record. Row @r@ is a host 'Type', not a 'Universe' constructor.
    Object Type
  | -- | Mutable JS object. Same row @r@ as 'Object'.
    MutableObject Type

-- | A fully evaluated host value indexed by its 'Universe'. This is the
-- host denotation used by 'JShark.evaluate' (@f = Value@).
data Value :: Universe -> Type where
  -- | JS array.
  ValueArray :: [Value u] -> Value ('Array u)
  -- | IEEE @Number@.
  ValueNumber :: Double -> Value 'Number
  -- | Arbitrary-precision integer.
  ValueBigInt :: Integer -> Value 'BigInt
  -- | Text string.
  ValueString :: Text -> Value 'String
  -- | Host function on 'Value's.
  ValueFunction :: (Value u -> Value v) -> Value ('Function u v)
  -- | The JS @undefined@ value.
  ValueUnit :: Value 'Unit
  -- | 'Nothing' is JS @null@; 'Just' is the present value.
  ValueOption :: Maybe (Value u) -> Value ('Option u)
  -- | 'Left' is an error; 'Right' is a success.
  ValueResult :: Either (Value e) (Value a) -> Value ('Result e a)
  -- | Regular-expression source.
  ValueRegex :: Text -> Value 'Regex
  -- | JS boolean.
  ValueBool :: Bool -> Value 'Bool
  ValueUint8Array ::
    ByteArray
    -> Value 'Uint8Array
    -- ^ Contents of a @Uint8Array@ (unpinned 'ByteArray').
  ValueFrozen ::
    [FieldLit Value r]
    -> Value ('Object r)
    -- ^ Eval-only; not a surface literal.

-- | How to render an 'FFI' callee. 'FFILambda' is parenthesized at codegen;
--   'FFIExpr' omits the trailing @()@ that 'FFICall' adds when args are empty.
data FFIForm
  = -- | Call a named callee: @name(args…)@.
    FFICall !Text
  | -- | Inline a lambda source string, parenthesized at codegen.
    FFILambda !Text
  | -- | Emit an expression, omitting the empty-argument @()@.
    FFIExpr !Text
  deriving stock (Eq, Ord)

-- | The impure PHOAS tree: statements, FFI, mutation, I/O, loops, and
-- free-text names. A value of type @Effect f u@ is an effectful
-- computation producing a @u@.
data Effect :: (Universe -> Type) -> Universe -> Type where
  Lift ::
    Expr f u
    -> Effect f u
    -- ^ Lift a pure expression into the effectful tree
  FFI ::
    FFIForm
    -> Rec (Arg f) us
    -> Effect f u
    -- ^ Foreign call. Args are 'Arg' so an effect need not pass through 'Expr'.
    -- | An opaque mutable object under a free-text (untyped) name.
  UnsafeObject :: Text -> Effect f ('MutableObject x)
  -- | Read a property from an opaque object: @o[k]@.
  UnsafeObjectGet :: Effect f object -> Text -> Effect f u
  -- | Assign a property on an opaque object: @o[k] = v@.
  UnsafeObjectAssign :: Effect f object -> Effect f assignment -> Effect f u
  CallMethod ::
    Effect f object
    -> Text
    -> Rec (Arg f) us
    -> Effect f u
    -- ^ @recv.method(args…)@
  Bind ::
    Maybe Text
    -> Effect f u
    -> (f u -> Effect f v)
    -> Effect f v
    -- ^ PHOAS bind (@const n = e@). The hint names the binder in readable
    --         output; 'EffectSyntax' @>>=@ captures it from 'HasCallStack'.
  ThenE ::
    Effect f u
    -> Effect f v
    -> Effect f v
    -- ^ Sequencing without a binder ('*>' / '>>' / discarded bind).
  BindRec ::
    (f u -> Effect f u)
    -> (f u -> Effect f v)
    -> Effect f v
    -- ^ Recursive bind (@let n; n = …n…@)
  LambdaE ::
    (f u -> Effect f v)
    -> Effect f ('Function u v)
    -- ^ Effectful function (weak PHOAS: binder is @f u@, not @Effect@)
    -- | Apply an effectful function.
  ApplyE :: Effect f ('Function u v) -> Effect f u -> Effect f v
  IfE ::
    Effect f 'Bool
    -> Effect f u
    -> Effect f u
    -> Effect f u
    -- ^ Effectful conditional. Statement @if@ is 'IfE' of two 'Unit' arms (see 'JShark.Api.discard'). A 'Lift'ed condition must not depend on 'Let' decls that only run once; an 'FFI' condition is re-emitted into the @if@ test.
  While ::
    Effect f 'Bool
    -> Effect f 'Unit
    -> Effect f 'Unit
    -- ^ Loop while the condition holds. The rendered condition is re-emitted on every iteration, so it must not depend on declarations that only run once. Use 'FFI' (not a bound var) when the test itself is a call.
  ForRange ::
    Expr f 'Number
    -> Expr f 'Number
    -> (f 'Number -> Effect f 'Unit)
    -> Effect f 'Unit
    -- ^ @for (let i = start; i < end; i++)@. Emits a C-style counted loop, not @forEach@.
  U8Set ::
    Expr f 'Uint8Array
    -> Expr f 'Number
    -> Expr f 'Number
    -> Effect f 'Unit
    -- ^ @u8[i] = v@.
  U8Fill ::
    Expr f 'Uint8Array
    -> Expr f 'Number
    -> Effect f 'Unit
    -- ^ @u8.fill(v)@.
  OptionCaseE ::
    Expr f ('Option u)
    -> Effect f v
    -> (f u -> Effect f v)
    -> Effect f v
    -- ^ Effectful 'optionCase'.
    -- | Effectful 'resultCase'.
  ResultCaseE ::
    Expr f ('Result e a) -> (f e -> Effect f v) -> (f a -> Effect f v) -> Effect f v
  StringCaseE ::
    Expr f 'String
    -> [(Text, Effect f v)]
    -> Effect f v
    -> Effect f v
    -- ^ @switch (s) { case k: …; default: … }@. First label wins; no fall-through.
  Throw ::
    Expr f 'String
    -> Effect f v
    -- ^ @throw e@. Never returns. Payload is a string.
  Try ::
    Effect f u
    -> (f 'String -> Effect f u)
    -> Effect f u
    -- ^ @try { a } catch (e) { k e }@
  ObjectLit ::
    [FieldLit f r]
    -> Effect f ('MutableObject r)
    -- ^ Typed @{k: v, …}@; keys come from 'FieldLit'
  DeleteProp ::
    Effect f object
    -> Expr f 'String
    -> Effect f 'Bool
    -- ^ @delete o[k]@
  ArrayLit ::
    [Effect f u]
    -> Effect f ('Array u)
    -- ^ @[e0, e1, …]@. Elements stay on 'Effect'.

-- | An FFI argument drawn from either syntax tree. This is the sanctioned
-- seam between 'Expr' and 'Effect'.
data Arg :: (Universe -> Type) -> Universe -> Type where
  -- | A pure expression argument.
  ArgExpr :: Expr f u -> Arg f u
  -- | An effectful argument.
  ArgEffect :: Effect f u -> Arg f u

-- | JS property type of row @r@ at key @k@. Open; each host row supplies
-- instances. The index on 'Object' / 'MutableObject' is this host 'Type', not a 'Universe'.
type family Field (r :: Type) (k :: Symbol) :: Universe

-- | @groupBy@ result row: @[{key, items}]@. Not a null-prototype dict.
data GroupBy (u :: Universe)

type instance Field (GroupBy u) "key" = 'String

type instance Field (GroupBy u) "items" = 'Array u

-- | @zipWith@ pair argument: @\{xs, ys\}@.
data ZipPair (a :: Universe) (b :: Universe)

type instance Field (ZipPair a b) "xs" = 'Array a

type instance Field (ZipPair a b) "ys" = 'Array b

-- | @reduce@ seed argument: @\{arr, z\}@.
data ReduceWith (acc :: Universe) (u :: Universe)

type instance Field (ReduceWith acc u) "arr" = 'Array u

type instance Field (ReduceWith acc u) "z" = acc

-- | One field of an object literal. @k@ is the JS name ('fieldKey').
-- Typed constructors require the value's universe to be 'Field' @r@ @k@.
-- Extra constructors carry a key that is not in the row (Generic sum
-- @payload@ on 'Tagged').
data FieldLit (f :: Universe -> Type) (r :: Type) where
  -- | A known field with a pure value; the universe comes from 'Field'.
  FieldLit ::
    forall k f r.
    KnownSymbol k =>
    Expr f (Field r k) -> FieldLit f r
  -- | A known field with an effectful value.
  FieldLitEffect ::
    forall k f r.
    KnownSymbol k =>
    Effect f (Field r k) -> FieldLit f r
  -- | An out-of-row field with a pure value (used for sum @payload@).
  FieldLitExtra ::
    forall k f r u.
    (KnownSymbol k, Typeable u) =>
    Expr f u -> FieldLit f r
  -- | An out-of-row field with an effectful value.
  FieldLitExtraEffect ::
    forall k f r u.
    (KnownSymbol k, Typeable u) =>
    Effect f u -> FieldLit f r

-- | The JS property name carried by a 'FieldLit'.
fieldKey :: FieldLit f r -> Text
fieldKey (FieldLit @k _) = T.pack (symbolVal (Proxy :: Proxy k))
fieldKey (FieldLitEffect @k _) = T.pack (symbolVal (Proxy :: Proxy k))
fieldKey (FieldLitExtra @k _) = T.pack (symbolVal (Proxy :: Proxy k))
fieldKey (FieldLitExtraEffect @k _) = T.pack (symbolVal (Proxy :: Proxy k))

-- | PHOAS spine for @'Fn'@: @JfCons@ binders, @JfNil@ body.
data FnBody (f :: Universe -> Type) (us :: [Universe]) (r :: Universe) where
  -- | The function body, with all parameters bound.
  JfNil :: Expr f r -> FnBody f '[] r
  -- | One parameter (with an optional name hint) and the rest of the spine.
  JfCons ::
    !(Maybe Text)
    -> (f u -> FnBody f us r)
    -> FnBody f (u ': us) r

-- | Codegen-only lambda metadata. 'lamTag' hoists a shared @$name@
-- binding; 'lamParam' is the JS parameter name when known.
data LamInfo = LamInfo
  { lamTag :: !(Maybe Text)
  -- ^ Shared @$name@ binding to hoist, if any.
  , lamParam :: !(Maybe Text)
  -- ^ JS parameter name, when known.
  }
  deriving (Eq, Show)

-- | 'LamInfo' with no tag and no known parameter name.
noLamInfo :: LamInfo
noLamInfo = LamInfo Nothing Nothing

-- | The pure PHOAS tree for the Good Parts kernel. @Expr f u@ is a pure
-- term producing a @u@; binders are parametric in @f@.
data Expr :: (Universe -> Type) -> Universe -> Type where
  -- Good Parts: values, arithmetic, strict equality, functions, @const@ lets
  Literal ::
    Value u
    -> Expr f u
    -- ^ A literal value. eg. 1, "foo", etc
  Let ::
    Maybe Text
    -> Expr f u
    -> (f u -> Expr f v)
    -> Expr f v
    -- ^ PHOAS let; codegen emits @const@. The hint names the binder in
    --         readable output ('esSourceNames'); 'JShark.Api.let_' sets it
    --         from 'HasCallStack'.
  LetRec ::
    (f u -> Expr f u)
    -> (f u -> Expr f v)
    -> Expr f v
    -- ^ Recursive let. The rhs must be productive; 'JShark.evaluate' ties
    --         the knot, so one that forces its own binder diverges.
  Lambda ::
    LamInfo
    -> (f u -> Expr f v)
    -> Expr f ('Function u v)
    -- ^ PHOAS lambda. 'lamTag' hoists a shared @$name@; 'lamParam'
    --         is the JS parameter name when known.
    -- | Function application: @f x@.
  Apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
  Var ::
    f u
    -> Expr f u
    -- ^ PHOAS variable (Kmett's Place / return)
  If ::
    Expr f 'Bool
    -> Expr f u
    -> Expr f u
    -> Expr f u
    -- ^ Ternary: @c ? t : e@
  OptionCase ::
    Expr f ('Option u)
    -> Expr f v
    -> (f u -> Expr f v)
    -> Expr f v
    -- ^ Eliminate a tagged 'Option', analogous to 'maybe'. Intro via
    -- 'JShark.Api.some' / 'JShark.Api.none' or @Literal (ValueOption …)@.
    -- This stays a primitive: 'JShark.evaluate' uses @f = Value@, so a
    -- bound @'Option u@ cannot be unwrapped by @if_ (opt .== none)@ plus a
    -- type-changing coerce.
    -- | Construct a successful 'Result'.
  ResultOk :: Expr f a -> Expr f ('Result e a)
  -- | Construct an error 'Result'.
  ResultErr :: Expr f e -> Expr f ('Result e a)
  ResultCase ::
    Expr f ('Result e a)
    -> (f e -> Expr f v)
    -> (f a -> Expr f v)
    -> Expr f v
    -- ^ Eliminate a 'Result', analogous to 'either'.
  Index ::
    Expr f ('Array u)
    -> Expr f 'Number
    -> Expr f u
    -- ^ JS @a[i]@. 'JShark.Array.index' wraps this with trunc / bounds / 'Error'.
  U8Index ::
    Expr f 'Uint8Array
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ JS @u8[i]@ without the bounds shim.
  Error ::
    Expr f 'String
    -> Expr f u
    -- ^ @throw new Error(msg)@. Partial, like Haskell 'error'.
  Std ::
    Std f u
    -> Expr f u
    -- ^ Pure JS standard library. Combinators (@zipWith@, @groupBy@) are
    --         Haskell functions over this tree, not extra constructors.
  FnLit ::
    forall f (us :: [Universe]) r.
    FnBody f us r
    -> Expr f ('Fn us r)
    -- ^ @function(a,b,…){ return … }@ from a row-typed callback ('fnLit' / 'toFn').
    -- Not a nested @'Function@ chain ('toLambda' / 'lambdaRow').
  UnsafeNullable ::
    Expr f u
    -> Expr f ('Option u)
    -- ^ Reinterpret a nullable JS value (e.g. from an FFI call) as an 'Option'.
  FrozenLit ::
    [FieldLit f r]
    -> Expr f ('Object r)
    -- ^ Frozen @{k: v, …}@. Identity-insensitive; field reads stay on 'Expr'.
  GetField ::
    forall k r f.
    KnownSymbol k =>
    Expr f ('Object r)
    -> Expr f (Field r k)
    -- ^ Pure @o.k@. Folded by 'sameSymbol' against 'FrozenLit'.

-- | Closed fixed-arity pure JS names (@Math.sin@, @arr.length@, …).
-- Higher-order stdlib (@map@, @reduce@, …) stays on 'Std' separately.
data FixedOp (a :: Universe) (b :: Universe) (c :: Universe) (u :: Universe) where
  FixSin :: FixedOp 'Number 'Unit 'Unit 'Number
  FixCos :: FixedOp 'Number 'Unit 'Unit 'Number
  FixTan :: FixedOp 'Number 'Unit 'Unit 'Number
  FixAsin :: FixedOp 'Number 'Unit 'Unit 'Number
  FixAcos :: FixedOp 'Number 'Unit 'Unit 'Number
  FixAtan :: FixedOp 'Number 'Unit 'Unit 'Number
  FixSinh :: FixedOp 'Number 'Unit 'Unit 'Number
  FixCosh :: FixedOp 'Number 'Unit 'Unit 'Number
  FixTanh :: FixedOp 'Number 'Unit 'Unit 'Number
  FixAsinh :: FixedOp 'Number 'Unit 'Unit 'Number
  FixAcosh :: FixedOp 'Number 'Unit 'Unit 'Number
  FixAtanh :: FixedOp 'Number 'Unit 'Unit 'Number
  FixSqrt :: FixedOp 'Number 'Unit 'Unit 'Number
  FixCbrt :: FixedOp 'Number 'Unit 'Unit 'Number
  FixExp :: FixedOp 'Number 'Unit 'Unit 'Number
  FixLog :: FixedOp 'Number 'Unit 'Unit 'Number
  FixLog2 :: FixedOp 'Number 'Unit 'Unit 'Number
  FixLog10 :: FixedOp 'Number 'Unit 'Unit 'Number
  FixFloor :: FixedOp 'Number 'Unit 'Unit 'Number
  FixCeil :: FixedOp 'Number 'Unit 'Unit 'Number
  FixRound :: FixedOp 'Number 'Unit 'Unit 'Number
  FixTrunc :: FixedOp 'Number 'Unit 'Unit 'Number
  FixAbs :: FixedOp 'Number 'Unit 'Unit 'Number
  FixSign :: FixedOp 'Number 'Unit 'Unit 'Number
  FixPow :: FixedOp 'Number 'Number 'Unit 'Number
  FixAtan2 :: FixedOp 'Number 'Number 'Unit 'Number
  FixMax :: FixedOp 'Number 'Number 'Unit 'Number
  FixMin :: FixedOp 'Number 'Number 'Unit 'Number
  FixHypot :: FixedOp 'Number 'Number 'Unit 'Number
  FixToUpper :: FixedOp 'String 'Unit 'Unit 'String
  FixToLower :: FixedOp 'String 'Unit 'Unit 'String
  FixTrim :: FixedOp 'String 'Unit 'Unit 'String
  FixArrLen :: FixedOp ('Array u) 'Unit 'Unit 'Number
  FixU8Len :: FixedOp 'Uint8Array 'Unit 'Unit 'Number
  FixStrLen :: FixedOp 'String 'Unit 'Unit 'Number
  FixStringify :: FixedOp u 'Unit 'Unit 'String
  FixIndexOf :: FixedOp 'String 'String 'Unit 'Number
  FixSplit :: FixedOp 'String 'String 'Unit ('Array 'String)
  FixIncludes :: FixedOp ('Array u) u 'Unit 'Bool
  FixConcat :: FixedOp ('Array u) ('Array u) 'Unit ('Array u)
  FixJoin :: FixedOp ('Array u) 'String 'Unit 'String
  FixTest :: FixedOp 'Regex 'String 'Unit 'Bool
  FixParseInt :: FixedOp 'String 'Number 'Unit 'Number
  FixToBigInt :: FixedOp 'Number 'Unit 'Unit 'BigInt
  FixFromBigInt :: FixedOp 'BigInt 'Unit 'Unit 'Number
  FixParseBigInt :: FixedOp 'String 'Unit 'Unit 'BigInt
  FixSlice :: FixedOp 'String 'Number 'Number 'String
  FixArrSlice :: FixedOp ('Array u) 'Number 'Number ('Array u)
  FixReplace :: FixedOp 'String 'String 'String 'String
  -- | Uncurried call @(f)(x, y)@ for hoisted two-arg helpers ('applyNamed2').
  FixCall2 ::
    FixedOp ('Function a ('Function b r)) a b r
  -- | @groupBy(arr, keyFn)@ — one pass, first-seen key order, append-only
  -- groups. Codegen emits the local-Map @$groupBy@ shim.
  FixGroupBy ::
    FixedOp ('Array u) ('Function u 'String) 'Unit ('Array ('Object (GroupBy u)))
  -- | Tagged @Option@ @some@: @{some: true, value: x}@. Native null\/value
  -- is 'UnsafeNullable', not this.
  FixSome ::
    FixedOp u 'Unit 'Unit ('Option u)

-- | Argument list for a 'FixedOp', matching its arity.
data FixedArgs f a b c where
  -- | One argument.
  ArgsU :: Expr f a -> FixedArgs f a 'Unit 'Unit
  -- | Two arguments.
  ArgsB :: Expr f a -> Expr f b -> FixedArgs f a b 'Unit
  -- | Three arguments.
  ArgsT :: Expr f a -> Expr f b -> Expr f c -> FixedArgs f a b c

-- | Apply a one-argument 'FixedOp' as a 'Std' term.
fixed1 :: FixedOp a 'Unit 'Unit u -> Expr f a -> Std f u
fixed1 op x = Fixed op (ArgsU x)

-- | Apply a two-argument 'FixedOp' as a 'Std' term.
fixed2 :: FixedOp a b 'Unit u -> Expr f a -> Expr f b -> Std f u
fixed2 op x y = Fixed op (ArgsB x y)

-- | Apply a three-argument 'FixedOp' as a 'Std' term.
fixed3 :: FixedOp a b c u -> Expr f a -> Expr f b -> Expr f c -> Std f u
fixed3 op x y z = Fixed op (ArgsT x y z)

-- | 'Std' 'Fixed' as an 'Expr' (for 'Num'/'Floating' instances).
expr1 :: FixedOp a 'Unit 'Unit u -> Expr f a -> Expr f u
expr1 op x = Std (fixed1 op x)

-- | Two-argument 'FixedOp' directly as an 'Expr'.
expr2 :: FixedOp a b 'Unit u -> Expr f a -> Expr f b -> Expr f u
expr2 op x y = Std (fixed2 op x y)

-- | Three-argument 'FixedOp' directly as an 'Expr'.
expr3 :: FixedOp a b c u -> Expr f a -> Expr f b -> Expr f c -> Expr f u
expr3 op x y z = Std (fixed3 op x y z)

-- | Higher-order array stdlib (@.map@, @.reduce@, @Array.from@, …).
data Method :: (Universe -> Type) -> Universe -> Type where
  -- | @arr.map(f)@.
  MethMap ::
    Expr f ('Array a)
    -> (f a -> Expr f b)
    -> Method f ('Array b)
  -- | @arr.filter(p)@.
  MethFilter ::
    Expr f ('Array a)
    -> (f a -> Expr f 'Bool)
    -> Method f ('Array a)
  -- | @arr.reduce(f, init)@, left to right.
  MethReduce ::
    Expr f ('Array a)
    -> Expr f b
    -> (f b -> f a -> Expr f b)
    -> Method f b
  -- | @arr.reduceRight(f, init)@.
  MethReduceRight ::
    Expr f ('Array a)
    -> Expr f b
    -> (f b -> f a -> Expr f b)
    -> Method f b
  -- | @arr.toSorted(cmp)@.
  MethToSorted ::
    Expr f ('Array a)
    -> (f a -> f a -> Expr f 'Number)
    -> Method f ('Array a)
  -- | @Array.from({length: n}, f)@.
  MethFrom ::
    Expr f 'Number
    -> (f 'Number -> Expr f a)
    -> Method f ('Array a)

-- | Exact integer kernel ops. JS @/@ on BigInt is truncating quot.
data BigBinOp
  = -- | @+@
    BPlus
  | -- | @-@
    BMinus
  | -- | @*@
    BTimes
  | -- | Truncating @/@ (JS BigInt division).
    BQuot
  | -- | @%@
    BRem
  | -- | @&@
    BBitAnd
  | -- | @|@
    BBitOr
  | -- | @^@
    BBitXor
  | -- | @<<@
    BShl
  | -- | @>>@
    BShr

-- | Good Parts kernel operators (@+@, @===@, @&&@, …).
data Kernel :: (Universe -> Type) -> Universe -> Type where
  KConcat ::
    Expr f 'String
    -> Expr f 'String
    -> Kernel f 'String
  KPlus ::
    Expr f 'Number
    -> Expr f 'Number
    -> Kernel f 'Number
  KTimes ::
    Expr f 'Number
    -> Expr f 'Number
    -> Kernel f 'Number
  KMinus ::
    Expr f 'Number
    -> Expr f 'Number
    -> Kernel f 'Number
  KNegate :: Expr f 'Number -> Kernel f 'Number
  KFracDiv ::
    Expr f 'Number
    -> Expr f 'Number
    -> Kernel f 'Number
  KRem ::
    Expr f 'Number
    -> Expr f 'Number
    -> Kernel f 'Number
  KBitAnd ::
    Expr f 'Number
    -> Expr f 'Number
    -> Kernel f 'Number
  KBitOr ::
    Expr f 'Number
    -> Expr f 'Number
    -> Kernel f 'Number
  KBitXor ::
    Expr f 'Number
    -> Expr f 'Number
    -> Kernel f 'Number
  KShl ::
    Expr f 'Number
    -> Expr f 'Number
    -> Kernel f 'Number
  KShr ::
    Expr f 'Number
    -> Expr f 'Number
    -> Kernel f 'Number
  KUShr ::
    Expr f 'Number
    -> Expr f 'Number
    -> Kernel f 'Number
  KBig ::
    BigBinOp
    -> Expr f 'BigInt
    -> Expr f 'BigInt
    -> Kernel f 'BigInt
  KBigNeg :: Expr f 'BigInt -> Kernel f 'BigInt
  KAnd ::
    Expr f 'Bool
    -> Expr f 'Bool
    -> Kernel f 'Bool
  KOr ::
    Expr f 'Bool
    -> Expr f 'Bool
    -> Kernel f 'Bool
  -- | First flag is 'True' when codegen must emit '$valueEq'.
  -- Scalars ('Number', 'String', 'Bool', …) pass 'False' and become @===@.
  KEq :: Bool -> Expr f a -> Expr f a -> Kernel f 'Bool
  KNEq :: Bool -> Expr f a -> Expr f a -> Kernel f 'Bool
  KGTh ::
    Comparable a =>
    Expr f a
    -> Expr f a
    -> Kernel f 'Bool
  KLTh ::
    Comparable a =>
    Expr f a
    -> Expr f a
    -> Kernel f 'Bool
  KGTEq ::
    Comparable a =>
    Expr f a
    -> Expr f a
    -> Kernel f 'Bool
  KLTEq ::
    Comparable a =>
    Expr f a
    -> Expr f a
    -> Kernel f 'Bool
  KShow :: Expr f a -> Kernel f 'String
  KTypeOf :: Expr f a -> Kernel f 'String

-- | Pure JS standard library, applied. One 'Expr' constructor ('Std')
-- holds this sum — not a constructor per method.
data Std :: (Universe -> Type) -> Universe -> Type where
  -- | A fixed-arity library call.
  Fixed ::
    FixedOp a b c u
    -> FixedArgs f a b c
    -> Std f u
  -- | A higher-order method.
  Method ::
    Method f u
    -> Std f u
  -- | A kernel operator.
  Kernel ::
    Kernel f u
    -> Std f u

-- | JS numeric addition: @x + y@.
pattern Plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Plus x y <- Std (Kernel (KPlus x y))
 where
  Plus = plusE

-- | JS numeric multiplication: @x * y@.
pattern Times :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Times x y <- Std (Kernel (KTimes x y))
 where
  Times = timesE

-- | JS numeric subtraction: @x - y@.
pattern Minus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Minus x y <- Std (Kernel (KMinus x y))
 where
  Minus = minusE

-- | JS numeric negation: @-x@.
pattern Negate :: Expr f 'Number -> Expr f 'Number
pattern Negate x <- Std (Kernel (KNegate x))
 where
  Negate = negateE

-- | JS floating division: @x / y@.
pattern FracDiv :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern FracDiv x y <- Std (Kernel (KFracDiv x y))
 where
  FracDiv = fracDivE

-- | JS remainder: @x % y@.
pattern Rem :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Rem x y <- Std (Kernel (KRem x y))
 where
  Rem = remE

-- | JS bitwise AND: @x & y@.
pattern BitAnd :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern BitAnd x y <- Std (Kernel (KBitAnd x y))
 where
  BitAnd = bitAndE

-- | JS bitwise OR: @x | y@.
pattern BitOr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern BitOr x y <- Std (Kernel (KBitOr x y))
 where
  BitOr = bitOrE

-- | JS bitwise XOR: @x ^ y@.
pattern BitXor :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern BitXor x y <- Std (Kernel (KBitXor x y))
 where
  BitXor = bitXorE

-- | JS left shift: @x << y@.
pattern Shl :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Shl x y <- Std (Kernel (KShl x y))
 where
  Shl = shlE

-- | JS sign-propagating right shift: @x >> y@.
pattern Shr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Shr x y <- Std (Kernel (KShr x y))
 where
  Shr = shrE

-- | JS unsigned right shift: @x >>> y@.
pattern UShr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern UShr x y <- Std (Kernel (KUShr x y))
 where
  UShr = ushrE

-- | Short-circuiting logical AND: @x && y@.
pattern And :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
pattern And x y <- Std (Kernel (KAnd x y))
 where
  And x y = Std (Kernel (KAnd x y))

-- | Short-circuiting logical OR: @x || y@.
pattern Or :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
pattern Or x y <- Std (Kernel (KOr x y))
 where
  Or x y = Std (Kernel (KOr x y))

-- | Strict equality. Scalars become JS @===@; aggregates use @$valueEq@.
pattern Eq :: Expr f a -> Expr f a -> Expr f 'Bool
pattern Eq x y <- Std (Kernel (KEq _ x y))
 where
  Eq x y = structuralEq x y

-- | Strict inequality. Scalars become JS @!==@; aggregates use @$valueEq@.
pattern NEq :: Expr f a -> Expr f a -> Expr f 'Bool
pattern NEq x y <- Std (Kernel (KNEq _ x y))
 where
  NEq x y = structuralNEq x y

-- | Equality that always uses structural @$valueEq@ (even for scalars).
structuralEq :: Expr f a -> Expr f a -> Expr f 'Bool
structuralEq x y = Std (Kernel (KEq True x y))

-- | Structural inequality, always via @$valueEq@.
structuralNEq :: Expr f a -> Expr f a -> Expr f 'Bool
structuralNEq x y = Std (Kernel (KNEq True x y))

-- | 'True' for universes that JS can compare with @===@ / @!==@.
-- The incoherent default keeps polymorphic @u@ on '$valueEq'.
class KnownScalar (u :: Universe) where
  isScalarTy :: Bool

instance KnownScalar 'Number where
  isScalarTy = True

instance KnownScalar 'BigInt where
  isScalarTy = True

instance KnownScalar 'String where
  isScalarTy = True

instance KnownScalar 'Bool where
  isScalarTy = True

instance KnownScalar 'Unit where
  isScalarTy = True

instance KnownScalar 'Regex where
  isScalarTy = True

-- | Equality that folds scalar literals at compile time. Uses @===@ when
-- @a@ is a 'KnownScalar', otherwise structural @$valueEq@.
mkEq :: forall f a. KnownScalar a => Expr f a -> Expr f a -> Expr f 'Bool
mkEq (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueBool (x == y))
mkEq (Literal (ValueBool x)) (Literal (ValueBool y)) =
  Literal (ValueBool (x == y))
mkEq (Literal (ValueString x)) (Literal (ValueString y)) =
  Literal (ValueBool (x == y))
mkEq x y = Std (Kernel (KEq (not (isScalarTy @a)) x y))
{-# INLINE [1] mkEq #-}

-- | Negated 'mkEq', with the same literal folding and scalar/structural split.
mkNEq :: forall f a. KnownScalar a => Expr f a -> Expr f a -> Expr f 'Bool
mkNEq (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueBool (x /= y))
mkNEq (Literal (ValueBool x)) (Literal (ValueBool y)) =
  Literal (ValueBool (x /= y))
mkNEq (Literal (ValueString x)) (Literal (ValueString y)) =
  Literal (ValueBool (x /= y))
mkNEq x y = Std (Kernel (KNEq (not (isScalarTy @a)) x y))
{-# INLINE [1] mkNEq #-}

-- | Compare helpers carry the 'Comparable' constraint GHC cannot attach to
-- the bidirectional pattern synonyms below (two @Expr f a@ fields scope
-- separate type variables in GHC 9.14).
mkGTh :: forall f a. Comparable a => Expr f a -> Expr f a -> Expr f 'Bool
mkGTh (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueBool (x > y))
mkGTh x y = Std (Kernel (KGTh x y))
{-# INLINE [1] mkGTh #-}

-- | @<@ helper that folds numeric literals; see 'mkGTh'.
mkLTh :: forall f a. Comparable a => Expr f a -> Expr f a -> Expr f 'Bool
mkLTh (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueBool (x < y))
mkLTh x y = Std (Kernel (KLTh x y))
{-# INLINE [1] mkLTh #-}

-- | @>=@ helper that folds numeric literals; see 'mkGTh'.
mkGTEq :: forall f a. Comparable a => Expr f a -> Expr f a -> Expr f 'Bool
mkGTEq (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueBool (x >= y))
mkGTEq x y = Std (Kernel (KGTEq x y))
{-# INLINE [1] mkGTEq #-}

-- | @<=@ helper that folds numeric literals; see 'mkGTh'.
mkLTEq :: forall f a. Comparable a => Expr f a -> Expr f a -> Expr f 'Bool
mkLTEq (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueBool (x <= y))
mkLTEq x y = Std (Kernel (KLTEq x y))
{-# INLINE [1] mkLTEq #-}

-- | @x > y@ for 'Comparable' universes.
pattern GTh x y = Std (Kernel (KGTh x y))

-- | @x < y@ for 'Comparable' universes.
pattern LTh x y = Std (Kernel (KLTh x y))

-- | @x >= y@ for 'Comparable' universes.
pattern GTEq x y = Std (Kernel (KGTEq x y))

-- | @x <= y@ for 'Comparable' universes.
pattern LTEq x y = Std (Kernel (KLTEq x y))

-- | JS string concatenation: @x + y@.
pattern Concat :: Expr f 'String -> Expr f 'String -> Expr f 'String
pattern Concat x y <- Std (Kernel (KConcat x y))
 where
  Concat = concatE

-- | Render a value to its JS string form (@$show@).
pattern Show :: Expr f a -> Expr f 'String
pattern Show x <- Std (Kernel (KShow x))
 where
  Show x = Std (Kernel (KShow x))

-- | JS @typeof x@.
pattern TypeOf :: Expr f a -> Expr f 'String
pattern TypeOf x <- Std (Kernel (KTypeOf x))
 where
  TypeOf x = Std (Kernel (KTypeOf x))

-- | Closed pure term: no free PHOAS binders. The end @forall f. 'Expr' f u@.
type ClosedExpr (u :: Universe) = forall (f :: Universe -> Type). Expr f u

-- | Closed effectful term: no free PHOAS binders. The end @forall f. 'Effect' f u@.
type ClosedEffect (u :: Universe) = forall (f :: Universe -> Type). Effect f u

-- | Ordering on the Good Parts primitives. Objects/arrays use JS
-- 'ToPrimitive' and are not constructible here.
class Comparable (u :: Universe)

instance Comparable 'Number

instance Comparable 'BigInt

instance Comparable 'String

instance Comparable 'Bool

-- | 'IsString' for JS string literals:
--
-- * @Value 'String@ — @"hi"@
-- * @Expr f 'String@ — @"hi"@ as 'Literal'
--
-- Prefer an explicit type signature when the hole is ambiguous with
-- 'String'/'Text'. Use 'JShark.Api.string' for runtime 'Text' values.
instance forall u. u ~ 'String => Exts.IsString (Value u) where
  fromString = ValueString . Exts.fromString

instance forall (f :: (Universe -> Type)) u. u ~ 'String => Exts.IsString (Expr f u) where
  fromString s = Literal (Exts.fromString s)

instance Semigroup (Expr f 'String) where
  (<>) = concatE

instance Monoid (Expr f 'String) where
  mempty = Literal (ValueString mempty)

instance Semigroup (Expr f ('Array u)) where
  (<>) xs ys = expr2 FixConcat xs ys

instance Monoid (Expr f ('Array u)) where
  mempty = Literal (ValueArray [])

-- | @base@ 'Maybe': combine innards when both are 'Some'. Not 'Alternative'.
-- The right argument is needed in both arms, so it is bound once with 'Let'.
instance Semigroup (Expr f u) => Semigroup (Expr f ('Option u)) where
  o <> d =
    Let Nothing d $ \dv ->
      OptionCase o (Var dv) $ \x ->
        OptionCase (Var dv) (expr1 FixSome (Var x)) $ \y ->
          expr1 FixSome (Var x <> Var y)

instance Semigroup (Expr f u) => Monoid (Expr f ('Option u)) where
  mempty = Literal (ValueOption Nothing)

instance Semigroup (Expr f ('Result e a)) where
  l <> r = ResultCase l (\_ -> r) (\_ -> l)

instance Semigroup (Expr f a) => Semigroup (Expr f ('Function r a)) where
  g <> h = Lambda noLamInfo (\x -> Apply g (Var x) <> Apply h (Var x))

instance Monoid (Expr f a) => Monoid (Expr f ('Function r a)) where
  mempty = Lambda noLamInfo (\_ -> mempty)

-- | 'Num' / 'Fractional' / 'Floating' for JS numbers:
--
-- * @Value 'Number@ — @1@, @2.5@; arithmetic runs eagerly on host
--   'Double's (so @'Literal' (1 + 2)@ is already @'Literal' 3@)
-- * @Expr f 'Number@ — literals via 'Literal'; ops go through
--   'plusE' / 'timesE' / … so smart constructors fold
--   literal-literal cases at compile time. Remaining ops stay
--   AST nodes and fold later in codegen. @(**)@ is @Math.pow@,
--   not @exp (log x * y)@.
--
-- Prefer a signature when the hole is ambiguous. Use 'JShark.Api.number'
-- for arbitrary runtime 'Double's (integer literals can use 'Num' directly).
instance {-# INCOHERENT #-} forall u. u ~ 'Number => Num (Value u) where
  (+) = liftValue2 (+)
  (*) = liftValue2 (*)
  (-) = liftValue2 (-)
  abs = liftValue1 abs
  signum = liftValue1 signum
  fromInteger = ValueNumber . fromInteger
  negate = liftValue1 negate

instance forall u. u ~ 'Number => Fractional (Value u) where
  (/) = liftValue2 (/)
  fromRational = ValueNumber . fromRational

liftValue1 :: (Double -> Double) -> Value 'Number -> Value 'Number
liftValue1 f (ValueNumber a) = ValueNumber (f a)

liftValue2 ::
  (Double -> Double -> Double) -> Value 'Number -> Value 'Number -> Value 'Number
liftValue2 f (ValueNumber a) (ValueNumber b) = ValueNumber (f a b)

-- | Smart constructors fold literal-literal cases. INCOHERENT 'Num'
-- often inlines '(+)' past a named wrapper; these equations still
-- match. @INLINE [1]@ unfolds to the kernel after the match.
numBinE ::
  (Double -> Double -> Double)
  -> (Expr f 'Number -> Expr f 'Number -> Kernel f 'Number)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
numBinE f _ (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueNumber (f x y))
numBinE _ op x y = Std (Kernel (op x y))
{-# INLINE [1] numBinE #-}

-- | Smart 'Plus' that folds two numeric literals.
plusE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
plusE = numBinE (+) KPlus
{-# INLINE [1] plusE #-}

-- | Smart 'Times' that folds two numeric literals.
timesE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
timesE = numBinE (*) KTimes
{-# INLINE [1] timesE #-}

-- | Smart 'Minus' that folds two numeric literals.
minusE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
minusE = numBinE (-) KMinus
{-# INLINE [1] minusE #-}

-- | Smart 'FracDiv' that folds two numeric literals.
fracDivE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
fracDivE = numBinE (/) KFracDiv
{-# INLINE [1] fracDivE #-}

-- | Smart 'Negate' that folds a numeric literal.
negateE :: Expr f 'Number -> Expr f 'Number
negateE (Literal (ValueNumber x)) = Literal (ValueNumber (negate x))
negateE x = Std (Kernel (KNegate x))
{-# INLINE [1] negateE #-}

-- | Short-circuit only when the left is already a literal. Dropping a
-- non-literal left of @&& false@ / @|| true@ would skip 'Error' and
-- 'FixStringify' (impure). Same gate as the optimizer's foldAnd/foldOr.
andE :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
andE (Literal (ValueBool False)) _ = Literal (ValueBool False)
andE (Literal (ValueBool True)) y = y
andE x (Literal (ValueBool True)) = x
andE x y = Std (Kernel (KAnd x y))
{-# INLINE [1] andE #-}

-- | Logical OR with the same literal short-circuit gate as 'andE'.
orE :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
orE (Literal (ValueBool True)) _ = Literal (ValueBool True)
orE (Literal (ValueBool False)) y = y
orE x (Literal (ValueBool False)) = x
orE x y = Std (Kernel (KOr x y))
{-# INLINE [1] orE #-}

-- | String concatenation that folds two string literals.
concatE :: Expr f 'String -> Expr f 'String -> Expr f 'String
concatE (Literal (ValueString x)) (Literal (ValueString y)) =
  Literal (ValueString (x <> y))
concatE x y = Std (Kernel (KConcat x y))
{-# INLINE [1] concatE #-}

-- | JS remainder that folds two numeric literals.
remE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
remE = numBinE jsRem KRem
{-# INLINE [1] remE #-}

-- | JS bitwise AND that folds two numeric literals.
bitAndE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
bitAndE = numBinE (jsBit2 (.&.)) KBitAnd
{-# INLINE [1] bitAndE #-}

-- | JS bitwise OR that folds two numeric literals.
bitOrE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
bitOrE = numBinE (jsBit2 (.|.)) KBitOr
{-# INLINE [1] bitOrE #-}

-- | JS bitwise XOR that folds two numeric literals.
bitXorE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
bitXorE = numBinE (jsBit2 xor) KBitXor
{-# INLINE [1] bitXorE #-}

-- | JS left shift that folds two numeric literals.
shlE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
shlE = numBinE jsShl KShl
{-# INLINE [1] shlE #-}

-- | JS sign-propagating right shift that folds two numeric literals.
shrE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
shrE = numBinE jsShr KShr
{-# INLINE [1] shrE #-}

-- | JS unsigned right shift that folds two numeric literals.
ushrE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
ushrE = numBinE jsUShr KUShr
{-# INLINE [1] ushrE #-}

-- | Remainder and bitwise ops shared by IEEE 'Number' and exact 'BigInt'.
-- @>>>@ stays Number-only ('UShr').
class NumericU (u :: Universe) where
  -- | Remainder: @%@ for 'Number', exact remainder for 'BigInt'.
  rem_ :: Expr f u -> Expr f u -> Expr f u

  -- | Bitwise AND.
  bitAnd :: Expr f u -> Expr f u -> Expr f u

  -- | Bitwise OR.
  bitOr :: Expr f u -> Expr f u -> Expr f u

  -- | Bitwise XOR.
  bitXor :: Expr f u -> Expr f u -> Expr f u

  -- | Left shift.
  shl :: Expr f u -> Expr f u -> Expr f u

  -- | Sign-propagating right shift.
  shr :: Expr f u -> Expr f u -> Expr f u

instance NumericU 'Number where
  rem_ = remE
  bitAnd = bitAndE
  bitOr = bitOrE
  bitXor = bitXorE
  shl = shlE
  shr = shrE

instance NumericU 'BigInt where
  rem_ = bigBin BRem
  bitAnd = bigBin BBitAnd
  bitOr = bigBin BBitOr
  bitXor = bigBin BBitXor
  shl = bigBin BShl
  shr = bigBin BShr

bigBin :: BigBinOp -> Expr f 'BigInt -> Expr f 'BigInt -> Expr f 'BigInt
bigBin op x y = Std (Kernel (KBig op x y))

bigNeg :: Expr f 'BigInt -> Expr f 'BigInt
bigNeg x = Std (Kernel (KBigNeg x))

liftBig1 :: (Integer -> Integer) -> Value 'BigInt -> Value 'BigInt
liftBig1 f (ValueBigInt a) = ValueBigInt (f a)

liftBig2 ::
  (Integer -> Integer -> Integer)
  -> Value 'BigInt
  -> Value 'BigInt
  -> Value 'BigInt
liftBig2 f (ValueBigInt a) (ValueBigInt b) = ValueBigInt (f a b)

instance Num (Value 'BigInt) where
  (+) = liftBig2 (+)
  (*) = liftBig2 (*)
  (-) = liftBig2 (-)
  abs = liftBig1 abs
  signum = liftBig1 signum
  fromInteger = ValueBigInt
  negate = liftBig1 negate

instance Num (Expr f 'BigInt) where
  (+) = bigBin BPlus
  (*) = bigBin BTimes
  (-) = bigBin BMinus
  abs x = If (GTEq x (Literal (ValueBigInt 0))) x (bigNeg x)
  signum x =
    If
      (GTh x (Literal (ValueBigInt 0)))
      (Literal (ValueBigInt 1))
      ( If
          (LTh x (Literal (ValueBigInt 0)))
          (bigNeg (Literal (ValueBigInt 1)))
          (Literal (ValueBigInt 0))
      )
  fromInteger n = Literal (ValueBigInt n)
  negate = bigNeg

instance {-# INCOHERENT #-} forall (f :: Universe -> Type) u. u ~ 'Number => Num (Expr f u) where
  (+) = plusE
  (*) = timesE
  (-) = minusE
  abs = expr1 FixAbs
  signum = expr1 FixSign
  fromInteger n = Literal (fromInteger n)
  negate = negateE

instance forall (f :: Universe -> Type) u. u ~ 'Number => Fractional (Expr f u) where
  (/) = fracDivE
  fromRational r = Literal (fromRational r)

instance forall (f :: Universe -> Type) u. u ~ 'Number => Floating (Expr f u) where
  pi = Literal (ValueNumber P.pi)
  exp = expr1 FixExp
  log = expr1 FixLog
  sqrt = expr1 FixSqrt
  (**) x y = expr2 FixPow x y
  sin = expr1 FixSin
  cos = expr1 FixCos
  tan = expr1 FixTan
  asin = expr1 FixAsin
  acos = expr1 FixAcos
  atan = expr1 FixAtan
  sinh = expr1 FixSinh
  cosh = expr1 FixCosh
  tanh = expr1 FixTanh
  asinh = expr1 FixAsinh
  acosh = expr1 FixAcosh
  atanh = expr1 FixAtanh

-- Monadic interface to expressions based on KeyMonad
-- (https://people.seas.harvard.edu/~pbuiras/publications/KeyMonadHaskell2016.pdf).

-- Analogous to RelativeMSyntax in section 3.3.

-- | A untyped monadic syntax for building 'Effect' terms with do-notation,
-- based on the KeyMonad encoding. Interpret it with 'fromSyntax'.
data EffectSyntax :: (Universe -> Type) -> Type -> Type where
  -- | A pure value.
  EffectSyntaxPure :: a -> EffectSyntax v a
  -- | Run an effect and bind its result.
  EffectSyntaxUnpure ::
    Maybe Text
    -> Effect v a
    -> (v a -> EffectSyntax v b)
    -> EffectSyntax v b
  EffectSyntaxThen ::
    Effect v u
    -> EffectSyntax v b
    -> EffectSyntax v b
    -- ^ Sequencing without bind codegen ('*>' / '>>').

deriving instance Functor (EffectSyntax f)

instance Applicative (EffectSyntax v) where
  pure = EffectSyntaxPure
  (<*>) = ap
  EffectSyntaxPure _ *> b = b
  -- Bind the effect, discard the binder, then run the continuation
  -- @g@. Dropping @g@ would silently lose every effect it sequences.
  EffectSyntaxUnpure hint m g *> b =
    EffectSyntaxUnpure hint m (\x -> g x *> b)
  EffectSyntaxThen m g *> b = EffectSyntaxThen m (g *> b)

-- Analogous to the Monad instance for RelativeMSyntax in section 3.3.
-- GHC 9.14 dropped `Monad.(>>)`; do-notation sequences with `Applicative.(*>)`,
-- which is ThenE here. The exported `(>>)` is the same operator.
instance Monad (EffectSyntax f) where
  (>>=) = bindEffectSyntax

bindEffectSyntax ::
  HasCallStack =>
  EffectSyntax f a
  -> (a -> EffectSyntax f b)
  -> EffectSyntax f b
bindEffectSyntax (EffectSyntaxPure x) g = g x
bindEffectSyntax (EffectSyntaxUnpure hint m g) h =
  EffectSyntaxUnpure (maybe callerBinderHint Just hint) m (\x -> g x >>= h)
bindEffectSyntax (EffectSyntaxThen m g) h = EffectSyntaxThen m (g >>= h)

-- | Sequence effects without bind codegen ('*>' / '>>').
seqSyntax :: EffectSyntax f a -> EffectSyntax f b -> EffectSyntax f b
seqSyntax = (*>)

infixr 1 >>

-- | Alias for '(>*>)': sequence effects, discarding the first result.
(>>) :: EffectSyntax f a -> EffectSyntax f b -> EffectSyntax f b
(>>) = (*>)

-- | Lift a single 'Effect' into 'EffectSyntax', yielding its PHOAS binder.
toSyntax :: HasCallStack => Effect f v -> EffectSyntax f (f v)
toSyntax m = EffectSyntaxUnpure callerBinderHint m EffectSyntaxPure

-- | Like 'toSyntax' but discards the effect's result.
toSyntax_ :: HasCallStack => Effect f v -> EffectSyntax f ()
toSyntax_ m = EffectSyntaxUnpure callerBinderHint m (const (EffectSyntaxPure ()))

-- | Bind an effect and reify the result as an 'Expr'.
bindExpr :: HasCallStack => Effect f u -> EffectSyntax f (Expr f u)
bindExpr m = EffectSyntaxUnpure callerBinderHint m (EffectSyntaxPure . Var)

-- | Interpret an 'EffectSyntax' term as an 'Effect'.
fromSyntax :: EffectSyntax f (f v) -> Effect f v
fromSyntax (EffectSyntaxPure x) = Lift (Var x)
fromSyntax (EffectSyntaxThen m b) = ThenE m (fromSyntax b)
fromSyntax (EffectSyntaxUnpure hint m g) = Bind hint m (fromSyntax . g)
