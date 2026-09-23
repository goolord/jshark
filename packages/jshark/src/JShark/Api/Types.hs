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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , U8Buffer
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
  , FixedOp (..)
  , Math1 (..)
  , Math2 (..)
  , NumOp (..)
  , CmpOp (..)
  , numOpFn
  , cmpOpFn
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
  , NumericU
  , rem_
  , bitAnd
  , bitOr
  , bitXor
  , shl
  , shr
  , eqE
  , cmpE
  , structuralEq
  , structuralNEq
  , numE
  , negateE
  , andE
  , orE
  , concatE
  )
where

import Data.Array.Byte (ByteArray)
import Data.Bits (xor, (.&.), (.|.))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified GHC.Exts as Exts
import GHC.TypeLits
  ( KnownSymbol
  , Symbol
  , symbolVal
  )
import JShark.Api.Rec
import JShark.Compiler.Emit (jsBit2, jsRem, jsShl, jsShr, jsUShr)

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
  | -- | JS @Uint8ClampedArray@. Same shape as 'Uint8Array' but element
    -- writes clamp to @0…255@ instead of wrapping mod 256. Not a
    -- 'MutableObject' row; 'JShark.Canvas.imageDataBytes' yields one.
    Uint8ClampedArray
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

-- | Byte buffers whose element writes the JS engine clamps
-- ('Uint8ClampedArray') or wraps mod 256 ('Uint8Array'). The emitted
-- @arr[i] = v@ is identical; the array's own type picks the semantics, so
-- the operations are shared and only the type distinguishes them.
class U8Buffer (u :: Universe)

instance U8Buffer 'Uint8Array

instance U8Buffer 'Uint8ClampedArray

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
  ValueUint8ClampedArray ::
    ByteArray
    -> Value 'Uint8ClampedArray
    -- ^ Contents of a @Uint8ClampedArray@ (unpinned 'ByteArray').
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
    U8Buffer u =>
    Expr f u
    -> Expr f 'Number
    -> Expr f 'Number
    -> Effect f 'Unit
    -- ^ @u8[i] = v@. Wraps for 'Uint8Array', clamps for
    -- 'Uint8ClampedArray' (the array's own write semantics).
  U8Fill ::
    U8Buffer u =>
    Expr f u
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
    U8Buffer u =>
    Expr f u
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
  -- | @Math.<name>(x)@.
  FixMath1 :: Math1 -> FixedOp 'Number 'Unit 'Unit 'Number
  -- | @Math.<name>(x, y)@.
  FixMath2 :: Math2 -> FixedOp 'Number 'Number 'Unit 'Number
  FixToUpper :: FixedOp 'String 'Unit 'Unit 'String
  FixToLower :: FixedOp 'String 'Unit 'Unit 'String
  FixTrim :: FixedOp 'String 'Unit 'Unit 'String
  FixArrLen :: FixedOp ('Array u) 'Unit 'Unit 'Number
  FixU8Len :: U8Buffer u => FixedOp u 'Unit 'Unit 'Number
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
  -- | Foreign-boundary adapter: unwrap a tagged 'Option' to a native
  -- @null@\/value. Inverse of 'UnsafeNullable'; emitted only when passing
  -- an 'Option' to a foreign parameter declared @T | null@.
  FixOptionToNative ::
    FixedOp ('Option u) 'Unit 'Unit u

-- | One-argument @Math@ functions; the JS name is the lowercased
-- constructor.
data Math1
  = Sin
  | Cos
  | Tan
  | Asin
  | Acos
  | Atan
  | Sinh
  | Cosh
  | Tanh
  | Asinh
  | Acosh
  | Atanh
  | Sqrt
  | Cbrt
  | Exp
  | Log
  | Log2
  | Log10
  | Floor
  | Ceil
  | Round
  | Trunc
  | Abs
  | Sign
  deriving (Eq, Show)

-- | Two-argument @Math@ functions; the JS name is the lowercased
-- constructor.
data Math2 = Pow | Atan2 | Max | Min | Hypot
  deriving (Eq, Show)

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

-- | Binary arithmetic and bitwise operators, shared by 'Number' and
-- 'BigInt' ('KNum'). On 'BigInt', @/@ truncates and @>>>@ throws.
data NumOp
  = NPlus
  | NTimes
  | NMinus
  | NDiv
  | NRem
  | NBitAnd
  | NBitOr
  | NBitXor
  | NShl
  | NShr
  | NUShr
  deriving (Eq)

-- | The host meaning of a 'NumOp', with JS 32-bit bitwise semantics.
numOpFn :: NumOp -> Double -> Double -> Double
numOpFn = \case
  NPlus -> (+)
  NTimes -> (*)
  NMinus -> (-)
  NDiv -> (/)
  NRem -> jsRem
  NBitAnd -> jsBit2 (.&.)
  NBitOr -> jsBit2 (.|.)
  NBitXor -> jsBit2 xor
  NShl -> jsShl
  NShr -> jsShr
  NUShr -> jsUShr

-- | Ordering comparisons: @>@, @<@, @>=@, @<=@.
data CmpOp = CGT | CLT | CGE | CLE

-- | Whether an 'Ordering' satisfies the comparison.
cmpOpFn :: CmpOp -> Ordering -> Bool
cmpOpFn = \case
  CGT -> (== GT)
  CLT -> (== LT)
  CGE -> (/= LT)
  CLE -> (/= GT)

-- | Good Parts kernel operators (@+@, @===@, @&&@, …).
data Kernel :: (Universe -> Type) -> Universe -> Type where
  KConcat ::
    Expr f 'String
    -> Expr f 'String
    -> Kernel f 'String
  -- | Arithmetic on 'Number' or 'BigInt' ('NumericU').
  KNum :: NumericU u => NumOp -> Expr f u -> Expr f u -> Kernel f u
  KNegate :: NumericU u => Expr f u -> Kernel f u
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
  KCmp :: Comparable a => CmpOp -> Expr f a -> Expr f a -> Kernel f 'Bool
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

-- | Equality (@True@) or inequality that folds scalar literals at compile
-- time. Uses @===@ when @a@ is a 'KnownScalar', otherwise @$valueEq@.
eqE :: forall f a. KnownScalar a => Bool -> Expr f a -> Expr f a -> Expr f 'Bool
eqE want x y = case (x, y) of
  (Literal (ValueNumber a), Literal (ValueNumber b)) -> lit (a == b)
  (Literal (ValueBool a), Literal (ValueBool b)) -> lit (a == b)
  (Literal (ValueString a), Literal (ValueString b)) -> lit (a == b)
  _ -> Std (Kernel ((if want then KEq else KNEq) (not (isScalarTy @a)) x y))
 where
  lit b = Literal (ValueBool (b == want))

-- | Ordering comparison that folds numeric literals.
cmpE :: Comparable a => CmpOp -> Expr f a -> Expr f a -> Expr f 'Bool
cmpE op (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal . ValueBool $ case op of
    CGT -> x > y
    CLT -> x < y
    CGE -> x >= y
    CLE -> x <= y
cmpE op x y = Std (Kernel (KCmp op x y))

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
--   'numE' so smart constructors fold
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

-- | Arithmetic smart constructor; folds two 'Number' literals.
numE :: NumericU u => NumOp -> Expr f u -> Expr f u -> Expr f u
numE op (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueNumber (numOpFn op x y))
numE op x y = Std (Kernel (KNum op x y))

-- | Negation; folds a 'Number' literal.
negateE :: NumericU u => Expr f u -> Expr f u
negateE (Literal (ValueNumber x)) = Literal (ValueNumber (negate x))
negateE x = Std (Kernel (KNegate x))

-- | Short-circuit only when the left is already a literal. Dropping a
-- non-literal left of @&& false@ / @|| true@ would skip 'Error' and
-- 'FixStringify' (impure). Same gate as the optimizer's foldAnd/foldOr.
andE :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
andE (Literal (ValueBool False)) _ = Literal (ValueBool False)
andE (Literal (ValueBool True)) y = y
andE x (Literal (ValueBool True)) = x
andE x y = Std (Kernel (KAnd x y))

-- | Logical OR with the same literal short-circuit gate as 'andE'.
orE :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
orE (Literal (ValueBool True)) _ = Literal (ValueBool True)
orE (Literal (ValueBool False)) y = y
orE x (Literal (ValueBool False)) = x
orE x y = Std (Kernel (KOr x y))

-- | String concatenation that folds two string literals.
concatE :: Expr f 'String -> Expr f 'String -> Expr f 'String
concatE (Literal (ValueString x)) (Literal (ValueString y)) =
  Literal (ValueString (x <> y))
concatE x y = Std (Kernel (KConcat x y))

-- | 'Number' and 'BigInt': the universes with arithmetic and bitwise ops.
-- @>>>@ is 'Number'-only ('JShark.Api.ushr').
class NumericU (u :: Universe)

instance NumericU 'Number

instance NumericU 'BigInt

-- | Remainder and bitwise operators on 'Number' (JS 32-bit semantics) and
-- 'BigInt' (exact).
rem_, bitAnd, bitOr, bitXor, shl, shr :: NumericU u => Expr f u -> Expr f u -> Expr f u
rem_ = numE NRem
bitAnd = numE NBitAnd
bitOr = numE NBitOr
bitXor = numE NBitXor
shl = numE NShl
shr = numE NShr

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
  (+) = numE NPlus
  (*) = numE NTimes
  (-) = numE NMinus
  abs x = If (cmpE CGE x 0) x (negate x)
  signum x = If (cmpE CGT x 0) 1 (If (cmpE CLT x 0) (-1) 0)
  fromInteger n = Literal (ValueBigInt n)
  negate = negateE

instance {-# INCOHERENT #-} forall (f :: Universe -> Type) u. u ~ 'Number => Num (Expr f u) where
  (+) = numE NPlus
  (*) = numE NTimes
  (-) = numE NMinus
  abs = expr1 (FixMath1 Abs)
  signum = expr1 (FixMath1 Sign)
  fromInteger n = Literal (fromInteger n)
  negate = negateE

instance forall (f :: Universe -> Type) u. u ~ 'Number => Fractional (Expr f u) where
  (/) = numE NDiv
  fromRational r = Literal (fromRational r)

instance forall (f :: Universe -> Type) u. u ~ 'Number => Floating (Expr f u) where
  pi = Literal (ValueNumber pi)
  exp = expr1 (FixMath1 Exp)
  log = expr1 (FixMath1 Log)
  sqrt = expr1 (FixMath1 Sqrt)
  (**) x y = expr2 (FixMath2 Pow) x y
  sin = expr1 (FixMath1 Sin)
  cos = expr1 (FixMath1 Cos)
  tan = expr1 (FixMath1 Tan)
  asin = expr1 (FixMath1 Asin)
  acos = expr1 (FixMath1 Acos)
  atan = expr1 (FixMath1 Atan)
  sinh = expr1 (FixMath1 Sinh)
  cosh = expr1 (FixMath1 Cosh)
  tanh = expr1 (FixMath1 Tanh)
  asinh = expr1 (FixMath1 Asinh)
  acosh = expr1 (FixMath1 Acosh)
  atanh = expr1 (FixMath1 Atanh)
