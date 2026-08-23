{-# LANGUAGE AllowAmbiguousTypes #-}
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
-- * Haskell sums encoded as JS: 'Option' is @null@ / the value;
--   'Result' is @{ok: Bool, value: …}@.
-- * 'Effect' is impure: statements, FFI, mutation, I/O, free-text names.
--
-- Binders are parametric (@f :: Universe -> Type@). A closed term is
-- an end @forall f. …@, the same quantification as Kmett's
-- @type End p = forall x. p x x@. The two trees meet at FFI via 'Arg'.
module JShark.Types
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
  , FixedOp (..)
  , FixedArgs (..)
  , fixed1
  , fixed2
  , fixed3
  , expr1
  , expr2
  , expr3
  , GroupBy
  , ClosedExpr
  , ClosedEffect
  , Comparable
  , EffectSyntax (..)
  , toSyntax
  , toSyntax_
  , bindExpr
  , fromSyntax
  , jsHelperValueEq
  , jsHelperArrayEq
  , jsHelperDeepEqual
  , jsHelperUint8ArrayEq
  , jsEqHelpers
)
where

import Control.Monad (ap, void)
import Data.Array.Byte (ByteArray)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Text (Text)
import qualified GHC.Exts as Exts
import GHC.TypeLits
  ( KnownSymbol
  , Symbol
  , symbolVal
  )
import JShark.Rec

data Universe
  = Number
  | String
  | Unit
  | Array Universe
  | -- | Unary. Nest for n-ary JS functions.
    Function Universe Universe
  | -- | JS @function(a, b, …) { … }@ — not a curried @'Function@ chain.
    -- Parameter universes match 'JShark.Params.RowUs' order ('fnLit' / 'toFn').
    Fn [Universe] Universe
  | Option Universe
  | -- | Haskell 'Either'; JS @{ok: Bool, value: …}@
    Result Universe Universe
  | Regex
  | Bool
  | -- | JS @Uint8Array@. Host 'ByteArray' literals and
    -- 'JShark.Api.newByteArray' (the latter on 'Effect', because
    -- allocation has identity). Not a 'MutableObject' row —
    -- 'JShark.Object.set' must not typecheck. JS can write the object.
    Uint8Array
  | -- | Frozen record. Row @r@ is a host 'Type', not a 'Universe' constructor.
    Object Type
  | -- | Mutable JS object. Same row @r@ as 'Object'.
    MutableObject Type

data Value :: Universe -> Type where
  ValueArray :: [Value u] -> Value ('Array u)
  ValueNumber :: Double -> Value 'Number
  ValueString :: Text -> Value 'String
  ValueFunction :: (Value u -> Value v) -> Value ('Function u v)
  ValueUnit :: Value 'Unit
  ValueOption :: Maybe (Value u) -> Value ('Option u)
  ValueResult :: Either (Value e) (Value a) -> Value ('Result e a)
  ValueRegex :: Text -> Value 'Regex
  ValueBool :: Bool -> Value 'Bool
  ValueUint8Array ::
    ByteArray
    -> Value 'Uint8Array
    -- ^ Contents of a @Uint8Array@ (unpinned 'ByteArray').
  ValueFrozen ::
    [FieldLit Value r]
    -> Value ('Object r)
    -- ^ Eval-only; not a surface literal.

-- | How to render an 'FFI' callee. 'FFILambda' is parenthesized at codegen.
data FFIForm
  = FFICall String
  | FFILambda String

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
  UnsafeObject :: Text -> Effect f ('MutableObject x)
  UnsafeObjectGet :: Effect f object -> String -> Effect f u
  UnsafeObjectAssign :: Effect f object -> Effect f assignment -> Effect f u
  CallMethod ::
    Effect f object
    -> String
    -> Rec (Arg f) us
    -> Effect f u
    -- ^ @recv.method(args…)@
  Bind ::
    Effect f u
    -> (f u -> Effect f v)
    -> Effect f v
    -- ^ PHOAS bind (@const n = e@)
  BindRec ::
    (f u -> Effect f u)
    -> (f u -> Effect f v)
    -> Effect f v
    -- ^ Recursive bind (@let n; n = …n…@)
  LambdaE ::
    (f u -> Effect f v)
    -> Effect f ('Function u v)
    -- ^ Effectful function (weak PHOAS: binder is @f u@, not @Effect@)
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
  OptionCaseE ::
    Expr f ('Option u)
    -> Effect f v
    -> (f u -> Effect f v)
    -> Effect f v
    -- ^ Effectful 'optionCase'.
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
  ArgExpr :: Expr f u -> Arg f u
  ArgEffect :: Effect f u -> Arg f u

-- | JS property type of row @r@ at key @k@. Open; each host row supplies
-- instances. The index on 'Object' / 'MutableObject' is this host 'Type', not a 'Universe'.
type family Field (r :: Type) (k :: Symbol) :: Universe

-- | @groupBy@ result row: @[{key, items}]@. Not a null-prototype dict.
data GroupBy (u :: Universe)

type instance Field (GroupBy u) "key" = 'String

type instance Field (GroupBy u) "items" = 'Array u

-- | One field of an object literal. @k@ is the JS name ('fieldKey').
-- Typed constructors require the value's universe to be 'Field' @r@ @k@.
-- Extra constructors carry a key that is not in the row (Generic sum
-- @payload@ on 'Tagged').
data FieldLit (f :: Universe -> Type) (r :: Type) where
  FieldLit ::
    forall k f r.
    KnownSymbol k =>
    Expr f (Field r k) -> FieldLit f r
  FieldLitEffect ::
    forall k f r.
    KnownSymbol k =>
    Effect f (Field r k) -> FieldLit f r
  FieldLitExtra ::
    forall k f r u.
    (KnownSymbol k, Typeable u) =>
    Expr f u -> FieldLit f r
  FieldLitExtraEffect ::
    forall k f r u.
    (KnownSymbol k, Typeable u) =>
    Effect f u -> FieldLit f r

fieldKey :: FieldLit f r -> String
fieldKey (FieldLit @k _) = symbolVal (Proxy :: Proxy k)
fieldKey (FieldLitEffect @k _) = symbolVal (Proxy :: Proxy k)
fieldKey (FieldLitExtra @k _) = symbolVal (Proxy :: Proxy k)
fieldKey (FieldLitExtraEffect @k _) = symbolVal (Proxy :: Proxy k)

-- | PHOAS spine for @'Fn'@: @JfCons@ binders, @JfNil@ body.
data FnBody (f :: Universe -> Type) (us :: [Universe]) (r :: Universe) where
  JfNil :: Expr f r -> FnBody f '[] r
  JfCons :: (f u -> FnBody f us r) -> FnBody f (u ': us) r

data Expr :: (Universe -> Type) -> Universe -> Type where
  -- Good Parts: values, arithmetic, strict equality, functions, @const@ lets
  Literal ::
    Value u
    -> Expr f u
    -- ^ A literal value. eg. 1, "foo", etc
  Concat ::
    Expr f 'String
    -> Expr f 'String
    -> Expr f 'String
    -- ^ String concatenation: @+@
  Plus ::
    Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ Addition: @+@
  Times ::
    Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ Multiplication: @*@
  Minus ::
    Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ Subtraction: @-@
  Negate ::
    Expr f 'Number
    -> Expr f 'Number
    -- ^ @-(x)@
  FracDiv ::
    Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ Division: @/@
  Rem ::
    Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ Remainder: @%@ (JS, not floor-mod)
  BitAnd ::
    Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ @&@ after ToInt32
  BitOr ::
    Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ @|@
  BitXor ::
    Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ @^@
  Shl ::
    Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ @<<@
  Shr ::
    Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ @>>@
  UShr ::
    Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ @>>>@
  And ::
    Expr f 'Bool
    -> Expr f 'Bool
    -> Expr f 'Bool
    -- ^ @&&@
  Or ::
    Expr f 'Bool
    -> Expr f 'Bool
    -> Expr f 'Bool
    -- ^ @||@
  Eq ::
    Expr f a
    -> Expr f a
    -> Expr f 'Bool
    -- ^ Strict equality: @===@ (never @==@)
  NEq ::
    Expr f a
    -> Expr f a
    -> Expr f 'Bool
    -- ^ Strict inequality: @!==@ (never @!=@)
  GTh ::
    Comparable a =>
    Expr f a
    -> Expr f a
    -> Expr f 'Bool
    -- ^ @>@ (numbers, strings, bools — not objects)
  LTh ::
    Comparable a =>
    Expr f a
    -> Expr f a
    -> Expr f 'Bool
    -- ^ @<@
  GTEq ::
    Comparable a =>
    Expr f a
    -> Expr f a
    -> Expr f 'Bool
    -- ^ @>=@
  LTEq ::
    Comparable a =>
    Expr f a
    -> Expr f a
    -> Expr f 'Bool
    -- ^ @<=@
  Let ::
    Expr f u
    -> (f u -> Expr f v)
    -> Expr f v
    -- ^ PHOAS let; codegen emits @const@
  LetRec ::
    (f u -> Expr f u)
    -> (f u -> Expr f v)
    -> Expr f v
    -- ^ Recursive let. The rhs must be productive; 'JShark.evaluate' ties
    --         the knot, so one that forces its own binder diverges.
  Lambda ::
    (f u -> Expr f v)
    -> Expr f ('Function u v)
    -- ^ Weak PHOAS lambda: binder is @f u@
  Apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
  Show ::
    Expr f u
    -> Expr f 'String
    -- ^ @String(x)@
  TypeOf ::
    Expr f u
    -> Expr f 'String
    -- ^ @typeof x@ (closed name; @null@ is @\"object\"@)
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
    -- ^ Eliminate an 'Option', analogous to 'maybe'. Option is JS @null@ /
    -- the value itself. Intro via 'UnsafeNullable' or
    -- @Literal (ValueOption …)@. This stays a primitive: 'JShark.evaluate'
    -- uses @f = Value@, so a bound @'Option u@ cannot be unwrapped by
    -- @if_ (opt .== none)@ plus a type-changing coerce.
  ResultOk :: Expr f a -> Expr f ('Result e a)
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
    FnBody f us r ->
    Expr f ('Fn us r)
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
  FixStrLen :: FixedOp 'String 'Unit 'Unit 'Number
  FixStringify :: FixedOp u 'Unit 'Unit 'String
  FixIndexOf :: FixedOp 'String 'String 'Unit 'Number
  FixSplit :: FixedOp 'String 'String 'Unit ('Array 'String)
  FixIncludes :: FixedOp ('Array u) u 'Unit 'Bool
  FixConcat :: FixedOp ('Array u) ('Array u) 'Unit ('Array u)
  FixJoin :: FixedOp ('Array u) 'String 'Unit 'String
  FixTest :: FixedOp 'Regex 'String 'Unit 'Bool
  FixParseInt :: FixedOp 'String 'Number 'Unit 'Number
  FixSlice :: FixedOp 'String 'Number 'Number 'String
  FixArrSlice :: FixedOp ('Array u) 'Number 'Number ('Array u)
  FixReplace :: FixedOp 'String 'String 'String 'String

data FixedArgs f a b c where
  ArgsU :: Expr f a -> FixedArgs f a 'Unit 'Unit
  ArgsB :: Expr f a -> Expr f b -> FixedArgs f a b 'Unit
  ArgsT :: Expr f a -> Expr f b -> Expr f c -> FixedArgs f a b c

fixed1 :: FixedOp a 'Unit 'Unit u -> Expr f a -> Std f u
fixed1 op x = Fixed op (ArgsU x)

fixed2 :: FixedOp a b 'Unit u -> Expr f a -> Expr f b -> Std f u
fixed2 op x y = Fixed op (ArgsB x y)

fixed3 :: FixedOp a b c u -> Expr f a -> Expr f b -> Expr f c -> Std f u
fixed3 op x y z = Fixed op (ArgsT x y z)

-- | 'Std' 'Fixed' as an 'Expr' (for 'Num'/'Floating' instances).
expr1 :: FixedOp a 'Unit 'Unit u -> Expr f a -> Expr f u
expr1 op x = Std (fixed1 op x)

expr2 :: FixedOp a b 'Unit u -> Expr f a -> Expr f b -> Expr f u
expr2 op x y = Std (fixed2 op x y)

expr3 :: FixedOp a b c u -> Expr f a -> Expr f b -> Expr f c -> Expr f u
expr3 op x y z = Std (fixed3 op x y z)

-- | Pure JS standard library, applied. One 'Expr' constructor ('Std')
-- holds this sum — not a constructor per method.
data Std :: (Universe -> Type) -> Universe -> Type where
  Fixed ::
    FixedOp a b c u
    -> FixedArgs f a b c
    -> Std f u
  Map ::
    Expr f ('Array a)
    -> (f a -> Expr f b)
    -> Std f ('Array b)
  Filter ::
    Expr f ('Array a)
    -> (f a -> Expr f 'Bool)
    -> Std f ('Array a)
  Reduce ::
    Expr f ('Array a)
    -> Expr f b
    -> (f b -> f a -> Expr f b)
    -> Std f b
  ReduceRight ::
    Expr f ('Array a)
    -> Expr f b
    -> (f b -> f a -> Expr f b)
    -> Std f b
  ToSorted ::
    Expr f ('Array a)
    -> (f a -> f a -> Expr f 'Number)
    -> Std f ('Array a)
  -- | @Array.from({length: n}, function(_, i) { return f(i); })@
  From ::
    Expr f 'Number
    -> (f 'Number -> Expr f a)
    -> Std f ('Array a)

-- | Closed pure term: no free PHOAS binders. The end @forall f. 'Expr' f u@.
type ClosedExpr (u :: Universe) = forall (f :: Universe -> Type). Expr f u

-- | Closed effectful term: no free PHOAS binders. The end @forall f. 'Effect' f u@.
type ClosedEffect (u :: Universe) = forall (f :: Universe -> Type). Effect f u

-- | Ordering on the Good Parts primitives. Objects/arrays use JS
-- 'ToPrimitive' and are not constructible here.
class Comparable (u :: Universe)

instance Comparable 'Number

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
  (<>) = Concat

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
    Let d $ \dv ->
      OptionCase o (Var dv) $ \x ->
        OptionCase (Var dv) (UnsafeNullable (Var x)) $ \y ->
          UnsafeNullable (Var x <> Var y)

instance Semigroup (Expr f u) => Monoid (Expr f ('Option u)) where
  mempty = Literal (ValueOption Nothing)

instance Semigroup (Expr f ('Result e a)) where
  l <> r = ResultCase l (\_ -> r) (\_ -> l)

instance Semigroup (Expr f a) => Semigroup (Expr f ('Function r a)) where
  g <> h = Lambda (\x -> Apply g (Var x) <> Apply h (Var x))

instance Monoid (Expr f a) => Monoid (Expr f ('Function r a)) where
  mempty = Lambda (\_ -> mempty)

-- | 'Num' / 'Fractional' / 'Floating' for JS numbers:
--
-- * @Value 'Number@ — @1@, @2.5@; arithmetic runs eagerly on host
--   'Double's (so @'Literal' (1 + 2)@ is already @'Literal' 3@)
-- * @Expr f 'Number@ — literals via 'Literal'; ops build AST nodes
--   ('Plus'/'Times'/'Std' ('Fixed …')/…) and fold later in codegen.
--   @(**)@ is @Math.pow@, not @exp (log x * y)@.
--
-- Prefer a signature when the hole is ambiguous. Use 'JShark.Api.number'
-- for arbitrary runtime 'Double's (integer literals can use 'Num' directly).
instance forall u. u ~ 'Number => Num (Value u) where
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

instance forall (f :: Universe -> Type) u. u ~ 'Number => Num (Expr f u) where
  (+) = Plus
  (*) = Times
  (-) = Minus
  abs = expr1 FixAbs
  signum = expr1 FixSign
  fromInteger n = Literal (fromInteger n)
  negate = Negate

instance forall (f :: Universe -> Type) u. u ~ 'Number => Fractional (Expr f u) where
  (/) = FracDiv
  fromRational r = Literal (fromRational r)

jsPi :: Double
jsPi = pi

instance forall (f :: Universe -> Type) u. u ~ 'Number => Floating (Expr f u) where
  pi = Literal (ValueNumber jsPi)
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
data EffectSyntax :: (Universe -> Type) -> Type -> Type where
  EffectSyntaxPure :: a -> EffectSyntax v a
  EffectSyntaxUnpure ::
    Effect v a
    -> (v a -> EffectSyntax v b)
    -> EffectSyntax v b

deriving instance Functor (EffectSyntax f)

instance Applicative (EffectSyntax v) where
  pure = EffectSyntaxPure
  (<*>) = ap

-- Analogous to the Monad instance for RelativeMSyntax in section 3.3.
instance Monad (EffectSyntax f) where
  EffectSyntaxPure x >>= g = g x
  EffectSyntaxUnpure m g >>= h = EffectSyntaxUnpure m (\x -> g x >>= h)

toSyntax :: Effect f v -> EffectSyntax f (f v)
toSyntax m = EffectSyntaxUnpure m EffectSyntaxPure

toSyntax_ :: Effect f v -> EffectSyntax f ()
toSyntax_ = void . toSyntax

-- | Bind an effect and reify the result as an 'Expr'.
bindExpr :: Effect f u -> EffectSyntax f (Expr f u)
bindExpr = fmap Var . toSyntax

fromSyntax :: EffectSyntax f (f v) -> Effect f v
fromSyntax (EffectSyntaxPure x) = Lift (Var x)
fromSyntax (EffectSyntaxUnpure m g) = Bind m (fromSyntax . g)

-- | Codegen helpers for the kernel 'Eq' operator. '$valueEq' dispatches;
-- '$arrayEq', '$deepEqual', and '$uint8ArrayEq' are the structural walks.
-- Not stdlib names on 'Expr'.
jsHelperValueEq :: (String, String)
jsHelperValueEq =
  ( "$valueEq"
  , "function(a,b){if(a===b)return true;if(Array.isArray(a)&&Array.isArray(b))return $arrayEq(a,b);if(a instanceof Uint8Array&&b instanceof Uint8Array)return $uint8ArrayEq(a,b);if(a&&b&&a.constructor===Object&&b.constructor===Object)return $deepEqual(a,b);return false}"
  )

jsHelperArrayEq :: (String, String)
jsHelperArrayEq =
  ( "$arrayEq"
  , "function(a,b){if(a===b)return true;if(!Array.isArray(b))return false;if(a.length!==b.length)return false;for(var i=0;i<a.length;i++)if(!$valueEq(a[i],b[i]))return false;return true}"
  )

jsHelperDeepEqual :: (String, String)
jsHelperDeepEqual =
  ( "$deepEqual"
  , "function(a,b){if(a===b)return true;if(a instanceof Date&&b instanceof Date)return a.getTime()===b.getTime();if(a instanceof RegExp&&b instanceof RegExp)return a.toString()===b.toString();var ka=Object.keys(a),kb=Object.keys(b);if(ka.length!==kb.length)return false;for(var i=0;i<ka.length;i++){var k=ka[i];if(!Object.prototype.hasOwnProperty.call(b,k))return false;var v1=a[k],v2=b[k],o=v1&&v2&&typeof v1==='object'&&typeof v2==='object';if(o){if(Array.isArray(v1)){if(!$arrayEq(v1,v2))return false}else if(v1 instanceof Uint8Array){if(!$uint8ArrayEq(v1,v2))return false}else if(!$deepEqual(v1,v2))return false}else if(v1!==v2&&!(Number.isNaN(v1)&&Number.isNaN(v2)))return false}return true}"
  )

jsHelperUint8ArrayEq :: (String, String)
jsHelperUint8ArrayEq =
  ( "$uint8ArrayEq"
  , "function(a,b){if(a===b)return true;if(a.length!==b.length)return false;for(var i=0;i<a.length;i++)if(a[i]!==b[i])return false;return true}"
  )

jsEqHelpers :: [(String, String)]
jsEqHelpers =
  [ jsHelperValueEq
  , jsHelperArrayEq
  , jsHelperDeepEqual
  , jsHelperUint8ArrayEq
  ]
