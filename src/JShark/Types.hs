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
  , ClosedExpr
  , ClosedEffect
  , Hvm2KernelEntry (..)
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
  , EffectSyntax (..)
  , toSyntax
  , toSyntax_
  , bindExpr
  , fromSyntax
  , seqSyntax
  , (>>)
  , jsHelperValueEq
  , jsHelperArrayEq
  , jsHelperDeepEqual
  , jsHelperUint8ArrayEq
  , jsEqHelpers
  )
where

import Prelude hiding ((>>))
import Control.Monad (ap)
import Data.Array.Byte (ByteArray)
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
import JShark.Rec
import Prelude hiding ((>>))

data Universe
  = Number
  | BigInt
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

data Value :: Universe -> Type where
  ValueArray :: [Value u] -> Value ('Array u)
  ValueNumber :: Double -> Value 'Number
  ValueBigInt :: Integer -> Value 'BigInt
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
  = FFICall !Text
  | FFILambda !Text

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
  UnsafeObjectGet :: Effect f object -> Text -> Effect f u
  UnsafeObjectAssign :: Effect f object -> Effect f assignment -> Effect f u
  CallMethod ::
    Effect f object
    -> Text
    -> Rec (Arg f) us
    -> Effect f u
    -- ^ @recv.method(args…)@
  Bind ::
    Effect f u
    -> (f u -> Effect f v)
    -> Effect f v
    -- ^ PHOAS bind (@const n = e@)
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

fieldKey :: FieldLit f r -> Text
fieldKey (FieldLit @k _) = T.pack (symbolVal (Proxy :: Proxy k))
fieldKey (FieldLitEffect @k _) = T.pack (symbolVal (Proxy :: Proxy k))
fieldKey (FieldLitExtra @k _) = T.pack (symbolVal (Proxy :: Proxy k))
fieldKey (FieldLitExtraEffect @k _) = T.pack (symbolVal (Proxy :: Proxy k))

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
  U8Index ::
    Expr f 'Uint8Array
    -> Expr f 'Number
    -> Expr f 'Number
    -- ^ JS @u8[i]@ without array bounds helper.
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
  Hvm2Kernel ::
    Text
    -> ClosedExpr u
    -> Expr f u
    -- ^ Closed pure subtree compiled to Bend for the HVM2 pipeline
    -- (Bend → HVM2 → C → WASM via Zig). Codegen emits a call to
    -- @globalThis.__jsharkHvm2.exports@; load the module with
    -- 'JShark.Api.loadHvm2Wasm' and build with 'JShark.Hvm2.bendModule' /
    -- 'JShark.Hvm2.compileHvm2Wasm'.

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

-- | Higher-order array stdlib (@.map@, @.reduce@, @Array.from@, …).
data Method :: (Universe -> Type) -> Universe -> Type where
  MethMap ::
    Expr f ('Array a)
    -> (f a -> Expr f b)
    -> Method f ('Array b)
  MethFilter ::
    Expr f ('Array a)
    -> (f a -> Expr f 'Bool)
    -> Method f ('Array a)
  MethReduce ::
    Expr f ('Array a)
    -> Expr f b
    -> (f b -> f a -> Expr f b)
    -> Method f b
  MethReduceRight ::
    Expr f ('Array a)
    -> Expr f b
    -> (f b -> f a -> Expr f b)
    -> Method f b
  MethToSorted ::
    Expr f ('Array a)
    -> (f a -> f a -> Expr f 'Number)
    -> Method f ('Array a)
  MethFrom ::
    Expr f 'Number
    -> (f 'Number -> Expr f a)
    -> Method f ('Array a)

-- | Exact integer kernel ops. JS @/@ on BigInt is truncating quot.
data BigBinOp
  = BPlus
  | BMinus
  | BTimes
  | BQuot
  | BRem
  | BBitAnd
  | BBitOr
  | BBitXor
  | BShl
  | BShr

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
  Fixed ::
    FixedOp a b c u
    -> FixedArgs f a b c
    -> Std f u
  Method ::
    Method f u
    -> Std f u
  Kernel ::
    Kernel f u
    -> Std f u

pattern Plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Plus x y <- Std (Kernel (KPlus x y))
 where
  Plus = plusE

pattern Times :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Times x y <- Std (Kernel (KTimes x y))
 where
  Times = timesE

pattern Minus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Minus x y <- Std (Kernel (KMinus x y))
 where
  Minus = minusE

pattern Negate :: Expr f 'Number -> Expr f 'Number
pattern Negate x <- Std (Kernel (KNegate x))
 where
  Negate = negateE

pattern FracDiv :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern FracDiv x y <- Std (Kernel (KFracDiv x y))
 where
  FracDiv = fracDivE

pattern Rem :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Rem x y <- Std (Kernel (KRem x y))
 where
  Rem x y = Std (Kernel (KRem x y))

pattern BitAnd :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern BitAnd x y <- Std (Kernel (KBitAnd x y))
 where
  BitAnd x y = Std (Kernel (KBitAnd x y))

pattern BitOr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern BitOr x y <- Std (Kernel (KBitOr x y))
 where
  BitOr x y = Std (Kernel (KBitOr x y))

pattern BitXor :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern BitXor x y <- Std (Kernel (KBitXor x y))
 where
  BitXor x y = Std (Kernel (KBitXor x y))

pattern Shl :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Shl x y <- Std (Kernel (KShl x y))
 where
  Shl x y = Std (Kernel (KShl x y))

pattern Shr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern Shr x y <- Std (Kernel (KShr x y))
 where
  Shr x y = Std (Kernel (KShr x y))

pattern UShr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pattern UShr x y <- Std (Kernel (KUShr x y))
 where
  UShr x y = Std (Kernel (KUShr x y))

pattern And :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
pattern And x y <- Std (Kernel (KAnd x y))
 where
  And = andE

pattern Or :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
pattern Or x y <- Std (Kernel (KOr x y))
 where
  Or = orE

pattern Eq :: Expr f a -> Expr f a -> Expr f 'Bool
pattern Eq x y <- Std (Kernel (KEq _ x y))
 where
  Eq x y = structuralEq x y

pattern NEq :: Expr f a -> Expr f a -> Expr f 'Bool
pattern NEq x y <- Std (Kernel (KNEq _ x y))
 where
  NEq x y = structuralNEq x y

structuralEq :: Expr f a -> Expr f a -> Expr f 'Bool
structuralEq x y = Std (Kernel (KEq True x y))

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

mkEq :: forall f a. KnownScalar a => Expr f a -> Expr f a -> Expr f 'Bool
mkEq x y = Std (Kernel (KEq (not (isScalarTy @a)) x y))
{-# INLINE [1] mkEq #-}

mkNEq :: forall f a. KnownScalar a => Expr f a -> Expr f a -> Expr f 'Bool
mkNEq x y = Std (Kernel (KNEq (not (isScalarTy @a)) x y))
{-# INLINE [1] mkNEq #-}

-- | Compare helpers carry the 'Comparable' constraint GHC cannot attach to
-- the bidirectional pattern synonyms below (two @Expr f a@ fields scope
-- separate type variables in GHC 9.14).
mkGTh :: forall f a. Comparable a => Expr f a -> Expr f a -> Expr f 'Bool
mkGTh x y = Std (Kernel (KGTh x y))
{-# INLINE [1] mkGTh #-}

mkLTh :: forall f a. Comparable a => Expr f a -> Expr f a -> Expr f 'Bool
mkLTh x y = Std (Kernel (KLTh x y))
{-# INLINE [1] mkLTh #-}

mkGTEq :: forall f a. Comparable a => Expr f a -> Expr f a -> Expr f 'Bool
mkGTEq x y = Std (Kernel (KGTEq x y))
{-# INLINE [1] mkGTEq #-}

mkLTEq :: forall f a. Comparable a => Expr f a -> Expr f a -> Expr f 'Bool
mkLTEq x y = Std (Kernel (KLTEq x y))
{-# INLINE [1] mkLTEq #-}

pattern GTh x y = Std (Kernel (KGTh x y))

pattern LTh x y = Std (Kernel (KLTh x y))

pattern GTEq x y = Std (Kernel (KGTEq x y))

pattern LTEq x y = Std (Kernel (KLTEq x y))

pattern Concat :: Expr f 'String -> Expr f 'String -> Expr f 'String
pattern Concat x y <- Std (Kernel (KConcat x y))
 where
  Concat = concatE

pattern Show :: Expr f a -> Expr f 'String
pattern Show x <- Std (Kernel (KShow x))
 where
  Show x = Std (Kernel (KShow x))

pattern TypeOf :: Expr f a -> Expr f 'String
pattern TypeOf x <- Std (Kernel (KTypeOf x))
 where
  TypeOf x = Std (Kernel (KTypeOf x))

-- | Closed pure term: no free PHOAS binders. The end @forall f. 'Expr' f u@.
type ClosedExpr (u :: Universe) = forall (f :: Universe -> Type). Expr f u

-- | Existential wrapper for kernels collected from an open 'Expr' tree.
data Hvm2KernelEntry = forall u. Hvm2KernelEntry !Text !(ClosedExpr u)

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
-- * @Expr f 'Number@ — literals via 'Literal'; ops go through
--   'plusE' / 'timesE' / … so GHC RULES can fold literal-literal
--   cases at compile time. Remaining ops stay AST nodes and fold
--   later in codegen. @(**)@ is @Math.pow@, not @exp (log x * y)@.
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

-- | Stable names for GHC RULES. Smart constructors fold literal-literal
-- cases even when a rule does not fire (INCOHERENT 'Num' often inlines
-- '(+)' straight to the kernel). @INLINE [1]@ keeps the binder through
-- phase 2, where the rules fire; later phases unfold to the kernel.
plusE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
plusE (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueNumber (x + y))
plusE x y = Std (Kernel (KPlus x y))
{-# INLINE [1] plusE #-}

timesE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
timesE (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueNumber (x * y))
timesE x y = Std (Kernel (KTimes x y))
{-# INLINE [1] timesE #-}

minusE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
minusE (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueNumber (x - y))
minusE x y = Std (Kernel (KMinus x y))
{-# INLINE [1] minusE #-}

fracDivE :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
fracDivE (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
  Literal (ValueNumber (x / y))
fracDivE x y = Std (Kernel (KFracDiv x y))
{-# INLINE [1] fracDivE #-}

negateE :: Expr f 'Number -> Expr f 'Number
negateE (Literal (ValueNumber x)) = Literal (ValueNumber (negate x))
negateE x = Std (Kernel (KNegate x))
{-# INLINE [1] negateE #-}

andE :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
andE (Literal (ValueBool False)) _ = Literal (ValueBool False)
andE (Literal (ValueBool True)) y = y
andE x (Literal (ValueBool True)) = x
andE _ (Literal (ValueBool False)) = Literal (ValueBool False)
andE x y = Std (Kernel (KAnd x y))
{-# INLINE [1] andE #-}

orE :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
orE (Literal (ValueBool True)) _ = Literal (ValueBool True)
orE (Literal (ValueBool False)) y = y
orE x (Literal (ValueBool False)) = x
orE _ (Literal (ValueBool True)) = Literal (ValueBool True)
orE x y = Std (Kernel (KOr x y))
{-# INLINE [1] orE #-}

concatE :: Expr f 'String -> Expr f 'String -> Expr f 'String
concatE (Literal (ValueString x)) (Literal (ValueString y)) =
  Literal (ValueString (x <> y))
concatE x y = Std (Kernel (KConcat x y))
{-# INLINE [1] concatE #-}

-- | Remainder and bitwise ops shared by IEEE 'Number' and exact 'BigInt'.
-- @>>>@ stays Number-only ('UShr').
class NumericU (u :: Universe) where
  rem_ :: Expr f u -> Expr f u -> Expr f u
  bitAnd :: Expr f u -> Expr f u -> Expr f u
  bitOr :: Expr f u -> Expr f u -> Expr f u
  bitXor :: Expr f u -> Expr f u -> Expr f u
  shl :: Expr f u -> Expr f u -> Expr f u
  shr :: Expr f u -> Expr f u -> Expr f u

instance NumericU 'Number where
  rem_ = Rem
  bitAnd = BitAnd
  bitOr = BitOr
  bitXor = BitXor
  shl = Shl
  shr = Shr

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
  (Integer -> Integer -> Integer) -> Value 'BigInt -> Value 'BigInt -> Value 'BigInt
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
      (If (LTh x (Literal (ValueBigInt 0))) (bigNeg (Literal (ValueBigInt 1))) (Literal (ValueBigInt 0)))
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

-- | Compile-time folds for common EDSL shapes. Match the JS optimizer
-- on literal-literal cases only. No @x+0@ / @x*1@ identities: IEEE
-- @-0@ makes those unsound. @&&@ / @||@ may drop an Expr arm because
-- 'Expr' is pure (the optimizer also does this).
{-# RULES
"jshark/plus/lit"
  forall x y.
    plusE (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
      Literal (ValueNumber (x + y))
"jshark/times/lit"
  forall x y.
    timesE (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
      Literal (ValueNumber (x * y))
"jshark/minus/lit"
  forall x y.
    minusE (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
      Literal (ValueNumber (x - y))
"jshark/div/lit"
  forall x y.
    fracDivE (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
      Literal (ValueNumber (x / y))
"jshark/negate/lit"
  forall x.
    negateE (Literal (ValueNumber x)) =
      Literal (ValueNumber (negate x))
"jshark/and/false-l"
  forall y.
    andE (Literal (ValueBool False)) y =
      Literal (ValueBool False)
"jshark/and/true-l"
  forall y.
    andE (Literal (ValueBool True)) y = y
"jshark/and/true-r"
  forall x.
    andE x (Literal (ValueBool True)) = x
"jshark/and/false-r"
  forall x.
    andE x (Literal (ValueBool False)) =
      Literal (ValueBool False)
"jshark/or/true-l"
  forall y.
    orE (Literal (ValueBool True)) y =
      Literal (ValueBool True)
"jshark/or/false-l"
  forall y.
    orE (Literal (ValueBool False)) y = y
"jshark/or/false-r"
  forall x.
    orE x (Literal (ValueBool False)) = x
"jshark/or/true-r"
  forall x.
    orE x (Literal (ValueBool True)) =
      Literal (ValueBool True)
"jshark/concat/lit"
  forall x y.
    concatE (Literal (ValueString x)) (Literal (ValueString y)) =
      Literal (ValueString (x <> y))
"jshark/eq/num"
  forall x y.
    mkEq (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
      Literal (ValueBool (x == y))
"jshark/eq/bool"
  forall x y.
    mkEq (Literal (ValueBool x)) (Literal (ValueBool y)) =
      Literal (ValueBool (x == y))
"jshark/eq/str"
  forall x y.
    mkEq (Literal (ValueString x)) (Literal (ValueString y)) =
      Literal (ValueBool (x == y))
"jshark/neq/num"
  forall x y.
    mkNEq (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
      Literal (ValueBool (x /= y))
"jshark/neq/bool"
  forall x y.
    mkNEq (Literal (ValueBool x)) (Literal (ValueBool y)) =
      Literal (ValueBool (x /= y))
"jshark/neq/str"
  forall x y.
    mkNEq (Literal (ValueString x)) (Literal (ValueString y)) =
      Literal (ValueBool (x /= y))
"jshark/gt/num"
  forall x y.
    mkGTh (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
      Literal (ValueBool (x > y))
"jshark/lt/num"
  forall x y.
    mkLTh (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
      Literal (ValueBool (x < y))
"jshark/gte/num"
  forall x y.
    mkGTEq (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
      Literal (ValueBool (x >= y))
"jshark/lte/num"
  forall x y.
    mkLTEq (Literal (ValueNumber x)) (Literal (ValueNumber y)) =
      Literal (ValueBool (x <= y))
  #-}

-- Monadic interface to expressions based on KeyMonad
-- (https://people.seas.harvard.edu/~pbuiras/publications/KeyMonadHaskell2016.pdf).

-- Analogous to RelativeMSyntax in section 3.3.
data EffectSyntax :: (Universe -> Type) -> Type -> Type where
  EffectSyntaxPure :: a -> EffectSyntax v a
  EffectSyntaxUnpure ::
    Effect v a
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
  EffectSyntaxUnpure m _ *> b = EffectSyntaxThen m b
  EffectSyntaxThen m g *> b = EffectSyntaxThen m (g *> b)

-- Analogous to the Monad instance for RelativeMSyntax in section 3.3.
-- GHC 9.14 dropped `Monad.(>>)`; do-notation sequences with `Applicative.(*>)`,
-- which is ThenE here. The exported `(>>)` is the same operator.
instance Monad (EffectSyntax f) where
  EffectSyntaxPure x >>= g = g x
  EffectSyntaxUnpure m g >>= h = EffectSyntaxUnpure m (\x -> g x >>= h)
  EffectSyntaxThen m g >>= h = EffectSyntaxThen m (g >>= h)

-- | Sequence effects without bind codegen ('*>' / '>>').
seqSyntax :: EffectSyntax f a -> EffectSyntax f b -> EffectSyntax f b
seqSyntax = (*>)

infixr 1 >>
(>>) :: EffectSyntax f a -> EffectSyntax f b -> EffectSyntax f b
(>>) = (*>)

toSyntax :: Effect f v -> EffectSyntax f (f v)
toSyntax m = EffectSyntaxUnpure m EffectSyntaxPure

toSyntax_ :: Effect f v -> EffectSyntax f ()
toSyntax_ m = EffectSyntaxUnpure m (const (EffectSyntaxPure ()))

-- | Bind an effect and reify the result as an 'Expr'.
bindExpr :: Effect f u -> EffectSyntax f (Expr f u)
bindExpr m = EffectSyntaxUnpure m (EffectSyntaxPure . Var)

fromSyntax :: EffectSyntax f (f v) -> Effect f v
fromSyntax (EffectSyntaxPure x) = Lift (Var x)
fromSyntax (EffectSyntaxThen m b) = ThenE m (fromSyntax b)
fromSyntax (EffectSyntaxUnpure m g) = Bind m (fromSyntax . g)

-- | Codegen helpers for the kernel 'Eq' operator. '$valueEq' dispatches;
-- '$arrayEq', '$deepEqual', and '$uint8ArrayEq' are the structural walks.
-- Not stdlib names on 'Expr'.
jsHelperValueEq :: (Text, Text)
jsHelperValueEq =
  ( "$valueEq"
  , "function(a,b){if(a===b)return true;if(a===null||b===null||typeof a!==\"object\"||typeof b!==\"object\")return false;if(Array.isArray(a)&&Array.isArray(b))return $arrayEq(a,b);if(a instanceof Uint8Array&&b instanceof Uint8Array)return $uint8ArrayEq(a,b);if(a.constructor===Object&&b.constructor===Object)return $deepEqual(a,b);return false}"
  )

jsHelperArrayEq :: (Text, Text)
jsHelperArrayEq =
  ( "$arrayEq"
  , "function(a,b){if(a===b)return true;if(!Array.isArray(b))return false;if(a.length!==b.length)return false;for(var i=0;i<a.length;i++)if(!$valueEq(a[i],b[i]))return false;return true}"
  )

jsHelperDeepEqual :: (Text, Text)
jsHelperDeepEqual =
  ( "$deepEqual"
  , "function(a,b){if(a===b)return true;if(a instanceof Date&&b instanceof Date)return a.getTime()===b.getTime();if(a instanceof RegExp&&b instanceof RegExp)return a.toString()===b.toString();var ka=Object.keys(a),kb=Object.keys(b);if(ka.length!==kb.length)return false;for(var i=0;i<ka.length;i++){var k=ka[i];if(!Object.prototype.hasOwnProperty.call(b,k))return false;var v1=a[k],v2=b[k],o=v1&&v2&&typeof v1==='object'&&typeof v2==='object';if(o){if(Array.isArray(v1)){if(!$arrayEq(v1,v2))return false}else if(v1 instanceof Uint8Array){if(!$uint8ArrayEq(v1,v2))return false}else if(!$deepEqual(v1,v2))return false}else if(v1!==v2&&!(Number.isNaN(v1)&&Number.isNaN(v2)))return false}return true}"
  )

jsHelperUint8ArrayEq :: (Text, Text)
jsHelperUint8ArrayEq =
  ( "$uint8ArrayEq"
  , "function(a,b){if(a===b)return true;if(a.length!==b.length)return false;for(var i=0;i<a.length;i++)if(a[i]!==b[i])return false;return true}"
  )

jsEqHelpers :: [(Text, Text)]
jsEqHelpers =
  [ jsHelperValueEq
  , jsHelperArrayEq
  , jsHelperDeepEqual
  , jsHelperUint8ArrayEq
  ]
