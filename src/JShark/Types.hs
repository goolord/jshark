{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DataKinds
  , DeriveFunctor
  , DerivingStrategies
  , FlexibleInstances
  , GADTs
  , KindSignatures
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeAbstractions
  , TypeApplications
  , TypeFamilies
  , TypeOperators
#-}
-- | Two PHOAS syntax trees for a typed subset of JavaScript.
--
-- Three layers live on the same GADTs:
--
-- * Good Parts kernel on 'Expr': literals, @===@, @?:@, @&&@/@||@,
--   unary functions, @const@ lets, arrays. No @==@, @with@, @eval@,
--   @new@, or @this@. Functions are unary; nest them (see 'JShark.Api.lambda2').
-- * Haskell sums encoded as JS: 'Option' is @null@ / the value;
--   'Result' is @{ok: Bool, value: …}@, not a JS built-in.
-- * Closed stdlib names ('ExprUnary' / 'MathUnary' / …) plus 'Effect'
--   for statements, FFI, mutation, and I/O.
--
-- Binders are parametric (@f :: Universe -> Type@), i.e. weak PHOAS.
-- A closed term is an end over that parameter: 'ClosedExpr' / 'ClosedEffect'
-- (@forall f. …@), the same quantification as Kmett's
-- @type End p = forall x. p x x@. The two trees meet at FFI via 'Arg'.
-- Named stdlib on 'Expr' is a closed set of constructors; free-text
-- escapes live on 'Effect'.
module JShark.Types
  ( Universe(..)
  , Value(..)
  , Effect(..)
  , Arg(..)
  , Field
  , FieldLit(..)
  , fieldKey
  , Expr(..)
  , StdUnary(..)
  , StdBinary(..)
  , StdTernary(..)
  , MathFn1(..)
  , MathFn2(..)
  , mathFn1Name
  , mathFn2Name
  , ClosedExpr
  , ClosedEffect
  , Comparable
  , EffectSyntax(..)
  , toSyntax
  , toSyntax_
  , fromSyntax
  ) where

import Control.Monad (ap, void)
import Data.Kind
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.TypeLits
  ( KnownSymbol
  , Symbol
  , symbolVal
  )
import JShark.Rec
import qualified GHC.Exts as Exts

data Universe
  = Number
  | String
  | Unit
  | Array Universe
  | Function Universe Universe -- ^ Unary. Nest for n-ary JS functions.
  | Option Universe
  | Result Universe Universe -- ^ Haskell 'Either'; JS @{ok: Bool, value: …}@
  | Regex
  | Bool
  | Object Type -- ^ Frozen record. Row @r@ is a host 'Type', not a 'Universe' constructor.
  | MutableObject Type -- ^ Mutable JS object. Same row @r@ as 'Object'.

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
  ValueFrozen :: [FieldLit Value r] -> Value ('Object r) -- ^ Eval-only; not a surface literal.

data Effect :: (Universe -> Type) -> Universe -> Type where
  Lift :: Expr f u -> Effect f u -- ^ Lift a pure expression into the effectful tree
  FFI :: String -> Rec (Arg f) us -> Effect f u -- ^ Foreign call: @name(args…)@. Args are 'Arg' so an effect (object handle, effectful function) need not pass through 'Expr'.
  UnsafeObject :: Text -> Effect f ('MutableObject x)
  UnsafeObjectGet :: Effect f object -> String -> Effect f u
  UnsafeObjectAssign :: Effect f object -> Effect f assignment -> Effect f u
  CallMethod :: Effect f object -> String -> Rec (Arg f) us -> Effect f u -- ^ @recv.method(args…)@
  Bind :: Effect f u -> (f u -> Effect f v) -> Effect f v -- ^ PHOAS bind (@const n = e@)
  BindRec :: (f u -> Effect f u) -> (f u -> Effect f v) -> Effect f v -- ^ Recursive bind (@let n; n = …n…@)
  LambdaE :: (f u -> Effect f v) -> Effect f ('Function u v) -- ^ Effectful function (weak PHOAS: binder is @f u@, not @Effect@)
  ApplyE :: Effect f ('Function u v) -> Effect f u -> Effect f v
  IfE :: Effect f 'Bool -> Effect f u -> Effect f u -> Effect f u -- ^ Effectful conditional. Statement @if@ is 'IfE' of two 'Unit' arms (see 'JShark.Api.discard'). A 'Lift'ed condition must not depend on 'Let' decls that only run once; an 'FFI' condition is re-emitted into the @if@ test.
  While :: Effect f 'Bool -> Effect f 'Unit -> Effect f 'Unit -- ^ Loop while the condition holds. The rendered condition is re-emitted on every iteration, so it must not depend on declarations that only run once. Use 'FFI' (not a bound var) when the test itself is a call.
  OptionCaseE :: Expr f ('Option u) -> Effect f v -> (f u -> Effect f v) -> Effect f v -- ^ Effectful 'optionCase'.
  ResultCaseE :: Expr f ('Result e a) -> (f e -> Effect f v) -> (f a -> Effect f v) -> Effect f v
  Throw :: Expr f 'String -> Effect f v -- ^ @throw e@. Never returns. Payload is a string.
  Try :: Effect f u -> (f 'String -> Effect f u) -> Effect f u -- ^ @try { a } catch (e) { k e }@
  ObjectLit :: [FieldLit f r] -> Effect f ('MutableObject r) -- ^ Typed @{k: v, …}@; keys come from 'FieldLit'
  DeleteProp :: Effect f object -> Expr f 'String -> Effect f 'Bool -- ^ @delete o[k]@
  ArrayLit :: [Effect f u] -> Effect f ('Array u) -- ^ @[e0, e1, …]@. Elements stay on 'Effect' (no 'UnsafeEffectExpr').
  ArraySort :: Expr f ('Array u) -> (f u -> f u -> Expr f 'Number) -> Effect f ('Array u) -- ^ @arr.sort(function(a,b){…})@

-- | An FFI argument drawn from either syntax tree. This is the sanctioned
-- seam between 'Expr' and 'Effect'; prefer it over 'UnsafeEffectExpr'.
data Arg :: (Universe -> Type) -> Universe -> Type where
  ArgExpr :: Expr f u -> Arg f u
  ArgEffect :: Effect f u -> Arg f u

-- | JS property type of row @r@ at key @k@. Open; each host row supplies
-- instances. The index on 'Object' / 'MutableObject' is this host 'Type', not a 'Universe'.
type family Field (r :: Type) (k :: Symbol) :: Universe

-- | One field of an object literal. @k@ is the JS name ('fieldKey'); the
-- value's universe is 'Field' @r@ @k@, so a list cannot mix rows.
data FieldLit (f :: Universe -> Type) (r :: Type) where
  FieldLit :: forall k f r. KnownSymbol k => Expr f (Field r k) -> FieldLit f r

fieldKey :: FieldLit f r -> String
fieldKey (FieldLit @k _) = symbolVal (Proxy :: Proxy k)

data Expr :: (Universe -> Type) -> Universe -> Type where
  -- Good Parts: values, arithmetic, strict equality, functions, @const@ lets
  Literal :: Value u -> Expr f u -- ^ A literal value. eg. 1, "foo", etc
  Concat :: Expr f 'String -> Expr f 'String -> Expr f 'String -- ^ String concatenation: @+@
  Plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Addition: @+@
  Times :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Multiplication: @*@
  Minus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Subtraction: @-@
  Negate :: Expr f 'Number -> Expr f 'Number -- ^ @-(x)@
  FracDiv :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Division: @/@
  Rem :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Remainder: @%@ (JS, not floor-mod)
  BitAnd :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ @&@ after ToInt32
  BitOr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ @|@
  BitXor :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ @^@
  Shl :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ @<<@
  Shr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ @>>@
  UShr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ @>>>@
  And :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool -- ^ @&&@
  Or :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool -- ^ @||@
  Eq :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ Strict equality: @===@ (never @==@)
  NEq :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ Strict inequality: @!==@ (never @!=@)
  GTh :: Comparable a => Expr f a -> Expr f a -> Expr f 'Bool -- ^ @>@ (numbers, strings, bools — not objects)
  LTh :: Comparable a => Expr f a -> Expr f a -> Expr f 'Bool -- ^ @<@
  GTEq :: Comparable a => Expr f a -> Expr f a -> Expr f 'Bool -- ^ @>=@
  LTEq :: Comparable a => Expr f a -> Expr f a -> Expr f 'Bool -- ^ @<=@
  Let :: Expr f u -> (f u -> Expr f v) -> Expr f v -- ^ PHOAS let; codegen emits @const@
  LetRec :: (f u -> Expr f u) -> (f u -> Expr f v) -> Expr f v -- ^ Recursive let
  Lambda :: (f u -> Expr f v) -> Expr f ('Function u v) -- ^ Weak PHOAS lambda: binder is @f u@
  Apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
  Show :: Expr f u -> Expr f 'String -- ^ @String(x)@
  TypeOf :: Expr f u -> Expr f 'String -- ^ @typeof x@ (closed name; @null@ is @\"object\"@)
  Var :: f u -> Expr f u -- ^ PHOAS variable (Kmett's Place / return)
  If :: Expr f 'Bool -> Expr f u -> Expr f u -> Expr f u -- ^ Ternary: @c ? t : e@
  -- Option is JS @null@ / the value itself. Intro via 'UnsafeNullable' or
  -- @Literal (ValueOption …)@. 'OptionCase' stays a primitive: 'evaluate'
  -- uses @f = Value@, so a bound @'Option u@ cannot be unwrapped by
  -- @if_ (opt .== none)@ plus a type-changing coerce.
  OptionCase :: Expr f ('Option u) -> Expr f v -> (f u -> Expr f v) -> Expr f v -- ^ Eliminate an 'Option', analogous to 'maybe'.
  ResultOk :: Expr f a -> Expr f ('Result e a)
  ResultErr :: Expr f e -> Expr f ('Result e a)
  ResultCase :: Expr f ('Result e a) -> (f e -> Expr f v) -> (f a -> Expr f v) -> Expr f v -- ^ Eliminate a 'Result', analogous to 'either'.
  -- Constrained JS surface (fixed names / universes; not a general FFI).
  -- True escapes ('alert', raw @foo()@, free-text methods) live on 'Effect'.
  ExprIndex :: Expr f ('Array u) -> Expr f 'Number -> Expr f u -- ^ Array indexing: @arr[i]@.
  MathUnary :: MathFn1 -> Expr f 'Number -> Expr f 'Number -- ^ Unary @Math.fn(x)@. Closed names; observationally pure.
  MathBinary :: MathFn2 -> Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Binary @Math.fn(x, y)@. Closed names; observationally pure.
  ExprUnary :: StdUnary a b -> Expr f a -> Expr f b -- ^ Closed-name unary (@toUpperCase@, @.length@, @JSON.stringify@).
  ExprBinary :: StdBinary a b c -> Expr f a -> Expr f b -> Expr f c -- ^ Closed-name binary (@indexOf@, @join@, …).
  ExprTernary :: StdTernary a b c d -> Expr f a -> Expr f b -> Expr f c -> Expr f d -- ^ Closed-name ternary (@slice@, @replace@).
  ExprMap :: Expr f ('Array u) -> (f u -> Expr f v) -> Expr f ('Array v) -- ^ @arr.map(function(x){…})@. Callback stays on 'Expr'.
  ExprFilter :: Expr f ('Array u) -> (f u -> Expr f 'Bool) -> Expr f ('Array u) -- ^ @arr.filter(function(x){…})@. Callback stays on 'Expr'.
  ExprReduce :: Expr f ('Array u) -> Expr f v -> (f v -> f u -> Expr f v) -> Expr f v -- ^ @arr.reduce(function(acc,x){…}, z)@.
  UnsafeNullable :: Expr f u -> Expr f ('Option u) -- ^ Reinterpret a nullable JS value (e.g. from an FFI call) as an 'Option'.
  UnsafeEffectExpr :: Effect f u -> Expr f u -- ^ Optimizer splice only; not part of the surface API. Pass effects to FFI via 'ArgEffect'.
  FrozenLit :: [FieldLit f r] -> Expr f ('Object r) -- ^ Frozen @{k: v, …}@. Identity-insensitive; field reads stay on 'Expr'.
  GetField :: forall k r f. KnownSymbol k => Expr f ('Object r) -> Expr f (Field r k) -- ^ Pure @o.k@. Folded by 'sameSymbol' against 'FrozenLit'.

-- | Closed unary names on 'Expr'. There is no way to write @alert@ here.
data StdUnary :: Universe -> Universe -> Type where
  StdToUpper   :: StdUnary 'String 'String
  StdToLower   :: StdUnary 'String 'String
  StdTrim      :: StdUnary 'String 'String
  StdArrLen    :: StdUnary ('Array u) 'Number
  StdStrLen    :: StdUnary 'String 'Number
  StdStringify :: StdUnary u 'String

-- | Closed binary names on 'Expr'.
data StdBinary :: Universe -> Universe -> Universe -> Type where
  StdIndexOf  :: StdBinary 'String 'String 'Number
  StdSplit    :: StdBinary 'String 'String ('Array 'String)
  StdIncludes :: StdBinary ('Array u) u 'Bool
  StdConcat   :: StdBinary ('Array u) ('Array u) ('Array u)
  StdJoin     :: StdBinary ('Array u) 'String 'String
  StdTest     :: StdBinary 'Regex 'String 'Bool
  StdParseInt :: StdBinary 'String 'Number 'Number -- ^ radix required (book appendix A)

-- | Closed ternary names on 'Expr'.
data StdTernary :: Universe -> Universe -> Universe -> Universe -> Type where
  StdSlice    :: StdTernary 'String 'Number 'Number 'String
  StdArrSlice :: StdTernary ('Array u) 'Number 'Number ('Array u)
  StdReplace  :: StdTernary 'String 'String 'String 'String

-- | Closed unary @Math.*@ names. JS identifier is 'mathFn1Name'.
data MathFn1
  = MathSin | MathCos | MathTan | MathAsin | MathAcos | MathAtan
  | MathSinh | MathCosh | MathTanh | MathAsinh | MathAcosh | MathAtanh
  | MathSqrt | MathCbrt | MathExp | MathLog | MathLog2 | MathLog10
  | MathFloor | MathCeil | MathRound | MathTrunc
  | MathAbs | MathSign

-- | Closed binary @Math.*@ names. JS identifier is 'mathFn2Name'.
data MathFn2
  = MathPow | MathAtan2 | MathMax | MathMin | MathHypot

mathFn1Name :: MathFn1 -> Text
mathFn1Name = \case
  MathSin -> "sin"
  MathCos -> "cos"
  MathTan -> "tan"
  MathAsin -> "asin"
  MathAcos -> "acos"
  MathAtan -> "atan"
  MathSinh -> "sinh"
  MathCosh -> "cosh"
  MathTanh -> "tanh"
  MathAsinh -> "asinh"
  MathAcosh -> "acosh"
  MathAtanh -> "atanh"
  MathSqrt -> "sqrt"
  MathCbrt -> "cbrt"
  MathExp -> "exp"
  MathLog -> "log"
  MathLog2 -> "log2"
  MathLog10 -> "log10"
  MathFloor -> "floor"
  MathCeil -> "ceil"
  MathRound -> "round"
  MathTrunc -> "trunc"
  MathAbs -> "abs"
  MathSign -> "sign"

mathFn2Name :: MathFn2 -> Text
mathFn2Name = \case
  MathPow -> "pow"
  MathAtan2 -> "atan2"
  MathMax -> "max"
  MathMin -> "min"
  MathHypot -> "hypot"

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
instance forall u. (u ~ 'String) => Exts.IsString (Value u) where
  fromString = ValueString . Exts.fromString

instance forall (f :: (Universe -> Type)) u. (u ~ 'String) => Exts.IsString (Expr f u) where
  fromString s = Literal (Exts.fromString s)

instance forall (f :: Universe -> Type) u. (u ~ 'String) => Semigroup (Expr f u) where
  (<>) = Concat

-- | 'Num' / 'Fractional' / 'Floating' for JS numbers:
--
-- * @Value 'Number@ — @1@, @2.5@; arithmetic runs eagerly on host
--   'Double's (so @'Literal' (1 + 2)@ is already @'Literal' 3@)
-- * @Expr f 'Number@ — literals via 'Literal'; ops build AST nodes
--   ('Plus'/'Times'/'MathUnary'/…) and fold later in codegen.
--   @(**)@ is @Math.pow@, not @exp (log x * y)@.
--
-- Prefer a signature when the hole is ambiguous. Use 'JShark.Api.number'
-- for arbitrary runtime 'Double's (integer literals can use 'Num' directly).
instance forall u. (u ~ 'Number) => Num (Value u) where
  (+) = liftValue2 (+)
  (*) = liftValue2 (*)
  (-) = liftValue2 (-)
  abs = liftValue1 abs
  signum = liftValue1 signum
  fromInteger = ValueNumber . fromInteger
  negate = liftValue1 negate

instance forall u. (u ~ 'Number) => Fractional (Value u) where
  (/) = liftValue2 (/)
  fromRational = ValueNumber . fromRational

liftValue1 :: (Double -> Double) -> Value 'Number -> Value 'Number
liftValue1 f (ValueNumber a) = ValueNumber (f a)

liftValue2 :: (Double -> Double -> Double) -> Value 'Number -> Value 'Number -> Value 'Number
liftValue2 f (ValueNumber a) (ValueNumber b) = ValueNumber (f a b)

instance forall (f :: Universe -> Type) u. (u ~ 'Number) => Num (Expr f u) where
  (+) = Plus
  (*) = Times
  (-) = Minus
  abs = MathUnary MathAbs
  signum = MathUnary MathSign
  fromInteger n = Literal (fromInteger n)
  negate = Negate

instance forall (f :: Universe -> Type) u. (u ~ 'Number) => Fractional (Expr f u) where
  (/) = FracDiv
  fromRational r = Literal (fromRational r)

jsPi :: Double
jsPi = pi

instance forall (f :: Universe -> Type) u. (u ~ 'Number) => Floating (Expr f u) where
  pi = Literal (ValueNumber jsPi)
  exp = MathUnary MathExp
  log = MathUnary MathLog
  sqrt = MathUnary MathSqrt
  (**) = MathBinary MathPow
  sin = MathUnary MathSin
  cos = MathUnary MathCos
  tan = MathUnary MathTan
  asin = MathUnary MathAsin
  acos = MathUnary MathAcos
  atan = MathUnary MathAtan
  sinh = MathUnary MathSinh
  cosh = MathUnary MathCosh
  tanh = MathUnary MathTanh
  asinh = MathUnary MathAsinh
  acosh = MathUnary MathAcosh
  atanh = MathUnary MathAtanh

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

fromSyntax :: EffectSyntax f (f v) -> Effect f v
fromSyntax (EffectSyntaxPure x) = Lift (Var x)
fromSyntax (EffectSyntaxUnpure m g) = Bind m (fromSyntax . g)
