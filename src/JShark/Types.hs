{-# LANGUAGE
    DataKinds
  , DeriveFunctor
  , DerivingStrategies
  , GADTs
  , KindSignatures
  , OverloadedStrings
  , RankNTypes
  , StandaloneDeriving
  , TypeOperators
#-}
-- | Two PHOAS syntax trees for a typed subset of JavaScript.
--
-- * 'Expr' is the /pure/ tree: Crockford's good parts as expressions
--   (literals, arithmetic, @===@, functions, @const@-bound lets, arrays).
-- * 'Effect' is the /impure/ tree: statements, FFI, mutation, I/O.
--
-- Binders are parametric (@f :: Universe -> Type@), i.e. weak PHOAS.
-- A closed term is an end over that parameter: 'ClosedExpr' / 'ClosedEffect'
-- (@forall f. …@), the same quantification as Kmett's
-- @type End p = forall x. p x x@. The two trees meet at FFI via 'Arg',
-- not by smuggling effects through 'Expr'.
module JShark.Types where
import Control.Monad (ap, void)
import Data.Kind
import Data.Text (Text)
import JShark.Rec
import Text.PrettyPrint (Doc)
import qualified GHC.Exts as Exts

data Universe
  = Number
  | String
  | Unit
  | Array Universe
  | Function Universe Universe
  | Option Universe
  | Bool
  | Object Type

data Value :: Universe -> Type where
  ValueArray :: [Value u] -> Value ('Array u)
  ValueNumber :: Double -> Value 'Number
  ValueString :: Text -> Value 'String
  ValueFunction :: (Value u -> Value v) -> Value ('Function u v)
  ValueUnit :: Value 'Unit
  ValueOption :: Maybe (Value u) -> Value ('Option u)
  ValueBool :: Bool -> Value 'Bool

data Effect :: (Universe -> Type) -> Universe -> Type where
  Lift :: Expr f u -> Effect f u -- ^ Lift a pure expression into the effectful tree
  FFI :: String -> Rec (Arg f) us -> Effect f u -- ^ Foreign call: @name(args…)@. Args are 'Arg' so an effect (object handle, effectful function) need not pass through 'Expr'.
  UnsafeObject :: Text -> Effect f ('Object x)
  UnsafeObjectGet :: Effect f object -> String -> Effect f u
  UnsafeObjectAssign :: Effect f object -> Effect f assignment -> Effect f u
  CallMethod :: Effect f object -> String -> Rec (Arg f) us -> Effect f u -- ^ @recv.method(args…)@
  Bind :: Effect f u -> (f u -> Effect f v) -> Effect f v -- ^ PHOAS bind (@const n = e@)
  LambdaE :: (f u -> Effect f v) -> Effect f ('Function u v) -- ^ Effectful function (weak PHOAS: binder is @f u@, not @Effect@)
  ApplyE :: Effect f ('Function u v) -> Effect f u -> Effect f v
  IfE :: Expr f 'Bool -> Effect f u -> Effect f u -> Effect f u -- ^ Effectful conditional. NB: the condition expression is currently assumed to require no intermediate declarations (i.e. no nested 'Let'); see 'While' for the same caveat.
  While :: Expr f 'Bool -> Effect f 'Unit -> Effect f 'Unit -- ^ Loop while the condition holds. NB: the condition is re-checked by emitting the *same* rendered expression into the generated @while@ test on every iteration, so it must not depend on declarations ('Let') that only run once.

-- | An FFI argument drawn from either syntax tree. This is the sanctioned
-- seam between 'Expr' and 'Effect'; prefer it over 'UnsafeEffectExpr'.
data Arg :: (Universe -> Type) -> Universe -> Type where
  ArgExpr :: Expr f u -> Arg f u
  ArgEffect :: Effect f u -> Arg f u

data Expr :: (Universe -> Type) -> Universe -> Type where
  -- Good Parts: values, arithmetic, strict equality, functions, @const@ lets
  Literal :: Value u -> Expr f u -- ^ A literal value. eg. 1, "foo", etc
  Concat :: Expr f 'String -> Expr f 'String -> Expr f 'String -- ^ String concatenation: @+@
  Plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Addition: @+@
  Times :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Multiplication: @*@
  Minus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Subtraction: @-@
  Negate :: Expr f 'Number -> Expr f 'Number -- ^ @-(x)@
  FracDiv :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Division: @/@
  And :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool -- ^ @&&@
  Or :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool -- ^ @||@
  Eq :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ Strict equality: @===@ (never @==@)
  NEq :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ Strict inequality: @!==@ (never @!=@)
  GTh :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ @>@
  LTh :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ @<@
  GTEq :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ @>=@
  LTEq :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ @<=@
  Let :: Expr f u -> (f u -> Expr f v) -> Expr f v -- ^ PHOAS let; codegen emits @const@
  Lambda :: (f u -> Expr f v) -> Expr f ('Function u v) -- ^ Weak PHOAS lambda: binder is @f u@
  Apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
  Show :: Expr f u -> Expr f 'String -- ^ @String(x)@
  Var :: f u -> Expr f u -- ^ PHOAS variable (Kmett's Place / return)
  If :: Expr f 'Bool -> Expr f u -> Expr f u -> Expr f u -- ^ Ternary: @c ? t : e@
  -- Option is JS @null@ / the value itself. Intro via 'UnsafeNullable' or
  -- @Literal (ValueOption …)@. 'OptionCase' stays a primitive: 'evaluate'
  -- uses @f = Value@, so a bound @'Option u@ cannot be unwrapped by
  -- @if_ (opt .== none)@ plus a type-changing coerce.
  OptionCase :: Expr f ('Option u) -> Expr f v -> (f u -> Expr f v) -> Expr f v -- ^ Eliminate an 'Option', analogous to 'maybe'.
  -- Constrained JS surface (fixed names / universes; not a general FFI)
  ExprIndex :: Expr f ('Array u) -> Expr f 'Number -> Expr f u -- ^ Array indexing: @arr[i]@.
  MathUnary :: Text -> Expr f 'Number -> Expr f 'Number -- ^ Unary @Math.fn(x)@. Only names in the optimizer whitelist are treated as pure; unknown names are kept (not DCE'd).
  MathBinary :: Text -> Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Binary @Math.fn(x, y)@. Same purity whitelist as 'MathUnary'.
  -- Untyped JS on the pure tree. Not referentially transparent: the name is
  -- unchecked, so @push@ / @alert@ type-check. Effectful calls belong on
  -- 'Effect' ('CallMethod' / 'FFI'). Named stdlib wrappers that return
  -- 'Expr' (e.g. 'toUpper') assume observational purity.
  UnsafeExprProp :: Expr f u -> Text -> Expr f v -- ^ @receiver.prop@
  UnsafeExprMethod :: Expr f u -> Text -> Rec (Expr f) us -> Expr f v -- ^ @receiver.method(args…)@
  UnsafeExprMethodCallback :: Expr f u -> Text -> (f a -> Expr f b) -> Expr f v -- ^ @receiver.map(function(x){…})@
  UnsafeExprFFI :: Text -> Rec (Expr f) us -> Expr f v -- ^ @fnName(args…)@
  UnsafeNullable :: Expr f u -> Expr f ('Option u) -- ^ Reinterpret a nullable JS value (e.g. from an FFI call) as an 'Option'.
  UnsafeEffectExpr :: Effect f u -> Expr f u -- ^ Embed an 'Effect' as a pure 'Expr'. Optimizer splice placeholder; at the surface, pass effects to FFI via 'ArgEffect' instead.

-- | Closed pure term: no free PHOAS binders. The end @forall f. 'Expr' f u@.
type ClosedExpr (u :: Universe) = forall (f :: Universe -> Type). Expr f u

-- | Closed effectful term: no free PHOAS binders. The end @forall f. 'Effect' f u@.
type ClosedEffect (u :: Universe) = forall (f :: Universe -> Type). Effect f u

-- | First-order fragment used by the original unused-binding experiment
-- ('JShark.ExprF'). Only 'Literal'/'Plus'/'Let'/'Lambda'/'Apply'/'Var'.
-- Full-program optimization uses 'JShark.optimize' on 'Expr' instead.
data ExprF :: (Type -> Type -> Type) -> (Universe -> Type) -> Universe -> Type where
  LiteralF :: Value u -> ExprF g f u
  PlusF :: ExprF g f 'Number -> ExprF g f 'Number -> ExprF g f 'Number
  LetF :: ExprF g f u -> g (f u) (ExprF g f v) -> ExprF g f v
  LambdaF :: g (f u) (ExprF g f v) -> ExprF g f ('Function u v)
  ApplyF :: ExprF g f ('Function u v) -> ExprF g f u -> ExprF g f v
  VarF :: f u -> ExprF g f u

-- | 'IsString' for JS string literals at each pure AST layer:
--
-- * @Value 'String@ — @"hi"@
-- * @Expr f 'String@ — @"hi"@ as 'Literal'
-- * @ExprF g f 'String@ — same for the ExprF fragment
--
-- Prefer an explicit type signature when the hole is ambiguous with
-- 'String'/'Text'. Use 'JShark.Api.string' for runtime 'Text' values.
instance forall u. (u ~ 'String) => Exts.IsString (Value u) where
  fromString = ValueString . Exts.fromString

instance forall (f :: (Universe -> Type)) u. (u ~ 'String) => Exts.IsString (Expr f u) where
  fromString s = Literal (Exts.fromString s)

instance forall g (f :: Universe -> Type) u. (u ~ 'String) => Exts.IsString (ExprF g f u) where
  fromString s = LiteralF (Exts.fromString s)

-- | 'Num' / 'Fractional' for JS numbers at each pure AST layer that
-- supports them:
--
-- * @Value 'Number@ — @1@, @2.5@; arithmetic runs eagerly on host
--   'Double's (so @'Literal' (1 + 2)@ is already @'Literal' 3@)
-- * @Expr f 'Number@ — literals via 'Literal'; ops build AST nodes
--   ('Plus'/'Times'/…) and fold later in codegen
--
-- 'ExprF' only has 'PlusF', so it has no 'Num' instance. Prefer a
-- signature when the hole is ambiguous. Use 'JShark.Api.number' for
-- arbitrary runtime 'Double's (integer literals can use 'Num' directly).
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

-- | JS names for 'Num' on 'Expr'. Must stay in 'JShark.mathUnaryOp'.
mathAbs, mathSign :: Text
mathAbs = "abs"
mathSign = "sign"

instance forall (f :: Universe -> Type) u. (u ~ 'Number) => Num (Expr f u) where
  (+) = Plus
  (*) = Times
  (-) = Minus
  abs = MathUnary mathAbs
  signum = MathUnary mathSign
  fromInteger n = Literal (fromInteger n)
  negate = Negate

instance forall (f :: Universe -> Type) u. (u ~ 'Number) => Fractional (Expr f u) where
  (/) = FracDiv
  fromRational r = Literal (fromRational r)

-- Monadic interface to expressions based on KeyMonad
-- (https://people.seas.harvard.edu/~pbuiras/publications/KeyMonadHaskell2016.pdf).

bindEffect :: Effect v a -> (v a -> Effect v b) -> Effect v b
bindEffect = Bind

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

data Code = Code 
  { codeDecl :: Doc 
  , codeRef :: Doc
  }

instance Semigroup Code where
  (Code a b) <> (Code x y) = Code (a <> b) (x <> y)

instance Monoid Code where
  mempty = Code mempty mempty
