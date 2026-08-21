{-# LANGUAGE
    DataKinds
  , DeriveFunctor
  , DerivingStrategies
  , GADTs
  , KindSignatures
  , RankNTypes
  , StandaloneDeriving
  , TypeOperators
#-}
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
  | Result Universe Universe
  | Bool
  | Object Type
  | Effectful Universe

data Value :: Universe -> Type where
  ValueArray :: [Value u] -> Value ('Array u)
  ValueNumber :: Double -> Value 'Number
  ValueString :: Text -> Value 'String
  ValueFunction :: (Value u -> Value v) -> Value ('Function u v)
  ValueUnit :: Value 'Unit
  ValueOption :: Maybe (Value u) -> Value ('Option u)
  ValueResult :: Either (Value u) (Value v) -> Value ('Result u v)
  ValueBool :: Bool -> Value 'Bool

data Effect :: (Universe -> Type) -> Universe -> Type where
  Lift :: Expr f u -> Effect f u -- ^ Lift a non-effectful computation into the effectful AST
  FFI :: String -> Rec (Expr f) us -> Effect f u -- ^ Foreign function interface: @FFI name args@.
  UnsafeObject :: Text -> Effect f ('Object x)
  UnsafeObjectGet :: Effect f object -> String -> Effect f u
  UnsafeObjectAssign :: Effect f object -> Effect f assignment -> Effect f u
  ObjectFFI :: Effect f object -> Effect f b -> Effect f u
  ForEach :: Expr f ('Array u) -> (f u -> Effect f u') -> Effect f 'Unit
  Bind :: Effect f u -> (f u -> Effect f v) -> Effect f v
  UnEffectful :: Expr f ('Effectful u) -> Effect f u
  LambdaE :: (f u -> Effect f v) -> Effect f ('Function u v) -- ^ A function, not *necessarily* anonymous
  ApplyE :: Effect f ('Function u v) -> Effect f u -> Effect f v -- ^ Apply a function
  IfE :: Expr f 'Bool -> Effect f u -> Effect f u -> Effect f u -- ^ Effectful conditional. NB: the condition expression is currently assumed to require no intermediate declarations (i.e. no nested 'Let'); see 'While' for the same caveat.
  While :: Expr f 'Bool -> Effect f 'Unit -> Effect f 'Unit -- ^ Loop while the condition holds. NB: the condition is re-checked by emitting the *same* rendered expression into the generated @while@ test on every iteration, so it must not depend on declarations ('Let') that only run once.

data Expr :: (Universe -> Type) -> Universe -> Type where
  Literal :: Value u -> Expr f u -- ^ A literal value. eg. 1, "foo", etc
  Concat :: Expr f 'String -> Expr f 'String -> Expr f 'String -- ^ Concatenation primitive: Concat = +
  Plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Addition primitive: Plus = +
  Times :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Multiplication primitive: Times = *
  Minus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Subtraction primitive: Minus = -
  Abs :: Expr f 'Number -> Expr f 'Number -- ^ Absolute value primitive: Abs x = Math.abs(x)
  Sign :: Expr f 'Number -> Expr f 'Number -- ^ Sign primitive: Sign x = Math.sign(x)
  Negate :: Expr f 'Number -> Expr f 'Number -- ^ Negate primitive: Negate x = (x * -1)
  FracDiv :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Division primitive: FracDiv = (/)
  And :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool -- ^ Logical And. And = (&&)
  Or :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool -- ^ Logical Or. Or = (||)
  Eq :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ Equality. Eq = (==)
  NEq :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ Inequality. NEq = (/=)
  GTh :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ Inequality check on ordering. GTh = (>)
  LTh :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ Inequality check on ordering. LTh = (<) 
  GTEq :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ Inequality check on ordering. GTEq = (>=) 
  LTEq :: Expr f a -> Expr f a -> Expr f 'Bool -- ^ Inequality check on ordering. LTEq = (<=) 
  Let :: Expr f u -> (f u -> Expr f v) -> Expr f v -- ^ Assign a value in an Expr
  Lambda :: (f u -> Expr f v) -> Expr f ('Function u v) -- ^ A function, not *necessarily* anonymous
  Apply :: Expr f ('Function u v) -> Expr f u -> Expr f v -- ^ Apply a function
  Show :: Expr f u -> Expr f 'String -- ^ String casting: Show x = String(x)
  Var :: f u -> Expr f u  -- ^ Variable reference
  If :: Expr f 'Bool -> Expr f u -> Expr f u -> Expr f u -- ^ Ternary conditional: If c t e = c ? t : e
  Some :: Expr f u -> Expr f ('Option u) -- ^ Option introduction. Represented at runtime as the wrapped value itself (see 'None').
  None :: Expr f ('Option u) -- ^ Option introduction: the absence of a value, represented as JS @null@.
  OptionCase :: Expr f ('Option u) -> Expr f v -> (f u -> Expr f v) -> Expr f v -- ^ Eliminate an 'Option', analogous to 'maybe'.
  Ok :: Expr f u -> Expr f ('Result u v) -- ^ Result introduction for the success case. Represented at runtime as @[true, x]@.
  Err :: Expr f v -> Expr f ('Result u v) -- ^ Result introduction for the failure case. Represented at runtime as @[false, x]@.
  ResultCase :: Expr f ('Result u v) -> (f u -> Expr f w) -> (f v -> Expr f w) -> Expr f w -- ^ Eliminate a 'Result', analogous to 'either'.
  UnsafeEffectExpr :: Effect f u -> Expr f u -- ^ Embed an 'Effect' as a pure 'Expr'. Unsafe: only sound when the embedded effect is self-contained.
  ExprFFI :: Text -> Rec (Expr f) us -> Expr f v -- ^ Call a named global JS function: @fnName(args...)@.
  ExprProp :: Expr f u -> Text -> Expr f v -- ^ Property access: @receiver.prop@.
  ExprMethod :: Expr f u -> Text -> Rec (Expr f) us -> Expr f v -- ^ Method call: @receiver.method(args...)@.
  ExprMethodCallback :: Expr f u -> Text -> (f a -> Expr f b) -> Expr f v -- ^ Method call taking a callback, e.g. @receiver.map(function(x){...})@.
  ExprIndex :: Expr f ('Array u) -> Expr f 'Number -> Expr f u -- ^ Array indexing: @arr[i]@.
  MathUnary :: Text -> Expr f 'Number -> Expr f 'Number -- ^ Call a unary @Math@ function: @Math.fn(x)@.
  MathBinary :: Text -> Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Call a binary @Math@ function: @Math.fn(x, y)@.
  UnsafeNullable :: Expr f u -> Expr f ('Option u) -- ^ Reinterpret a nullable JS value (e.g. from an FFI call) as an 'Option'.

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

-- | 'IsString' for JS string literals at each AST layer:
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

instance forall (f :: Universe -> Type) u. (u ~ 'Number) => Num (Expr f u) where
  (+) = Plus
  (*) = Times
  (-) = Minus
  abs = Abs
  signum = Sign
  fromInteger = Literal . ValueNumber . fromInteger
  negate = Negate

instance forall (f :: Universe -> Type) u. (u ~ 'Number) => Fractional (Expr f u) where
  (/) = FracDiv
  fromRational = Literal . ValueNumber . fromRational

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
