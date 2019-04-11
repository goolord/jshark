{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}

module JShark.Types where

import Control.Monad (ap, void)
import Data.Kind
import Data.Text (Text)
import Topaz.Types
import qualified GHC.Exts as Exts

data Universe
  = Number
  | String
  | Unit
  | Element
  | Array Universe
  | Function Universe Universe
  | Option Universe
  | Result Universe Universe
  | Bool
  | forall a. Object a

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
  Host :: Effect f 'String -- ^ window.location.host
  Log :: Expr f u -> Effect f 'Unit -- ^ console.log(x)
  LookupId :: Expr f 'String -> (f 'Element -> Effect f u) -> Effect f u -- ^ const n0 = document.getElementById(x); <effect n0>
  LookupSelector :: Expr f 'String -> (f ('Array 'Element) -> Effect f u) -> Effect f u -- ^ const n0 = document.querySelectorAll(x); <effect n0>
  Lift :: Expr f u -> Effect f u -- ^ Lift a non-effectful computation into the effectful AST
  FFI :: String -> Rec (Expr f) us -> Effect f u -- ^ Foreign function interface. Takes the name of the function as a String, and then a Rec of its arguments. This is unsafe, but if you supply the correct types in a helper function, the type checker will enforce these types on the user.
  UnsafeObject :: Expr f ('Object a) -> String -> Effect f u -- ^ Don't currently know how to construct an object
  ObjectFFI :: Expr f ('Object a) -> Effect f b -> Effect f u
  ClassToggle :: Expr f 'Element -> Expr f 'String -> Effect f 'Unit -- ^ x.classList.toggle(y)
  ClassAdd :: Expr f 'Element -> Expr f 'String -> Effect f 'Unit -- ^ x.classList.add(y) 
  ClassRemove :: Expr f 'Element -> Expr f 'String -> Effect f 'Unit -- ^ x.classList.remove(y) 
  ForEach :: Expr f ('Array u) -> (f u -> Effect f u') -> Effect f 'Unit
  Bind :: Effect f u -> (f u -> Effect f v) -> Effect f v
  SpaceShip :: Effect f u -> Effect f v -> Effect f v

data Expr :: (Universe -> Type) -> Universe -> Type where
  Literal :: Value u -> Expr f u -- ^ A literal value. eg. 1, "foo", etc
  Concat :: Expr f 'String -> Expr f 'String -> Expr f 'String -- ^ Concatenation primitive: Concat = +
  Plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Addition primitive: Plus = +
  Times :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Multiplication primitive: Times = *
  Minus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Subtraction primitive: Minus = -
  Abs :: Expr f 'Number -> Expr f 'Number -- ^ Absolute value primitive: Abs x = Math.abs(x)
  Sign :: Expr f 'Number -> Expr f 'Number -- ^ Sign primitive: Sign x = Math.sign(x)
  Negate :: Expr f 'Number -> Expr f 'Number -- ^ Negate primitive: Negate x = (x * -1)
  FracDiv :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -- ^ Division primitive: FracDiv = /
  Let :: Expr f u -> (f u -> Expr f v) -> Expr f v -- ^ Assign a value in an Expr
  Lambda :: (f u -> Expr f v) -> Expr f ('Function u v) -- ^ A function, not *necessarily* anonymous
  Apply :: Expr f ('Function u v) -> Expr f u -> Expr f v -- ^ Apply a function
  Show :: Expr f u -> Expr f 'String -- ^ String casting: Show x = String(x)
  Var :: f u -> Expr f u  -- ^ Assignment

data ExprF :: (Type -> Type -> Type) -> (Universe -> Type) -> Universe -> Type where
  LiteralF :: Value u -> ExprF g f u
  PlusF :: ExprF g f 'Number -> ExprF g f 'Number -> ExprF g f 'Number
  LetF :: ExprF g f u -> g (f u) (ExprF g f v) -> ExprF g f v
  LambdaF :: g (f u) (ExprF g f v) -> ExprF g f ('Function u v)
  ApplyF :: ExprF g f ('Function u v) -> ExprF g f u -> ExprF g f v
  VarF :: f u -> ExprF g f u

data Statement :: (Universe -> Type) -> Universe -> Type where
  SLiteral :: Value u -> Statement f u
  SFFI :: Text -> Statement f u

instance forall (f :: (Universe -> Type)) u. (u ~ 'String) => Exts.IsString (Expr f u) where
  fromString = Literal . ValueString . Exts.fromString

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

-- newtype Expression :: (Universe -> Type) -> Universe -> Type where
--   Expression :: ExprArrow (->) f u -> Expression f u

-- Op :: Operation _ u -> ExprArrow g f u
-- data Operation :: () -> Universe -> Type
  -- Plus :: Operation f 'Number -> Operation f 'Number -> Operation f 'Number

data Optimization 
  = ConstantFolding
  | UnusedBindings

-- This is a monadic interface to expressions that uses some tricks
-- from https://people.seas.harvard.edu/~pbuiras/publications/KeyMonadHaskell2016.pdf

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
  (*>) = spaceShip

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

spaceShip :: EffectSyntax v a -> EffectSyntax v b -> EffectSyntax v b
spaceShip _ (EffectSyntaxPure b) = EffectSyntaxPure b
spaceShip EffectSyntaxPure{} (EffectSyntaxUnpure m g) = EffectSyntaxUnpure m g
spaceShip (EffectSyntaxUnpure n _) (EffectSyntaxUnpure m g) = EffectSyntaxUnpure (n `SpaceShip` m) g
