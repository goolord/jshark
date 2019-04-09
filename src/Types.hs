{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}

module Types where

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

data Value :: Universe -> Type where
  ValueArray :: [Value u] -> Value ('Array u)
  ValueNumber :: Double -> Value 'Number
  ValueString :: Text -> Value 'String
  ValueFunction :: (Value u -> Value v) -> Value ('Function u v)
  ValueUnit :: Value 'Unit
  ValueOption :: Maybe (Value u) -> Value ('Option u)
  ValueResult :: Either (Value u) (Value v) -> Value ('Result u v)

data Effect :: (Universe -> Type) -> Universe -> Type where
  Host :: (f 'String -> Effect f u) -> Effect f u -- ^ window.location.host
  Log :: Expr f u -> Effect f u' -> Effect f u' -- ^ console.log(x); <effect>
  LookupId :: Expr f 'String -> (f 'Element -> Effect f u) -> Effect f u -- ^ const n0 = document.getElementById(x); <effect n0>
  LookupSelector :: Expr f 'String -> (f ('Array 'Element) -> Effect f u) -> Effect f u -- ^ const n0 = document.querySelectorAll(x); <effect n0>
  Lift :: Expr f u -> Effect f u -- ^ Lift a non-effectful computation into the effectful AST
  FFI :: String -> Rec (Expr f) (u' ': us) -> Effect f u -- ^ Foreign function interface. Takes the name of the function as a String, and then a Rec of its arguments. This is unsafe, but if you supply the correct types in a helper function, the type checker will enforce these types on the user.
  ClassToggle :: Expr f 'Element -> Expr f 'String -> Effect f 'Unit -- ^ x.classList.toggle(y)
  ClassAdd :: Expr f 'Element -> Expr f 'String -> Effect f 'Unit -- ^ x.classList.add(y) 
  ClassRemove :: Expr f 'Element -> Expr f 'String -> Effect f 'Unit -- ^ x.classList.remove(y) 
  ForEach :: Expr f ('Array u) -> (f u -> Effect f u') -> Effect f 'Unit

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

