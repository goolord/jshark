{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}

{-# options_ghc -fno-warn-unused-top-binds #-}

module Types where

import Data.Kind
import Data.Sequence (Seq(..), (|>))
import Data.Text (Text)
import Topaz.Types
import qualified GHC.Exts as Exts
import qualified Language.JavaScript.AST as GP
import qualified Language.JavaScript.Pretty as GP
import qualified Text.PrettyPrint.Leijen as PP

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
  Host :: (f 'String -> Effect f u) -> Effect f u
  Log :: Expr f u -> Effect f u' -> Effect f u'
  LookupId :: Expr f 'String -> (f 'Element -> Effect f u) -> Effect f u
  LookupSelector :: Expr f 'String -> (f ('Array 'Element) -> Effect f u) -> Effect f u
  Lift :: Expr f u -> Effect f u
  FFI :: String -> Rec (Expr f) (u' ': us) -> Effect f u

data Expr :: (Universe -> Type) -> Universe -> Type where
  Literal :: Value u -> Expr f u
  Concat :: Expr f 'String -> Expr f 'String -> Expr f 'String
  Plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
  Times :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
  Minus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
  Abs :: Expr f 'Number -> Expr f 'Number
  Sign :: Expr f 'Number -> Expr f 'Number
  Negate :: Expr f 'Number -> Expr f 'Number
  FracDiv :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
  Recip :: Expr f 'Number -> Expr f 'Number
  Let :: Expr f u -> (f u -> Expr f v) -> Expr f v
  Lambda :: (f u -> Expr f v) -> Expr f ('Function u v)
  Apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
  Show :: Expr f u -> Expr f 'String
  Var :: f u -> Expr f u 

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

instance forall (f :: Universe -> Type) u. (u ~ 'Number) => Fractional (Expr f u) where
  (/) = FracDiv
  recip = Recip
  fromRational = Literal . ValueNumber . fromRational

-- newtype Expression :: (Universe -> Type) -> Universe -> Type where
--   Expression :: ExprArrow (->) f u -> Expression f u

-- Op :: Operation _ u -> ExprArrow g f u
-- data Operation :: () -> Universe -> Type
  -- Plus :: Operation f 'Number -> Operation f 'Number -> Operation f 'Number

data Optimization 
  = ConstantFolding
  | UnusedBindings

data Computation = Computation GP.Expr (Seq GP.VarStmt)
newtype EffComputation = EffComputation (Seq (Either GP.VarStmt GP.Expr))

instance PP.Pretty EffComputation where
  pretty (EffComputation x) = foldr1 (PP.<$$>) (fmap (either PP.pretty PP.pretty) x)

instance PP.Pretty Computation where
  pretty (Computation ex vars) = 
    foldr1 (PP.<$$>) ( fmap PP.pretty vars |> PP.pretty ex)
