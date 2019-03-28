{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language ExistentialQuantification #-}
{-# language StandaloneDeriving #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleInstances #-}

-- {-# OPTIONS_GHC -Wall -Werror #-}

module Javascript where
  -- ( -- * Types
    -- Universe(..)
  -- , Value(..)
  -- , Strategy(..)
--  , Statement(..)
    -- * Construction
--  , literal
    -- * Interpretation
--  , interpret
  -- ) where

import Data.Void (absurd)
import qualified GHC.Num as Num

data Universe
  = Null -- ^ null
  | Number -- ^ IEEE 754 floating point double precision
  | String -- ^ javascript strings
  | Array Universe -- ^ javascript arrays
--  | Option Universe -- ^ option type. does not exist in source js.
--  | Result Universe Universe -- ^ result type. does not exist in source js.

data Value :: Universe -> Type where
  ValueNull :: Void -> Value 'Null -- ^ absurd
  ValueNumber :: Double -> Value 'Number
  ValueString :: Text -> Value 'String
  ValueArray :: [Value u] -> Value ('Array u)
--  ValueOption ::
--  ValueResult ::

instance Num.Num (Value 'Number) where
  (ValueNumber a) + (ValueNumber b) = ValueNumber (a + b)
  (ValueNumber a) * (ValueNumber b) = ValueNumber (a * b)
  (ValueNumber a) - (ValueNumber b) = ValueNumber (a - b)
  abs (ValueNumber a) = ValueNumber (Num.abs a)
  signum (ValueNumber a) = ValueNumber (Num.signum a)
  fromInteger n = ValueNumber (Num.fromInteger n)

instance Semiring (Value 'Number) where
  plus = (Num.+)
  zero = ValueNumber 0
  times = (Num.*)
  one = ValueNumber 1

deriving instance Show (Value u)

data Option n = Some (Value n) | None

-- data Function :: Type -> [Universe] -> Universe -> Type where
  -- Function :: STRef s _ -> Function s rs res

newtype Binding :: (Universe -> Type) -> Universe -> Type where
  Binding :: f u -> Binding f u

-- | Code Evaluation.
newtype Evaluate :: Universe -> Type where
  Evaluate :: Value u -> Evaluate u

-- | Code Generation.
newtype Generate :: Universe -> Type where
  Generate :: Double -> Generate u

-- | The type of expressions, which are pure computations.
data Expr (f :: Universe -> Type) n where
  Literal :: -- ^ Const literals.
      Value u
   -> (Binding f u -> n)
   -> Expr f n
  Plus :: -- ^ IEEE 754 double-precision addition.
      Binding f 'Number
   -> Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  Times :: -- ^ IEEE 754 double-precision multiplication.
      Binding f 'Number
   -> Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  Minus :: -- ^ IEEE 754 double-precision subtraction.
      Binding f 'Number
   -> Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n

-- | Imperitive statemnets, which are impure computations
data Statement (f :: Universe -> Type) n where
  Log :: 
       Binding f u
    -> n
    -> Statement f n
  Foreach ::
       Binding f 'Number
    -> Binding f ('Array u)
    -> Binding f u
    -> (Binding f u  -> Free (Statement f) ())
    -> n
    -> Statement f n
  -- ^ Foreach is an inherently imperative construct. Consequently, it does not
  --   return anything.

  -- | forall (rs :: [Universe]) (res :: Universe). Declare (Rec (Binding s t) rs -> Free (Statement s t) (Binding s t res)) (Function rs res -> n)
    -- ^ Not totally sure if Declare should have the function arrow in its first arg.
  -- | forall (rs :: [Universe]) (res :: Universe). Call (Function rs res) (Rec Value rs) (Value res -> n)

deriving stock instance Functor (Statement f)
deriving stock instance Functor (Expr f)

type JSM f = Free (Expr f)

-- Create a binding to an literal
literal :: Value u -> JSM f (Binding f u)
literal = \case
 v@ValueNumber{} -> liftF (Literal v id)
 v@ValueString{} -> liftF (Literal v id)
 ValueArray a -> error "idk" -- liftF (Literal (ValueArray a) id)
 ValueNull a -> absurd a

plus' :: Binding f 'Number -> Binding f 'Number -> JSM f (Binding f 'Number)
plus' x y = liftF $ Plus x y id

interpret :: (forall f. JSM f (Binding f u)) -> Value u
interpret a = internalInterpret a

-- Not exported
internalInterpret :: JSM Evaluate (Binding Evaluate u) -> Value u
internalInterpret (Free (Plus (Binding (Evaluate (ValueNumber num1))) (Binding (Evaluate (ValueNumber num2))) f)) = do
  internalInterpret $ f $ Binding $ Evaluate $ ValueNumber (num1 + num2)
internalInterpret (Free (Literal a f)) = internalInterpret $ f (Binding $ Evaluate a)
internalInterpret (Pure (Binding (Evaluate u))) = u

example :: Value 'Number
example = interpret $ do
  x <- literal $ ValueNumber 2
  y <- literal $ ValueNumber 3
  xy <- plus' x y
  pure xy
