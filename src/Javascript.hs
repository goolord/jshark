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

module Javascript 
  ( -- * Types
    Universe(..)
  , Value(..)
  -- , Strategy(..)
--  , Statement(..)
    -- * Construction
--  , literal
    -- * Interpretation
--  , interpret
  ) where

import Key

data Universe
  = Null -- ^ null
  | Number -- ^ IEEE 754 floating point double precision
  | String -- ^ javascript strings
  | Array Universe -- ^ javascript arrays
--  | Option Universe -- ^ option type. does not exist in source js.
--  | Result Universe Universe -- ^ result type. does not exist in source js.

data Value :: Universe -> Type where
  ValueNull :: Void -> Value 'Null -- ^ absurd
  ValueNumber :: Int64 -> Value 'Number
  ValueString :: Text -> Value 'String
  ValueArray :: [Value u] -> Value ('Array u)
--  ValueOption ::
--  ValueResult ::

-- data Function :: Type -> [Universe] -> Universe -> Type where
  -- Function :: STRef s _ -> Function s rs res

newtype Binding :: (Universe -> Type) -> Universe -> Type where
  Binding :: f u -> Binding f u

-- | Code Evaluation.
newtype Evaluate :: Universe -> Type where
  Evaluate :: Value u -> Evaluate u

-- | Code Generation.
newtype Generate :: Universe -> Type where
  Generate :: Int64 -> Generate u

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
  Sin :: -- ^ IEEE 754 double-precision sin.
      Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  Cos :: -- ^ IEEE 754 double-precision cos.
      Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  Tan :: -- ^ IEEE 754 double-precision tan.
      Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  Exp :: -- ^ IEEE 754 double-precision exp.
      Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  ExpM1 :: -- ^ IEEE 754 double-precision expm1.
      Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  LogE :: -- ^ IEEE 754 double-precision log.
      Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  LogP1 :: -- ^ IEEE 754 double-precision logp1.
      Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  Random :: -- ^ IEEE 754 double-precision random.
      Expr f n

data Statement (f :: Universe -> Type) n where
  Log ::
       Binding f 'String
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

--deriving stock instance Functor (Statement f)

--type JSM f = Free (Statement f)

-- Create a binding to an literal
--literal :: Value u -> JSM f (Binding f u)
--literal = \case
--  v@ValueNumber{} -> liftF (Literal v id)
--  v@ValueString{} -> liftF (Literal v id)
--  ValueArray a -> error "idk" -- liftF (Literal (ValueArray a) id)

--interpret :: (forall s t. Free (Statement s t) (Binding s t u)) -> Value u
--interpret a = internalInterpret a

-- Not exported
--internalInterpret :: (forall s t. Free (Statement s 'Evaluate) (Binding s t u)) -> Value u
--internalInterpret = _ -- write me
