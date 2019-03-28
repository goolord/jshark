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
  , Expr(..)
  , Statement(..)
  , Action(..)
  , JSE, JSM, JSA

    -- * Construction
  , literal
  , bind

    -- * Interpretation
--  , interpret
  ) where

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
--  ValueOption :: Option u -> Value ('Option u)

--data Option u = Some (Value u) | None

data Function :: [Universe] -> Universe -> Type where
  Function ::

--Function args output

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

deriving stock instance Functor (Expr f)

type JSE = Free (Expr f)

-- Create a binding to an literal
literal :: Value u -> JSC f (Binding f u)
literal = \case
  v@ValueNumber{} -> liftF (Literal v id)
  v@ValueString{} -> liftF (Literal v id)
  v@ValueNull{} -> liftF (Literal v id)
  v@ValueArray{} -> liftF (Literal v id)

-- | The type of statements, which are not necessarily pure computations.
data Statement (f :: Universe -> Type) n where
  Bind ::
      Value u
   -> (Binding f u -> n)
   -> Statement f n
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

type JSM f = Free (Statement f)

data Action f n where
  EAction :: Expr f n -> Action f n
  SAction :: Statement f n -> Action f n

deriving stock instance Functor (Action f)

type JSA f = Free (Action f)

-- | Create a binding to a not-necessarily pure variable
bind :: Value u -> JSM f (Binding f u)
bind = \case
  v@ValueNumber{} -> liftF (Bind v id)
  v@ValueString{} -> liftF (Bind v id)
  v@ValueNull{} -> liftF (Bind v id)
  v@ValueArray{} -> liftF (Bind v id)


