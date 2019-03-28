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
  , Action(..)
    -- * Construction
  , literal
    -- * Interpretation
--  , interpret
  ) where

import Key

data Universe = Number | String | Array Universe

data Value :: Universe -> Type where
  ValueNumber :: Int64 -> Value 'Number
  ValueString :: Text -> Value 'String
  ValueArray :: [Value u] -> Value ('Array u)

--data Rec :: (Universe -> Type) -> [Universe] -> Type where
--  RecNil :: Rec f '[]
--  RecCons :: Rec f r -> Rec f rs -> Rec f (r ': rs)

-- data Function :: Type -> [Universe] -> Universe -> Type where
  -- Function :: STRef s _ -> Function s rs res

-- When generating code, we ignore the type of the value
-- since we are just going to produce a monotonically
-- increasing identifier. When evaluating, we use a haskell
-- value whose type corresponds to that of the binding in
-- the EDSL.
-- type family Bound (t :: Strategy) (u :: Universe) :: (b :: Type) | b -> t where
  -- Bound 'Generate _ = Integer
  -- Bound 'Evaluate u = Value u

newtype Binding :: (Universe -> Type) -> Universe -> Type where
  Binding :: f u -> Binding f u

-- Are we evaluating or generating code
-- data Strategy = Evaluate | Generate

data Action (f :: Universe -> Type) n where
  Literal ::
       Value u
    -> (Binding f u -> n)
    -> Action f n
  Plus ::
       Binding f 'Number
    -> Binding f 'Number
    -> (Binding f 'Number -> n)
    -> Action f n
  Log ::
       Binding f 'String
    -> n
    -> Action f n
  Foreach ::
       Binding f 'Number
    -> Binding f ('Array u)
    -> Binding f u
    -> (Binding f u  -> Free (Action f) ())
    -> n
    -> Action f n
  -- ^ Foreach is an inherently imperative construct. Consequently, it does not
  --   return anything.

  -- | forall (rs :: [Universe]) (res :: Universe). Declare (Rec (Binding s t) rs -> Free (Action s t) (Binding s t res)) (Function rs res -> n)
    -- ^ Not totally sure if Declare should have the function arrow in its first arg.
  -- | forall (rs :: [Universe]) (res :: Universe). Call (Function rs res) (Rec Value rs) (Value res -> n)

deriving stock instance Functor (Action f)

type JSM f = Free (Action f)

-- Create a binding to an literal
literal :: Value u -> JSM f (Binding f u)
literal = \case
  v@ValueNumber{} -> liftF (Literal v id)
  v@ValueString{} -> liftF (Literal v id)
  ValueArray a -> error "idk" -- liftF (Literal (ValueArray a) id)

--interpret :: (forall s t. Free (Action s t) (Binding s t u)) -> Value u
--interpret a = internalInterpret a

-- Not exported
--internalInterpret :: (forall s t. Free (Action s 'Evaluate) (Binding s t u)) -> Value u
--internalInterpret = _ -- write me
