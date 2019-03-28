{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language ExistentialQuantification #-}
{-# language StandaloneDeriving #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Javascript 
  ( -- * Types
    Universe(..)
  , Value(..)
  , Strategy(..)
  , Action(..)
    -- * Construction
  , literal
    -- * Interpretation
--  , interpret
  ) where

import Data.Kind
import Data.STRef
import Data.Text (Text)
import Data.Int
import Control.Monad.Free

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
type family Bound (t :: Strategy) (u :: Universe) :: Type where
  Bound 'Generate _ = Int
  Bound 'Evaluate u = Value u

newtype Binding :: Type -> Strategy -> Universe -> Type where
  Binding :: STRef s (Bound t u) -> Binding s t u

-- Are we evaluating or generating code
data Strategy = Evaluate | Generate

data Action s (t :: Strategy) n where
  Literal :: ()
    => Value u
    -> (Binding s t u -> n)
    -> Action s t n
  Plus :: ()
    => Binding s t 'Number
    -> Binding s t 'Number
    -> (Binding s t 'Number -> n)
    -> Action s t n
  Log :: ()
    => Binding s t 'String
    -> n
    -> Action s t n
  Foreach :: ()
    => Binding s t 'Number
    -> Binding s t ('Array u)
    -> Binding s t u
    -> Free (Action s t) ()
    -> n
    -> Action s t n
  -- ^ Foreach is an inherently imperative construct. Consequently, it does not
  --   return anything.

{-
  -- | forall (rs :: [Universe]) (res :: Universe). Declare (Rec (Binding s t) rs -> Free (Action s t) (Binding s t res)) (Function rs res -> n)
    -- ^ Not totally sure if Declare should have the function arrow in its first arg.
  -- | forall (rs :: [Universe]) (res :: Universe). Call (Function rs res) (Rec Value rs) (Value res -> n)
-}

deriving instance Functor (Action s t)

type JSM s n = Free (Action s n)

-- Create a binding to an literal
literal :: Value u -> JSM s n (Binding s t u)
literal = \case
  ValueNumber i -> liftF (Literal (ValueNumber i) undefined)
  ValueString s -> liftF (Literal (ValueString s) undefined)
  ValueArray _ -> error "idk"
 
--interpret :: (forall s t. Free (Action s t) (Binding s t u)) -> Value u
--interpret a = internalInterpret a

-- Not exported
--internalInterpret :: (forall s t. Free (Action s 'Evaluate) (Binding s t u)) -> Value u
--internalInterpret = _ -- write me
