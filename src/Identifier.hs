{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Identifier
  ( Identifier(..)
  , GenIdentifier(..)
  , genIdentifier
  ) where

import Control.Monad.Trans.State (StateT(..),State)
import Control.Monad.State.Class (MonadState(..))

newtype Identifier = Identifier { getIdentifier :: Int64 }

newtype GenIdentifier a = GenIdentifier (State Identifier a)
  deriving newtype (Functor,Applicative,Monad)
  deriving newtype (MonadState Identifier)

genIdentifier :: GenIdentifier Identifier
genIdentifier = do
  i <- get
  put $ Identifier (getIdentifier i + 1)
  pure i
