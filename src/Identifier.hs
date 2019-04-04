{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Identifier
  ( GenIdentifier
  , genIdentifier
  ) where

import Control.Monad.Trans.State.Strict
import Data.Int (Int64)

newtype GenIdentifier a = GenIdentifier (State Int64 a)
  deriving newtype (Functor,Applicative,Monad)

genIdentifier :: GenIdentifier Int64
genIdentifier = GenIdentifier $ do
  i <- get
  put $ (i + 1)
  pure i
