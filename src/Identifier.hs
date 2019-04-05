{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Identifier
  ( GenIdentifier
  , evalGenIdentifier
  ) where

import Control.Monad.Trans.State.Strict
import Data.Int (Int64)

type GenIdentifier a = State Int a

evalGenIdentifier :: Int -> GenIdentifier a -> a
evalGenIdentifier i s = evalState s i
