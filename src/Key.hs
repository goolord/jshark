{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}

module Key
  ( KeyM
  , Key
  , newKey
  , runKeyM
  ) where

import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)

-- Should these use Int or Integer?
-- Not sure we care about unboxing here.
newtype KeyM s a = KeyM { getKeyM :: IORef Integer -> a }
  deriving newtype (Functor,Applicative,Monad)

newtype Key s a = Key { getKey :: Integer }

instance TestEquality (Key s) where
  testEquality (Key i) (Key j)
    | i == j = Just (unsafeCoerce Refl)
    | otherwise = Nothing

newKey :: KeyM s (Key s a)
newKey = KeyM $ \r -> unsafePerformIO $ do
  k <- atomicModifyIORef' r (\n -> (n + 1, Key n))
  pure k

runKeyM :: (forall s. KeyM s a) -> a
runKeyM (KeyM f) = f (unsafePerformIO $ newIORef 0)
