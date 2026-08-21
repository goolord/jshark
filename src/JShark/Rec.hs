{-# LANGUAGE
    DataKinds
  , ExplicitForAll
  , GADTs
  , KindSignatures
  , PolyKinds
  , TypeOperators
#-}
-- | A minimal heterogeneous list (record) type, indexed by a type-level
-- list of kinds. Vendored subset of @Topaz.Rec@ for GHC compatibility.
module JShark.Rec
  ( Rec(..)
  , (<:)
  ) where

import Data.Kind (Type)

data Rec :: (k -> Type) -> [k] -> Type where
  RecNil :: Rec f '[]
  RecCons :: f r -> Rec f rs -> Rec f (r ': rs)

infixr 7 <:

-- | Infix 'RecCons' with proper fixity.
(<:) :: forall k (f :: k -> Type) (r :: k) (rs :: [k]). f r -> Rec f rs -> Rec f (r ': rs)
(<:) = RecCons
