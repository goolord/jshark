{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Heterogeneous argument lists for 'CallMethod' and FFI ('arg' / '(<:)').
module JShark.Api.Rec
  ( Rec (..)
  , (<:)
  )
where

import Data.Kind (Type)

data Rec :: (k -> Type) -> [k] -> Type where
  RecNil :: Rec f '[]
  RecCons :: f r -> Rec f rs -> Rec f (r ': rs)

infixr 7 <:

-- | Infix 'RecCons' with proper fixity.
(<:) ::
  forall k (f :: k -> Type) (r :: k) (rs :: [k]).
  f r -> Rec f rs -> Rec f (r ': rs)
(<:) = RecCons
