{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | A minimal heterogeneous list (record) type, indexed by a type-level
-- list of kinds. Vendored subset of @Topaz.Rec@ for GHC compatibility.
module JShark.Rec
  ( Rec (..)
  , (<:)
  , mapRec
  , mapAccumRec
  , recFold
  , recCodes
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

mapRec :: (forall x. f x -> g x) -> Rec f xs -> Rec g xs
mapRec _ RecNil = RecNil
mapRec t (RecCons x xs) = RecCons (t x) (mapRec t xs)

mapAccumRec ::
  (forall x. s -> f x -> (s, g x)) -> s -> Rec f xs -> (s, Rec g xs)
mapAccumRec _ s RecNil = (s, RecNil)
mapAccumRec t !s (RecCons x xs) =
  let
    (s1, x') = t s x
    (s2, xs') = mapAccumRec t s1 xs
   in
    (s2, RecCons x' xs')

recFold :: (forall x. a -> f x -> a) -> a -> Rec f xs -> a
recFold _ z RecNil = z
recFold t !z (RecCons x xs) = recFold t (t z x) xs

recCodes :: (forall x. s -> f x -> (s, a)) -> s -> Rec f xs -> (s, [a])
recCodes _ s RecNil = (s, [])
recCodes t !s (RecCons x xs) =
  let
    (s1, a) = t s x
    (s2, as) = recCodes t s1 xs
   in
    (s2, a : as)
