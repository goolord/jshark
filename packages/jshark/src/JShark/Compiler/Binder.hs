{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}

-- | Optimizer / codegen binder tag.
module JShark.Compiler.Binder
  ( Stamp (..)
  , pattern Name
  , stampId
  , nestedDummyId
  , nestedDummy
  , strictFoldMap
  )
where

import JShark.Api.Types

-- | A codegen\/optimizer binder is just an 'Int' tag (phantom-indexed by
-- universe so 'JShark.Api.Types.Var' stays well-typed). 'Stamp' used to
-- carry 'Embed'\/'EmbedEff' PHOAS inlining holes; those died with the
-- PHOAS optimizer, so tags are plain integers again.
data Stamp (u :: Universe) where
  Stamp :: Int -> Stamp u

-- | Readable alias for binder tags.
pattern Name :: Int -> Stamp u
pattern Name i = Stamp i

stampId :: Stamp u -> Int
stampId (Stamp i) = i

nestedDummyId :: Int
nestedDummyId = minBound

nestedDummy :: Stamp u
nestedDummy = Name nestedDummyId

-- | Strict left fold. Lazy 'foldMap' thunks IntMap unions on IR metadata.
strictFoldMap :: Monoid m => (a -> m) -> [a] -> m
strictFoldMap f xs = foldl' (\ !acc x -> acc <> f x) mempty xs
{-# INLINE strictFoldMap #-}
