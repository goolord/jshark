{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}

-- | Optimizer / codegen binder tag.
module JShark.Compiler.Binder
  ( Stamp
  , pattern Stamp
  , pattern Name
  , stampId
  , strictFoldMap
  )
where

import Data.Functor.Const (Const (..), getConst)

-- | A codegen\/optimizer binder is a 'Const' 'Int' over the phantom
-- universe index, so 'JShark.Api.Types.Var' stays well-typed. 'Const''s
-- second parameter is kind-polymorphic, so the partially applied
-- 'Const Int' fills the @Universe -> Type@ functor role of the PHOAS
-- types. 'Stamp' used to carry 'Embed'\/'EmbedEff' inlining holes; those
-- died with the PHOAS optimizer, leaving the plain tag 'Const' represents.
type Stamp = Const Int

-- | Pattern form of a binder tag.
pattern Stamp :: Int -> Stamp u
pattern Stamp i = Const i

{-# COMPLETE Stamp #-}

-- | Readable alias for binder tags.
pattern Name :: Int -> Stamp u
pattern Name i = Stamp i

stampId :: Stamp u -> Int
stampId = getConst

-- | Strict left fold. Lazy 'foldMap' thunks IntMap unions on IR metadata.
strictFoldMap :: Monoid m => (a -> m) -> [a] -> m
strictFoldMap f xs = foldl' (\ !acc x -> acc <> f x) mempty xs
{-# INLINE strictFoldMap #-}
