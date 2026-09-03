{-# LANGUAGE OverloadedStrings #-}

-- | PHOAS tree metadata for optimizer size/purity tracking.
module JShark.Compiler.Metadata
  ( Metadata (..)
  , optStep
  , optSmall
  )
where

-- | Tags step by two, keeping the optimizer on the even negatives.
-- Codegen's 'allocTag' owns the odd ones, so neither can name a binder the
-- other is counting.
optStep :: Int
optStep = 2

-- | Re-opt only small trees. A second walk of a @bindRec@ / do-chain
-- paint body is what hung todo-mvc and breakout.
optSmall :: Int
optSmall = 16

data Metadata = Metadata
  { mdSize :: {-# UNPACK #-} !Int
  , mdIsPure :: !Bool
  , mdIsCheap :: !Bool
  }

instance Semigroup Metadata where
  Metadata s1 p1 c1 <> Metadata s2 p2 c2 =
    Metadata (s1 + s2) (p1 && p2) (c1 && c2)

instance Monoid Metadata where
  mempty = Metadata 0 True True
