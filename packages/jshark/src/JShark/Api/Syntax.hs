{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | The 'EffectSyntax' do-notation bridge: the KeyMonad-style monadic syntax
-- for building 'Effect' terms. Split out of 'JShark.Api.Types' so the raw
-- AST and the construction monad are separate responsibilities.
--
-- The convention, so a reader can tell the three apart at a glance:
--
-- * 'bindExpr' binds once and yields an 'Expr' (\"@x <- bindExpr e@\"): the
--   effect runs once, and the reified expression can be used any number of
--   times (a JS @const@).
-- * 'toSyntax' yields the raw PHOAS binder @f u@ for a single use. This is
--   the ergonomic form when the value exists only inside the block.
-- * 'toSyntax_' runs an effect for its side effect and yields Haskell @()@;
--   use it for discarded statements. @(*>)@ / 'seqSyntax' sequence two
--   effects and discard the first result.
--
-- A raw repeatable computation is therefore an 'Effect' passed to
-- 'bindExpr'; a discarded one is 'toSyntax_'; and a bind-once value is
-- 'bindExpr'. Use 'expr' to lift a pure 'Expr' into an 'Effect' when it
-- needs to be sequenced.
module JShark.Api.Syntax
  ( EffectSyntax (..)
  , toSyntax
  , toSyntax_
  , bindExpr
  , fromSyntax
  , seqSyntax
  , (>>)
  )
where

import Control.Monad (ap)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import JShark.Api.Caller (callerBinderHint)
import JShark.Api.Types
  ( Effect (..)
  , Expr (Var)
  , Universe
  )
import Prelude hiding ((>>))

-- Monadic interface to expressions based on KeyMonad
-- (https://people.seas.harvard.edu/~pbuiras/publications/KeyMonadHaskell2016.pdf).

-- Analogous to RelativeMSyntax in section 3.3.

-- | A untyped monadic syntax for building 'Effect' terms with do-notation,
-- based on the KeyMonad encoding. Interpret it with 'fromSyntax'.
data EffectSyntax :: (Universe -> Type) -> Type -> Type where
  -- | A pure value.
  EffectSyntaxPure :: a -> EffectSyntax v a
  -- | Run an effect and bind its result.
  EffectSyntaxUnpure ::
    Maybe Text
    -> Effect v a
    -> (v a -> EffectSyntax v b)
    -> EffectSyntax v b
  EffectSyntaxThen ::
    Effect v u
    -> EffectSyntax v b
    -> EffectSyntax v b
    -- ^ Sequencing without bind codegen ('*>' / '>>').

deriving instance Functor (EffectSyntax f)

instance Applicative (EffectSyntax v) where
  pure = EffectSyntaxPure
  (<*>) = ap
  EffectSyntaxPure _ *> b = b
  -- Bind the effect, discard the binder, then run the continuation
  -- @g@. Dropping @g@ would silently lose every effect it sequences.
  EffectSyntaxUnpure hint m g *> b =
    EffectSyntaxUnpure hint m (\x -> g x *> b)
  EffectSyntaxThen m g *> b = EffectSyntaxThen m (g *> b)

-- Analogous to the Monad instance for RelativeMSyntax in section 3.3.
-- GHC 9.14 dropped `Monad.(>>)`; do-notation sequences with `Applicative.(*>)`,
-- which is ThenE here. The exported `(>>)` is the same operator.
instance Monad (EffectSyntax f) where
  (>>=) = bindEffectSyntax

bindEffectSyntax ::
  HasCallStack =>
  EffectSyntax f a
  -> (a -> EffectSyntax f b)
  -> EffectSyntax f b
bindEffectSyntax (EffectSyntaxPure x) g = g x
bindEffectSyntax (EffectSyntaxUnpure hint m g) h =
  EffectSyntaxUnpure (maybe callerBinderHint Just hint) m (\x -> g x >>= h)
bindEffectSyntax (EffectSyntaxThen m g) h = EffectSyntaxThen m (g >>= h)

-- | Sequence effects without bind codegen ('*>' / '>>').
seqSyntax :: EffectSyntax f a -> EffectSyntax f b -> EffectSyntax f b
seqSyntax = (*>)

infixr 1 >>

-- | Alias for '(>*>)': sequence effects, discarding the first result.
(>>) :: EffectSyntax f a -> EffectSyntax f b -> EffectSyntax f b
(>>) = (*>)

-- | Lift a single 'Effect' into 'EffectSyntax', yielding its PHOAS binder.
toSyntax :: HasCallStack => Effect f v -> EffectSyntax f (f v)
toSyntax m = EffectSyntaxUnpure callerBinderHint m EffectSyntaxPure

-- | Like 'toSyntax' but discards the effect's result.
toSyntax_ :: HasCallStack => Effect f v -> EffectSyntax f ()
toSyntax_ m = EffectSyntaxUnpure callerBinderHint m (const (EffectSyntaxPure ()))

-- | Bind an effect and reify the result as an 'Expr'.
bindExpr :: HasCallStack => Effect f u -> EffectSyntax f (Expr f u)
bindExpr m = EffectSyntaxUnpure callerBinderHint m (EffectSyntaxPure . Var)

-- | Interpret an 'EffectSyntax' term as an 'Effect'.
fromSyntax :: EffectSyntax f (f v) -> Effect f v
fromSyntax (EffectSyntaxPure x) = Lift (Var x)
fromSyntax (EffectSyntaxThen m b) = ThenE m (fromSyntax b)
fromSyntax (EffectSyntaxUnpure hint m g) = Bind hint m (fromSyntax . g)
