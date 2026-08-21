{-# LANGUAGE
    BangPatterns
  , DataKinds
  , GADTs
  , LambdaCase
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
#-}
-- | Finished form of the original unused-binding experiment.
--
-- First-order fragment of the pure tree, in the same spirit as Kmett's
-- @Rec p a b = Place b | Roll (p a (Rec p a b))@: 'LetF'/'LambdaF' store
-- binders as @g (f u) (ExprF g f v)@ (@(->)@ at the surface, pairs after
-- identify). The production pipeline walks full 'Expr'/'Effect' in
-- 'JShark.optimize' instead.
--
-- The 2019 code allocated an 'STRef' per binder and round-tripped through
-- 'identify'/'unidentify', but never counted uses or dropped a 'LetF'.
-- This module keeps the same shape (identify → count → unidentify) with
-- two intentional upgrades:
--
-- * Binder identity is a unique 'Int' (not 'STRef' pointer equality), so
--   matching no longer compares refs via 'unsafeCoerce'.
-- * A bottom-up pass drops dead 'LetF's and only counts uses in the live
--   body, so eliminating an inner let can free an outer binder.
--
-- Recovering a typed binder from the environment still uses
-- 'unsafeCoerce' (standard PHOAS unembedding). Full-program codegen uses
-- 'JShark.optimize' on 'Expr' instead; this fragment is only
-- Literal/Plus/Let/Lambda/Apply/Var.
--
-- 'removeUnusedBindings' expects a /closed/ binder-polymorphic tree (no
-- free 'VarF'). 'toExprF' only accepts 'Expr' ('Const' 'Int') and converts
-- each binder once (no deferred re-entry after a coerce).
module JShark.ExprF
  ( ExprF(..)
  , toExprF
  , fromExprF
  , removeUnusedBindings
  , removeUnusedBindingsExpr
  ) where

import Control.Monad.ST (ST, runST)
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)
import JShark.Types
import Unsafe.Coerce (unsafeCoerce)

-- | Convert a 'Const Int'-tagged 'Expr' into the 'ExprF' fragment.
-- 'Nothing' if any constructor outside Literal/Plus/Let/Lambda/Apply/Var
-- appears. Each 'Let'/'Lambda' body is converted once under a fresh tag,
-- then rebound — the resulting 'ExprF' closures do not re-enter 'Expr'.
toExprF :: Expr (Const Int) u -> Maybe (ExprF (->) (Const Int) u)
toExprF e = case go (-2) e of
  Nothing -> Nothing
  Just (_, e') -> Just e'
  where
  go :: Int -> Expr (Const Int) v -> Maybe (Int, ExprF (->) (Const Int) v)
  go t0 = \case
    Literal v -> Just (t0, LiteralF v)
    Plus a b -> do
      (t1, a') <- go t0 a
      (t2, b') <- go t1 b
      Just (t2, PlusF a' b')
    Apply f x -> do
      (t1, f') <- go t0 f
      (t2, x') <- go t1 x
      Just (t2, ApplyF f' x')
    Var v -> Just (t0, VarF v)
    Let x g -> do
      (t1, x') <- go t0 x
      let tag = t1
      (t2, body) <- go (t1 - 1) (g (Const tag))
      Just (t2, LetF x' (\v -> substTag tag v body))
    Lambda g -> do
      let tag = t0
      (t1, body) <- go (t0 - 1) (g (Const tag))
      Just (t1, LambdaF (\v -> substTag tag v body))
    _ -> Nothing

-- Replace 'VarF' ('Const' tag) with 'VarF' v. Used to rebind a body that
-- was converted under a concrete tag into a real PHOAS lambda.
substTag :: forall (u :: Universe) (v :: Universe).
  Int -> Const Int u -> ExprF (->) (Const Int) v -> ExprF (->) (Const Int) v
substTag tag v = go
  where
  go :: ExprF (->) (Const Int) w -> ExprF (->) (Const Int) w
  go = \case
    LiteralF x -> LiteralF x
    PlusF a b -> PlusF (go a) (go b)
    ApplyF f x -> ApplyF (go f) (go x)
    VarF (Const i)
      | i == tag -> VarF (unsafeCoerce v)
      | otherwise -> VarF (Const i)
    LetF x g -> LetF (go x) (go . g)
    LambdaF g -> LambdaF (go . g)

fromExprF :: ExprF (->) f u -> Expr f u
fromExprF = \case
  LiteralF v -> Literal v
  PlusF a b -> Plus (fromExprF a) (fromExprF b)
  LetF x g -> Let (fromExprF x) (fromExprF . g)
  LambdaF g -> Lambda (fromExprF . g)
  ApplyF f x -> Apply (fromExprF f) (fromExprF x)
  VarF v -> Var v

-- | Drop unused 'LetF' bindings in a /closed/ binder-polymorphic 'ExprF'
-- (no free 'VarF'). Lambdas are kept even when the parameter is unused.
removeUnusedBindings ::
     (forall (g :: Universe -> Type). ExprF (->) g u)
  -> ExprF (->) f u
removeUnusedBindings e0 = runST $ do
  supply <- newSTRef (0 :: Int)
  e1 <- identify supply e0
  pure (unidentify [] (dce e1))

-- | 'toExprF' then dead-let elimination on the 'Const Int' fragment.
-- 'Nothing' if any unsupported 'Expr' constructor is present.
removeUnusedBindingsExpr ::
     forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Maybe (Expr (Const Int) u)
removeUnusedBindingsExpr e = do
  let eC :: Expr (Const Int) u
      eC = e
  eF <- toExprF eC
  pure (fromExprF (dceConst eF))

-- Dead-let elimination on a 'Const Int'-tagged 'ExprF' (used by
-- 'removeUnusedBindingsExpr'). Same bottom-up rule as 'dce', no coerce.
dceConst :: ExprF (->) (Const Int) u -> ExprF (->) (Const Int) u
dceConst e0 = snd (go (-2) e0)
  where
  go :: Int -> ExprF (->) (Const Int) v -> (Int, ExprF (->) (Const Int) v)
  go t0 = \case
    LiteralF v -> (t0, LiteralF v)
    VarF v -> (t0, VarF v)
    PlusF a b ->
      let (t1, a') = go t0 a
          (t2, b') = go t1 b
       in (t2, PlusF a' b')
    ApplyF f x ->
      let (t1, f') = go t0 f
          (t2, x') = go t1 x
       in (t2, ApplyF f' x')
    LetF x body ->
      let (t1, x') = go t0 x
          tag = t1
          (t2, body') = go (t1 - 1) (body (Const tag))
          n = countTag tag body'
       in if n == 0
            then (t2, body')
            else (t2, LetF x' (\v -> substTag tag v body'))
    LambdaF body ->
      let tag = t0
          (t1, body') = go (t0 - 1) (body (Const tag))
       in (t1, LambdaF (\v -> substTag tag v body'))

countTag :: Int -> ExprF (->) (Const Int) u -> Int
countTag tag = go
  where
  go :: ExprF (->) (Const Int) v -> Int
  go = \case
    LiteralF _ -> 0
    VarF (Const i) -> if i == tag then 1 else 0
    PlusF a b -> go a + go b
    ApplyF f x -> go f + go x
    -- Body is counted under a fresh dummy so this binder's tag is not
    -- confused with an inner binder that happens to reuse ids (go always
    -- allocates decreasing tags, so siblings are distinct; nested lets
    -- in the already-substituted body' use their own tags).
    LetF x g -> go x + go (g (Const (minBound :: Int)))
    LambdaF g -> go (g (Const (minBound :: Int)))

-- identify / dce (polymorphic closed trees) -------------------------------

-- | Binder installed by identify (unique id; replaces the original STRef).
newtype Ann (u :: Universe) = Ann Int

-- | Frozen binder id after DCE.
newtype Frozen (u :: Universe) = Frozen Int

identify ::
     STRef s Int
  -> ExprF (->) Ann u
  -> ST s (ExprF (,) Ann u)
identify supply = \case
  LiteralF v -> pure (LiteralF v)
  VarF v -> pure (VarF v)
  PlusF a b -> do
    a' <- identify supply a
    b' <- identify supply b
    pure (PlusF a' b')
  ApplyF f x -> do
    f' <- identify supply f
    x' <- identify supply x
    pure (ApplyF f' x')
  LetF x body -> do
    ann <- newAnn supply
    x' <- identify supply x
    body' <- identify supply (body ann)
    pure (LetF x' (ann, body'))
  LambdaF body -> do
    ann <- newAnn supply
    body' <- identify supply (body ann)
    pure (LambdaF (ann, body'))

newAnn :: STRef s Int -> ST s (Ann u)
newAnn supply = do
  n <- readSTRef supply
  modifySTRef' supply (+ 1)
  pure (Ann n)

-- Bottom-up: DCE the body first, then count this binder only in the live
-- remainder (so a use that lived solely in a dropped inner let goes away).
--
-- 'countId' walks the live body once per 'LetF', so a chain of n lets is
-- O(n²). Acceptable for this toy fragment; do not grow the fragment without
-- switching to a one-pass use map.
dce :: ExprF (,) Ann u -> ExprF (,) Frozen u
dce = \case
  LiteralF v -> LiteralF v
  VarF (Ann i) -> VarF (Frozen i)
  PlusF a b -> PlusF (dce a) (dce b)
  ApplyF f x -> ApplyF (dce f) (dce x)
  LetF x (Ann i, body) ->
    let body' = dce body
     in if countId i body' == 0
          then body'
          else LetF (dce x) (Frozen i, body')
  LambdaF (Ann i, body) ->
    LambdaF (Frozen i, dce body)

countId :: Int -> ExprF (,) Frozen u -> Int
countId target = go
  where
  go :: ExprF (,) Frozen v -> Int
  go = \case
    LiteralF _ -> 0
    VarF (Frozen i) -> if i == target then 1 else 0
    PlusF a b -> go a + go b
    ApplyF f x -> go f + go x
    LetF x (_, body) -> go x + go body
    LambdaF (_, body) -> go body

-- unidentify --------------------------------------------------------------

data Together f where
  Together :: {-# UNPACK #-} !Int -> f u -> Together f

unidentify :: [Together f] -> ExprF (,) Frozen u -> ExprF (->) f u
unidentify env = \case
  LiteralF v -> LiteralF v
  VarF (Frozen i) -> VarF (match i env)
  PlusF a b -> PlusF (unidentify env a) (unidentify env b)
  ApplyF f x -> ApplyF (unidentify env f) (unidentify env x)
  LetF x (Frozen i, body) ->
    LetF (unidentify env x) (\z -> unidentify (Together i z : env) body)
  LambdaF (Frozen i, body) ->
    LambdaF (\z -> unidentify (Together i z : env) body)

match :: Int -> [Together f] -> f u
match !_ [] = error "JShark.ExprF.unidentify: unbound variable (tree was not closed)"
match !i (Together j v : xs)
  | i == j = unsafeCoerce v
  | otherwise = match i xs
