{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language EmptyCase #-}
{-# language ExistentialQuantification #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeInType #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module Reboot
  ( Expr(..)
  , Value(..)
    -- Operators
  , let_
  , lambda
  , number
    -- Evaluation
  , evaluate
  , evaluateNumber
  , pretty
  ) where

-- This uses a higher-order PHOAS approach as described by
-- https://www.reddit.com/r/haskell/comments/85een6/sharing_from_phoas_multiple_interpreters_from_free/dvxhlba

import Data.Text (Text)
import Data.Functor.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Product (Product(Pair))
import Data.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as T

data Universe = Number | String | Function Universe Universe

data SingUniverse :: Universe -> Type where
  SingUniverseNumber :: SingUniverse 'Number
  SingUniverseString :: SingUniverse 'String
  SingUniverseFunction :: SingUniverse a -> SingUniverse b -> SingUniverse ('Function a b)

data Value :: Universe -> Type where
  ValueNumber :: Int -> Value 'Number
  ValueString :: Text -> Value 'String
  ValueFunction :: (Value u -> Value v) -> Value ('Function u v)

data Expr :: (Universe -> Type) -> Universe -> Type where
  Literal :: Value u -> Expr f u
  Plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
  Let :: Expr f u -> (f u -> Expr f v) -> Expr f v
  Lambda :: (f u -> Expr f v) -> Expr f ('Function u v)
  Apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
  Var :: f u -> Expr f u

data ExprF :: (Type -> Type -> Type) -> (Universe -> Type) -> Universe -> Type where
  LiteralF :: Value u -> ExprF g f u
  PlusF :: ExprF g f 'Number -> ExprF g f 'Number -> ExprF g f 'Number
  LetF :: ExprF g f u -> g (f u) (ExprF g f v) -> ExprF g f v
  LambdaF :: g (f u) (ExprF g f v) -> ExprF g f ('Function u v)
  ApplyF :: ExprF g f ('Function u v) -> ExprF g f u -> ExprF g f v
  VarF :: f u -> ExprF g f u

unNumber :: Value 'Number -> Int
unNumber (ValueNumber i) = i

unFunction :: Value ('Function u v) -> Value u -> Value v
unFunction (ValueFunction f) = f

let_ ::
     Expr f u
  -> (Expr f u -> Expr f v)
  -> Expr f v
let_ e f = Let e (f . Var)

lambda :: 
     (Expr f u -> Expr f v)
  -> Expr f ('Function u v)
lambda f = Lambda (f . Var)

number :: Int -> Expr f 'Number
number = Literal . ValueNumber

evaluateNumber :: (forall (f :: Universe -> Type). Expr f 'Number) -> Int
evaluateNumber e = unNumber (evaluate e)

evaluate :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Value u
evaluate = go where
  go :: forall v. Expr Value v -> Value v
  go (Literal v) = v
  go (Plus x y) = ValueNumber (unNumber (go x) + unNumber (go y))
  go (Var x) = x
  go (Let x g) = go (g (go x))
  go (Apply g x) = unFunction (go g) (go x)
  go (Lambda g) = ValueFunction (go . g)

pretty :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Text
pretty = getConst . go 0 where
  go :: forall v. Int -> Expr (Const Text) v -> Const Text v
  go !_ (Literal v) = case v of
    ValueNumber n -> Const $ T.pack (show n)
    ValueString t -> Const $ T.pack (show t)
    ValueFunction _ -> Const "<builtin>"
  go !n (Plus x y) = Const ("plus (" <> getConst (go n x) <> ") (" <> getConst (go n y) <> ")")
  go !_ (Var x) = x
  go !n (Lambda g) =
    let name = "x" <> T.pack (show n)
     in Const
        $  "λ"
        <> name
        <> " -> "
        <> getConst (go (n + 1) (g (Const name)))
  go !n (Apply g x) = Const ("(" <> getConst (go n g) <> ") (" <> getConst (go n x) <> ")")
  go !n (Let x g) =
    let name = "x" <> T.pack (show n)
     in Const
        $  "let "
        <> name
        <> " = {"
        <> getConst (go (n + 1) x)
        <> "} in {"
        <> getConst (go (n + 1) (g (Const name)))
        <> "}"

-- data Ref s a = Ref !Addr !(STRef s a)
-- 
-- testRefEquality :: STRef s a -> STRef s b -> Maybe (a :~: b)



-- newtype Detector :: (Universe -> Type) -> Universe -> Type where
--   Detector :: (f u -> _) -> Detector f u

-- eliminateUnusedBindings :: forall (f :: Universe -> Type) (u :: Universe).
--      (forall (g :: Universe -> Type). Expr g u)
--   -> Expr f u
-- eliminateUnusedBindings e = case go e of
--   Nothing -> e
--   Just r -> r
--   where
--   go :: forall (v :: Universe). Expr (Compose Maybe f) v -> Maybe (Expr f v)
--   go (Literal v) = pure (Literal v)
--   go (Var (Compose v)) = case v of
--     Just w -> Just (Var w)
--     Nothing -> Nothing
--   go (Let x g) = case go (g (Compose Nothing)) of
--     Nothing -> do
--       y <- go x
--       b <- go (g (Compose (Just _)))
--       Just (Let y h)
--     Just r -> Just r

-- data TupleRef :: Type -> Type -> Type -> Type where
--   TupleRef :: STRef s x -> y -> TupleRef s x y


identify :: ExprF (->) (Compose (STRef s) Proxy) u -> ST s (ExprF (,) (Compose (STRef s) Proxy) u)
identify (LiteralF v) = pure (LiteralF v)
identify (VarF v) = pure (VarF v)
identify (LetF x g) = do
  r <- newSTRef Proxy
  x' <- identify x
  g' <- identify (g (Compose r))
  pure (LetF x' (Compose r,g'))
identify (PlusF a b) = liftA2 PlusF (identify a) (identify b)
identify (ApplyF g a) = liftA2 ApplyF (identify g) (identify a)
identify (LambdaF g) = do
  r <- newSTRef Proxy
  g' <- identify (g (Compose r))
  pure (LambdaF (Compose r,g'))

data Together :: Type -> (Universe -> Type) -> Type where
  Together :: STRef s (Proxy u) -> f u -> Together s f

match :: forall (f :: Universe -> Type) (s :: Type) (u :: Universe).
  STRef s (Proxy u) -> [Together s f] -> f u
match !_ [] = error "match: implementation error in unidentify"
match !r (Together x v : xs) = if r == unsafeCoerce x
  then unsafeCoerce v
  else match r xs

unidentify :: forall (f :: Universe -> Type) (s :: Type) (u :: Universe).
  [Together s f]  -> ExprF (,) (Compose (STRef s) Proxy) u -> ExprF (->) f u
unidentify _ (LiteralF v) = LiteralF v
unidentify rs (VarF (Compose v)) = VarF (match v rs)
unidentify rs (LetF x (Compose ref,expr)) =
  LetF (unidentify rs x) (\z -> unidentify (Together ref z : rs) expr)
unidentify rs (PlusF a b) = PlusF (unidentify rs a) (unidentify rs b)
unidentify rs (ApplyF g x) = ApplyF (unidentify rs g) (unidentify rs x)
unidentify rs (LambdaF (Compose ref,expr)) =
  LambdaF (\z -> unidentify (Together ref z : rs) expr)


-- eliminateUnusedBindings :: forall (f :: Universe -> Type) (u :: Universe).
--      (forall (g :: Universe -> Type). Expr g u)
--   -> Expr f u
-- eliminateUnusedBindings e = case go e of
--   Nothing -> e
--   Just r -> r
--   where
--   go :: forall (v :: Universe). Expr (Compose Maybe f) v -> Maybe (f v -> Expr f v)
--   go (Let x g) = case go (g (Compose Nothing)) of
--     Nothing -> do
--       y <- go x
--       b <- go (g (Compose (Just y)))
--       Just (Let y b)
--     Just r -> Just r
