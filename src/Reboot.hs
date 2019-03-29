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

import Data.Functor.Const (Const(..))
import qualified Data.Text as T

data Universe
  = Number
  | String
  | Function Universe Universe

data Value :: Universe -> Type where
  ValueNumber :: Double -> Value 'Number
  ValueString :: Text -> Value 'String
  ValueFunction :: (Value u -> Value v) -> Value ('Function u v)

data Expr :: (Universe -> Type) -> Universe -> Type where
  Literal :: Value u -> Expr f u
  Plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
  Let :: Expr f u -> (f u -> Expr f v) -> Expr f v
  Lambda :: (f u -> Expr f v) -> Expr f ('Function u v)
  Apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
  Var :: f u -> Expr f u

unNumber :: Value 'Number -> Double
unNumber (ValueNumber d) = d

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

number :: Double -> Expr f 'Number
number = Literal . ValueNumber

evaluateNumber :: (forall (f :: Universe -> Type). Expr f 'Number) -> Double
evaluateNumber e = unNumber (evaluate e)

evaluate :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Value u
evaluate = go where
  go :: forall v. Expr Value v -> Value v
  go = \case
    Literal v -> v
    Plus x y -> ValueNumber (unNumber (go x) + unNumber (go y))
    Var x -> x
    Let x g -> go (g (go x))
    Apply g x -> unFunction (go g) (go x)
    Lambda g -> ValueFunction (go . g)

pretty :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Text
pretty = getConst . go 0 where
  go :: forall v. Int -> Expr (Const Text) v -> Const Text v
  go !n = \case
    Literal v -> case v of
      ValueNumber d -> Const $ T.pack (show d)
      ValueString t -> Const $ T.pack (show t)
      ValueFunction _ -> Const $ T.pack "<function>"
    Plus x y -> Const ("plus (" <> getConst (go n x) <> ") (" <> getConst (go n y) <> ")")
    Var x -> x
    Lambda g ->
      let name = "x" <> T.pack (show n)
       in Const  
          $  "λ"
          <> name
          <> " -> "
          <> getConst (go (n + 1) (g (Const name)))
    Apply g x -> Const ("(" <> getConst (go n g) <> ") (" <> getConst (go n x) <> ")")
    Let x g ->
      let name = "x" <> T.pack (show n)
       in Const
          $  "let "
          <> name
          <> " = {"
          <> getConst (go (n + 1) x)
          <> "} in {"
          <> getConst (go (n + 1) (g (Const name)))
          <> "}"

