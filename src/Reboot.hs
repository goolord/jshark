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

{-# options_ghc -w #-}

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

import Data.Tuple (snd)
import Data.List ((++))
import Data.Foldable (for_)
import Numeric (showFFloat)

import Data.Functor.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Product (Product(Pair))
import Data.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Pretty.Printer
import qualified Data.Text as T
import qualified Data.List as List

data Universe
  = Number
  | String
  | Function Universe Universe

data SingUniverse :: Universe -> Type where
  SingUniverseNumber :: SingUniverse 'Number
  SingUniverseString :: SingUniverse 'String
  SingUniverseFunction :: SingUniverse a -> SingUniverse b -> SingUniverse ('Function a b)

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

data ExprF :: (Type -> Type -> Type) -> (Universe -> Type) -> Universe -> Type where
  LiteralF :: Value u -> ExprF g f u
  PlusF :: ExprF g f 'Number -> ExprF g f 'Number -> ExprF g f 'Number
  LetF :: ExprF g f u -> g (f u) (ExprF g f v) -> ExprF g f v
  LambdaF :: g (f u) (ExprF g f v) -> ExprF g f ('Function u v)
  ApplyF :: ExprF g f ('Function u v) -> ExprF g f u -> ExprF g f v
  VarF :: f u -> ExprF g f u

data Statement :: (Universe -> Type) -> Universe -> Type where
  SLiteral :: Value u -> Statement f u
  SFFI :: Text -> Statement f u

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

data Optimization 
  = ConstantFolding
  | UnusedBindings

prettyJS :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> String
prettyJS = getConst . go 0 where
  go :: forall v. Int -> Expr (Const String) v -> Const String v
  go !n = \case
    Literal v -> case v of
      ValueNumber d -> Const (show d)
      ValueString t -> Const (show t)
      ValueFunction _ -> Const "<function>"
    Plus x y -> Const ("plus (" <> getConst (go n x) <> ") (" <> getConst (go n y) <> ")")
    Var x -> x
    Lambda g ->
      let name = "x" <> (show n)
       in Const  
          $  "λ"
          <> name
          <> " -> "
          <> getConst (go (n + 1) (g (Const name)))
    Apply g x -> Const ("(" <> getConst (go n g) <> ") (" <> getConst (go n x) <> ")")
    Let x g ->
      let name = "x" <> (show n)
       in Const
          $  "let "
          <> name
          <> " = {"
          <> getConst (go (n + 1) x)
          <> "} in {"
          <> getConst (go (n + 1) (g (Const name)))
          <> "}"

data Computation = Computation JSExpression [JSStatement]
  deriving (Show)

printComputation :: Computation -> IO ()
printComputation (Computation e ss) = do
  putStrLn $ renderToString $ JSAstProgram (List.reverse (JSExpressionStatement e JSSemiAuto : ss)) JSAnnotSpace

simple :: [JSStatement] -> JSExpression -> Computation
simple ss e = Computation e ss

convertAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Computation
convertAST x = snd (go 0 [] x)
  where 
  go :: forall v. Int -> [JSStatement] -> Expr (Const Int) v 
     -> (Int,Computation)
  go !n !ss = \case
    Literal v -> case v of
      ValueNumber d -> (n,simple ss $ JSLiteral JSAnnotSpace (showFFloat Nothing d ""))
      ValueString t -> (n,simple ss $ JSLiteral JSAnnotSpace (show t))
      ValueFunction _ -> (n,simple ss $ JSLiteral JSAnnotSpace "<function>")
    Plus x y ->
      let (m,Computation exprX rs) = go n ss x
          (p,Computation exprY ts) = go m rs y
       in (p,Computation (JSExpressionBinary exprX (JSBinOpPlus JSAnnotSpace) exprY) ts)
    Var (Const v) -> (n,simple ss $ JSIdentifier JSAnnotSpace ('n':(show v)))
    Let e g ->
      let (m,Computation exprE rs) = go n ss e
          ts = JSConstant JSAnnotSpace (JSLOne $ JSAssignExpression (JSIdentifier JSAnnotSpace ('n':(show m))) (JSAssign JSAnnotSpace) exprE) JSSemiAuto : rs
       in go (m + 1) ts (g (Const m))

mathy :: Expr f 'Number
mathy =
  let_ (Plus (number 5) (number 6)) $ \x ->
  let_ (Plus (number 7) x) $ \y ->
  Plus x y

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
