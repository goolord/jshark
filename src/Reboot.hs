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

{-# options_ghc -fno-warn-unused-top-binds #-}

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
  -- , pretty
  ) where

-- This uses a higher-order PHOAS approach as described by
-- https://www.reddit.com/r/haskell/comments/85een6/sharing_from_phoas_multiple_interpreters_from_free/dvxhlba

import Data.Tuple (snd)
import Data.Char (Char)

import Data.Functor.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import qualified Language.JavaScript.AST as GP
import qualified Language.JavaScript.Pretty as GP
import qualified Data.Text as T
import qualified Data.List as List
import qualified Text.PrettyPrint.Leijen as PP

data Universe
  = Number
  | String
  | Unit
  | Element
  | List Universe
  | Effectful Universe
  | Function Universe Universe

data Value :: Universe -> Type where
  ValueNumber :: Double -> Value 'Number
  ValueString :: Text -> Value 'String
  ValueEffect :: (forall f. Effect f u) -> Value ('Effectful u)
  ValueFunction :: (Value u -> Value v) -> Value ('Function u v)

data Effect :: (Universe -> Type) -> Universe -> Type where
  Host :: (f 'String -> Effect f u) -> Effect f u
  Log :: Expr f 'String -> Effect f u -> Effect f u
  LookupId :: f 'String -> (f 'Element -> Effect f u) -> Effect f u
  LookupClass :: f 'String -> (f ('List 'Element) -> Effect f u) -> Effect f u
  Lift :: Expr f u -> Effect f u

data Expr :: (Universe -> Type) -> Universe -> Type where
  Literal :: Value u -> Expr f u
  Concat :: Expr f 'String -> Expr f 'String -> Expr f 'String
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

unString :: Value 'String -> Text
unString (ValueString s) = s

unFunction :: Value ('Function u v) -> Value u -> Value v
unFunction (ValueFunction f) = f

host ::
     (Expr f 'String -> Effect f u)
  -> Effect f u
host f = Host (f . Var)

expr :: Expr f u -> Effect f u
expr = Lift

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

string :: Text -> Expr f 'String
string = Literal . ValueString

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
    Concat x y -> ValueString (unString (go x) <> unString (go y))

-- pretty :: forall (u :: Universe).
     -- (forall (f :: Universe -> Type). Expr f u)
  -- -> Text
-- pretty = getConst . go 0 where
  -- go :: forall v. Int -> Expr (Const Text) v -> Const Text v
  -- go !n = \case
    -- Literal v -> case v of
      -- ValueNumber d -> Const $ T.pack (show d)
      -- ValueString t -> Const $ T.pack (show t)
      -- ValueFunction _ -> Const $ T.pack "<function>"
    -- Plus x y -> Const ("plus (" <> getConst (go n x) <> ") (" <> getConst (go n y) <> ")")
    -- Var x -> x
    -- Lambda g ->
      -- let name = "x" <> T.pack (show n)
       -- in Const  
          -- $  "λ"
          -- <> name
          -- <> " -> "
          -- <> getConst (go (n + 1) (g (Const name)))
    -- Apply g x -> Const ("(" <> getConst (go n g) <> ") (" <> getConst (go n x) <> ")")
    -- Let x g ->
      -- let name = "x" <> T.pack (show n)
       -- in Const
          -- $  "let "
          -- <> name
          -- <> " = {"
          -- <> getConst (go (n + 1) x)
          -- <> "} in {"
          -- <> getConst (go (n + 1) (g (Const name)))
          -- <> "}"

data Optimization 
  = ConstantFolding
  | UnusedBindings

-- prettyJS :: forall (u :: Universe).
     -- (forall (f :: Universe -> Type). Expr f u)
  -- -> String
-- prettyJS = getConst . go 0 where
  -- go :: forall v. Int -> Expr (Const String) v -> Const String v
  -- go !n = \case
    -- Literal v -> case v of
      -- ValueNumber d -> Const (show d)
      -- ValueString t -> Const (show t)
      -- ValueFunction _ -> Const "<function>"
    -- Plus x y -> Const ("plus (" <> getConst (go n x) <> ") (" <> getConst (go n y) <> ")")
    -- Var x -> x
    -- Lambda g ->
      -- let name = "x" <> (show n)
       -- in Const  
          -- $  "λ"
          -- <> name
          -- <> " -> "
          -- <> getConst (go (n + 1) (g (Const name)))
    -- Apply g x -> Const ("(" <> getConst (go n g) <> ") (" <> getConst (go n x) <> ")")
    -- Let x g ->
      -- let name = "x" <> (show n)
       -- in Const
          -- $  "let "
          -- <> name
          -- <> " = {"
          -- <> getConst (go (n + 1) x)
          -- <> "} in {"
          -- <> getConst (go (n + 1) (g (Const name)))
          -- <> "}"

data Computation = Computation GP.Expr [GP.VarStmt]

printComputation :: Computation -> IO ()
printComputation (Computation e ss) = do
  putStrLn $ show $ 
       (GP.pretty $ GP.Program (List.reverse ss) [])
    <> GP.pretty (PP.text "\n")
    <> GP.pretty e

simple :: [GP.VarStmt] -> GP.Expr -> Computation
simple ss e = Computation e ss

fromRightE :: Either [Char] c -> c
fromRightE = either error id

effectfulAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Effect f u)
  -> Computation
effectfulAST = effectfulAST' []

effectfulAST' :: [GP.VarStmt] -> Effect (Const Int) v -> Computation
effectfulAST' !ss = \case
  -- window.location.host
  -- Host f -> simple ss $
    -- (GP.ExprName $ fromRightE $ GP.name "window")
    -- `GP.ExprRefinement`
    -- (GP.Property $ fromRightE $ GP.name "location")
    -- `GP.ExprRefinement`
    -- (GP.Property $ fromRightE $ GP.name "host")
  -- console.log(x)
  Log x y ->
    let (_, Computation x' _) = convertAST' 0 ss x in
    simple ss $
    GP.ExprInvocation
      ( (GP.ExprName $ fromRightE $ GP.name "console")
        `GP.ExprRefinement`
        (GP.Property $ fromRightE $ GP.name "log")
      )
      (GP.Invocation [x'])
  Lift x -> snd $ convertAST' 0 ss x
      

convertAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Computation
convertAST = snd . convertAST' 0 []

convertAST' :: forall v. Int -> [GP.VarStmt] -> Expr (Const Int) v 
   -> (Int,Computation)
convertAST' !n !ss = \case
  Literal v -> case v of
    ValueNumber d -> (n,simple ss $ GP.ExprLit $ GP.LitNumber $ GP.Number d)
    ValueString t -> (n,simple ss $ GP.ExprLit $ GP.LitString $ fromRightE $ GP.jsString (T.unpack t))
    -- v don't know what to do here
    ValueFunction _ -> (n,simple ss $ GP.ExprLit $ undefined)
    ValueEffect eff -> (n,effectfulAST eff)
  Plus x y ->
    let (m,Computation exprX rs) = convertAST' n ss x
        (p,Computation exprY ts) = convertAST' m rs y
     in (p,Computation (GP.ExprInfix GP.Add exprX exprY) ts)
  Var (Const v) -> (n,simple ss $ GP.ExprName $ fromRightE $ GP.name ('n':(show v)))
  Let e g ->
    let (m,Computation exprE rs) = convertAST' n ss e
        vs = (GP.ConstStmt $ GP.VarDecl (fromRightE $ GP.name ('n':(show m))) (Just exprE)) : rs
     in convertAST' (m + 1) vs (g (Const m))
  Concat x y ->
    let (m,Computation exprX rs) = convertAST' n ss x
        (p,Computation exprY ts) = convertAST' m rs y
     in (p,Computation (GP.ExprInfix GP.Add exprX exprY) ts)

mathy :: Expr f 'Number
mathy =
  let_ (Plus (number 5) (number 6)) $ \x ->
  let_ (Plus (number 7) x) $ \y ->
  Plus x y

-- mathy :: Expr f 'Number
-- mathy = do
--   x <- let_ (Plus (number 5) (number 6))
--   y <- let_ (Plus (number 7) x)
--   pure (Plus x y)

stringy :: Effect f 'String
stringy =
  host $ \x ->
  expr (Concat x x)

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
unidentify rs (LetF x (Compose ref,exprA)) =
  LetF (unidentify rs x) (\z -> unidentify (Together ref z : rs) exprA)
unidentify rs (PlusF a b) = PlusF (unidentify rs a) (unidentify rs b)
unidentify rs (ApplyF g x) = ApplyF (unidentify rs g) (unidentify rs x)
unidentify rs (LambdaF (Compose ref,exprA)) =
  LambdaF (\z -> unidentify (Together ref z : rs) exprA)

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
