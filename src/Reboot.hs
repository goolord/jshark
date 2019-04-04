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
{-# options_ghc -fno-warn-orphans #-}

module Reboot
  ( Expr(..)
  , Value(..)
    -- Operators
  , let_
  , lambda
  , number
  , plus
  , apply
    -- Evaluation
  , evaluate
  , evaluateNumber
  -- , pretty
  ) where

-- This uses a higher-order PHOAS approach as described by
-- https://www.reddit.com/r/haskell/comments/85een6/sharing_from_phoas_multiple_interpreters_from_free/dvxhlba

import Control.Applicative
import Control.Monad.ST
import Data.Coerce
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Kind
import Data.STRef
import Data.Text (Text)
import Data.Tuple (snd)
import Unsafe.Coerce (unsafeCoerce)
import Data.Sequence (Seq(..), (|>), (<|))
-- import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Language.JavaScript.AST as GP
import qualified Language.JavaScript.Pretty as GP
import qualified Text.PrettyPrint.Leijen as PP
import qualified GHC.Exts as Exts

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
  ValueUnit :: Value 'Unit

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
  Times :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
  Minus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
  Abs :: Expr f 'Number -> Expr f 'Number
  Sign :: Expr f 'Number -> Expr f 'Number
  Negate :: Expr f 'Number -> Expr f 'Number
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

-- newtype Expression :: (Universe -> Type) -> Universe -> Type where
--   Expression :: ExprArrow (->) f u -> Expression f u

-- Op :: Operation _ u -> ExprArrow g f u
-- data Operation :: () -> Universe -> Type
--   Plus :: Operation f 'Number -> Operation f 'Number -> Operation f 'Number

instance Exts.IsString (Expr f 'String) where
  fromString = Literal . ValueString . Exts.fromString

instance forall (f :: (Universe -> Type)) u. (u ~ 'Number) => Num (Expr f u) where
  (+) = Plus
  (*) = Times
  (-) = Minus
  abs = Abs
  signum = Sign
  fromInteger = number . fromInteger

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

consoleLog :: Expr f 'String -> Effect f a -> Effect f a
consoleLog str eff = Log str eff

expr :: Expr f u -> Effect f u
expr = Lift

plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
plus a b = (Plus a b)

apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
apply g a = (Apply g a)

let_ ::
     Expr f u
  -> (Expr f u -> Expr f v)
  -> Expr f v
let_ e f = (Let e (coerce f . Var))

lambda :: 
     (Expr f u -> Expr f v)
  -> Expr f ('Function u v)
lambda f = Lambda (coerce f . Var)

number :: Double -> Expr f 'Number
number = Literal . ValueNumber

string :: Text -> Expr f 'String
string = Literal . ValueString

evaluateNumber :: (forall (f :: Universe -> Type). Expr f 'Number) -> Double
evaluateNumber e = unNumber (evaluate e)

evaluate :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Value u
evaluate e0 = go e0 where
  go :: forall v. Expr Value v -> Value v
  go = \case
    Literal v -> v
    Plus x y -> ValueNumber (unNumber (go x) + unNumber (go y))
    Var x -> x
    Let x g -> go (g (go x))
    Apply g x -> unFunction (go g) (go x)
    Lambda g -> ValueFunction (go . g)
    Concat x y -> ValueString (unString (go x) <> unString (go y))
    _ -> undefined -- just to get rid of errors for now

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

data Computation = Computation GP.Expr (Seq GP.VarStmt)
newtype EffComputation = EffComputation (Seq (Either GP.VarStmt GP.Expr))

instance PP.Pretty EffComputation where
  pretty (EffComputation x) = foldr1 (PP.<$$>) (fmap PP.pretty x)

instance PP.Pretty Computation where
  pretty (Computation ex vars) = 
    foldr1 (PP.<$$>) ( fmap PP.pretty vars |> PP.pretty ex)

instance (PP.Pretty a, PP.Pretty b) => PP.Pretty (Either a b) where
  pretty (Left a) = PP.pretty a
  pretty (Right a) = PP.pretty a

printComputation :: Computation -> IO ()
printComputation (computation) = do
  putStrLn $ show $ PP.pretty computation

printEffComputation :: EffComputation -> IO ()
printEffComputation (effComp) = do
  putStrLn $ show $ GP.pretty effComp

simple :: Seq (GP.VarStmt) -> GP.Expr -> Computation
simple ss e = Computation e ss

simpleEff :: Seq (GP.VarStmt) -> GP.Expr -> EffComputation
simpleEff ss eff = EffComputation $ fmap Left ss |> Right eff

simpleEffs :: Seq (GP.VarStmt) -> Seq (GP.Expr) -> EffComputation
simpleEffs ss effs = EffComputation $ fmap Left ss <> fmap Right effs

pureToEff :: (Int, Computation) -> (Int, EffComputation)
pureToEff (n, c) = (n, compToEff c)
  where
  compToEff (Computation ex vars) = EffComputation $ fmap Left vars |> Right ex

fromRightE :: Either [Char] c -> c
fromRightE = either error id

effectfulAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Effect f u)
  -> EffComputation
effectfulAST = snd . effectfulAST' 0 mempty

effectfulAST' :: forall v. Int -> Seq (GP.VarStmt) -> Effect (Const Int) v -> (Int, EffComputation)
effectfulAST' !n !ss = \case
  -- window.location.host
  Host f -> 
    let windowLocationHost =  
          (GP.ExprName $ fromRightE $ GP.name "window")
          `GP.ExprRefinement` (GP.Property $ fromRightE $ GP.name "location")
          `GP.ExprRefinement` (GP.Property $ fromRightE $ GP.name "host")
        vs = ss |> (GP.ConstStmt $ GP.VarDecl (fromRightE $ GP.name ('n':(show n))) (Just windowLocationHost))
     in effectfulAST' (n+1) vs (f (Const n))
  -- console.log(x)
  Log x eff ->
    let (m, Computation x' ss') = convertAST' n ss x
        (o, EffComputation as) = effectfulAST' m mempty eff
        logX = GP.ExprInvocation
          ( (GP.ExprName $ fromRightE $ GP.name "console")
            `GP.ExprRefinement`
            (GP.Property $ fromRightE $ GP.name "log")
          )
          (GP.Invocation [x'])
    in (o, EffComputation $ fmap Left ss' <> (Right logX <| as ))
  -- document.getElementById(x)
  -- LookupId x f ->
    -- let documentGetElementById =  
          -- (GP.ExprName $ fromRightE $ GP.name "document")
          -- `GP.ExprRefinement` (GP.Property $ fromRightE $ GP.name "getElementById")
        -- (m, Computation x' ss') = convertAST' n ss x
     -- in  undefined
        -- vs = ss |> (GP.ConstStmt $ GP.VarDecl (fromRightE $ GP.name ('n':(show n))) (Just windowLocationHost))
     -- in effectfulAST' (n+1) vs (f (Const x))
  Lift (Literal ValueUnit) -> (n, EffComputation $ fmap Left ss)
  Lift x -> pureToEff $ convertAST' n ss x

convertAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Computation
convertAST = snd . convertAST' 0 mempty

convertAST' :: forall v. Int -> Seq (GP.VarStmt) -> Expr (Const Int) v 
   -> (Int,Computation)
convertAST' !n !ss = \case
  Literal v -> case v of
    ValueNumber d -> (n,simple ss $ GP.ExprLit $ GP.LitNumber $ GP.Number d)
    ValueString t -> (n,simple ss $ GP.ExprLit $ GP.LitString $ fromRightE $ GP.jsString (T.unpack t))
    -- v don't know what to do here
    ValueFunction _ -> (n,simple ss $ GP.ExprLit $ undefined)
    ValueEffect eff -> undefined --no longer works -- (n,effectfulAST eff)
    ValueUnit -> (n, simple ss $ error "impossible: don't do this")
  Plus x y ->
    let (m,Computation exprX rs) = convertAST' n ss x
        (p,Computation exprY ts) = convertAST' m rs y
     in (p,Computation (GP.ExprInfix GP.Add exprX exprY) ts)
  Minus x y ->
    let (m,Computation exprX rs) = convertAST' n ss x
        (p,Computation exprY ts) = convertAST' m rs y
     in (p,Computation (GP.ExprInfix GP.Sub exprX exprY) ts)
  Times x y ->
    let (m,Computation exprX rs) = convertAST' n ss x
        (p,Computation exprY ts) = convertAST' m rs y
     in (p,Computation (GP.ExprInfix GP.Mul exprX exprY) ts)
  Abs x ->
    let (m,Computation exprX rs) = convertAST' n ss x
     in (m,Computation (GP.ExprInvocation (GP.ExprName $ fromRightE $ GP.name "Math.abs") (GP.Invocation [exprX])) rs)
  Negate x ->
    let (m,Computation exprX rs) = convertAST' n ss x
     in (m,Computation (GP.ExprPrefix GP.Negate exprX) rs)
  Sign x ->
    let (m,Computation exprX rs) = convertAST' n ss x
     in (m,Computation (GP.ExprInvocation (GP.ExprName $ fromRightE $ GP.name "Math.sign") (GP.Invocation [exprX])) rs)
  Var (Const v) -> (n,simple ss $ GP.ExprName $ fromRightE $ GP.name ('n':(show v)))
  Let e g ->
    let (m,Computation exprE rs) = convertAST' n ss e
        vs = rs |> (GP.ConstStmt $ GP.VarDecl (fromRightE $ GP.name ('n':(show m))) (Just exprE))
     in convertAST' (m + 1) vs (g (Const m))
  Concat x y ->
    let (m,Computation exprX rs) = convertAST' n ss x
        (p,Computation exprY ts) = convertAST' m rs y
     in (p,Computation (GP.ExprInfix GP.Add exprX exprY) ts)
  Lambda f ->
    let _name = 'x':(show n) in
    undefined

mathy :: Expr f 'Number
mathy =
  let_ (Plus 5 6) $ \x ->
  let_ (Plus 7 x) $ \y ->
  Plus x y

mathy2 :: Expr f 'Number
mathy2 = negate (-1)

-- mathy :: Expr f 'Number
-- mathy = do
--   x <- let_ (Plus (number 5) (number 6))
--   y <- let_ (Plus (number 7) x)
--   pure (Plus x y)

stringy :: Effect f 'String
stringy =
  host $ \x ->
  expr (Concat x x)

loggy :: Effect f 'Unit
loggy = 
  host $ \n0 ->
  host $ \n1 ->
  consoleLog "foo" $ consoleLog "bar" $ consoleLog n0 $ consoleLog n1 noOp

noOp :: Effect f 'Unit
noOp = expr (Literal ValueUnit)

pretty :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Text
pretty e0 = getConst (go 0 e0) where
  go :: forall v. Int -> Expr (Const Text) v -> Const Text v
  go !n = \case
    Literal v -> case v of
      ValueNumber d -> Const $ T.pack (show d)
      ValueString t -> Const $ T.pack (show t)
      ValueFunction _ -> Const $ T.pack "<function>"
      ValueEffect _ -> Const $ T.pack "<effect>"
    Plus x y -> Const ("plus (" <> getConst (go n x) <> ") (" <> getConst (go n y) <> ")")
    Concat x y -> Const ("concat (" <> getConst (go n x) <> ") (" <> getConst (go n y) <> ")")
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

identify ::
     (forall (v :: Universe). g v)
  -> ExprF (->) (Compose (STRef s) g) u
  -> ST s (ExprF (,) (Compose (STRef s) g) u)
identify _ (LiteralF v) = pure (LiteralF v)
identify _ (VarF v) = pure (VarF v)
identify z (LetF x g) = do
  r <- newSTRef z
  x' <- identify z x
  g' <- identify z (g (Compose r))
  pure (LetF x' (Compose r,g'))
identify z (PlusF a b) = liftA2 PlusF (identify z a) (identify z b)
identify z (ApplyF g a) = liftA2 ApplyF (identify z g) (identify z a)
identify z (LambdaF g) = do
  r <- newSTRef z
  g' <- identify z (g (Compose r))
  pure (LambdaF (Compose r,g'))

removeUnusedBindings :: 
     (forall (g :: Universe -> Type). ExprF (->) g u)
  -> ExprF (->) f u
removeUnusedBindings e0 = runST $ do
  e1 <- identify (Const (0 :: Int)) e0
  pure (unidentify [] e1)

data Together :: Type -> (Universe -> Type) -> (Universe -> Type) -> Type where
  Together :: STRef s (g u) -> f u -> Together s g f

match :: forall (f :: Universe -> Type) (g :: Universe -> Type) (s :: Type) (u :: Universe).
  STRef s (g u) -> [Together s g f] -> f u
match !_ [] = error "match: implementation error in unidentify"
match !r (Together x v : xs) = if r == unsafeCoerce x
  then unsafeCoerce v
  else match r xs

unidentify :: forall (f :: Universe -> Type) (g :: Universe -> Type) (s :: Type) (u :: Universe).
  [Together s g f]  -> ExprF (,) (Compose (STRef s) g) u -> ExprF (->) f u
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
