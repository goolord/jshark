{-# language BangPatterns #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}

{-# options_ghc -fno-warn-unused-top-binds #-}

module Reboot
  ( Expr(..)
  , Value(..)
    -- Operators
  , let_
  , lambda
  , number
  , plus
  , apply
  , host
  , expr
  , consoleLog
  , noOp
  , lookupId
  , lookupSelector
  , ffi
    -- Evaluation
  , evaluate
  , evaluateNumber
  -- , pretty
  , effectfulAST
  , convertAST
  , printEffComputation
  , printComputation
  ) where

-- This uses a higher-order PHOAS approach as described by
-- https://www.reddit.com/r/haskell/comments/85een6/sharing_from_phoas_multiple_interpreters_from_free/dvxhlba

-- import qualified Data.Sequence as Seq
import Control.Applicative
import Control.Monad.ST
import Data.Coerce
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Kind
import Data.STRef
import Data.Sequence (Seq(..), (|>), (<|))
import Data.Text (Text)
import Data.Tuple (snd)
import Topaz.Types
import Types
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as T
import qualified Language.JavaScript.AST as GP
import qualified Text.PrettyPrint.Leijen as PP

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

lookupId ::
     Expr f 'String
  -> (Expr f 'Element -> Effect f u)
  -> Effect f u
lookupId x f = LookupId x (f . Var)

lookupSelector :: 
     Expr f 'String
  -> (Expr f ('Array 'Element) -> Effect f u)
  -> Effect f u
lookupSelector x f = LookupSelector x (f . Var)

consoleLog :: Expr f u -> Effect f a -> Effect f a
consoleLog u eff = Log u eff

ffi :: String -> Rec (Expr f) (u' : us) -> Effect f v
ffi name args = FFI name args

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
    Times x y -> ValueNumber (unNumber (go x) * unNumber (go y))
    Minus x y -> ValueNumber (unNumber (go x) - unNumber (go y))
    Abs x -> ValueNumber (abs (unNumber (go x)))
    Sign x -> ValueNumber (signum (unNumber (go x)))
    Negate x -> ValueNumber (negate (unNumber (go x)))
    FracDiv x y -> ValueNumber (unNumber (go x) / unNumber (go y))
    Recip x -> ValueNumber (recip (unNumber (go x)))
    Var x -> x
    Let x g -> go (g (go x))
    Apply g x -> unFunction (go g) (go x)
    Lambda g -> ValueFunction (go . g)
    Concat x y -> ValueString (unString (go x) <> unString (go y))
    Show _x -> undefined -- FIXME: this might be complicated
    -- _ -> undefined -- just to get rid of errors for now

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

printComputation :: Computation -> IO ()
printComputation (computation) = do
  putStrLn $ show $ PP.pretty computation

printEffComputation :: EffComputation -> IO ()
printEffComputation (effComp) = do
  putStrLn $ show $ PP.pretty effComp

effectfulAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Effect f u)
  -> EffComputation
effectfulAST = snd . effectfulAST' 0 mempty

name' :: String -> GP.Name
name' = fromRightE . GP.name

effectfulAST' :: forall v. Int -> Seq (GP.VarStmt) -> Effect (Const Int) v -> (Int, EffComputation)
effectfulAST' !n0 !ss0 = \case
  -- window.location.host
  Host f -> 
    let windowLocationHost =  
          (GP.ExprName $ name' "window")
          `GP.ExprRefinement` (GP.Property $ name' "location")
          `GP.ExprRefinement` (GP.Property $ name' "host")
        vs = ss0 |> (GP.ConstStmt $ GP.VarDecl (name' ('n':show n0)) (Just windowLocationHost))
     in effectfulAST' (n0+1) vs (f (Const n0))
  -- console.log(x)
  Log x eff ->
    let (n1, Computation x' ss') = convertAST' n0 ss0 x
        (n2, EffComputation as) = effectfulAST' n1 mempty eff
        logX = GP.ExprInvocation
          ((GP.ExprName $ name' "console")
           `GP.ExprRefinement` (GP.Property $ name' "log")
          ) (GP.Invocation [x'])
    in (n2, EffComputation $ fmap Left ss' <> (Right logX <| as ))
  -- document.getElementById(x)
  LookupId x f ->
    let documentGetElementById =  
          (GP.ExprName $ name' "document")
          `GP.ExprRefinement` (GP.Property $ name' "getElementById")
        (n1, Computation x' ss') = convertAST' n0 mempty x
        getX = GP.ExprInvocation documentGetElementById (GP.Invocation [x'])
        varX = GP.ConstStmt $ GP.VarDecl (name' ('n':show n1)) (Just getX)
        (n2, EffComputation as) = effectfulAST' (n1 + 1) mempty (f (Const n1))
     in (n2, EffComputation $ fmap Left ss0 <> (Left varX <| fmap Left ss') <> as)
  LookupSelector x f ->
    let documentQuerySelectorAll =  
          (GP.ExprName $ name' "document")
          `GP.ExprRefinement` (GP.Property $ name' "querySelectorAll")
        (n1, Computation x' ss') = convertAST' n0 mempty x
        getX = GP.ExprInvocation documentQuerySelectorAll (GP.Invocation [x'])
        varX = GP.ConstStmt $ GP.VarDecl (name' ('n':show n1)) (Just getX)
        (n2, EffComputation as) = effectfulAST' (n1 + 1) mempty (f (Const n1))
     in (n2, EffComputation $ fmap Left ss0 <> (Left varX <| fmap Left ss') <> as)
  FFI fn args ->
    let foo :: Int -> Seq (GP.VarStmt) -> Rec (Expr (Const Int)) u' -> (Int, Seq (GP.VarStmt), [GP.Expr])
        foo n'0 ss'0 (RecCons x xs) = 
          let (n'1, Computation x' ss'1) = convertAST' n'0 ss'0 x 
              (n'2, ss'2, cs) = foo n'1 ss'1 xs
           in (n'2, ss'2, x' : cs)
        foo n' ss' RecNil = (n',ss',[])
        (n1, ss1, lArgs) = foo n0 ss0 args
        foreignFunction = GP.ExprInvocation (GP.ExprName (name' fn)) (GP.Invocation lArgs)
     in (n1, EffComputation (fmap Left ss1 |> Right foreignFunction))
  Lift (Literal ValueUnit) -> (n0, EffComputation $ fmap Left ss0)
  Lift x -> pureToEff $ convertAST' n0 ss0 x

convertAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Computation
convertAST = snd . convertAST' 0 mempty

convertAST' :: forall v. Int -> Seq (GP.VarStmt) -> Expr (Const Int) v 
   -> (Int,Computation)
convertAST' !n0 !ss0 = \case
  Literal v -> case v of
    ValueNumber d -> (n0,simple ss0 $ GP.ExprLit $ GP.LitNumber $ GP.Number d)
    ValueString t -> (n0,simple ss0 $ GP.ExprLit $ GP.LitString $ fromRightE $ GP.jsString (T.unpack t))
    ValueArray xs -> 
      let foo :: Int -> Seq GP.VarStmt -> [Value u] -> (Int, Seq GP.VarStmt, [GP.Expr])
          foo n'0 ss'0 (x:xs') = 
            let (n'1, Computation x' ss'1) = convertAST' n'0 ss'0 (Literal x)
                (n'2, ss'2, cs) = foo n'1 ss'1 xs'
             in (n'2, ss'2, x' : cs)
          foo n' ss' [] = (n', ss', [])
          (n1, ss1, exprs) = foo n0 ss0 (toList xs)
       in (n1,simple ss1 $ GP.ExprLit $ GP.LitArray $ GP.ArrayLit $ exprs)
    -- v don't know what to do here
    ValueFunction _ -> (n0,simple ss0 $ GP.ExprLit $ undefined)
    ValueUnit -> (n0, simple ss0 $ error "impossible: don't do this")
  Plus x y ->
    let (n1,Computation exprX rs) = convertAST' n0 ss0 x
        (n2,Computation exprY ts) = convertAST' n1 rs y
     in (n2,Computation (GP.ExprInfix GP.Add exprX exprY) ts)
  Minus x y ->
    let (n1,Computation exprX rs) = convertAST' n0 ss0 x
        (n2,Computation exprY ts) = convertAST' n1 rs y
     in (n2,Computation (GP.ExprInfix GP.Sub exprX exprY) ts)
  Times x y ->
    let (n1,Computation exprX rs) = convertAST' n0 ss0 x
        (n2,Computation exprY ts) = convertAST' n1 rs y
     in (n2,Computation (GP.ExprInfix GP.Mul exprX exprY) ts)
  FracDiv x y ->
    let (n1,Computation exprX rs) = convertAST' n0 ss0 x
        (n2,Computation exprY ts) = convertAST' n1 rs y
     in (n2,Computation (GP.ExprInfix GP.Div exprX exprY) ts)
  Abs x ->
    let (n1,Computation exprX rs) = convertAST' n0 ss0 x
     in (n1,Computation (GP.ExprInvocation (GP.ExprName $ name' "Math.abs") (GP.Invocation [exprX])) rs)
  Negate x ->
    let (n1,Computation exprX rs) = convertAST' n0 ss0 x
     in (n1,Computation (GP.ExprPrefix GP.Negate exprX) rs)
  Sign x ->
    let (n1,Computation exprX rs) = convertAST' n0 ss0 x
     in (n1,Computation (GP.ExprInvocation (GP.ExprName $ name' "Math.sign") (GP.Invocation [exprX])) rs)
  Var (Const v) -> (n0,simple ss0 $ GP.ExprName $ name' ('n':show v))
  Let e g ->
    let (n1,Computation exprE rs) = convertAST' n0 ss0 e
        vs = rs |> (GP.ConstStmt $ GP.VarDecl (name' ('n':show n1)) (Just exprE))
     in convertAST' (n1 + 1) vs (g (Const n1))
  Concat x y ->
    let (n1,Computation exprX rs) = convertAST' n0 ss0 x
        (n2,Computation exprY ts) = convertAST' n1 rs y
     in (n2,Computation (GP.ExprInfix GP.Add exprX exprY) ts)
  Lambda f ->
    let _name = 'x':show n0 in
    undefined
  Show x ->
    let (n1,Computation exprX rs) = convertAST' n0 ss0 x
     in (n1, Computation (GP.ExprInvocation (GP.ExprName $ name' "String") (GP.Invocation [exprX])) rs)

noOp :: Effect f 'Unit
noOp = expr (Literal ValueUnit)

pretty :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Text
pretty e0 = getConst (go 0 e0) where
  go :: forall v. Int -> Expr (Const Text) v -> Const Text v
  go !n0 = \case
    Literal v -> case v of
      ValueNumber d -> Const $ T.pack (show d)
      ValueString t -> Const $ T.pack (show t)
      ValueFunction _ -> Const $ T.pack "<function>"
      ValueUnit -> Const $ "()"
    Plus x y -> Const ("plus (" <> getConst (go n0 x) <> ") (" <> getConst (go n0 y) <> ")")
    Times x y -> Const ("times (" <> getConst (go n0 x) <> ") (" <> getConst (go n0 y) <> ")")
    Minus x y -> Const ("minus (" <> getConst (go n0 x) <> ") (" <> getConst (go n0 y) <> ")")
    Concat x y -> Const ("concat (" <> getConst (go n0 x) <> ") (" <> getConst (go n0 y) <> ")")
    Abs x -> Const ("abs (" <> getConst (go n0 x) <> ")")
    Sign x -> Const ("sign (" <> getConst (go n0 x) <> ")")
    Negate x -> Const ("negate (" <> getConst (go n0 x) <> ")")
    Show x -> Const ("show (" <> getConst (go n0 x) <> ")")
    Var x -> x
    Lambda g ->
      let name = "x" <> T.pack (show n0)
       in Const  
          $  "λ"
          <> name
          <> " -> "
          <> getConst (go (n0 + 1) (g (Const name)))
    Apply g x -> Const ("(" <> getConst (go n0 g) <> ") (" <> getConst (go n0 x) <> ")")
    Let x g ->
      let name = "x" <> T.pack (show n0)
       in Const
          $  "let "
          <> name
          <> " = {"
          <> getConst (go (n0 + 1) x)
          <> "} in {"
          <> getConst (go (n0 + 1) (g (Const name)))
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
