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
  , apply
  , consoleLog
  , expr
  , ffi
  , host
  , lambda
  , let_
  , lookupId
  , lookupSelector
  , noOp
  , number
  , plus
  , string
  , classAdd
  , classRemove
  , classToggle
  , forEach
  , bool
    -- Evaluation
  , evaluate
  , evaluateNumber
  -- , pretty
  , pureAST
  , effectfulAST
  , printComputation
  , renderJS
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
import Numeric (showFFloat)
import Text.PrettyPrint ((<+>), Doc, ($+$))
import Topaz.Types
import Types
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as T
import qualified Text.PrettyPrint as P

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

classAdd, classRemove, classToggle :: Expr f 'Element -> Expr f 'String -> Effect f 'Unit
classAdd = ClassAdd
classRemove = ClassRemove
classToggle = ClassToggle

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

ffi :: String -> Rec (Expr f) us -> Effect f v
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

bool :: Bool -> Expr f 'Bool
bool = Literal . ValueBool

string :: Text -> Expr f 'String
string = Literal . ValueString

forEach :: Expr f ('Array u) -> (Expr f u -> Effect f u') -> Effect f 'Unit
forEach arr f = ForEach arr (coerce f . Var)

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
    Var x -> x
    Let x g -> go (g (go x))
    Apply g x -> unFunction (go g) (go x)
    Lambda g -> ValueFunction (go . g)
    Concat x y -> ValueString (unString (go x) <> unString (go y))
    Show _x -> undefined -- FIXME: this might be complicated
    -- _ -> undefined -- just to get rid of errors for now

fromRightE :: Either [Char] c -> c
fromRightE = either error id

printComputation :: Doc -> IO ()
printComputation (computation) = do
  putStrLn $ P.renderStyle P.style computation

renderJS :: Doc -> String
renderJS = P.renderStyle P.style

effectfulAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Effect f u)
  -> Doc
effectfulAST = snd . effectfulAST' 0

effectfulAST' :: forall v. Int -> Effect (Const Int) v -> (Int, Doc)
effectfulAST' !n0 = \case
  Host f -> 
    let windowLocationHost = ("const" <+> P.text ('n':show n0) <+> "=" <+> "window.location.host") <> P.semi
        (n1, x) = effectfulAST' (n0+1) (f (Const n0))
     in (n1, windowLocationHost $+$ x)
  Log x eff ->
    let (n1, x') = pureAST' n0 x
        (n2, as) = effectfulAST' n1 eff
        logX = "console.log" <> P.parens x' <> P.semi
     in (n2, logX $+$ as)
  Lift x -> pureAST' n0 x
  FFI fn args ->
    let foo :: Int -> Rec (Expr (Const Int)) u' -> (Int, [P.Doc])
        foo n'0 (RecCons x xs) = 
          let (n'1, x') = pureAST' n'0 x 
              (n'2, cs) = foo n'1 xs
           in (n'2, x' : cs)
        foo n' RecNil = (n',[])
        (n1, lArgs) = foo n0 args
        foreignFunction = P.text fn <> P.parens (P.hcat (P.punctuate ", " lArgs))
     in (n1, foreignFunction)
  ForEach xs f ->
    let (n1, xs') = pureAST' n0 xs
        (n2, as) = effectfulAST' n0 (f (Const n0))
        forE = xs' <> ".forEach" <> (P.parens 
               $ "function" <> P.parens (P.text ('n':show n1))
               <> P.braces as) <> P.semi
     in (n2, forE)
  LookupSelector x f ->
    let (n1, x') = pureAST' n0 x
        getX = "document.querySelectorAll" <> P.parens x'
        varX = ("const" <+> P.text ('n':show n1) <+> "=" <+> getX) <> P.semi
        (n2, as) = effectfulAST' (n1 + 1) (f (Const n1))
     in (n2, varX $+$ as)
  LookupId x f ->
    let (n1, x') = pureAST' n0 x
        getX = "document.getElementById" <> P.parens x'
        varX = ("const" <+> P.text ('n':show n1) <+> "=" <+> getX) <> P.semi
        (n2, as) = effectfulAST' (n1 + 1) (f (Const n1))
     in (n2, varX $+$ as)
  ClassToggle x cl ->
    let (n1, x') = pureAST' n0 x
        (n2, cl') = pureAST' n1 cl
        toggleCl = x' <> ".classList.toggle" <> P.parens cl'
     in (n2, toggleCl)
  ClassAdd x cl ->
    let (n1, x') = pureAST' n0 x
        (n2, cl') = pureAST' n1 cl
        toggleCl = x' <> ".classList.toggle" <> P.parens cl'
     in (n2, toggleCl)
  ClassRemove x cl ->
    let (n1, x') = pureAST' n0 x
        (n2, cl') = pureAST' n1 cl
        toggleCl = x' <> ".classList.toggle" <> P.parens cl'
     in (n2, toggleCl)

pureAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Doc
pureAST = snd . pureAST' 0

pureAST' :: forall v. Int -> Expr (Const Int) v 
   -> (Int,Doc)
pureAST' !n0 = \case
  Literal v -> case v of
    ValueNumber d -> (n0,(P.text $ showFFloat Nothing d ""))
    ValueArray xs ->       
      let foo :: Int -> [Value u] -> (Int, [Doc])
          foo n'0 (x:xs') = 
            let (n'1, x') = pureAST' n'0 (Literal x)
                (n'2, cs) = foo n'1 xs'
             in (n'2, x' : cs)
          foo n' [] = (n', [])
          (n1, exprs) = foo n0 xs
       in (n1, P.brackets (P.hcat $ P.punctuate ", " exprs))
    ValueString s -> (n0, P.doubleQuotes (P.text $ T.unpack s))
    ValueFunction _f -> undefined
    ValueUnit -> (n0, mempty) -- FIXME: is this correct
    ValueOption (Just x) -> pureAST' n0 (Literal x)
    ValueOption Nothing -> (n0, "null") -- FIXME: is this correct
    ValueResult _ -> undefined
    ValueBool True -> (n0, "true")
    ValueBool False -> (n0, "false")
  Concat x y ->
    let (n1, x1) = pureAST' n0 x
        (n2, y1) = pureAST' n1 y
     in (n2, x1 <+> "+" <+> y1)
  Plus x y ->
    let (n1, x1) = pureAST' n0 x
        (n2, y1) = pureAST' n1 y
     in (n2, x1 <+> "+" <+> y1)
  Times x y ->
    let (n1, x1) = pureAST' n0 x
        (n2, y1) = pureAST' n1 y
     in (n2, x1 <+> "*" <+> y1)
  FracDiv x y ->
    let (n1, x1) = pureAST' n0 x
        (n2, y1) = pureAST' n1 y
     in (n2, x1 <+> "/" <+> y1)
  Minus x y ->
    let (n1, x1) = pureAST' n0 x
        (n2, y1) = pureAST' n1 y
     in (n2, x1 <+> "-" <+> y1)
  Abs x ->
    let (n1, x1) = pureAST' n0 x
     in (n1, "Math.abs" <> P.parens x1)
  Sign x ->
    let (n1, x1) = pureAST' n0 x
     in (n1, "Math.sign" <> P.parens x1)
  Show x ->
    let (n1, x1) = pureAST' n0 x
     in (n1, "String" <> P.parens x1)
  Negate x ->
    let (n1, x1) = pureAST' n0 x
     in (n1, "-" <> P.parens x1)
  Let x g ->
    let (n1, x1) = pureAST' n0 x
        constX = ("const" <+> P.text ('n':show n1) <+> "=" <+> x1) <> P.semi
        (n2, x2) = pureAST' (n1 + 1) (g (Const n1))
     in (n2, constX $+$ x2)
  Lambda f ->
    let ex = f (Const n0)
        (n1, exprX ) = pureAST' (n0) ex
     in ( n1
        ,     "function" 
          <+> P.parens (P.text $ 'n':show n0)
          <+> P.braces ("return" <+> (P.parens exprX))
        )
  Apply fex ex ->
    let (n1, exprX) = pureAST' n0 fex
        (n2, exprY) = pureAST' n1 ex
     in ( n2+2
        , ("const" <+> (P.text $ 'n':show (n2+1)) <+> "=" <+> exprX) <> P.semi
        $+$ (P.text ('n':show (n2+1)) <> P.parens exprY) 
        )
  Var (Const x) -> (n0, P.text ('n':show x))

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
      ValueArray xs -> Const $ "[" <> T.intercalate ", " (fmap (getConst . go n0 . Literal) xs) <> "]"
      ValueFunction _ -> Const $ T.pack "<function>"
      ValueUnit -> Const $ "()"
    Plus x y -> Const ("plus (" <> getConst (go n0 x) <> ") (" <> getConst (go n0 y) <> ")")
    Times x y -> Const ("times (" <> getConst (go n0 x) <> ") (" <> getConst (go n0 y) <> ")")
    FracDiv x y -> Const ("div (" <> getConst (go n0 x) <> ") (" <> getConst (go n0 y) <> ")")
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
unidentify ss1 (VarF (Compose v)) = VarF (match v ss1)
unidentify ss1 (LetF x (Compose ref,exprA)) =
  LetF (unidentify ss1 x) (\z -> unidentify (Together ref z : ss1) exprA)
unidentify ss1 (PlusF a b) = PlusF (unidentify ss1 a) (unidentify ss1 b)
unidentify ss1 (ApplyF g x) = ApplyF (unidentify ss1 g) (unidentify ss1 x)
unidentify ss1 (LambdaF (Compose ref,exprA)) =
  LambdaF (\z -> unidentify (Together ref z : ss1) exprA)

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
