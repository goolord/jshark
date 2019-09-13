{-# language BangPatterns #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}

{-# options_ghc -fno-warn-unused-top-binds #-}

module JShark
  ( Expr(..)
  , Value(..)
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
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Kind
import Data.STRef
import Data.Text (Text)
import Data.Tuple (snd)
import Numeric (showFFloat)
import Text.PrettyPrint ((<+>), Doc, ($$))
import Topaz.Types
import JShark.Types
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as T
import qualified Text.PrettyPrint as P

unNumber :: Value 'Number -> Double
unNumber (ValueNumber d) = d

unBool :: Value 'Bool -> Bool
unBool (ValueBool b) = b

unString :: Value 'String -> Text
unString (ValueString s) = s

unFunction :: Value ('Function u v) -> Value u -> Value v
unFunction (ValueFunction f) = f

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
    Apply g x -> unFunction (go g) (go x)
    Lambda g -> ValueFunction (go . g)
    Concat x y -> ValueString (unString (go x) <> unString (go y))
    Show _x -> undefined -- FIXME: this might be complicated
    And x y -> ValueBool (unBool (go x) && unBool (go y))
    Or x y -> ValueBool (unBool (go x) || unBool (go y))
    Eq _ _ -> undefined -- Value doesn't have an Eq instance because of ValueFunction
    NEq _ _ -> undefined 
    GTh _ _ -> undefined 
    LTh _ _ -> undefined 
    GTEq _ _ -> undefined 
    LTEq _ _ -> undefined 
    Let x g -> go (g (go x)) 

fromRightE :: Either [Char] c -> c
fromRightE = either error id

printComputation :: Doc -> IO ()
printComputation (computation) = do
  putStrLn $ P.renderStyle P.style computation

renderJS :: Doc -> String
renderJS = P.renderStyle P.style

renderCode :: Code -> Doc
renderCode (Code a b) = a $$ b

partitionCode :: [Code] -> ([Doc], [Doc])
partitionCode ((Code a b):cs) = let (as,bs) = partitionCode cs in ((a:as),(b:bs))
partitionCode [] = ([], [])

effectfulAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Effect f u)
  -> Doc
effectfulAST = renderCode . snd . effectfulAST' 0

effectfulAST' :: forall v. Int -> Effect (Const Int) v -> (Int, Code)
effectfulAST' !n0 = \case
  Lift x -> pureAST' n0 x
  FFI fn args ->
    let foo :: Int -> Rec (Expr (Const Int)) u' -> (Int, [Code])
        foo n'0 (RecCons x xs) = 
          let (n'1, x') = pureAST' n'0 x 
              (n'2, cs) = foo n'1 xs
           in (n'2, x' : cs)
        foo n' RecNil = (n',[])
        (n1, lArgs') = foo n0 args
        (lVars, lArgs) = partitionCode lArgs'
        foreignFunction = P.text fn <> P.parens (P.hcat (P.punctuate ", " lArgs))
     in (n1, Code (P.vcat lVars) foreignFunction)
  ForEach xs f ->
    let (n1, (Code xsDecl xsRef)) = pureAST' n0 xs
        (n2, (Code asDecl asRef)) = effectfulAST' n0 (f (Const n0))
        forE = xsRef <> ".forEach" <> (P.parens 
               $ "function" <> P.parens (P.text ('n':show n1))
               <> P.braces (P.nest 2 asRef)) <> P.semi
     in (n2, Code (xsDecl $$ asDecl) forE)
  Bind (Lift (Literal ValueUnit)) f -> effectfulAST' (n0-1) (f (Const (n0-1)))
  Bind x f ->
    let (n1, (Code x1Decl x1Ref)) = effectfulAST' n0 x
        constX = ("const" <+> P.text ('n':show n1) <+> "=" <+> x1Ref) <> P.semi
        (n2, (Code x2Decl x2Ref)) = effectfulAST' (n1 + 1) (f (Const n1))
     in (n2, Code (x1Decl $$ constX $$ x2Decl) x2Ref)
  UnsafeObject obj -> (n0, Code mempty $ P.text $ T.unpack obj)
  UnsafeObjectGet x string ->
    let (n1, (Code x1Decl x1Ref)) = effectfulAST' n0 x
    in (n1, Code x1Decl $ x1Ref <> "." <> P.text string)
  UnsafeObjectAssign x y ->
    let (n1, (Code x1Decl x1Ref)) = effectfulAST' n0 x
        (n2, (Code y1Decl y1Ref)) = effectfulAST' n1 y
    in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <> " = " <> y1Ref )
  ObjectFFI x ffi ->
    let (n1, (Code x1Decl x1Ref)) = effectfulAST' n0 x
        (n2, (Code ffi1Decl ffi1Ref)) = effectfulAST' n1 ffi
    in (n2, Code (x1Decl $$ ffi1Decl) $ x1Ref <> "." <> ffi1Ref)
  UnEffectful x -> 
    let (n1, (Code a1Decl a1Ref)) = pureAST' n0 x
     in (n1, (Code a1Decl $ a1Ref <> P.parens mempty))
  LambdaE f ->
    let ex = f (Const n0)
        (n1, (Code exprXDecl exprXRef)) = effectfulAST' n0 ex
     in ( n1 + 1
        , Code mempty
            $ "function" 
            <+> P.parens (P.text $ 'n':show (n1))
            <+> P.braces ( (exprXDecl $$ "return" <+> exprXRef) )
        )
  ApplyE fex ex ->
    let (n1, (Code exprXDecl exprXRef)) = effectfulAST' n0 fex
        (n2, (Code exprYDecl exprYRef)) = effectfulAST' n1 ex
     in ( n2+2
        , Code (exprXDecl $$ exprYDecl $$ ("const" <+> (P.text $ 'n':show (n2+1)) <+> "=" <+> exprXRef) <> P.semi)
            (P.text ('n':show (n2+1)) <> P.parens exprYRef)
        )

pureAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Doc
pureAST = renderCode . snd . pureAST' 0

pureAST' :: forall v. Int -> Expr (Const Int) v 
   -> (Int, Code)
pureAST' !n0 = \case
  Literal v -> case v of
    ValueNumber d -> (n0,Code mempty (P.text $ showFFloat Nothing d ""))
    ValueArray xs ->       
      let foo :: Int -> [Value u] -> (Int, [Code])
          foo n'0 (x:xs') = 
            let (n'1, x') = pureAST' n'0 (Literal x)
                (n'2, cs) = foo n'1 xs'
             in (n'2, x' : cs)
          foo n' [] = (n', [])
          (n1, exprs) = foo n0 xs
          (exprDecls, exprRefs) = partitionCode exprs
       in (n1, Code (P.vcat exprDecls) $ P.brackets (P.hcat $ P.punctuate ", " exprRefs))
    ValueString s -> (n0, Code mempty $ P.doubleQuotes (P.text $ T.unpack s))
    ValueFunction _f -> undefined
    ValueUnit -> (n0, mempty) -- FIXME: is this correct
    ValueOption (Just x) -> pureAST' n0 (Literal x)
    ValueOption Nothing -> (n0, Code mempty "null") -- FIXME: is this correct
    ValueResult _ -> undefined
    ValueBool True -> (n0, Code mempty "true")
    ValueBool False -> (n0, Code mempty "false")
  Concat x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "+" <+> y1Ref)
  Plus x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "+" <+> y1Ref)
  Minus x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "-" <+> y1Ref)
  Times x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "*" <+> y1Ref)
  FracDiv x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "/" <+> y1Ref)
  Abs x ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
     in (n1, Code x1Decl $ "Math.abs" <> P.parens x1Ref)
  Sign x ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
     in (n1, Code x1Decl $ "Math.sign" <> P.parens x1Ref)
  Show x ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
     in (n1, Code x1Decl $ "String" <> P.parens x1Ref)
  Negate x ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
     in (n1, Code x1Decl $ "-" <> P.parens x1Ref)
  Lambda f ->
    let ex = f (Const n0)
        (n1, (Code exprXDecl exprXRef)) = pureAST' n0 ex
     in ( n1
        , Code exprXDecl 
            $ "function" 
            <+> P.parens (P.text $ 'n':show n0)
            <+> P.braces ("return" <+> (P.parens exprXRef))
        )
  And x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "&&" <+> y1Ref)
  Or x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "||" <+> y1Ref)
  Eq x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "===" <+> y1Ref)
  NEq x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "!==" <+> y1Ref)
  GTh x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> ">" <+> y1Ref)
  LTh x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "<" <+> y1Ref)
  GTEq x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> ">=" <+> y1Ref)
  LTEq x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "<=" <+> y1Ref)
  Let x g ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        constX = ("const" <+> P.text ('n':show n1) <+> "=" <+> x1Ref) <> P.semi
        (n2, (Code x2Decl x2Ref)) = pureAST' (n1 + 1) (g (Const n1))
     in (n2, Code (x1Decl $$ constX $$ x2Decl) (x2Ref))
  Apply fex ex ->
    let (n1, (Code exprXDecl exprXRef)) = pureAST' n0 fex
        (n2, (Code exprYDecl exprYRef)) = pureAST' n1 ex
     in ( n2+2
        , Code (exprXDecl $$ exprYDecl $$ ("const" <+> (P.text $ 'n':show (n2+1)) <+> "=" <+> exprXRef) <> P.semi)
            (P.text ('n':show (n2+1)) <> P.parens exprYRef)
        )
  Var (Const x) -> (n0, Code mempty $ P.text ('n':show x))

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

