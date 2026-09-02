{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

-- | Let/bind elimination during PHOAS optimize.
module JShark.Compiler.Optimize.Elim
  ( elimLetFrom
  , elimBindFrom
  )
where

import Data.Text (Text)
import JShark.Api.Types
import JShark.Compiler.Binder (Stamp (..))
import JShark.Compiler.Flatten
  ( inlineEff
  , inlineExpr
  , occursVarInEff
  , occursVarInExpr
  , rebindEff
  , rebindExpr
  )
import JShark.Compiler.Metadata (Metadata (..), mdIsCheap, mdIsPure, optSmall)
import JShark.Compiler.Optimize.Analysis
  ( elimEffUses
  , elimExprUses
  , isAliasBind
  , nodeCountEff
  , nodeCountExpr
  , pureEffect
  )

data ElimOps src body = ElimOps
  { elimCount :: Int -> body -> Metadata -> Int
  , elimPure :: Metadata -> Bool
  , elimCheap :: Metadata -> Bool
  , elimSize :: Metadata -> Int
  , elimRebuild :: body -> body
  , elimSplice :: Int -> (Int, body, Metadata)
  , elimDropUnused :: Metadata -> Bool
  , elimOccurs :: Int -> body -> Bool
  }

elimFrom ::
  (?keepLets :: Bool) =>
  Bool
  -> ElimOps src body
  -> Int
  -> Metadata
  -> Int
  -> body
  -> Metadata
  -> (Int, body, Metadata)
elimFrom preserveOnce ops t mdX tag body mdBody =
  let
    uses = elimCount ops tag body mdBody
    kept = elimRebuild ops body
    inlined
      | elimSize ops mdBody > optSmall = (t, kept, mdBody)
      | otherwise = elimSplice ops t
   in
    case uses of
      0
        | elimPure ops mdX
        , elimDropUnused ops mdX
        , not (elimOccurs ops tag body) ->
            (t, body, mdBody)
      0 -> (t, kept, mdBody)
      1
        | ?keepLets && preserveOnce ->
            (t, kept, mdBody)
      1 -> inlined
      _ | elimCheap ops mdX -> inlined
      _ -> (t, kept, mdBody)

isLambdaExpr :: Expr f u -> Bool
isLambdaExpr = \case
  Lambda {} -> True
  _ -> False

isLambdaEff :: Effect f u -> Bool
isLambdaEff = \case
  LambdaE {} -> True
  _ -> False

isIdentityExpr :: Int -> Expr Stamp u -> Bool
isIdentityExpr tag = \case
  Var (Stamp i) -> i == tag
  Var (Embed e) -> isIdentityExpr tag e
  Var (EmbedEff (Lift e)) -> isIdentityExpr tag e
  _ -> False

isIdentityEff :: Int -> Effect Stamp u -> Bool
isIdentityEff tag = \case
  Lift e -> isIdentityExpr tag e
  _ -> False

elimLetFrom ::
  (?keepLets :: Bool) =>
  (Int -> Expr Stamp v -> (Int, Expr Stamp v, Metadata))
  -> Int
  -> Maybe Text
  -> Expr Stamp u
  -> Metadata
  -> (Stamp u -> Expr Stamp v)
  -> Int
  -> Expr Stamp v
  -> Metadata
  -> (Int, Expr Stamp v, Metadata)
elimLetFrom optExpr t hint x mdX f tag body mdBody =
  elimFrom
    (not (isLambdaExpr x) && not (isIdentityExpr tag body))
    ElimOps
      { elimCount = elimExprUses
      , elimPure = mdIsPure
      , elimCheap = mdIsCheap
      , elimSize = \_ -> nodeCountExpr body
      , elimRebuild = Let hint x . rebindExpr tag
      , elimSplice = \t' -> optExpr t' (inlineExpr f x)
      , elimDropUnused = const True
      , elimOccurs = occursVarInExpr
      }
    t
    mdX
    tag
    body
    mdBody

elimBindFrom ::
  (?keepLets :: Bool) =>
  (Int -> Effect Stamp v -> (Int, Effect Stamp v, Metadata))
  -> Int
  -> Maybe Text
  -> Effect Stamp u
  -> Metadata
  -> (Stamp u -> Effect Stamp v)
  -> Int
  -> Effect Stamp v
  -> Metadata
  -> (Int, Effect Stamp v, Metadata)
elimBindFrom optEffect t hint x mdX f tag body mdBody =
  elimFrom
    ( not (isLambdaEff x)
        && not (isIdentityEff tag body)
        && not (isAliasBind x)
    )
    ElimOps
      { elimCount = elimEffUses
      , elimPure = const (pureEffect x)
      , elimCheap = mdIsCheap
      , elimSize = \_ -> nodeCountEff body
      , elimRebuild = Bind hint x . rebindEff tag
      , elimSplice = \t' -> optEffect t' (inlineEff f x)
      , elimDropUnused = \_ -> not (isAliasBind x)
      , elimOccurs = occursVarInEff
      }
    t
    mdX
    tag
    body
    mdBody
