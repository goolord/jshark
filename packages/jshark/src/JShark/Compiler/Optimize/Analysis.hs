{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

-- | Static analysis for PHOAS optimize and codegen bind probes.
module JShark.Compiler.Optimize.Analysis
  ( bindProbeTag
  , letProbeTag
  , elimExprUses
  , elimEffUses
  , nodeCountExpr
  , nodeCountEff
  , cheapExpr
  , cheapEffect
  , pureExpr
  , pureEffect
  , isAliasBind
  )
where

import Data.Monoid (All (..), Sum (..))
import JShark.Api.Prim (isPureFixed)
import JShark.Api.Types
import JShark.Compiler.Binder (Stamp (..), nestedDummy)
import JShark.Compiler.Evaluate (isCheapValue)
import JShark.Compiler.Flatten
  ( foldEff
  , foldExpr
  , occursVarInEff
  , occursVarInExpr
  )
import JShark.Compiler.Metadata (Metadata (..))

countLazyExpr :: Int -> Expr Stamp u -> Int
countLazyExpr t e = if occursVarInExpr t e then 2 else 0
{-# NOINLINE countLazyExpr #-}

countLazyEffect :: Int -> Effect Stamp u -> Int
countLazyEffect t e = if occursVarInEff t e then 2 else 0
{-# NOINLINE countLazyEffect #-}

countExpr :: Int -> Expr Stamp u -> Int
countExpr t e = case e of
  Var (Stamp i) -> if i == t then 1 else 0
  Var (Embed e') -> countExpr t e'
  Var (EmbedEff e') -> countEffect t e'
  _ ->
    getSum
      ( foldExpr
          nestedDummy
          (Sum . countExpr t)
          (Sum . countLazyExpr t)
          (Sum . countEffect t)
          e
      )

countEffect :: Int -> Effect Stamp u -> Int
countEffect t e =
  getSum
    ( foldEff
        nestedDummy
        (Sum . countExpr t)
        (Sum . countEffect t)
        (Sum . countLazyEffect t)
        e
    )

effectBindUses :: Int -> Effect Stamp u -> Int
effectBindUses tag e =
  let
    n = countEffect tag e
   in
    if n == 0 && occursVarInEff tag e then 2 else n

bindProbeTag :: Int -> Effect Stamp u -> (Int, Bool)
bindProbeTag probeTag tagged =
  (probeTag, occursVarInEff probeTag tagged)

letProbeTag :: Int -> Expr Stamp u -> (Int, Int)
letProbeTag probeTag tagged =
  (probeTag, elimExprUses probeTag tagged mempty)

elimExprUses :: Int -> Expr Stamp v -> Metadata -> Int
elimExprUses tag body _ =
  let
    n = countExpr tag body
   in
    if n == 0 && occursVarInExpr tag body then 1 else n

elimEffUses :: Int -> Effect Stamp v -> Metadata -> Int
elimEffUses tag body _ = effectBindUses tag body

nodeCountExpr :: Expr Stamp u -> Int
nodeCountExpr expr = case expr of
  Var (Embed e') -> nodeCountExpr e'
  Var (EmbedEff e') -> nodeCountEff e'
  e ->
    1
      + getSum
        ( foldExpr
            nestedDummy
            (Sum . nodeCountExpr)
            (Sum . nodeCountExpr)
            (Sum . nodeCountEff)
            e
        )

nodeCountEff :: Effect Stamp u -> Int
nodeCountEff e =
  1
    + getSum
      ( foldEff
          nestedDummy
          (Sum . nodeCountExpr)
          (Sum . nodeCountEff)
          (Sum . nodeCountEff)
          e
      )

cheapExpr :: Expr Stamp u -> Bool
cheapExpr = \case
  Literal v -> isCheapValue v
  Var (Embed e') -> cheapExpr e'
  Var (EmbedEff e') -> cheapEffect e'
  Var _ -> True
  e ->
    let
      here = case e of
        UnsafeNullable {} -> True
        GetField {} -> True
        _ -> False
     in
      here
        && getAll
          ( foldExpr
              nestedDummy
              (All . cheapExpr)
              (const mempty)
              (All . cheapEffect)
              e
          )

cheapEffect :: Effect Stamp u -> Bool
cheapEffect e =
  let
    here = case e of
      Lift {} -> True
      _ -> False
   in
    here
      && getAll
        ( foldEff
            nestedDummy
            (All . cheapExpr)
            (All . cheapEffect)
            (const mempty)
            e
        )

pureExpr :: Expr Stamp u -> Bool
pureExpr = \case
  Literal _ -> True
  Var (Embed e') -> pureExpr e'
  Var (EmbedEff e') -> pureEffect e'
  Var _ -> True
  e ->
    let
      here = case e of
        Std (Fixed op _) -> isPureFixed op
        _ -> True
     in
      here
        && getAll
          ( foldExpr
              nestedDummy
              (All . pureExpr)
              (const mempty)
              (All . pureEffect)
              e
          )

isAliasBind :: Effect Stamp u -> Bool
isAliasBind (Lift (Var (EmbedEff e))) = isAliasBind e
isAliasBind (Lift (Var _)) = True
isAliasBind (Lift (UnsafeNullable (Var _))) = True
isAliasBind _ = False

pureEffect :: Effect Stamp u -> Bool
pureEffect e =
  let
    here = case e of
      FFI {} -> False
      UnsafeObjectGet {} -> False
      UnsafeObjectAssign {} -> False
      CallMethod {} -> False
      ApplyE {} -> False
      While {} -> False
      ForRange {} -> False
      U8Set {} -> False
      U8Fill {} -> False
      Throw {} -> False
      Try {} -> False
      DeleteProp {} -> False
      _ -> True
   in
    here
      && getAll
        ( foldEff
            nestedDummy
            (All . pureExpr)
            (All . pureEffect)
            (const mempty)
            e
        )
