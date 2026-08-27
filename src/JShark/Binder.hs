{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}

-- | PHOAS binder tags and literal-peeling helpers for opt/codegen.
module JShark.Binder
  ( Stamp (..)
  , pattern Name
  , stampId
  , peelResult
  , peelOption
  , peelBoolEffect
  , peelString
  , nestedDummyId
  , nestedDummy
  )
where

import Data.Text (Text)
import JShark.Types

-- | Optimizer / codegen name. 'Stamp' is an untyped tag for use-counting.
-- 'Embed' / 'EmbedEff' are typed hole fillers for bind inlining.
data Stamp (u :: Universe) where
  Stamp :: Int -> Stamp u
  Embed :: Expr Stamp u -> Stamp u
  EmbedEff :: Effect Stamp u -> Stamp u

-- | Codegen / dummy binder. Same as 'Stamp'; kept so call sites that
-- only need a name stay readable.
pattern Name :: Int -> Stamp u
pattern Name i = Stamp i

{-# COMPLETE Stamp, Embed, EmbedEff #-}

stampId :: Stamp u -> Int
stampId (Stamp i) = i
stampId (Embed _) = error "JShark.stampId: Embed (flatten first)"
stampId (EmbedEff _) = error "JShark.stampId: EmbedEff (flatten first)"

peelResult ::
  Expr Stamp ('Result e a) -> Maybe (Either (Expr Stamp e) (Expr Stamp a))
peelResult = \case
  Literal (ValueResult (Left v)) -> Just (Left (Literal v))
  Literal (ValueResult (Right v)) -> Just (Right (Literal v))
  ResultOk x -> Just (Right x)
  ResultErr x -> Just (Left x)
  _ -> Nothing

-- | Tag equality is binder identity; each tag is allocated at one @u@.
peelOption :: Expr Stamp ('Option u) -> Maybe (Maybe (Expr Stamp u))
peelOption = \case
  Literal (ValueOption Nothing) -> Just Nothing
  Literal (ValueOption (Just v)) -> Just (Just (Literal v))
  -- Host literals are never JS null. FFI / vars stay unpeeled so
  -- 'Storage.getItem' keeps its @=== null@ check.
  UnsafeNullable (Literal v) -> Just (Just (Literal v))
  _ -> Nothing

peelBoolEffect :: Effect Stamp 'Bool -> Maybe Bool
peelBoolEffect (Lift (Literal (ValueBool b))) = Just b
peelBoolEffect _ = Nothing

peelString :: Expr Stamp 'String -> Maybe Text
peelString (Literal (ValueString s)) = Just s
peelString _ = Nothing

nestedDummyId :: Int
nestedDummyId = minBound

nestedDummy :: Stamp u
nestedDummy = Name nestedDummyId
