{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | The four showcase examples as a single registry: canonical label
-- order, each example's whole client program, and ready-made compile
-- jobs. The dev server, the offline compiler, and the example benches
-- all consume this so the example set is only written down once.
module JShark.Example.Registry
  ( exampleLabels
  , exampleJobs
  , exampleMainJS
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import JShark.Api.Types (ClosedEffect, Universe (Unit), fromSyntax)
import JShark.Compiler (CompilerConfig)
import qualified JShark.Example.Breakout as Breakout
import qualified JShark.Example.Life as Life
import qualified JShark.Example.Synth as Synth
import qualified JShark.Example.TodoMvc as TodoMvc

-- | Example labels in canonical compile order.
exampleLabels :: [Text]
exampleLabels = ["breakout", "todo-mvc", "synth", "life"]

-- | Each example's whole client program, keyed by label
-- (total over 'exampleLabels', errors on anything else).
exampleMainJS :: Text -> ClosedEffect 'Unit
exampleMainJS = \case
  "breakout" -> fromSyntax Breakout.mainJS
  "todo-mvc" -> fromSyntax TodoMvc.mainJS
  "synth" -> fromSyntax Synth.mainJS
  "life" -> fromSyntax Life.mainJS
  other -> error ("JShark.Example.Registry: unknown example " <> T.unpack other)

-- | One @(label, config, program)@ compile job per example, in
-- canonical order.
exampleJobs :: CompilerConfig -> [(Text, CompilerConfig, ClosedEffect 'Unit)]
exampleJobs cfg = [(label, cfg, exampleMainJS label) | label <- exampleLabels]
