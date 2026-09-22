{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Compiler benches over each example's whole program (Life is the slow
-- path). Manual profiling only:
--
--   cabal bench jshark-examples-bench -- -p life
module Main (main) where

import Bench.Stages (stageBenches)
import JShark.Api.Types (ClosedEffect, Universe (Unit))
import JShark.Example.Registry (exampleMainJS)
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ stageBenches "breakout" breakout
    , stageBenches "todo-mvc" todoMvc
    , stageBenches "synth" synth
    , stageBenches "life" life
    ]

breakout, todoMvc, synth, life :: ClosedEffect 'Unit
breakout = exampleMainJS "breakout"
todoMvc = exampleMainJS "todo-mvc"
synth = exampleMainJS "synth"
life = exampleMainJS "life"
