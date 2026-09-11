{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Full-example compiler benches — the only target that exercises each
-- example's whole AST (Life is the slow path).
--
-- Manual profiling only (not CI-gated):
--
--   cabal bench jshark-compiler-examples -- jshark-compiler-examples -p life
--   cabal bench jshark-compiler-examples -- jshark-compiler-examples -p 'life/optimize'
--   cabal bench jshark-compiler-examples -- jshark-compiler-examples -p 'life/compileEffect'
module Main (main) where

import Bench.Stages (emitLen, nfClosed, stageBenches)
import JShark.Api.Types (ClosedEffect, Universe (Unit))
import JShark.Example.Registry (exampleMainJS)
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ bgroup
        "emit"
        [ bench "breakout" $ nfClosed emitLen breakout
        , bench "todo-mvc" $ nfClosed emitLen todoMvc
        , bench "synth" $ nfClosed emitLen synth
        , bench "life" $ nfClosed emitLen life
        ]
    , stageBenches "breakout" breakout
    , stageBenches "todo-mvc" todoMvc
    , stageBenches "synth" synth
    , stageBenches "life" life
    ]

breakout, todoMvc, synth, life :: ClosedEffect 'Unit
breakout = exampleMainJS "breakout"
todoMvc = exampleMainJS "todo-mvc"
synth = exampleMainJS "synth"
life = exampleMainJS "life"
