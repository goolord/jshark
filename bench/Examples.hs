{-# LANGUAGE DataKinds #-}
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

import qualified Breakout
import JShark.Api (stmts)
import JShark.Api.Types (ClosedEffect, Universe (Unit))
import qualified Life
import Stages (emitLen, nfClosed, stageBenches)
import qualified Synth
import Test.Tasty.Bench
import qualified TodoMvc

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
breakout = stmts Breakout.mainJS
todoMvc = stmts TodoMvc.mainJS
synth = stmts Synth.mainJS
life = stmts Life.mainJS
