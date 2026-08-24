{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Full-example compiler benches. Life codegen is the slow path;
-- the other examples are here as a comparison.
--
--   cabal bench jshark-compiler-examples -- jshark-compiler-examples -p life
--   cabal bench jshark-compiler-examples -- jshark-compiler-examples -p 'life/optimize'
module Main (main) where

import qualified Breakout
import JShark.Api (stmts)
import JShark.Types (ClosedEffect, Universe (Unit))
import qualified Life
import Stages (emit, nfClosed, stageBenches)
import qualified Synth
import Test.Tasty.Bench
import qualified TodoMvc

main :: IO ()
main =
  defaultMain
    [ bgroup
        "emit"
        [ bench "breakout" $ nfClosed emit breakout
        , bench "todo-mvc" $ nfClosed emit todoMvc
        , bench "synth" $ nfClosed emit synth
        , bench "life" $ nfClosed emit life
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
