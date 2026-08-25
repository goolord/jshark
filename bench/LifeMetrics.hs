{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark (closedEffectNodes, nodeCountEff, optimizeEffect)
import JShark.Api (stmts)
import JShark.Types (ClosedEffect, Universe (Unit))
import Life (mainJS)

life :: ClosedEffect Unit
life = stmts mainJS

runRawNodes :: ClosedEffect Unit -> Int
runRawNodes = closedEffectNodes
{-# NOINLINE runRawNodes #-}

runOptimize :: ClosedEffect Unit -> Int
runOptimize e = nodeCountEff (optimizeEffect e)
{-# NOINLINE runOptimize #-}

main :: IO ()
main = do
  putStrLn $ "rawNodes," ++ show (runRawNodes life)
  t0 <- getMonotonicTime
  let
    optNodes = runOptimize life
  t1 <- getMonotonicTime
  evaluate optNodes
  putStrLn $ "optimize," ++ show (t1 - t0)
  putStrLn $ "optNodes," ++ show optNodes
