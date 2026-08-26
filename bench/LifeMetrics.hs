{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark
  ( closedEffectNodes
  , effectfulASTFromFlat
  , flatPrepareCore
  , flatProgramNodeCount
  , nodeCountEff
  , optimizeEffect
  , optIrLargeThreshold
  , renderJSCompact
  )
import JShark.Api (stmts)
import JShark.CompileTiming (FlatPrepareTiming (..), seconds)
import JShark.Types (ClosedEffect, Universe (Unit))
import Life (mainJS)
import qualified Data.Text as T

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
  putStrLn $ "irThreshold," ++ show optIrLargeThreshold
  putStrLn $ "rawNodes," ++ show (runRawNodes life)
  t0 <- getMonotonicTime
  let
    optNodes = runOptimize life
  t1 <- getMonotonicTime
  evaluate optNodes
  putStrLn $ "phoasOptimize," ++ show (seconds t0 t1)
  putStrLn $ "optNodes," ++ show optNodes
  (prog, FlatPrepareTiming {..}) <- flatPrepareCore life
  evaluate prog
  putStrLn $ "flatLower," ++ show fptLowerSec
  putStrLn $ "flatIrOpt," ++ show fptIrOptSec
  putStrLn $ "flatPack," ++ show fptPackSec
  putStrLn $ "flatOpt," ++ show fptFlatOptSec
  putStrLn $ "flatPrepare," ++ show fptTotalSec
  putStrLn $ "flatNodes," ++ show (flatProgramNodeCount prog)
  t2 <- getMonotonicTime
  js <- evaluate $ renderJSCompact (effectfulASTFromFlat life)
  t3 <- getMonotonicTime
  putStrLn $ "flatEmit," ++ show (seconds t2 t3)
  putStrLn $ "jsBytes," ++ show (T.length js)
