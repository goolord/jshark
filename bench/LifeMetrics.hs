{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark
  ( effectfulASTFromPrepared
  , flatPrepareCore
  , flatProgramNodeCount
  , nodeCountEff
  , optIrLargeThreshold
  , optimizeEffectFromIr
  , renderJSCompact
  )
import JShark.Api (stmts)
import JShark.CompileTiming (FlatPrepareTiming (..), seconds)
import qualified JShark.Ir as Ir
import JShark.Types (ClosedEffect, Universe (Unit))
import Life (mainJS)

life :: ClosedEffect Unit
life = stmts mainJS

runOptimizeFromIr :: Ir.IrEffect Unit -> Int
runOptimizeFromIr ir = nodeCountEff (optimizeEffectFromIr ir)
{-# NOINLINE runOptimizeFromIr #-}

main :: IO ()
main = do
  putStrLn $ "irThreshold," ++ show optIrLargeThreshold
  (prog, FlatPrepareTiming {..}, irNodes, irOpt) <- flatPrepareCore life
  evaluate prog
  putStrLn $ "rawNodes," ++ show irNodes
  t0 <- getMonotonicTime
  let
    optNodes = runOptimizeFromIr irOpt
  t1 <- getMonotonicTime
  evaluate optNodes
  putStrLn $ "phoasOptimize," ++ show (seconds t0 t1)
  putStrLn $ "optNodes," ++ show optNodes
  putStrLn $ "flatLower," ++ show fptLowerSec
  putStrLn $ "flatIrOpt," ++ show fptIrOptSec
  putStrLn $ "flatPack," ++ show fptPackSec
  putStrLn $ "flatOpt," ++ show fptFlatOptSec
  putStrLn $ "flatPrepare," ++ show fptTotalSec
  putStrLn $ "flatNodes," ++ show (flatProgramNodeCount prog)
  t2 <- getMonotonicTime
  js <- evaluate $ renderJSCompact (effectfulASTFromPrepared prog)
  t3 <- getMonotonicTime
  putStrLn $ "flatEmit," ++ show (seconds t2 t3)
  putStrLn $ "jsBytes," ++ show (T.length js)
