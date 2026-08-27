{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark
  ( effectfulASTFromSoA
  , flatPrepareCore
  , flatSoaNodeCount
  , optIrLargeThreshold
  , phoasNodeCountFromIr
  , renderJSCompact
  )
import JShark.Api (stmts)
import JShark.CompileTiming (FlatPrepareTiming (..), seconds)
import JShark.Types (ClosedEffect, Universe (Unit))
import Life (mainJS)

life :: ClosedEffect Unit
life = stmts mainJS

main :: IO ()
main = do
  putStrLn $ "irThreshold," ++ show optIrLargeThreshold
  (soa, FlatPrepareTiming {..}, irNodes, irOpt) <- flatPrepareCore life
  evaluate soa
  putStrLn $ "rawNodes," ++ show irNodes
  t0 <- getMonotonicTime
  let
    optNodes = phoasNodeCountFromIr irOpt
  t1 <- getMonotonicTime
  evaluate optNodes
  putStrLn $ "phoasOptimize," ++ show (seconds t0 t1)
  putStrLn $ "optNodes," ++ show optNodes
  putStrLn $ "flatIrPrepare," ++ show fptIrPrepareSec
  putStrLn $ "flatPack," ++ show fptPackSec
  putStrLn $ "flatOpt," ++ show fptFlatOptSec
  putStrLn $ "flatPrepare," ++ show fptTotalSec
  putStrLn $ "flatNodes," ++ show (flatSoaNodeCount soa)
  t2 <- getMonotonicTime
  js <- evaluate $ renderJSCompact (effectfulASTFromSoA soa)
  t3 <- getMonotonicTime
  putStrLn $ "flatEmit," ++ show (seconds t2 t3)
  putStrLn $ "jsBytes," ++ show (T.length js)
