{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import JShark (irOptimizedEffectFromClosed, profileFlatOptFromIr)
import JShark.Api (stmts)
import JShark.Api.Types (ClosedEffect, Universe (Unit))
import JShark.Compiler.CompileTiming (FlatOptProfile (..))
import JShark.Example.Life (mainJS)

life :: ClosedEffect Unit
life = stmts mainJS

printProfile :: FlatOptProfile -> IO ()
printProfile FlatOptProfile {..} = do
  putStrLn "phase,seconds,detail"
  putStrLn $
    "constantFoldPar,"
      ++ show fopFoldSec
      ++ ",passes="
      ++ show fopFoldPasses
      ++ " folded="
      ++ show fopFolded
  putStrLn $ "constantFoldSeq," ++ show fopFoldSeqSec ++ ",one pass"
  putStrLn $
    "propagatePure,"
      ++ show fopPureSec
      ++ ",passes="
      ++ show fopPurePasses
      ++ " pure="
      ++ show fopPureCount
  putStrLn $ "attachPure," ++ show fopAttachSec ++ ",nodes=" ++ show fopNodeCount
  putStrLn $ "flatOptTotal," ++ show fopTotalSec ++ ",nodes=" ++ show fopNodeCount

main :: IO ()
main = do
  let
    !irOpt = irOptimizedEffectFromClosed life
  profile <- profileFlatOptFromIr irOpt
  printProfile profile
