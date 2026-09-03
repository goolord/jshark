{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import JShark (profileIrOptFromClosed)
import JShark.Api (stmts)
import JShark.Api.Types (ClosedEffect, Universe (Unit))
import JShark.Compiler.CompileTiming (IrOptProfile (..))
import JShark.Example.Life (mainJS)

life :: ClosedEffect Unit
life = stmts mainJS

printProfile :: IrOptProfile -> IO ()
printProfile IrOptProfile {..} = do
  putStrLn "phase,seconds,detail"
  putStrLn $
    "lower,"
      ++ show iopLowerSec
      ++ ",lazyWhnf diagnostic=1"
  putStrLn $
    "prepare,"
      ++ show iopPrepareSec
      ++ ",opt+meta optNodes="
      ++ show iopOptNodes
      ++ " matchesFlatIrPrepare=1"
  putStrLn $ "irOptTotal," ++ show iopTotalSec ++ ",nodes=" ++ show iopOptNodes
  putStrLn "phase,seconds,detail,breakdownOnForcedRaw"
  putStrLn $
    "metaRaw,"
      ++ show iopMetaRawSec
      ++ ",rawNodes="
      ++ show iopRawNodes
  putStrLn $
    "opt,"
      ++ show iopOptSec
      ++ ",optNodes="
      ++ show iopOptNodes
  putStrLn $
    "metaOpt,"
      ++ show iopMetaOptSec
      ++ ",optNodes="
      ++ show iopOptNodes

main :: IO ()
main = do
  profile <- profileIrOptFromClosed life
  printProfile profile
