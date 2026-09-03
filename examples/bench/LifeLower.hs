{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import JShark (profileLowerFromClosed)
import JShark.Api (stmts)
import JShark.Api.Types (ClosedEffect, Universe (Unit))
import JShark.Compiler.CompileTiming (LowerProfile (..))
import JShark.Example.Life (mainJS)

life :: ClosedEffect Unit
life = stmts mainJS

printProfile :: LowerProfile -> IO ()
printProfile LowerProfile {..} = do
  putStrLn "phase,seconds,detail"
  putStrLn $
    "lazyLower,"
      ++ show lopLazySec
      ++ ",whnfOnly=1"
  putStrLn $
    "forceLower,"
      ++ show lopForceSec
      ++ ",rawNodes="
      ++ show lopRawNodes
  putStrLn $ "lowerTotal," ++ show lopTotalSec ++ ",nodes=" ++ show lopRawNodes

main :: IO ()
main = do
  profile <- profileLowerFromClosed life
  printProfile profile
