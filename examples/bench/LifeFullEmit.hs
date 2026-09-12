{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Bench.Stages (emitLen)
import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark.Api (stmts)
import JShark.Api.Types (ClosedEffect)
import qualified JShark.Api.Types as T
import JShark.Example.Life (mainJS)

life :: ClosedEffect T.Unit
life = stmts mainJS

main :: IO ()
main = do
  start <- getMonotonicTime
  bytes <- evaluate (emitLen life)
  end <- getMonotonicTime
  putStrLn $ show bytes ++ "," ++ show (end - start)
