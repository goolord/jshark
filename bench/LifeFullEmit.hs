{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark.Api (stmts)
import JShark.Api.Types (ClosedEffect)
import qualified JShark.Api.Types as T
import Life (mainJS)
import Stages (emitLen)

life :: ClosedEffect T.Unit
life = stmts mainJS

main :: IO ()
main = do
  start <- getMonotonicTime
  let
    bytes = emitLen life
  end <- getMonotonicTime
  evaluate bytes
  putStrLn $ show bytes ++ "," ++ show (end - start)
