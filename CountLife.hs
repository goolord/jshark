module Main where

import JShark
import JShark.Api
import JShark.Types
import Life

main :: IO ()
main = do
  let
    l = stmts Life.mainJS
  putStrLn $ "Life size (pre-opt): " ++ show (optimizedEffectSize l)
