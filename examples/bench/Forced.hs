{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import JShark
import JShark.Api
import qualified JShark.Example.Life as Life
import System.Environment (getArgs)
import Test.Tasty.Bench

-- Avoid GHC constant folding by using a function that depends on runtime input
forcedOptimize :: Int -> ClosedEffect 'Unit -> Int
forcedOptimize n e = if n > 0 then optimizedEffectSize e else 0

main :: IO ()
main = do
  _ <- getArgs
  let
    l = stmts Life.mainJS
  defaultMain
    [ bench "life-forced" $ nf (\n -> forcedOptimize n l) 1
    ]
