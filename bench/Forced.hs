
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified Life
import JShark
import JShark.Api
import Test.Tasty.Bench
import System.Environment (getArgs)

-- Avoid GHC constant folding by using a function that depends on runtime input
forcedOptimize :: Int -> ClosedEffect 'Unit -> Int
forcedOptimize n e = if n > 0 then optimizedEffectSize e else 0

main :: IO ()
main = do
  _ <- getArgs
  let l = stmts Life.mainJS
  defaultMain
    [ bench "life-forced" $ nf (\n -> forcedOptimize n l) 1
    ]
