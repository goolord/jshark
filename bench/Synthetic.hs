{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JShark
import JShark.Api
import Test.Tasty.Bench

-- A long chain of binds:
-- do
--   x1 <- 1
--   x2 <- 1
--   ...
--   xN <- 1
--   x1 + x2 + ... + xN
longChain :: Int -> ClosedEffect 'Number
longChain n = fromSyntax $ do
  vars <- mapM (\_ -> toSyntax (Lift (number 1))) [1..n]
  toSyntax $ Lift (sum (map Var vars))

main :: IO ()
main = defaultMain
  [ bgroup "longChain"
    [ bench "n=5"  $ nf (\n -> optimizedEffectSize (longChain n)) 5
    , bench "n=10"  $ nf (\n -> optimizedEffectSize (longChain n)) 10
    , bench "n=15"  $ nf (\n -> optimizedEffectSize (longChain n)) 15
    -- , bench "n=100" $ nf (\n -> optimizedEffectSize (longChain n)) 100
    -- , bench "n=500" $ nf (\n -> optimizedEffectSize (longChain n)) 500
    ]
  ]
