
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Life
import JShark
import JShark.Api
import System.CPUTime
import Text.Printf

main :: IO ()
main = do
  let l = stmts Life.mainJS
  start <- getCPUTime
  let !size = optimizedEffectSize l
  end <- getCPUTime
  let diff = fromIntegral (end - start) / 1e12 :: Double
  printf "Life size: %d\n" size
  printf "Optimization time: %0.3f sec\n" diff
