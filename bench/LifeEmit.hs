{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Wall-clock timing for lifeStep emit (not CAF-safe for tasty-bench).
module Main (main) where

import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark.Api
import JShark.Api.Types (ClosedEffect)
import qualified JShark.Api.Types as T
import LifeTestSupport (runStepGridOnce, seedBlock)
import Stages (emitLen)

lifeStep :: ClosedEffect T.Unit
lifeStep = fromSyntax $ do
  let
    w = number 8
    h = number 8
  alive <- bindExpr (newByteArray (w * h))
  species <- bindExpr (newByteArray (w * h))
  nextAlive <- bindExpr (newByteArray (w * h))
  nextSpecies <- bindExpr (newByteArray (w * h))
  seedBlock alive w h
  _ <-
    runStepGridOnce
      alive
      species
      nextAlive
      nextSpecies
      w
      h
      (number 0)
      (number 0)
      (number 7)
      (number 7)
  done

main :: IO ()
main = do
  start <- getMonotonicTime
  let
    bytes = emitLen lifeStep
  end <- getMonotonicTime
  evaluate bytes
  putStrLn $ show bytes ++ "," ++ show (end - start)
