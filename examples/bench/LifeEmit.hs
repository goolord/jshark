{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Wall-clock timing for lifeStep emit (not CAF-safe for tasty-bench).
module Main (main) where

import Bench.Stages (emitLen)
import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark.Api
import JShark.Api.Types (ClosedEffect)
import qualified JShark.Api.Types as T
import JShark.Example.Life.Grid (CellGrids (..), StepRegion (..))
import JShark.Example.Life.LifeTestSupport
  ( newCellGrids
  , runStepGridOnce
  , seedBlock
  )

lifeStep :: ClosedEffect T.Unit
lifeStep = fromSyntax $ do
  (cells, region) <- newCellGrids (number 8) (number 8)
  seedBlock (cgAlive cells) (srW region) (srH region)
  _ <- runStepGridOnce cells region
  done

main :: IO ()
main = do
  start <- getMonotonicTime
  bytes <- evaluate (emitLen lifeStep)
  end <- getMonotonicTime
  putStrLn $ show bytes ++ "," ++ show (end - start)
