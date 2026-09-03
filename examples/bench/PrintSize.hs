{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Routing evidence for 'optIrLargeThreshold': how big Life is, and what
-- each optimizer path costs on it.
--
--   cabal run print-size
module Main (main) where

import qualified Control.Exception as Ex
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import JShark
import JShark.Api
import qualified JShark.Example.Life as Life
import System.Timeout (timeout)
import Text.Printf

-- | Seconds, or Nothing when the stage did not finish in time.
timeStage :: Int -> (() -> Int) -> IO (Maybe (Double, Int))
timeStage limitSec f = do
  t0 <- getMonotonicTime
  res <- timeout (limitSec * 1000000) (Ex.evaluate (f ()))
  t1 <- getMonotonicTime
  pure (fmap (\n -> (t1 - t0, n)) res)

timePath :: Int -> (() -> T.Text) -> IO (Maybe (Double, Int))
timePath limitSec f = timeStage limitSec (T.length . f)

report :: String -> Maybe (Double, Int) -> IO ()
report name = \case
  Nothing -> printf "%-14s TIMEOUT\n" name
  Just (secs, n) -> printf "%-14s %8.3f sec  %7d chars\n" name secs n

main :: IO ()
main = do
  let
    l = stmts Life.mainJS
  t0 <- getMonotonicTime
  !nodes <- Ex.evaluate (closedEffectNodes l)
  t1 <- getMonotonicTime
  printf "Life nodes:    %d  (counted in %.3f sec)\n" nodes (t1 - t0)
  printf "IR threshold:  %d\n" optIrLargeThreshold

  irOpt <- timeStage 60 (\() -> optimizedEffectSize l)
  report "ir optimize" irOpt
  -- Walking with 'nestedDummy' takes the identity branch of every
  -- rebind closure, so this is reify plus one traversal and no renames.
  reify <- timeStage 60 (\() -> nodeCountEff (optimizeEffectIr l))
  report "ir reify" reify
  ir <- timePath 60 (\() -> renderJSCompact (effectfulASTIr l))
  report "ir + codegen" ir
  phoa <- timePath 60 (\() -> renderJSCompact (effectfulAST l))
  report "phoa + codegen" phoa
  case phoa of
    Just (_, nPhoa) ->
      case ir of
        Just (_, nIr) ->
          printf
            "parity:        %s (%d vs %d chars)\n"
            (if nPhoa == nIr then "ok" else "MISMATCH")
            nPhoa
            nIr
        Nothing -> pure ()
    Nothing -> pure ()
