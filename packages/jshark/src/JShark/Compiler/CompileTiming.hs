{-# LANGUAGE BangPatterns #-}

-- | Wall-clock timing for the flat-prepare path. Enable stderr logging
--   with @JSHARK_COMPILE_TIMING=1@.
--
-- Internal to the JShark compiler; this module is exposed for tests and
-- tooling and its API may change between 0.x releases.
module JShark.Compiler.CompileTiming
  ( FlatPrepareTiming (..)
  , reportFlatPrepareTiming
  , seconds
  )
where

import Control.Monad (when)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

-- | Wall-clock (monotonic) breakdown of the flat-prepare path.
data FlatPrepareTiming = FlatPrepareTiming
  { fptIrPrepareSec :: !Double
  , fptPackSec :: !Double
  , fptFlatOptSec :: !Double
  , fptTotalSec :: !Double
  }
  deriving (Eq, Show)

timingEnabled :: IO Bool
timingEnabled = maybe False (const True) <$> lookupEnv "JSHARK_COMPILE_TIMING"

reportFlatPrepareTiming :: FlatPrepareTiming -> IO ()
reportFlatPrepareTiming t = do
  ok <- timingEnabled
  when ok
    $ hPutStrLn stderr
    $ unlines
      [ "JShark flat prepare timing (seconds):"
      , "  ir-prep:  " ++ show (fptIrPrepareSec t)
      , "  pack:     " ++ show (fptPackSec t)
      , "  flat-opt: " ++ show (fptFlatOptSec t)
      , "  total:    " ++ show (fptTotalSec t)
      ]

seconds :: Double -> Double -> Double
seconds t0 t1 = t1 - t0
