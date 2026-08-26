{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | Wall-clock breakdown for flat and PHOAS compile prepare paths.
--   Enable stderr logging with @JSHARK_COMPILE_TIMING=1@.
module JShark.CompileTiming
  ( FlatPrepareTiming (..)
  , PhoasPrepareTiming (..)
  , timingEnabled
  , reportFlatPrepareTiming
  , reportPhoasPrepareTiming
  , seconds
  )
where

import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

data FlatPrepareTiming = FlatPrepareTiming
  { fptLowerSec :: !Double
  , fptIrOptSec :: !Double
  , fptPackSec :: !Double
  , fptFlatOptSec :: !Double
  , fptTotalSec :: !Double
  }
  deriving (Eq, Show)

data PhoasPrepareTiming = PhoasPrepareTiming
  { pptOptimizeSec :: !Double
  , pptTotalSec :: !Double
  }
  deriving (Eq, Show)

timingEnabled :: IO Bool
timingEnabled = maybe False (const True) <$> lookupEnv "JSHARK_COMPILE_TIMING"

reportFlatPrepareTiming :: FlatPrepareTiming -> IO ()
reportFlatPrepareTiming t = do
  ok <- timingEnabled
  when ok $
    hPutStrLn stderr $
      unlines
        [ "JShark flat prepare timing (seconds):"
        , "  lower:    " ++ show (fptLowerSec t)
        , "  ir-opt:   " ++ show (fptIrOptSec t)
        , "  pack:     " ++ show (fptPackSec t)
        , "  flat-opt: " ++ show (fptFlatOptSec t)
        , "  total:    " ++ show (fptTotalSec t)
        ]

reportPhoasPrepareTiming :: PhoasPrepareTiming -> IO ()
reportPhoasPrepareTiming t = do
  ok <- timingEnabled
  when ok $
    hPutStrLn stderr $
      unlines
        [ "JShark PHOAS prepare timing (seconds):"
        , "  optimize: " ++ show (pptOptimizeSec t)
        , "  total:    " ++ show (pptTotalSec t)
        ]

seconds :: Double -> Double -> Double
seconds t0 t1 = t1 - t0

when :: Bool -> IO () -> IO ()
when b io = if b then io else pure ()
