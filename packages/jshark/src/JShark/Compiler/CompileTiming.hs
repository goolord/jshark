{-# LANGUAGE BangPatterns #-}

-- | Wall-clock breakdown for flat and PHOAS compile prepare paths.
--   Enable stderr logging with @JSHARK_COMPILE_TIMING=1@.
module JShark.Compiler.CompileTiming
  ( FlatPrepareTiming (..)
  , FlatOptProfile (..)
  , IrOptProfile (..)
  , LowerProfile (..)
  , reportFlatPrepareTiming
  , seconds
  )
where

import Control.Monad (when)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

data FlatPrepareTiming = FlatPrepareTiming
  { fptIrPrepareSec :: !Double
  , fptPackSec :: !Double
  , fptFlatOptSec :: !Double
  , fptTotalSec :: !Double
  }
  deriving (Eq, Show)

-- | Sub-step breakdown of 'fptFlatOptSec' (constant fold vs attach); purity
-- itself is computed at pack time, not in the flat-opt phase.
data FlatOptProfile = FlatOptProfile
  { fopNodeCount :: !Int
  , fopFoldSec :: !Double
  , fopFoldSeqSec :: !Double
  , fopFoldPasses :: !Int
  , fopFolded :: !Bool
  , fopPureCount :: !Int
  , fopAttachSec :: !Double
  , fopTotalSec :: !Double
  }
  deriving (Eq, Show)

-- | Sub-step breakdown of 'fptIrPrepareSec' (lower vs optimize vs metadata).
--
-- 'iopLowerSec' is lazy PHOAS-to-IR WHNF.  Most lowering work is deferred
-- into 'iopPrepareSec' ('optEffectClosed' forces the raw tree).  The
-- meta/opt lines come from a second pass on the already-forced raw IR.
data IrOptProfile = IrOptProfile
  { iopRawNodes :: !Int
  , iopOptNodes :: !Int
  , iopLowerSec :: !Double
  , iopMetaRawSec :: !Double
  , iopOptSec :: !Double
  , iopMetaOptSec :: !Double
  , iopPrepareSec :: !Double
  , iopTotalSec :: !Double
  }
  deriving (Eq, Show)

-- | PHOAS-to-IR lower breakdown ('lowerEffectClosed' is lazy WHNF until forced).
data LowerProfile = LowerProfile
  { lopRawNodes :: !Int
  , lopLazySec :: !Double
  , lopForceSec :: !Double
  , lopTotalSec :: !Double
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
