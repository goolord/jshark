{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Wall-clock breakdown for flat and PHOAS compile prepare paths.
--   Enable stderr logging with @JSHARK_COMPILE_TIMING=1@.
--   Batch compiles with @--progress@ print a summary table via
--   'renderCompileStatsTable'.
module JShark.CompileTiming
  ( CompileForm (..)
  , CompileJobStats (..)
  , FlatPrepareTiming (..)
  , PhoasPrepareTiming (..)
  , timingEnabled
  , reportFlatPrepareTiming
  , reportPhoasPrepareTiming
  , renderCompileStatsTable
  , seconds
  )
where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

data CompileForm
  = FormReadable
  | FormMinified
  deriving (Eq, Show)

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

data CompileJobStats = CompileJobStats
  { cjsLabel :: !Text
  , cjsForm :: !CompileForm
  , cjsLintSec :: !Double
  , cjsLowerSec :: !Double
  , cjsIrOptSec :: !Double
  , cjsPackSec :: !Double
  , cjsFlatOptSec :: !Double
  , cjsPhoasOptSec :: !Double
  , cjsEmitSec :: !Double
  , cjsMinifySec :: !Double
  , cjsTotalSec :: !Double
  , cjsJsBytes :: !Int
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
      , "  lower:    " ++ show (fptLowerSec t)
      , "  ir-opt:   " ++ show (fptIrOptSec t)
      , "  pack:     " ++ show (fptPackSec t)
      , "  flat-opt: " ++ show (fptFlatOptSec t)
      , "  total:    " ++ show (fptTotalSec t)
      ]

reportPhoasPrepareTiming :: PhoasPrepareTiming -> IO ()
reportPhoasPrepareTiming t = do
  ok <- timingEnabled
  when ok
    $ hPutStrLn stderr
    $ unlines
      [ "JShark PHOAS prepare timing (seconds):"
      , "  optimize: " ++ show (pptOptimizeSec t)
      , "  total:    " ++ show (pptTotalSec t)
      ]

renderCompileStatsTable :: Maybe Double -> [CompileJobStats] -> String
renderCompileStatsTable mBatchWall stats =
  unlines (header : map renderRow sorted ++ [footer])
 where
  sorted = sortBy (comparing cjsLabel) stats
  header =
    padRight 26 "program"
      ++ padLeft 7 "total"
      ++ padLeft 6 "lint"
      ++ padLeft 6 "lower"
      ++ padLeft 6 "iopt"
      ++ padLeft 6 "pack"
      ++ padLeft 6 "fopt"
      ++ padLeft 6 "phopt"
      ++ padLeft 6 "emit"
      ++ padLeft 6 "min"
      ++ padLeft 8 "bytes"
  footer =
    "totals"
      ++ padLeft 7 (fmtSec footerWall)
      ++ padLeft 6 (fmtSec totalLint)
      ++ padLeft 6 (fmtSec totalLower)
      ++ padLeft 6 (fmtSec totalIrOpt)
      ++ padLeft 6 (fmtSec totalPack)
      ++ padLeft 6 (fmtSec totalFlatOpt)
      ++ padLeft 6 (fmtSec totalPhoasOpt)
      ++ padLeft 6 (fmtSec totalEmit)
      ++ padLeft 6 (fmtSec totalMinify)
      ++ padLeft 8 (show totalBytes)
  footerWall =
    case mBatchWall of
      Just w -> w
      Nothing -> sum (map cjsTotalSec sorted)
  totalLint = sum (map cjsLintSec sorted)
  totalLower = sum (map cjsLowerSec sorted)
  totalIrOpt = sum (map cjsIrOptSec sorted)
  totalPack = sum (map cjsPackSec sorted)
  totalFlatOpt = sum (map cjsFlatOptSec sorted)
  totalPhoasOpt = sum (map cjsPhoasOptSec sorted)
  totalEmit = sum (map cjsEmitSec sorted)
  totalMinify = sum (map cjsMinifySec sorted)
  totalBytes = sum (map cjsJsBytes sorted)
  renderRow stat =
    padRight 26 (T.unpack (cjsLabel stat) ++ formSuffix (cjsForm stat))
      ++ padLeft 7 (fmtSec (cjsTotalSec stat))
      ++ padLeft 6 (fmtSec (cjsLintSec stat))
      ++ padLeft 6 (fmtSec (cjsLowerSec stat))
      ++ padLeft 6 (fmtSec (cjsIrOptSec stat))
      ++ padLeft 6 (fmtSec (cjsPackSec stat))
      ++ padLeft 6 (fmtSec (cjsFlatOptSec stat))
      ++ padLeft 6 (fmtSec (cjsPhoasOptSec stat))
      ++ padLeft 6 (fmtSec (cjsEmitSec stat))
      ++ padLeft 6 (fmtSec (cjsMinifySec stat))
      ++ padLeft 8 (show (cjsJsBytes stat))

formSuffix :: CompileForm -> String
formSuffix = \case
  FormReadable -> " [readable]"
  FormMinified -> " [min]"

fmtSec :: Double -> String
fmtSec s
  | s <= 0 = "-"
  | s < 0.001 = show (round (s * 1e6 :: Double) :: Integer) ++ "u"
  | s < 1 = show (round (s * 1000 :: Double) :: Integer) ++ "m"
  | otherwise = showFFloat (Just 2) s ""

padLeft :: Int -> String -> String
padLeft w s =
  let
    k = w - length s
   in
    if k > 0 then replicate k ' ' ++ s else take w s

padRight :: Int -> String -> String
padRight w s =
  let
    k = w - length s
   in
    if k > 0 then s ++ replicate k ' ' else take w s

seconds :: Double -> Double -> Double
seconds t0 t1 = t1 - t0

when :: Bool -> IO () -> IO ()
when b io = if b then io else pure ()
