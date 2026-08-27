{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Compile-time progress reporting as an 'effectful' effect.
--
-- Interpret with 'runCompileReportSilent' for quiet builds (pure JShark
-- programs and tests). Use 'runCompileReportIO' for terminal progress bars
-- and timing lines on effectful compiles.
--
-- 'LogFallback' is /not/ silenced: minifier fallback notices still go to
-- stderr under 'runCompileReportSilent' (only bars and timing are skipped).
--
-- Batch sub-job phase ticks ('configProgressSlot') go through
-- 'JShark.CompileProgress' directly so concurrent workers stay coherent.
module JShark.CompileReport
  ( CompileReport
  , runCompileReportSilent
  , runCompileReportIO
  , runCompileReportFromConfig
  , drawSingleDone
  , drawBatchDone
  , drawBatchStats
  , logFallback
  , progressStyleIO
  , toCompileProgressStyle
  , writeProgressLine
  , picosecondsToSecs
  )
where

import Data.Char (chr)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret_, send)
import JShark.CompileProgress as CP
import JShark.CompileTiming (CompileJobStats (..), renderCompileStatsTable)
import Numeric (showFFloat)
import System.IO (hFlush, hIsTerminalDevice, hPutStr, hPutStrLn, stderr)

-- | Progress reporting commands issued during compilation.
data CompileReport :: Effect where
  DrawSingleDone :: Double -> CompileReport m ()
  DrawBatchDone :: Int -> Double -> CompileReport m ()
  DrawBatchStats :: Double -> [CompileJobStats] -> CompileReport m ()
  LogFallback :: String -> CompileReport m ()

type instance DispatchOf CompileReport = Dynamic

runCompileReportSilent ::
  IOE :> es => Eff (CompileReport : es) a -> Eff es a
runCompileReportSilent = interpret_ $ \case
  DrawSingleDone _ -> pure ()
  DrawBatchDone _ _ -> pure ()
  DrawBatchStats _ _ -> pure ()
  LogFallback msg -> liftIO (logFallbackIO msg)

runCompileReportIO ::
  IOE :> es => Eff (CompileReport : es) a -> Eff es a
runCompileReportIO = interpret_ $ \case
  DrawSingleDone secs -> liftIO (drawSingleDoneIO secs)
  DrawBatchDone total secs -> liftIO (drawBatchDoneIO total secs)
  DrawBatchStats batchWall stats -> liftIO (drawBatchStatsIO batchWall stats)
  LogFallback msg -> liftIO (logFallbackIO msg)

runCompileReportFromConfig ::
  IOE :> es =>
  Bool
  -> Eff (CompileReport : es) a
  -> Eff es a
runCompileReportFromConfig enabled eff =
  if enabled
    then runCompileReportIO eff
    else runCompileReportSilent eff

drawSingleDone :: CompileReport :> es => Double -> Eff es ()
drawSingleDone = send . DrawSingleDone

drawBatchDone :: CompileReport :> es => Int -> Double -> Eff es ()
drawBatchDone total secs = send (DrawBatchDone total secs)

drawBatchStats :: CompileReport :> es => Double -> [CompileJobStats] -> Eff es ()
drawBatchStats batchWall stats = send (DrawBatchStats batchWall stats)

logFallback :: CompileReport :> es => String -> Eff es ()
logFallback = send . LogFallback

logFallbackIO :: String -> IO ()
logFallbackIO msg =
  CP.withProgressIO $
    hPutStrLn stderr ("JShark.Compiler: " ++ msg ++ "; using unminified source")

drawSingleDoneIO :: Double -> IO ()
drawSingleDoneIO secs = do
  style <- progressStyleIO
  CP.withProgressIO $
    hPutStrLn stderr (renderSingleDone style secs)

drawBatchDoneIO :: Int -> Double -> IO ()
drawBatchDoneIO total secs = do
  style <- progressStyleIO
  CP.withProgressIO $ do
    hPutStrLn stderr ""
    hPutStrLn stderr (renderBatchDone style total secs)

drawBatchStatsIO :: Double -> [CompileJobStats] -> IO ()
drawBatchStatsIO batchWall stats
  | null stats = pure ()
  | otherwise = do
      CP.withProgressIO $ do
        hPutStrLn stderr ""
        hPutStrLn stderr "compile stats (seconds):"
        hPutStrLn stderr (renderCompileStatsTable (Just batchWall) stats)

data TerminalStyle = TerminalPlain | TerminalTTY

toCompileProgressStyle :: TerminalStyle -> CP.ProgressStyle
toCompileProgressStyle TerminalPlain = CP.ProgressPlain
toCompileProgressStyle TerminalTTY = CP.ProgressTTY

progressStyleIO :: IO TerminalStyle
progressStyleIO = do
  tty <- hIsTerminalDevice stderr
  pure (if tty then TerminalTTY else TerminalPlain)

renderSingleDone :: TerminalStyle -> Double -> String
renderSingleDone style secs =
  let
    dur = formatDuration secs
   in
    case style of
      TerminalPlain ->
        "JShark.Compiler: compiled in " ++ dur
      TerminalTTY ->
        ansiGreen
          ++ [chr 0x2713]
          ++ " "
          ++ ansiReset
          ++ "JShark compiled in "
          ++ ansiCyan
          ++ dur
          ++ ansiReset

renderBatchDone :: TerminalStyle -> Int -> Double -> String
renderBatchDone style total secs =
  let
    dur = formatDuration secs
   in
    case style of
      TerminalPlain ->
        "JShark.Compiler: compiled "
          ++ show total
          ++ " programs in "
          ++ dur
      TerminalTTY ->
        ansiGreen
          ++ [chr 0x2713]
          ++ " "
          ++ ansiReset
          ++ "JShark compiled "
          ++ ansiBold
          ++ show total
          ++ ansiReset
          ++ " programs in "
          ++ ansiCyan
          ++ dur
          ++ ansiReset

writeProgressLine :: String -> IO ()
writeProgressLine line =
  hPutStr stderr ("\r\ESC[2K" ++ line) >> hFlush stderr

picosecondsToSecs :: Integer -> Double
picosecondsToSecs ps = fromIntegral ps / 1e12

formatDuration :: Double -> String
formatDuration s
  | s < 0.001 = show (round (s * 1e6 :: Double) :: Integer) ++ "us"
  | s < 1 = show (round (s * 1000 :: Double) :: Integer) ++ "ms"
  | otherwise = showFFloat (Just 2) s ""

ansiReset, ansiBold, ansiCyan, ansiGreen :: String
ansiReset = "\ESC[0m"
ansiBold = "\ESC[1m"
ansiCyan = "\ESC[36m"
ansiGreen = "\ESC[32m"
