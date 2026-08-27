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
-- 'JShark.Compiler.CompileProgress' directly so concurrent workers stay coherent.
module JShark.Compiler.CompileReport
  ( CompileReport
  , runCompileReportSilent
  , runCompileReportIO
  , runCompileReportFromConfig
  , drawSingleDone
  , drawBatchDone
  , drawBatchStats
  , logFallback
  , progressStyleIO
  , writeProgressLine
  , picosecondsToSecs
  )
where

import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret_, send)
import JShark.Compiler.CompileProgress as CP
import JShark.Compiler.CompileTerminal as CT
import JShark.Compiler.CompileTiming (CompileJobStats (..))
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)

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

drawBatchStats ::
  CompileReport :> es => Double -> [CompileJobStats] -> Eff es ()
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
    hPutStrLn stderr (CT.renderSingleDone style secs)

drawBatchDoneIO :: Int -> Double -> IO ()
drawBatchDoneIO total secs = do
  style <- progressStyleIO
  CP.withProgressIO $ do
    hPutStrLn stderr ""
    hPutStrLn stderr (CT.renderBatchDone style total secs)

drawBatchStatsIO :: Double -> [CompileJobStats] -> IO ()
drawBatchStatsIO batchWall stats
  | null stats = pure ()
  | otherwise = do
      style <- progressStyleIO
      CP.withProgressIO $ do
        hPutStrLn stderr ""
        hPutStrLn stderr (styledTitle style)
        hPutStrLn stderr (CT.renderStatsTable style (Just batchWall) stats)

progressStyleIO :: IO CT.TerminalStyle
progressStyleIO = CT.terminalStyleIO

styledTitle :: CT.TerminalStyle -> String
styledTitle CT.TerminalPlain = "compile stats (seconds):"
styledTitle CT.TerminalTTY =
  CT.styled CT.TerminalTTY CT.boldSGR "compile stats"
    ++ CT.styled CT.TerminalTTY CT.dimSGR " (seconds)"

writeProgressLine :: String -> IO ()
writeProgressLine line =
  hPutStr stderr (CT.clearLine ++ line) >> hFlush stderr

picosecondsToSecs :: Integer -> Double
picosecondsToSecs ps = fromIntegral ps / 1e12
