{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-job compile progress for batch builds.
--
-- A batch compile creates a 'ProgressBoard' with one slot per job, then runs
-- each job under 'withActiveJob' so that phase reports issued by the compiler
-- driver and prepare passes land on the right slot. Jobs on threads without an
-- active slot (single compiles, tests) report nowhere.
--
-- Terminal output is serialized through a hand-rolled CAS gate
-- ('withProgressIO') so concurrent jobs do not interleave their redraws.
module JShark.Compiler.CompileProgress
  ( CompilePhase (..)
  , JobProgress (..)
  , ProgressBoard (..)
  , ProgressBoardHandle
  , TerminalStyle (..)
  , newProgressBoard
  , readProgressBoard
  , initJob
  , markJobDone
  , renderBatchProgress
  , renderDoneLine
  , renderBatchDoneLine
  , terminalStyleIO
  , writeProgressLine
  , withActiveJob
  , finishEmitPhase
  , setProgressRedraw
  , clearProgressRedraw
  , withProgressIO
  , EmitCtx
  , captureEmitCtx
  , initEmitCtxTotal
  , reportPackPhase
  , reportFlatOptPhase
  , reportIrPreparePhase
  )
where

import Control.Concurrent (ThreadId, myThreadId, threadDelay)
import Control.Exception (bracket_, finally)
import Control.Monad (when)
import Data.Atomics (casIORef, peekTicket, readForCAS)
import Data.Atomics.Counter
  ( AtomicCounter
  , incrCounter_
  , newCounter
  , readCounter
  , writeCounter
  )
import Data.Char (chr, toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.IO.Encoding (TextEncoding, textEncodingName)
import GHC.IO.Unsafe (unsafePerformIO)
import Numeric (showFFloat)
import System.Console.ANSI
  ( Color (..)
  , ColorIntensity (..)
  , ConsoleIntensity (..)
  , ConsoleLayer (..)
  , SGR (..)
  , hSupportsANSI
  )
import System.Console.ANSI.Codes
  ( clearLineCode
  , cursorUpCode
  , setSGRCode
  )
import System.IO (hFlush, hGetEncoding, hPutStr, stderr)

-- | The ordered phases of a single compile, as shown on the progress bar.
data CompilePhase
  = PhaseIrPrepare
  | PhasePack
  | PhaseFlatOpt
  | PhaseEmit
  | PhaseDone
  deriving (Eq, Show)

data JobProgress = JobProgress
  { jpLabel :: !T.Text
  , jpPhase :: !CompilePhase
  , jpIndex :: !Int
  , jpTotal :: !Int
  , jpDone :: !Bool
  }
  deriving (Eq, Show)

data ProgressBoard = ProgressBoard
  { pbDone :: !Int
  , pbTotal :: !Int
  , pbJobs :: !(V.Vector JobProgress)
  }
  deriving (Eq, Show)

data JobSlot = JobSlot
  { jsLabel :: !(IORef T.Text)
  , jsPhase :: !AtomicCounter
  , jsIndex :: !AtomicCounter
  , jsTotal :: !AtomicCounter
  , jsDone :: !AtomicCounter
  }

data ProgressBoardHandle = ProgressBoardHandle
  { pbhDone :: !AtomicCounter
  , pbhTotal :: !Int
  , pbhJobs :: !(V.Vector JobSlot)
  }

data ActiveJobState = ActiveJobState
  { ajsSlot :: !Int
  , ajsBoard :: !ProgressBoardHandle
  , ajsEmitTotal :: !(IORef Int)
  }

-- | The emit-phase view of an active job: which slot/board to report on, and
-- the shared emit node total (set by 'initEmitCtxTotal').
data EmitCtx = EmitCtx
  { ecSlot :: !Int
  , ecBoard :: !ProgressBoardHandle
  , ecTotal :: !(IORef Int)
  }

emitCtxFromJob :: ActiveJobState -> EmitCtx
emitCtxFromJob ActiveJobState {ajsSlot, ajsBoard, ajsEmitTotal} =
  EmitCtx
    { ecSlot = ajsSlot
    , ecBoard = ajsBoard
    , ecTotal = ajsEmitTotal
    }

captureEmitCtx :: IO (Maybe EmitCtx)
captureEmitCtx = fmap (emitCtxFromJob <$>) lookupActiveJob

-- | Record the emit node total and open the emit phase at index 0.
initEmitCtxTotal :: EmitCtx -> Int -> IO ()
initEmitCtxTotal EmitCtx {ecSlot, ecBoard, ecTotal} n = do
  let
    total = max 1 n
  writeIORef ecTotal total
  reportJobPhaseDirect ecBoard ecSlot PhaseEmit 0 total

reportFlatOptPhase :: EmitCtx -> Int -> Int -> IO ()
reportFlatOptPhase EmitCtx {ecBoard, ecSlot} idx tot =
  reportJobPhaseDirect ecBoard ecSlot PhaseFlatOpt idx tot

reportIrPreparePhase :: EmitCtx -> Int -> Int -> IO ()
reportIrPreparePhase EmitCtx {ecBoard, ecSlot} idx tot =
  reportJobPhaseDirect ecBoard ecSlot PhaseIrPrepare idx tot

reportPackPhase :: EmitCtx -> Int -> Int -> IO ()
reportPackPhase EmitCtx {ecBoard, ecSlot} idx tot =
  reportJobPhaseDirect ecBoard ecSlot PhasePack idx tot

{-# NOINLINE progressActive #-}
progressActive :: IORef (Map.Map ThreadId ActiveJobState)
progressActive = unsafePerformIO (newIORef Map.empty)

{-# NOINLINE progressGate #-}
progressGate :: IORef Int
progressGate = unsafePerformIO (newIORef 0)

{-# NOINLINE progressRedraw #-}
progressRedraw :: IORef (Maybe (IO ()))
progressRedraw = unsafePerformIO (newIORef Nothing)

{-# NOINLINE pendingRedraw #-}
pendingRedraw :: IORef Bool
pendingRedraw = unsafePerformIO (newIORef False)

gateSpinMicros :: Int
gateSpinMicros = 1000

releaseProgressGate :: IO ()
releaseProgressGate = writeIORef progressGate 0

acquireProgressGate :: IO ()
acquireProgressGate = do
  t <- readForCAS progressGate
  case peekTicket t of
    0 -> do
      (ok, _) <- casIORef progressGate t 1
      if ok then pure () else acquireProgressGate
    _ -> threadDelay gateSpinMicros >> acquireProgressGate

tryProgressIO :: IO a -> IO (Maybe a)
tryProgressIO io = do
  t <- readForCAS progressGate
  case peekTicket t of
    0 -> do
      (ok, _) <- casIORef progressGate t 1
      if ok
        then Just <$> bracket_ (pure ()) releaseProgressGate io
        else tryProgressIO io
    _ -> pure Nothing

flushPendingRedraw :: IO ()
flushPendingRedraw = do
  pending <- readIORef pendingRedraw
  when pending $ do
    writeIORef pendingRedraw False
    m <- readIORef progressRedraw
    case m of
      Nothing -> pure ()
      Just io ->
        tryProgressIO io >>= \case
          Nothing -> writeIORef pendingRedraw True
          Just _ -> pure ()

withProgressIO :: IO a -> IO a
withProgressIO io =
  bracket_ acquireProgressGate releaseProgressGate $ do
    r <- io
    flushPendingRedraw
    pure r

setProgressRedraw :: IO () -> IO ()
setProgressRedraw io = writeIORef progressRedraw (Just io)

clearProgressRedraw :: IO ()
clearProgressRedraw = writeIORef progressRedraw Nothing

maybeRedraw :: IO ()
maybeRedraw = do
  m <- readIORef progressRedraw
  case m of
    Nothing -> pure ()
    Just io ->
      tryProgressIO io >>= \case
        Nothing -> writeIORef pendingRedraw True
        Just {} -> pure ()

lookupActiveJob :: IO (Maybe ActiveJobState)
lookupActiveJob = do
  tid <- myThreadId
  atomicModifyIORef' progressActive $ \m -> (m, Map.lookup tid m)

phaseToInt :: CompilePhase -> Int
phaseToInt = \case
  PhaseIrPrepare -> 0
  PhasePack -> 1
  PhaseFlatOpt -> 2
  PhaseEmit -> 3
  PhaseDone -> 4

phaseFromInt :: Int -> CompilePhase
phaseFromInt = \case
  0 -> PhaseIrPrepare
  1 -> PhasePack
  2 -> PhaseFlatOpt
  3 -> PhaseEmit
  _ -> PhaseIrPrepare

phaseWeight :: CompilePhase -> Double
phaseWeight = \case
  PhaseIrPrepare -> 0.10
  PhasePack -> 0.05
  PhaseFlatOpt -> 0.05
  PhaseEmit -> 0.70
  PhaseDone -> 1.0

phaseOrder :: CompilePhase -> Int
phaseOrder = phaseToInt

phaseLabel :: CompilePhase -> String
phaseLabel = \case
  PhaseIrPrepare -> "irprep"
  PhasePack -> "pack"
  PhaseFlatOpt -> "fopt"
  PhaseEmit -> "emit"
  PhaseDone -> "done"

completedPhaseWeight :: CompilePhase -> Double
completedPhaseWeight phase =
  sum
    [ phaseWeight p
    | p <- [PhaseIrPrepare, PhasePack, PhaseFlatOpt, PhaseEmit, PhaseDone]
    , phaseOrder p < phaseOrder phase
    ]

jobProgressPct :: JobProgress -> Double
jobProgressPct JobProgress {jpPhase, jpIndex, jpTotal, jpDone} =
  if jpDone
    then 1
    else
      let
        base = completedPhaseWeight jpPhase
        within =
          if jpTotal <= 0
            then 1
            else fromIntegral (min jpIndex jpTotal) / fromIntegral jpTotal
        cur = phaseWeight jpPhase * within
       in
        min 1 (base + cur)

newJobSlot :: IO JobSlot
newJobSlot = do
  lbl <- newIORef ""
  ph <- newCounter 0
  idx <- newCounter 0
  tot <- newCounter 1
  done <- newCounter 0
  pure
    JobSlot
      { jsLabel = lbl
      , jsPhase = ph
      , jsIndex = idx
      , jsTotal = tot
      , jsDone = done
      }

newProgressBoard :: Int -> IO ProgressBoardHandle
newProgressBoard total = do
  done <- newCounter 0
  jobs <- V.replicateM total newJobSlot
  pure
    ProgressBoardHandle
      { pbhDone = done
      , pbhTotal = total
      , pbhJobs = jobs
      }

readJobSlot :: JobSlot -> IO JobProgress
readJobSlot JobSlot {jsLabel, jsPhase, jsIndex, jsTotal, jsDone} = do
  lbl <- readIORef jsLabel
  ph <- phaseFromInt <$> readCounter jsPhase
  idx <- readCounter jsIndex
  tot <- readCounter jsTotal
  done <- (/= 0) <$> readCounter jsDone
  pure
    JobProgress
      { jpLabel = lbl
      , jpPhase = ph
      , jpIndex = idx
      , jpTotal = tot
      , jpDone = done
      }

readProgressBoard :: ProgressBoardHandle -> IO ProgressBoard
readProgressBoard ProgressBoardHandle {pbhDone, pbhTotal, pbhJobs} = do
  done <- readCounter pbhDone
  jobs <- V.mapM readJobSlot pbhJobs
  pure ProgressBoard {pbDone = done, pbTotal = pbhTotal, pbJobs = jobs}

writeJobPhase :: JobSlot -> CompilePhase -> Int -> Int -> IO ()
writeJobPhase JobSlot {jsPhase, jsIndex, jsTotal} phase idx total = do
  writeCounter jsPhase (phaseToInt phase)
  writeCounter jsIndex idx
  writeCounter jsTotal (max 1 total)

initJob :: ProgressBoardHandle -> Int -> T.Text -> IO ()
initJob ProgressBoardHandle {pbhJobs} slot label =
  case pbhJobs V.!? slot of
    Nothing -> pure ()
    Just slot' -> do
      writeIORef (jsLabel slot') label
      writeJobPhase slot' PhaseIrPrepare 0 1
      writeCounter (jsDone slot') 0

reportJobPhaseDirect ::
  ProgressBoardHandle -> Int -> CompilePhase -> Int -> Int -> IO ()
reportJobPhaseDirect ProgressBoardHandle {pbhJobs} slot phase idx total =
  case pbhJobs V.!? slot of
    Nothing -> pure ()
    Just jobSlot -> do
      writeJobPhase jobSlot phase idx total
      maybeRedraw

markJobDone :: ProgressBoardHandle -> Int -> IO ()
markJobDone ProgressBoardHandle {pbhDone, pbhJobs} slot =
  case pbhJobs V.!? slot of
    Nothing -> pure ()
    Just jobSlot -> do
      let
        doneCounter = jsDone jobSlot
      already <- readCounter doneCounter
      when (already == 0) $ do
        writeCounter doneCounter 1
        incrCounter_ 1 pbhDone
        writeJobPhase jobSlot PhaseDone 1 1
      maybeRedraw

-- | Bind the current thread to job @slot@ on @board@ for the duration of
-- @io@, so any 'captureEmitCtx' calls made inside it land on the right slot.
withActiveJob :: Int -> ProgressBoardHandle -> IO a -> IO a
withActiveJob slot board io = do
  tid <- myThreadId
  emitTotal <- newIORef 0
  let
    !ctx =
      ActiveJobState
        { ajsSlot = slot
        , ajsBoard = board
        , ajsEmitTotal = emitTotal
        }
  atomicModifyIORef' progressActive $ \m -> (Map.insert tid ctx m, ())
  io
    `finally` do
      atomicModifyIORef' progressActive $ \m -> (Map.delete tid m, ())

-- | Close the emit phase at full width for the job running on this thread.
finishEmitPhase :: IO ()
finishEmitPhase = do
  mJob <- lookupActiveJob
  case mJob of
    Just ActiveJobState {ajsSlot, ajsBoard, ajsEmitTotal} -> do
      total <- readIORef ajsEmitTotal
      when (total > 0) $
        reportJobPhaseDirect ajsBoard ajsSlot PhaseEmit total total
    _ -> pure ()

subBarWidth :: Int
subBarWidth = 18

mainBarWidth :: Int
mainBarWidth = 28

renderBatchProgress :: TerminalStyle -> ProgressBoard -> Int -> String
renderBatchProgress style board prevLines =
  let
    done = pbDone board
    total = max 1 (pbTotal board)
    mainPct = fromIntegral done / fromIntegral total
    mainFilled = min mainBarWidth (floor (mainPct * fromIntegral mainBarWidth))
    mainEmpty = mainBarWidth - mainFilled
    pctInt = floor (mainPct * 100 :: Double) :: Int
    mainLine =
      styled style boldSGR "compile"
        ++ " "
        ++ renderBar style mainFilled mainEmpty
        ++ " "
        ++ styled style boldSGR (padLeft 5 (show done ++ "/" ++ show total))
        ++ " "
        ++ styled style dimSGR (padLeft 4 (show pctInt ++ "%"))
    subLines =
      [ renderSubLine style j
      | j <- V.toList (pbJobs board)
      , not (jpDone j)
      , not (T.null (jpLabel j))
      ]
    lines' = mainLine : subLines
    up = cursorUp prevLines
   in
    up ++ unlines (map (clearLine ++) lines')

renderSubLine :: TerminalStyle -> JobProgress -> String
renderSubLine style j =
  let
    lbl = jpLabel j
    ph = jpPhase j
    idx = jpIndex j
    tot = jpTotal j
    pct = jobProgressPct j
    filled = min subBarWidth (floor (pct * fromIntegral subBarWidth))
    empty = subBarWidth - filled
    name = truncateLabel 18 (T.unpack lbl)
    phase = padRight 6 (phaseLabel ph)
    idxShow =
      if tot > 1 && phaseUsesIndex ph
        then " " ++ show (min idx tot) ++ "/" ++ show tot
        else ""
   in
    "  "
      ++ styled style cyanSGR name
      ++ " "
      ++ styled style dimSGR phase
      ++ idxShow
      ++ " "
      ++ renderBar style filled empty
      ++ " "
      ++ styled style dimSGR (show (floor (pct * 100 :: Double) :: Int) ++ "%")

renderBar :: TerminalStyle -> Int -> Int -> String
renderBar style filled empty =
  case style of
    TerminalPlain ->
      "["
        ++ replicate filled '='
        ++ replicate empty '-'
        ++ "]"
    TerminalTTY ->
      "["
        ++ styled style cyanSGR (replicate filled (chr 9608))
        ++ styled style dimSGR (replicate empty (chr 9617))
        ++ "]"

-- | Single-compile completion line ("compiled in 1.2s").
renderDoneLine :: TerminalStyle -> Double -> String
renderDoneLine style secs =
  let
    dur = formatDuration secs
   in
    case style of
      TerminalPlain -> "JShark.Compiler: compiled in " ++ dur
      TerminalTTY ->
        styled style greenSGR "✅"
          ++ " "
          ++ "JShark compiled in "
          ++ styled style cyanSGR dur

-- | Batch completion line ("compiled 4 programs in 3.2s").
renderBatchDoneLine :: TerminalStyle -> Int -> Double -> String
renderBatchDoneLine style total secs =
  let
    dur = formatDuration secs
   in
    case style of
      TerminalPlain ->
        "JShark.Compiler: compiled " ++ show total ++ " programs in " ++ dur
      TerminalTTY ->
        styled style greenSGR "✅"
          ++ " "
          ++ "JShark compiled "
          ++ styled style boldSGR (show total)
          ++ " programs in "
          ++ styled style cyanSGR dur

-- | Write one redraw of the progress block to stderr.
writeProgressLine :: String -> IO ()
writeProgressLine line = hPutStr stderr (clearLine ++ line) >> hFlush stderr

truncateLabel :: Int -> String -> String
truncateLabel n s
  | length s <= n = s
  | n <= 1 = take n s
  | otherwise = take (n - 1) s ++ "."

phaseUsesIndex :: CompilePhase -> Bool
phaseUsesIndex = \case
  PhaseIrPrepare -> True
  PhasePack -> True
  PhaseFlatOpt -> True
  PhaseEmit -> True
  _ -> False

padLeft :: Int -> String -> String
padLeft w s =
  let
    k = w - length s
   in
    if k > 0 then replicate k ' ' ++ s else s

padRight :: Int -> String -> String
padRight w s =
  let
    k = w - length s
   in
    if k > 0 then s ++ replicate k ' ' else take w s

-- Terminal styling ------------------------------------------------------------

data TerminalStyle = TerminalPlain | TerminalTTY
  deriving (Eq, Show)

encodingSupportsUnicode :: Maybe TextEncoding -> Bool
encodingSupportsUnicode Nothing = False
encodingSupportsUnicode (Just enc) =
  let
    name = map toLower (textEncodingName enc)
   in
    "utf-8" `isInfixOf` name || "utf8" `isInfixOf` name

terminalStyleIO :: IO TerminalStyle
terminalStyleIO = do
  ansi <- hSupportsANSI stderr
  unicode <- encodingSupportsUnicode <$> hGetEncoding stderr
  pure (if ansi && unicode then TerminalTTY else TerminalPlain)

boldSGR, dimSGR, cyanSGR, greenSGR :: [SGR]
boldSGR = [SetConsoleIntensity BoldIntensity]
dimSGR = [SetConsoleIntensity FaintIntensity]
cyanSGR = [SetColor Foreground Vivid Cyan]
greenSGR = [SetColor Foreground Vivid Green]

styled :: TerminalStyle -> [SGR] -> String -> String
styled TerminalPlain _ s = s
styled TerminalTTY sgr s = setSGRCode sgr ++ s ++ setSGRCode [Reset]

clearLine :: String
clearLine = "\r" ++ clearLineCode

cursorUp :: Int -> String
cursorUp n = if n > 0 then cursorUpCode n else ""

formatDuration :: Double -> String
formatDuration s
  | s < 0.001 = show (round (s * 1e6 :: Double) :: Integer) ++ "us"
  | s < 1 = show (round (s * 1000 :: Double) :: Integer) ++ "ms"
  | otherwise = showFFloat (Just 2) s ""
