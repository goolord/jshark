{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-job compile progress for batch builds (phase + flat-AST node index).
--
-- Emit ticks from pure codegen use 'unsafePerformIO' with an 'EmitCtx'
-- captured once per job ('withActiveJob'). Only batch compiles with
-- '--progress' and 'configProgressSlot' set are supported; other paths
-- leave 'cgEmitCtx' empty and skip ticks.
module JShark.CompileProgress
  ( CompilePhase (..)
  , JobProgress (..)
  , ProgressBoard (..)
  , ProgressBoardHandle
  , ProgressStyle (..)
  , newProgressBoard
  , readProgressBoard
  , initJob
  , reportJobPhase
  , markJobDone
  , renderBatchProgress
  , withActiveJob
  , finishEmitPhase
  , phaseLabel
  , jobProgressPct
  , setProgressRedraw
  , clearProgressRedraw
  , withProgressIO
  , EmitCtx
  , captureEmitCtx
  , initEmitCtxTotal
  , reportPackPhase
  , tickEmitCtx
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
import Data.Char (chr)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.IO.Unsafe (unsafePerformIO)
import System.CPUTime (getCPUTime)

data CompilePhase
  = PhaseLint
  | PhasePack
  | PhaseEmit
  | PhaseMinify
  | PhaseDone
  deriving (Eq, Show)

data JobProgress = JobProgress
  { jpLabel :: !Text
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

data ProgressStyle = ProgressPlain | ProgressTTY
  deriving (Eq, Show)

data JobSlot = JobSlot
  { jsLabel :: !(IORef Text)
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
  , ajsEmitIndex :: !(IORef Int)
  , ajsEmitStep :: !(IORef Int)
  , ajsLastEmit :: !(IORef Integer)
  }

data EmitCtx = EmitCtx
  { ecSlot :: !Int
  , ecBoard :: !ProgressBoardHandle
  , ecIndex :: !(IORef Int)
  , ecTotal :: !(IORef Int)
  , ecStep :: !(IORef Int)
  , ecLast :: !(IORef Integer)
  }

emitCtxFromJob :: ActiveJobState -> EmitCtx
emitCtxFromJob ActiveJobState
                 { ajsSlot
                 , ajsBoard
                 , ajsEmitTotal
                 , ajsEmitIndex
                 , ajsEmitStep
                 , ajsLastEmit
                 } =
  EmitCtx
    { ecSlot = ajsSlot
    , ecBoard = ajsBoard
    , ecIndex = ajsEmitIndex
    , ecTotal = ajsEmitTotal
    , ecStep = ajsEmitStep
    , ecLast = ajsLastEmit
    }

captureEmitCtx :: IO (Maybe EmitCtx)
captureEmitCtx =
  fmap (emitCtxFromJob <$>) lookupActiveJob

initEmitCtxTotal :: EmitCtx -> Int -> IO ()
initEmitCtxTotal EmitCtx {ecSlot, ecBoard, ecTotal, ecIndex, ecStep} n = do
  let
    total = max 1 n
    step = emitStepSize total
  writeIORef ecTotal total
  writeIORef ecIndex 0
  writeIORef ecStep step
  reportJobPhaseDirect ecBoard ecSlot PhaseEmit 0 total

reportPackPhase :: EmitCtx -> Int -> Int -> IO ()
reportPackPhase EmitCtx {ecBoard, ecSlot} idx tot =
  reportJobPhaseDirect ecBoard ecSlot PhasePack idx tot

tickEmitCtx :: EmitCtx -> IO ()
tickEmitCtx EmitCtx {ecSlot, ecBoard, ecIndex, ecTotal, ecStep, ecLast} = do
  total <- readIORef ecTotal
  when (total > 0) $ do
    idx <- (+ 1) <$> readIORef ecIndex
    writeIORef ecIndex idx
    step <- readIORef ecStep
    lastEmit <- readIORef ecLast
    let
      should =
        idx == 1
          || idx >= total
          || (idx `mod` step == 0)
    when should $ do
      now <- getCPUTime
      when (idx == 1 || idx >= total || now - lastEmit > 50_000_000_000) $ do
        writeIORef ecLast now
        reportJobPhaseDirect ecBoard ecSlot PhaseEmit idx total

{-# NOINLINE progressActive #-}
progressActive :: IORef (Map ThreadId ActiveJobState)
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
  PhaseLint -> 0
  PhasePack -> 1
  PhaseEmit -> 2
  PhaseMinify -> 3
  PhaseDone -> 4

phaseFromInt :: Int -> CompilePhase
phaseFromInt = \case
  0 -> PhaseLint
  1 -> PhasePack
  2 -> PhaseEmit
  3 -> PhaseMinify
  4 -> PhaseDone
  _ -> PhaseLint

phaseWeight :: CompilePhase -> Double
phaseWeight = \case
  PhaseLint -> 0.05
  PhasePack -> 0.15
  PhaseEmit -> 0.70
  PhaseMinify -> 0.10
  PhaseDone -> 1.0

phaseOrder :: CompilePhase -> Int
phaseOrder = \case
  PhaseLint -> 0
  PhasePack -> 1
  PhaseEmit -> 2
  PhaseMinify -> 3
  PhaseDone -> 4

phaseLabel :: CompilePhase -> String
phaseLabel = \case
  PhaseLint -> "lint"
  PhasePack -> "pack"
  PhaseEmit -> "emit"
  PhaseMinify -> "min"
  PhaseDone -> "done"

completedPhaseWeight :: CompilePhase -> Double
completedPhaseWeight phase =
  sum
    [ phaseWeight p
    | p <- [PhaseLint, PhasePack, PhaseEmit, PhaseMinify, PhaseDone]
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

initJob :: ProgressBoardHandle -> Int -> Text -> IO ()
initJob ProgressBoardHandle {pbhJobs} slot label =
  case pbhJobs V.!? slot of
    Nothing -> pure ()
    Just slot' -> do
      writeIORef (jsLabel slot') label
      writeJobPhase slot' PhaseLint 0 1
      writeCounter (jsDone slot') 0

reportJobPhase :: Int -> CompilePhase -> Int -> Int -> IO ()
reportJobPhase slot phase idx total = do
  mJob <- lookupActiveJob
  case mJob of
    Nothing -> pure ()
    Just ActiveJobState {ajsSlot, ajsBoard}
      | slot /= ajsSlot -> pure ()
      | otherwise -> reportJobPhaseDirect ajsBoard slot phase idx total

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

withActiveJob :: Int -> ProgressBoardHandle -> IO a -> IO a
withActiveJob slot board io = do
  tid <- myThreadId
  emitTotal <- newIORef 0
  emitIndex <- newIORef 0
  emitStep <- newIORef 32
  lastEmit <- newIORef (0 :: Integer)
  let
    !ctx =
      ActiveJobState
        { ajsSlot = slot
        , ajsBoard = board
        , ajsEmitTotal = emitTotal
        , ajsEmitIndex = emitIndex
        , ajsEmitStep = emitStep
        , ajsLastEmit = lastEmit
        }
  atomicModifyIORef' progressActive $ \m -> (Map.insert tid ctx m, ())
  io
    `finally` do
      atomicModifyIORef' progressActive $ \m -> (Map.delete tid m, ())

emitStepSize :: Int -> Int
emitStepSize n = max 1 (min 500 (n `div` 100))

finishEmitPhase :: IO ()
finishEmitPhase = do
  mJob <- lookupActiveJob
  case mJob of
    Just ActiveJobState {ajsSlot, ajsBoard, ajsEmitTotal, ajsEmitIndex} -> do
      total <- readIORef ajsEmitTotal
      idx <- readIORef ajsEmitIndex
      when (total > 0) $
        reportJobPhaseDirect ajsBoard ajsSlot PhaseEmit (max idx total) total
    _ -> pure ()

subBarWidth :: Int
subBarWidth = 16

mainBarWidth :: Int
mainBarWidth = 24

renderBatchProgress :: ProgressStyle -> ProgressBoard -> Int -> String
renderBatchProgress style board prevLines =
  let
    done = pbDone board
    total = max 1 (pbTotal board)
    mainPct = fromIntegral done / fromIntegral total
    mainFilled = min mainBarWidth (floor (mainPct * fromIntegral mainBarWidth))
    mainEmpty = mainBarWidth - mainFilled
    mainLine =
      progressHeader style
        ++ " ["
        ++ renderBar style mainFilled mainEmpty
        ++ "] "
        ++ progressCount style (show done ++ "/" ++ show total)
        ++ " "
        ++ progressPct style (show (floor (mainPct * 100 :: Double) :: Int) ++ "%")
    subLines =
      [ renderSubLine style j
      | j <- V.toList (pbJobs board)
      , not (jpDone j)
      , not (T.null (jpLabel j))
      ]
    lines' = mainLine : subLines
    up =
      if prevLines > 0
        then "\ESC[" ++ show prevLines ++ "A"
        else ""
   in
    up ++ unlines (map (progressClear ++) lines')

renderSubLine :: ProgressStyle -> JobProgress -> String
renderSubLine style j =
  let
    lbl = jpLabel j
    ph = jpPhase j
    idx = jpIndex j
    tot = jpTotal j
    pct =
      jobProgressPct
        ( JobProgress
            { jpLabel = lbl
            , jpPhase = ph
            , jpIndex = idx
            , jpTotal = tot
            , jpDone = False
            }
        )
    filled = min subBarWidth (floor (pct * fromIntegral subBarWidth))
    empty = subBarWidth - filled
    name = truncateLabel 18 (T.unpack lbl)
    phase = padRight 4 (phaseLabel ph)
    idxShow =
      if tot > 1 && (ph == PhaseEmit || ph == PhasePack)
        then " " ++ show (min idx tot) ++ "/" ++ show tot
        else ""
   in
    "  "
      ++ progressLabel style name
      ++ " "
      ++ progressPct style phase
      ++ idxShow
      ++ " ["
      ++ renderBar style filled empty
      ++ "] "
      ++ progressPct style (show (floor (pct * 100 :: Double) :: Int) ++ "%")

progressClear :: String
progressClear = "\r\ESC[2K"

progressHeader :: ProgressStyle -> String
progressHeader = \case
  ProgressPlain -> "compile"
  ProgressTTY -> ansiBold ++ "compile" ++ ansiReset

progressCount :: ProgressStyle -> String -> String
progressCount ProgressPlain s = padLeft 5 s
progressCount ProgressTTY s = ansiBold ++ padLeft 5 s ++ ansiReset

progressPct :: ProgressStyle -> String -> String
progressPct ProgressPlain s = padLeft 4 s
progressPct ProgressTTY s = ansiDim ++ padLeft 4 s ++ ansiReset

progressLabel :: ProgressStyle -> String -> String
progressLabel ProgressPlain s = s
progressLabel ProgressTTY s = ansiCyan ++ s ++ ansiReset

renderBar :: ProgressStyle -> Int -> Int -> String
renderBar ProgressPlain filled empty =
  replicate filled '#' ++ replicate empty '-'
renderBar ProgressTTY filled empty =
  ansiCyan
    ++ replicate filled (chr 9608)
    ++ ansiDim
    ++ replicate empty (chr 9617)
    ++ ansiReset

truncateLabel :: Int -> String -> String
truncateLabel n s
  | length s <= n = s
  | n <= 1 = take n s
  | otherwise = take (n - 1) s ++ "."

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

ansiReset, ansiBold, ansiDim, ansiCyan :: String
ansiReset = "\ESC[0m"
ansiBold = "\ESC[1m"
ansiDim = "\ESC[2m"
ansiCyan = "\ESC[36m"
