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
module JShark.Compiler.CompileProgress
  ( CompilePhase (..)
  , JobProgress (..)
  , ProgressBoard (..)
  , ProgressBoardHandle
  , TerminalStyle (..)
  , newProgressBoard
  , readProgressBoard
  , initJob
  , reportJobPhase
  , markJobDone
  , renderBatchProgress
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
  , tickEmitCtx
  , recordJobLintSec
  , recordJobCodegenSec
  , recordJobMinifySec
  , recordJobJsBytes
  , recordJobFlatPrepare
  , recordJobPhoasPrepare
  , recordJobForm
  , snapshotJobStatsFromSlot
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
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.IO.Unsafe (unsafePerformIO)
import JShark.Compiler.CompileTerminal
  ( TerminalStyle (..)
  , boldSGR
  , clearLine
  , cursorUp
  , cyanSGR
  , dimSGR
  , styled
  )
import JShark.Compiler.CompileTiming
  ( CompileForm (..)
  , CompileJobStats (..)
  , FlatPrepareTiming (..)
  , PhoasPrepareTiming (..)
  )
import System.CPUTime (getCPUTime)

data CompilePhase
  = PhaseLint
  | PhaseIrPrepare
  | PhasePack
  | PhaseFlatOpt
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

data JobSlot = JobSlot
  { jsLabel :: !(IORef Text)
  , jsPhase :: !AtomicCounter
  , jsIndex :: !AtomicCounter
  , jsTotal :: !AtomicCounter
  , jsDone :: !AtomicCounter
  , jsTiming :: !JobTiming
  }

data ProgressBoardHandle = ProgressBoardHandle
  { pbhDone :: !AtomicCounter
  , pbhTotal :: !Int
  , pbhJobs :: !(V.Vector JobSlot)
  }

data JobTiming = JobTiming
  { jtForm :: !(IORef CompileForm)
  , jtLintSec :: !(IORef Double)
  , jtCodegenSec :: !(IORef Double)
  , jtMinifySec :: !(IORef Double)
  , jtJsBytes :: !(IORef Int)
  , jtFlatPrepare :: !(IORef (Maybe FlatPrepareTiming))
  , jtPhoasPrepare :: !(IORef (Maybe PhoasPrepareTiming))
  }

data ActiveJobState = ActiveJobState
  { ajsSlot :: !Int
  , ajsBoard :: !ProgressBoardHandle
  , ajsEmitTotal :: !(IORef Int)
  , ajsEmitIndex :: !(IORef Int)
  , ajsEmitStep :: !(IORef Int)
  , ajsLastEmit :: !(IORef Integer)
  , ajsTiming :: !JobTiming
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
emitCtxFromJob
  ActiveJobState
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

newJobTiming :: IO JobTiming
newJobTiming = do
  form <- newIORef FormMinified
  lint <- newIORef 0
  codegen <- newIORef 0
  minify <- newIORef 0
  bytes <- newIORef 0
  flat <- newIORef Nothing
  phoas <- newIORef Nothing
  pure
    JobTiming
      { jtForm = form
      , jtLintSec = lint
      , jtCodegenSec = codegen
      , jtMinifySec = minify
      , jtJsBytes = bytes
      , jtFlatPrepare = flat
      , jtPhoasPrepare = phoas
      }

lookupJobTiming :: IO (Maybe JobTiming)
lookupJobTiming = fmap ajsTiming <$> lookupActiveJob

resetJobTiming :: JobTiming -> IO ()
resetJobTiming
  JobTiming
    { jtForm
    , jtLintSec
    , jtCodegenSec
    , jtMinifySec
    , jtJsBytes
    , jtFlatPrepare
    , jtPhoasPrepare
    } = do
    writeIORef jtForm FormMinified
    writeIORef jtLintSec 0
    writeIORef jtCodegenSec 0
    writeIORef jtMinifySec 0
    writeIORef jtJsBytes 0
    writeIORef jtFlatPrepare Nothing
    writeIORef jtPhoasPrepare Nothing

snapshotJobStatsFromTiming ::
  JobTiming -> Text -> Double -> IO CompileJobStats
snapshotJobStatsFromTiming
  JobTiming
    { jtForm
    , jtLintSec
    , jtCodegenSec
    , jtMinifySec
    , jtJsBytes
    , jtFlatPrepare
    , jtPhoasPrepare
    }
  label
  totalSec = do
    form <- readIORef jtForm
    lint <- readIORef jtLintSec
    codegen <- readIORef jtCodegenSec
    minify <- readIORef jtMinifySec
    bytes <- readIORef jtJsBytes
    flat <- readIORef jtFlatPrepare
    phoas <- readIORef jtPhoasPrepare
    let
      irPrepare = maybe 0 fptIrPrepareSec flat
      pack = maybe 0 fptPackSec flat
      flatOpt = maybe 0 fptFlatOptSec flat
      phoasOpt = maybe 0 pptOptimizeSec phoas
      hasPrepare = isJust flat || isJust phoas
      prepareTotal =
        maybe 0 fptTotalSec flat + maybe 0 pptTotalSec phoas
      emit =
        if hasPrepare
          then max 0 (codegen - prepareTotal)
          else codegen
     in
      pure
        CompileJobStats
          { cjsLabel = label
          , cjsForm = form
          , cjsLintSec = lint
          , cjsIrPrepareSec = irPrepare
          , cjsPackSec = pack
          , cjsFlatOptSec = flatOpt
          , cjsPhoasOptSec = phoasOpt
          , cjsEmitSec = emit
          , cjsMinifySec = minify
          , cjsTotalSec = totalSec
          , cjsJsBytes = bytes
          }

snapshotJobStats :: Text -> Double -> IO CompileJobStats
snapshotJobStats label totalSec = do
  m <- lookupJobTiming
  case m of
    Nothing ->
      pure
        CompileJobStats
          { cjsLabel = label
          , cjsForm = FormMinified
          , cjsLintSec = 0
          , cjsIrPrepareSec = 0
          , cjsPackSec = 0
          , cjsFlatOptSec = 0
          , cjsPhoasOptSec = 0
          , cjsEmitSec = 0
          , cjsMinifySec = 0
          , cjsTotalSec = totalSec
          , cjsJsBytes = 0
          }
    Just timing -> snapshotJobStatsFromTiming timing label totalSec

snapshotJobStatsFromSlot ::
  ProgressBoardHandle -> Int -> Text -> Double -> IO CompileJobStats
snapshotJobStatsFromSlot board slot label totalSec =
  case pbhJobs board V.!? slot of
    Nothing -> snapshotJobStats label totalSec
    Just JobSlot {jsTiming} -> snapshotJobStatsFromTiming jsTiming label totalSec

recordJobLintSec :: Double -> IO ()
recordJobLintSec sec = do
  m <- lookupJobTiming
  case m of
    Nothing -> pure ()
    Just JobTiming {jtLintSec} -> writeIORef jtLintSec sec

recordJobCodegenSec :: Double -> IO ()
recordJobCodegenSec sec = do
  m <- lookupJobTiming
  case m of
    Nothing -> pure ()
    Just JobTiming {jtCodegenSec} -> writeIORef jtCodegenSec sec

recordJobMinifySec :: Double -> IO ()
recordJobMinifySec sec = do
  m <- lookupJobTiming
  case m of
    Nothing -> pure ()
    Just JobTiming {jtMinifySec} -> writeIORef jtMinifySec sec

recordJobJsBytes :: Int -> IO ()
recordJobJsBytes n = do
  m <- lookupJobTiming
  case m of
    Nothing -> pure ()
    Just JobTiming {jtJsBytes} -> writeIORef jtJsBytes n

recordJobFlatPrepare :: FlatPrepareTiming -> IO ()
recordJobFlatPrepare t = do
  m <- lookupJobTiming
  case m of
    Nothing -> pure ()
    Just JobTiming {jtFlatPrepare} -> writeIORef jtFlatPrepare (Just t)

recordJobPhoasPrepare :: PhoasPrepareTiming -> IO ()
recordJobPhoasPrepare t = do
  m <- lookupJobTiming
  case m of
    Nothing -> pure ()
    Just JobTiming {jtPhoasPrepare} -> writeIORef jtPhoasPrepare (Just t)

recordJobForm :: CompileForm -> IO ()
recordJobForm form = do
  m <- lookupJobTiming
  case m of
    Nothing -> pure ()
    Just JobTiming {jtForm} -> writeIORef jtForm form

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

reportFlatOptPhase :: EmitCtx -> Int -> Int -> IO ()
reportFlatOptPhase EmitCtx {ecBoard, ecSlot} idx tot =
  reportJobPhaseDirect ecBoard ecSlot PhaseFlatOpt idx tot

reportIrPreparePhase :: EmitCtx -> Int -> Int -> IO ()
reportIrPreparePhase EmitCtx {ecBoard, ecSlot} idx tot =
  reportJobPhaseDirect ecBoard ecSlot PhaseIrPrepare idx tot

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
  PhaseLint -> 0
  PhaseIrPrepare -> 1
  PhasePack -> 2
  PhaseFlatOpt -> 3
  PhaseEmit -> 4
  PhaseMinify -> 5
  PhaseDone -> 6

phaseFromInt :: Int -> CompilePhase
phaseFromInt = \case
  0 -> PhaseLint
  1 -> PhaseIrPrepare
  2 -> PhasePack
  3 -> PhaseFlatOpt
  4 -> PhaseEmit
  5 -> PhaseMinify
  6 -> PhaseDone
  _ -> PhaseLint

phaseWeight :: CompilePhase -> Double
phaseWeight = \case
  PhaseLint -> 0.03
  PhaseIrPrepare -> 0.10
  PhasePack -> 0.05
  PhaseFlatOpt -> 0.05
  PhaseEmit -> 0.62
  PhaseMinify -> 0.10
  PhaseDone -> 1.0

phaseOrder :: CompilePhase -> Int
phaseOrder = \case
  PhaseLint -> 0
  PhaseIrPrepare -> 1
  PhasePack -> 2
  PhaseFlatOpt -> 3
  PhaseEmit -> 4
  PhaseMinify -> 5
  PhaseDone -> 6

phaseLabel :: CompilePhase -> String
phaseLabel = \case
  PhaseLint -> "lint"
  PhaseIrPrepare -> "irpr"
  PhasePack -> "pack"
  PhaseFlatOpt -> "fopt"
  PhaseEmit -> "emit"
  PhaseMinify -> "min"
  PhaseDone -> "done"

completedPhaseWeight :: CompilePhase -> Double
completedPhaseWeight phase =
  sum
    [ phaseWeight p
    | p <-
        [ PhaseLint
        , PhaseIrPrepare
        , PhasePack
        , PhaseFlatOpt
        , PhaseEmit
        , PhaseMinify
        , PhaseDone
        ]
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
  timing <- newJobTiming
  pure
    JobSlot
      { jsLabel = lbl
      , jsPhase = ph
      , jsIndex = idx
      , jsTotal = tot
      , jsDone = done
      , jsTiming = timing
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
withActiveJob slot board@ProgressBoardHandle {pbhJobs} io = do
  tid <- myThreadId
  emitTotal <- newIORef 0
  emitIndex <- newIORef 0
  emitStep <- newIORef 32
  lastEmit <- newIORef (0 :: Integer)
  timing <-
    case pbhJobs V.!? slot of
      Nothing -> newJobTiming
      Just JobSlot {jsTiming} -> do
        resetJobTiming jsTiming
        pure jsTiming
  let
    !ctx =
      ActiveJobState
        { ajsSlot = slot
        , ajsBoard = board
        , ajsEmitTotal = emitTotal
        , ajsEmitIndex = emitIndex
        , ajsEmitStep = emitStep
        , ajsLastEmit = lastEmit
        , ajsTiming = timing
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
