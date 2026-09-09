{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Flat / SoA test helpers (not part of the main codegen API).
module FlatTest
  ( flatSoaPureNodeCount
  , flatDirectPackDeterministic
  , flatDirectPackForRangeOk
  , flatDirectPackOptimizeStable
  , flatOpcodeRoundTripOk
  , lowerOptEffectRegressionOk
  , optIrEffectForRangeImpure
  , batchJobSlotTimingOk
  )
where

import Data.Int (Int32)
import Data.List (nub)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8)
import JShark (irEffectFromClosed)
import JShark.Api.Types
  ( ClosedEffect
  , FixedOp (FixArrLen)
  , Universe (Unit)
  , Value (..)
  )
import JShark.Compiler.CompileProgress
  ( newProgressBoard
  , recordJobFlatPrepare
  , recordJobLintSec
  , snapshotJobStatsFromSlot
  , withActiveJob
  )
import JShark.Compiler.CompileTiming
  ( FlatPrepareTiming (..)
  , cjsIrPrepareSec
  , cjsLintSec
  )
import qualified JShark.Compiler.Flat as Flat
import JShark.Compiler.FlatEnc (Enc (..))
import qualified JShark.Compiler.FlatEnc as FlatEnc
import qualified JShark.Compiler.FlatSoA as FlatSoA
import qualified JShark.Compiler.Ir as Ir
import JShark.Compiler.Lower
  ( lowerEffectAt
  , lowerOptEffectAt
  , reifyEffect
  )

flatDirectPackDeterministic :: ClosedEffect u -> Bool
flatDirectPackDeterministic e =
  let
    ir = irEffectFromClosed e
    soa1 = FlatSoA.packEffectProgramDirect ir
    soa2 = FlatSoA.packEffectProgramDirect ir
   in
    FlatSoA.soaColumnsEqual soa1 soa2

flatDirectPackForRangeOk :: Bool
flatDirectPackForRangeOk =
  let
    soa1 = FlatSoA.packEffectProgramDirect forRangeU8SetLoop
    soa2 = FlatSoA.packEffectProgramDirect forRangeU8SetLoop
   in
    FlatSoA.soaColumnsEqual soa1 soa2

-- | Composed 'lowerOptEffectAt' must match 'lowerEffectAt' then 'optIrEffect'.
lowerOptEffectRegressionOk :: Bool
lowerOptEffectRegressionOk =
  let
    ?keepLets = False
   in
    let
      probe = reifyEffect forRangeU8SetLoop
      (tLower, irLower) = lowerEffectAt (-2) probe
      (_, irOpt, mdOpt) = lowerOptEffectAt (-2) probe
      (_, irManual, mdManual) = Ir.optIrEffect tLower irLower
      nOpt = Ir.irSize mdOpt
      soaOpt = FlatSoA.packEffectProgramDirect irOpt
      soaManual = FlatSoA.packEffectProgramDirect irManual
     in
      FlatSoA.soaColumnsEqual soaOpt soaManual && nOpt == Ir.irSize mdManual

flatDirectPackOptimizeStable :: ClosedEffect u -> Bool
flatDirectPackOptimizeStable e =
  let
    ir = irEffectFromClosed e
    soa0 = FlatSoA.packEffectProgramDirect ir
    soa1 = FlatSoA.optimizeFlatPack soa0
    soa2 = FlatSoA.optimizeFlatPack soa1
   in
    FlatSoA.soaColumnsEqual soa1 soa2

-- | Every opcode in 'FlatEnc.flatOpTable' decodes, and re-encoding the
-- decoded node yields the same opcode and operand columns (side-table
-- rows re-allocate from index zero). Catches missing decode arms,
-- duplicate opcode values, and encode\/decode operand-order drift: a
-- new 'Flat.FlatNode' constructor without a matching opcode or with
-- swapped operands fails here instead of at emit time.
flatOpcodeRoundTripOk :: Bool
flatOpcodeRoundTripOk =
  length (nub ops) == n
    && length expectedRows == n
    && all roundTrip [0 .. n - 1]
 where
  ops = map snd FlatEnc.flatOpTable
  n = length ops
  soa =
    FlatSoA.FlatSoA
      { FlatSoA.fsaOpcodes = VU.fromList ops
      , FlatSoA.fsaA = VU.replicate n 1
      , FlatSoA.fsaB = VU.replicate n 2
      , FlatSoA.fsaC = VU.replicate n 3
      , FlatSoA.fsaD = VU.replicate n 4
      , FlatSoA.fsaE = VU.replicate n 5
      , FlatSoA.fsaPure = VU.replicate n 0
      , FlatSoA.fsaFixed =
          V.fromList [Flat.FlatFixedU FixArrLen 0, Flat.FlatFixedU FixArrLen 0]
      , FlatSoA.fsaFnLit = V.fromList [([0], [Nothing]), ([0], [Nothing])]
      , FlatSoA.fsaArrayGroups = V.fromList [V.fromList [0], V.fromList [0]]
      , FlatSoA.fsaLits = V.empty
      , FlatSoA.fsaTexts = V.empty
      , FlatSoA.fsaFFIs = V.empty
      , FlatSoA.fsaStrCases = V.empty
      , FlatSoA.fsaFieldGroups = V.empty
      , FlatSoA.fsaArgGroups = V.empty
      , FlatSoA.fsaHoistTags = V.empty
      , FlatSoA.fsaParamNames = V.empty
      , FlatSoA.fsaRoot = 0
      , FlatSoA.fsaSubtreeSizes = V.empty
      }
  roundTrip i =
    let
      node = FlatSoA.flatSoaNode soa i
      op = ops !! i
      name = fst (FlatEnc.flatOpTable !! i)
      Enc o a b c d e = fst (Flat.encodeFlatNode node Flat.emptySoaSideAcc)
     in
      o == op
        && [a, b, c, d, e] == expectedRow name
  expectedRow name =
    case lookup name expectedRows of
      Just row -> row
      Nothing -> error ("FlatTest.flatOpcodeRoundTripOk: no expected row for " ++ name)

-- | Canonical re-encoded operand columns for one synthetic row
-- @(a,b,c,d,e) = (1,2,3,4,5)@ per opcode, generated once and pinned:
-- encode reads back the operands decode unpacked, unused slots zero.
-- Side-table rows ('oFE_FIXED', 'oFE_FNLIT', 'oFX_ARRAYLIT')
-- re-allocate their table index from zero. A new opcode without an
-- entry here fails the round-trip test.
expectedRows :: [(String, [Int32])]
expectedRows =
  [ ("oFE_LITERAL", [1, 0, 0, 0, 0])
  , ("oFE_VAR", [1, 0, 0, 0, 0])
  , ("oFE_LET", [1, 2, 3, 0, 0])
  , ("oFE_LETREC", [1, 2, 3, 0, 0])
  , ("oFE_LAMBDA", [1, 2, 0, 0, 0])
  , ("oFE_APPLY", [1, 2, 0, 0, 0])
  , ("oFE_EMBEDEFF", [1, 0, 0, 0, 0])
  , ("oFE_IF", [1, 2, 3, 0, 0])
  , ("oFE_OPTIONCASE", [1, 2, 3, 4, 0])
  , ("oFE_RESOK", [1, 0, 0, 0, 0])
  , ("oFE_RESERR", [1, 0, 0, 0, 0])
  , ("oFE_RESCASE", [1, 2, 3, 4, 5])
  , ("oFE_INDEX", [1, 2, 0, 0, 0])
  , ("oFE_U8INDEX", [1, 2, 0, 0, 0])
  , ("oFE_ERROR", [1, 0, 0, 0, 0])
  , ("oFE_FIXED", [0, 0, 0, 0, 0])
  , ("oFE_FNLIT", [0, 2, 0, 0, 0])
  , ("oFE_FROZEN", [1, 0, 0, 0, 0])
  , ("oFE_GETFIELD", [1, 2, 0, 0, 0])
  , ("oFE_UNSAFENULL", [1, 0, 0, 0, 0])
  , ("oFE_KCONCAT", [1, 2, 0, 0, 0])
  , ("oFE_KPLUS", [1, 2, 0, 0, 0])
  , ("oFE_KTIMES", [1, 2, 0, 0, 0])
  , ("oFE_KMINUS", [1, 2, 0, 0, 0])
  , ("oFE_KNEG", [1, 0, 0, 0, 0])
  , ("oFE_KDIV", [1, 2, 0, 0, 0])
  , ("oFE_KREM", [1, 2, 0, 0, 0])
  , ("oFE_KBITAND", [1, 2, 0, 0, 0])
  , ("oFE_KBITOR", [1, 2, 0, 0, 0])
  , ("oFE_KBITXOR", [1, 2, 0, 0, 0])
  , ("oFE_KSHL", [1, 2, 0, 0, 0])
  , ("oFE_KSHR", [1, 2, 0, 0, 0])
  , ("oFE_KUSHR", [1, 2, 0, 0, 0])
  , ("oFE_KBIG", [1, 2, 3, 0, 0])
  , ("oFE_KBIGNEG", [1, 0, 0, 0, 0])
  , ("oFE_KAND", [1, 2, 0, 0, 0])
  , ("oFE_KOR", [1, 2, 0, 0, 0])
  , ("oFE_KEQ", [1, 2, 3, 0, 0])
  , ("oFE_KNEQ", [1, 2, 3, 0, 0])
  , ("oFE_KGTH", [1, 2, 0, 0, 0])
  , ("oFE_KLTH", [1, 2, 0, 0, 0])
  , ("oFE_KGTEQ", [1, 2, 0, 0, 0])
  , ("oFE_KLTEQ", [1, 2, 0, 0, 0])
  , ("oFE_KSHOW", [1, 0, 0, 0, 0])
  , ("oFE_KTYPEOF", [1, 0, 0, 0, 0])
  , ("oFE_MMAP", [1, 2, 3, 0, 0])
  , ("oFE_MFILTER", [1, 2, 3, 0, 0])
  , ("oFE_MREDUCE", [1, 2, 3, 4, 5])
  , ("oFE_MREDUCER", [1, 2, 3, 4, 5])
  , ("oFE_MTOSORTED", [1, 2, 3, 4, 0])
  , ("oFE_MFROM", [1, 2, 3, 0, 0])
  , ("oFE_HVM2REF", [1, 0, 0, 0, 0])
  , ("oFX_LIFT", [1, 0, 0, 0, 0])
  , ("oFX_FFI", [1, 2, 0, 0, 0])
  , ("oFX_UNSAFEOBJ", [1, 0, 0, 0, 0])
  , ("oFX_UNSAFEOBJGET", [1, 2, 0, 0, 0])
  , ("oFX_UNSAFEOBJSET", [1, 2, 0, 0, 0])
  , ("oFX_CALLMETHOD", [1, 2, 3, 0, 0])
  , ("oFX_BIND", [1, 2, 3, 0, 0])
  , ("oFX_THENE", [1, 2, 0, 0, 0])
  , ("oFX_BINDREC", [1, 2, 3, 0, 0])
  , ("oFX_LAMBDAE", [1, 2, 0, 0, 0])
  , ("oFX_APPLYE", [1, 2, 0, 0, 0])
  , ("oFX_IFE", [1, 2, 3, 0, 0])
  , ("oFX_WHILE", [1, 2, 0, 0, 0])
  , ("oFX_FORRANGE", [1, 2, 3, 4, 0])
  , ("oFX_U8SET", [1, 2, 3, 0, 0])
  , ("oFX_U8FILL", [1, 2, 0, 0, 0])
  , ("oFX_OPTCASEE", [1, 2, 3, 4, 0])
  , ("oFX_RESCASEE", [1, 2, 3, 4, 5])
  , ("oFX_STRCASEE", [1, 2, 3, 0, 0])
  , ("oFX_THROW", [1, 0, 0, 0, 0])
  , ("oFX_TRY", [1, 2, 3, 0, 0])
  , ("oFX_OBJLIT", [1, 0, 0, 0, 0])
  , ("oFX_DELETEPROP", [1, 2, 0, 0, 0])
  , ("oFX_ARRAYLIT", [0, 0, 0, 0, 0])
  ]

flatSoaPureNodeCount :: ClosedEffect u -> Int
flatSoaPureNodeCount e =
  let
    soa =
      FlatSoA.optimizeFlatPack
        (FlatSoA.packEffectProgramDirect (irEffectFromClosed e))
    n = FlatSoA.flatSoaNodeCount soa
   in
    countPure (FlatSoA.soaPureVector soa) n

countPure :: Vector Word8 -> Int -> Int
countPure v n =
  length [i | i <- [0 .. n - 1], i < V.length v, v V.! i == 1]

forRangeU8SetLoop :: Ir.IrEffect 'Unit
forRangeU8SetLoop =
  Ir.IrForRange
    (Ir.IrLiteral (ValueNumber 0))
    (Ir.IrLiteral (ValueNumber 4))
    0
    ( Ir.IrU8Set
        (Ir.IrVar 99)
        (Ir.IrLiteral (ValueNumber 0))
        (Ir.IrLiteral (ValueNumber 1))
    )

-- | @optIrEffect@ must mark @ForRange@ + @IrU8Set@ impure so flat codegen
-- keeps mutation loops.
optIrEffectForRangeImpure :: Bool
optIrEffectForRangeImpure =
  let
    ?keepLets = False
   in
    let
      (_, _, md) = Ir.optIrEffect 0 forRangeU8SetLoop
     in
      not (Ir.irPure md)

-- | Slot-backed timing refs survive snapshot after 'withActiveJob' returns.
batchJobSlotTimingOk :: IO Bool
batchJobSlotTimingOk = do
  board <- newProgressBoard 1
  _ <-
    withActiveJob 0 board $ do
      recordJobLintSec 0.001
      recordJobFlatPrepare
        FlatPrepareTiming
          { fptIrPrepareSec = 0.01
          , fptPackSec = 0
          , fptFlatOptSec = 0
          , fptTotalSec = 0.01
          }
      pure ()
  stats <- snapshotJobStatsFromSlot board 0 "test" 0.05
  pure (cjsLintSec stats == 0.001 && cjsIrPrepareSec stats == 0.01)
