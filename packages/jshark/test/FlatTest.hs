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
  , optIrEffectForRangeImpure
  , optConstantFoldPreservesInput
  , optConstantFoldManyLits
  , flatEmitOrderValidates
  )
where

import Data.Int (Int32)
import Data.List (nub)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8)
import JShark.Api.Types
  ( ClosedEffect
  , FixedOp (FixArrLen)
  , Value (..)
  )
import JShark.Internal (irEffectFromClosed)
import qualified JShark.Internal as Flat
import qualified JShark.Internal as Ir

flatDirectPackDeterministic :: ClosedEffect u -> Bool
flatDirectPackDeterministic e =
  let
    ir = irEffectFromClosed e
    soa1 = Flat.packProgramDirect ir
    soa2 = Flat.packProgramDirect ir
   in
    Flat.soaColumnsEqual soa1 soa2

flatDirectPackForRangeOk :: Bool
flatDirectPackForRangeOk =
  let
    soa1 = Flat.packProgramDirect forRangeU8SetLoop
    soa2 = Flat.packProgramDirect forRangeU8SetLoop
   in
    Flat.soaColumnsEqual soa1 soa2

flatDirectPackOptimizeStable :: ClosedEffect u -> Bool
flatDirectPackOptimizeStable e =
  let
    ir = irEffectFromClosed e
    soa0 = Flat.packProgramDirect ir
    soa1 = Flat.optimizeFlatPack soa0
    soa2 = Flat.optimizeFlatPack soa1
   in
    Flat.soaColumnsEqual soa1 soa2

-- | The emit order must be a child-before-parent traversal covering every
-- reachable node exactly once.
flatEmitOrderValidates :: ClosedEffect u -> Bool
flatEmitOrderValidates e =
  let
    soa = Flat.optimizeFlatPack (Flat.packProgramDirect (irEffectFromClosed e))
    root = Flat.fsaRoot soa
    order = Flat.flatSoaEmitOrder soa root
   in
    null (Flat.validateFlatSoaEmitOrder soa root order)

-- | Constant folding must be a pure function of its input: the caller may-- still hold (and reuse) the 'FlatSoA' it passed in. Mutating the immutable
-- input's backing store in place would corrupt it. Build a minimal
-- @1 + 2@ program that forces a fold, then check the input columns.
optConstantFoldPreservesInput :: Bool
optConstantFoldPreservesInput =
  let
    soa =
      Flat.FlatSoA
        { Flat.fsaOpcodes =
            VU.fromList (map Flat.opCode [Flat.FE_LITERAL, Flat.FE_LITERAL, Flat.FE_KPLUS])
        , Flat.fsaA = VU.fromList [0, 1, 0]
        , Flat.fsaB = VU.fromList [0, 0, 1]
        , Flat.fsaC = VU.replicate 3 0
        , Flat.fsaD = VU.replicate 3 0
        , Flat.fsaE = VU.replicate 3 0
        , Flat.fsaFixed = V.empty
        , Flat.fsaFnLit = V.empty
        , Flat.fsaArrayGroups = V.empty
        , Flat.fsaLits = V.fromList [Flat.FLit (ValueNumber 1), Flat.FLit (ValueNumber 2)]
        , Flat.fsaTexts = V.empty
        , Flat.fsaFFIs = V.empty
        , Flat.fsaStrCases = V.empty
        , Flat.fsaFieldGroups = V.empty
        , Flat.fsaArgGroups = V.empty
        , Flat.fsaHoistTags = V.empty
        , Flat.fsaParamNames = V.empty
        , Flat.fsaRoot = 2
        }
    ops0 = VU.toList (Flat.fsaOpcodes soa)
    a0 = VU.toList (Flat.fsaA soa)
    b0 = VU.toList (Flat.fsaB soa)
    lits0 = V.toList (Flat.fsaLits soa)
    (folded, changed) = Flat.optConstantFoldNumOnce soa
   in
    changed
      && VU.toList (Flat.fsaOpcodes soa) == ops0
      && VU.toList (Flat.fsaA soa) == a0
      && VU.toList (Flat.fsaB soa) == b0
      && V.length (Flat.fsaLits soa) == length lits0
      && Flat.fsaOpcodes folded /= VU.fromList ops0

-- | @k@ independent @1 + 2@ triples. One pass folds all of them and appends
-- @k@ literals, exercising the geometric literal-column growth; the result
-- must contain exactly @2 + k@ literals, none of them stale capacity.
optConstantFoldManyLits :: Int -> Bool
optConstantFoldManyLits k =
  let
    soa =
      Flat.FlatSoA
        { Flat.fsaOpcodes =
            VU.fromList
              ( concat
                  [ [ Flat.opCode Flat.FE_LITERAL
                    , Flat.opCode Flat.FE_LITERAL
                    , Flat.opCode Flat.FE_KPLUS
                    ]
                  | _ <- [1 .. k]
                  ]
              )
        , Flat.fsaA =
            VU.fromList
              (concat [[0, 1, fromIntegral (3 * i :: Int)] | i <- [0 .. k - 1]])
        , Flat.fsaB =
            VU.fromList
              (concat [[0, 0, fromIntegral (3 * i + 1 :: Int)] | i <- [0 .. k - 1]])
        , Flat.fsaC = VU.replicate (3 * k) 0
        , Flat.fsaD = VU.replicate (3 * k) 0
        , Flat.fsaE = VU.replicate (3 * k) 0
        , Flat.fsaFixed = V.empty
        , Flat.fsaFnLit = V.empty
        , Flat.fsaArrayGroups = V.empty
        , Flat.fsaLits = V.fromList [Flat.FLit (ValueNumber 1), Flat.FLit (ValueNumber 2)]
        , Flat.fsaTexts = V.empty
        , Flat.fsaFFIs = V.empty
        , Flat.fsaStrCases = V.empty
        , Flat.fsaFieldGroups = V.empty
        , Flat.fsaArgGroups = V.empty
        , Flat.fsaHoistTags = V.empty
        , Flat.fsaParamNames = V.empty
        , Flat.fsaRoot = 3 * k - 1
        }
    (folded, changed) = Flat.optConstantFoldNumOnce soa
   in
    changed
      && VU.all (== Flat.opCode Flat.FE_LITERAL) (Flat.fsaOpcodes folded)
      && V.length (Flat.fsaLits folded) == 2 + k
      && V.length (Flat.fsaLits soa) == 2

-- | Every opcode ('Flat.FlatOp', 'Flat.Bounded') decodes, and re-encoding the
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
  ops = map Flat.opCode ([minBound .. maxBound] :: [Flat.FlatOp])
  n = length ops
  soa =
    Flat.FlatSoA
      { Flat.fsaOpcodes = VU.fromList ops
      , Flat.fsaA = VU.replicate n 1
      , Flat.fsaB = VU.replicate n 2
      , Flat.fsaC = VU.replicate n 3
      , Flat.fsaD = VU.replicate n 4
      , Flat.fsaE = VU.replicate n 5
      , Flat.fsaFixed =
          V.fromList [Flat.FlatFixedU FixArrLen 0, Flat.FlatFixedU FixArrLen 0]
      , Flat.fsaFnLit = V.fromList [([0], [Nothing]), ([0], [Nothing])]
      , Flat.fsaArrayGroups = V.fromList [V.fromList [0], V.fromList [0]]
      , Flat.fsaLits = V.empty
      , Flat.fsaTexts = V.empty
      , Flat.fsaFFIs = V.empty
      , Flat.fsaStrCases = V.empty
      , Flat.fsaFieldGroups = V.empty
      , Flat.fsaArgGroups = V.empty
      , Flat.fsaHoistTags = V.empty
      , Flat.fsaParamNames = V.empty
      , Flat.fsaRoot = 0
      }
  roundTrip i =
    let
      node = Flat.flatSoaNode soa i
      op = ops !! i
      name = show (Flat.flatOpOf op)
      Flat.Enc o a b c d e = fst (Flat.encodeFlatNode node Flat.emptySoaSideAcc)
     in
      Flat.opCode o == op
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
  [ ("FE_LITERAL", [1, 0, 0, 0, 0])
  , ("FE_VAR", [1, 0, 0, 0, 0])
  , ("FE_LET", [1, 2, 3, 0, 0])
  , ("FE_LETREC", [1, 2, 3, 0, 0])
  , ("FE_LAMBDA", [1, 2, 0, 0, 0])
  , ("FE_APPLY", [1, 2, 0, 0, 0])
  , ("FE_EMBEDEFF", [1, 0, 0, 0, 0])
  , ("FE_IF", [1, 2, 3, 0, 0])
  , ("FE_OPTIONCASE", [1, 2, 3, 4, 0])
  , ("FE_RESOK", [1, 0, 0, 0, 0])
  , ("FE_RESERR", [1, 0, 0, 0, 0])
  , ("FE_RESCASE", [1, 2, 3, 4, 5])
  , ("FE_INDEX", [1, 2, 0, 0, 0])
  , ("FE_U8INDEX", [1, 2, 0, 0, 0])
  , ("FE_ERROR", [1, 0, 0, 0, 0])
  , ("FE_FIXED", [0, 0, 0, 0, 0])
  , ("FE_FNLIT", [0, 2, 0, 0, 0])
  , ("FE_FROZEN", [1, 0, 0, 0, 0])
  , ("FE_GETFIELD", [1, 2, 0, 0, 0])
  , ("FE_UNSAFENULL", [1, 0, 0, 0, 0])
  , ("FE_KCONCAT", [1, 2, 0, 0, 0])
  , ("FE_KPLUS", [1, 2, 0, 0, 0])
  , ("FE_KTIMES", [1, 2, 0, 0, 0])
  , ("FE_KMINUS", [1, 2, 0, 0, 0])
  , ("FE_KNEG", [1, 0, 0, 0, 0])
  , ("FE_KDIV", [1, 2, 0, 0, 0])
  , ("FE_KREM", [1, 2, 0, 0, 0])
  , ("FE_KBITAND", [1, 2, 0, 0, 0])
  , ("FE_KBITOR", [1, 2, 0, 0, 0])
  , ("FE_KBITXOR", [1, 2, 0, 0, 0])
  , ("FE_KSHL", [1, 2, 0, 0, 0])
  , ("FE_KSHR", [1, 2, 0, 0, 0])
  , ("FE_KUSHR", [1, 2, 0, 0, 0])
  , ("FE_KBIG", [1, 2, 3, 0, 0])
  , ("FE_KBIGNEG", [1, 0, 0, 0, 0])
  , ("FE_KAND", [1, 2, 0, 0, 0])
  , ("FE_KOR", [1, 2, 0, 0, 0])
  , ("FE_KEQ", [1, 2, 3, 0, 0])
  , ("FE_KNEQ", [1, 2, 3, 0, 0])
  , ("FE_KGTH", [1, 2, 0, 0, 0])
  , ("FE_KLTH", [1, 2, 0, 0, 0])
  , ("FE_KGTEQ", [1, 2, 0, 0, 0])
  , ("FE_KLTEQ", [1, 2, 0, 0, 0])
  , ("FE_KSHOW", [1, 0, 0, 0, 0])
  , ("FE_KTYPEOF", [1, 0, 0, 0, 0])
  , ("FE_MMAP", [1, 2, 3, 0, 0])
  , ("FE_MFILTER", [1, 2, 3, 0, 0])
  , ("FE_MREDUCE", [1, 2, 3, 4, 5])
  , ("FE_MREDUCER", [1, 2, 3, 4, 5])
  , ("FE_MTOSORTED", [1, 2, 3, 4, 0])
  , ("FE_MFROM", [1, 2, 3, 0, 0])
  , ("FX_LIFT", [1, 0, 0, 0, 0])
  , ("FX_EXTERN", [1, 2, 0, 0, 0])
  , ("FX_UNSAFEOBJ", [1, 0, 0, 0, 0])
  , ("FX_UNSAFEOBJGET", [1, 2, 0, 0, 0])
  , ("FX_UNSAFEOBJSET", [1, 2, 0, 0, 0])
  , ("FX_CALLMETHOD", [1, 2, 3, 0, 0])
  , ("FX_BIND", [1, 2, 3, 0, 0])
  , ("FX_THENE", [1, 2, 0, 0, 0])
  , ("FX_BINDREC", [1, 2, 3, 0, 0])
  , ("FX_LAMBDAE", [1, 2, 0, 0, 0])
  , ("FX_APPLYE", [1, 2, 0, 0, 0])
  , ("FX_IFE", [1, 2, 3, 0, 0])
  , ("FX_WHILE", [1, 2, 0, 0, 0])
  , ("FX_FORRANGE", [1, 2, 3, 4, 0])
  , ("FX_U8SET", [1, 2, 3, 0, 0])
  , ("FX_U8FILL", [1, 2, 0, 0, 0])
  , ("FX_OPTCASEE", [1, 2, 3, 4, 0])
  , ("FX_RESCASEE", [1, 2, 3, 4, 5])
  , ("FX_STRCASEE", [1, 2, 3, 0, 0])
  , ("FX_THROW", [1, 0, 0, 0, 0])
  , ("FX_TRY", [1, 2, 3, 0, 0])
  , ("FX_OBJLIT", [1, 0, 0, 0, 0])
  , ("FX_DELETEPROP", [1, 2, 0, 0, 0])
  , ("FX_ARRAYLIT", [0, 0, 0, 0, 0])
  ]

flatSoaPureNodeCount :: ClosedEffect u -> Int
flatSoaPureNodeCount e =
  let
    soa =
      Flat.optimizeFlatPack
        (Flat.packProgramDirect (irEffectFromClosed e))
    n = Flat.flatSoaNodeCount soa
   in
    countPure (Flat.soaPureVector soa) n

countPure :: Vector Word8 -> Int -> Int
countPure v n =
  length [i | i <- [0 .. n - 1], i < V.length v, v V.! i == 1]

forRangeU8SetLoop :: Ir.IrNode
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

-- | @optIr@ must mark @ForRange@ + @IrU8Set@ impure so flat codegen
-- keeps mutation loops.
optIrEffectForRangeImpure :: Bool
optIrEffectForRangeImpure =
  let
    ?keepLets = False
   in
    let
      (_, _, md) = Ir.optIr 0 forRangeU8SetLoop
     in
      not (Ir.irPure md)
