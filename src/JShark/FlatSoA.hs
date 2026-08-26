{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Struct-of-arrays flat IR and bulk-friendly optimizer passes.
--
-- Column arrays ('fsaOpcodes', 'fsaA'…'fsaE') are laid out for contiguous
-- scans; 'fromProgram' writes them in one pass without an intermediate
-- @[(Enc)]@. Optimizer passes that walk node indices sequentially
-- ('optConstantFoldNumOnce', 'propagatePureFlagsPass') remain
-- data-dependent and are poor SIMD targets; equality checks on columns
-- already delegate to unboxed vector primitives.
module JShark.FlatSoA
  ( FlatSoA (..)
  , fromProgram
  , toProgram
  , optimizeFlatProgram
  , flatSoaParallelThreshold
  , propagatePureFlags
  , optConstantFoldNum
  , optimizeSoA
  , packEffectProgramSoA
  , packEffectProgramDirect
  , optimizeFlatPack
  , soaPureCount
  , exprMask
  , soaColumnsEqual
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (runST)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Word (Word16, Word8)
import JShark.Flat
  ( FlatArg (..)
  , FlatField (..)
  , FlatFixed (..)
  , FlatLit (..)
  , FlatNode (..)
  , FlatProgram (..)
  , NodeId
  , fpPure
  , packEffectProgramState
  , buildFlatProgram
  )
import JShark.Ir (IrEffect)
import JShark.Types (BigBinOp (..), FFIForm (..), Value (..))
import Data.Maybe (catMaybes)
import GHC.IO.Unsafe (unsafePerformIO)

type Op = Word16

oFE_LITERAL
  , oFE_VAR
  , oFE_LET
  , oFE_LETREC
  , oFE_LAMBDA
  , oFE_APPLY
  , oFE_EMBEDEFF ::
    Op
oFE_IF
  , oFE_OPTIONCASE
  , oFE_RESOK
  , oFE_RESERR
  , oFE_RESCASE
  , oFE_INDEX
  , oFE_U8INDEX ::
    Op
oFE_ERROR
  , oFE_FIXED
  , oFE_FNLIT
  , oFE_FROZEN
  , oFE_GETFIELD
  , oFE_UNSAFENULL
  , oFE_KCONCAT ::
    Op
oFE_KPLUS
  , oFE_KTIMES
  , oFE_KMINUS
  , oFE_KNEG
  , oFE_KDIV
  , oFE_KREM
  , oFE_KBITAND
  , oFE_KBITOR ::
    Op
oFE_KBITXOR
  , oFE_KSHL
  , oFE_KSHR
  , oFE_KUSHR
  , oFE_KBIG
  , oFE_KBIGNEG
  , oFE_KAND
  , oFE_KOR ::
    Op
oFE_KEQ
  , oFE_KNEQ
  , oFE_KGTH
  , oFE_KLTH
  , oFE_KGTEQ
  , oFE_KLTEQ
  , oFE_KSHOW
  , oFE_KTYPEOF ::
    Op
oFE_MMAP
  , oFE_MFILTER
  , oFE_MREDUCE
  , oFE_MREDUCER
  , oFE_MTOSORTED
  , oFE_MFROM
  , oFE_HVM2REF ::
    Op
oFX_LIFT
  , oFX_FFI
  , oFX_UNSAFEOBJ
  , oFX_UNSAFEOBJGET
  , oFX_UNSAFEOBJSET
  , oFX_CALLMETHOD ::
    Op
oFX_BIND
  , oFX_THENE
  , oFX_BINDREC
  , oFX_LAMBDAE
  , oFX_APPLYE
  , oFX_IFE
  , oFX_WHILE
  , oFX_FORRANGE ::
    Op
oFX_U8SET
  , oFX_U8FILL
  , oFX_OPTCASEE
  , oFX_RESCASEE
  , oFX_STRCASEE
  , oFX_THROW
  , oFX_TRY ::
    Op
oFX_OBJLIT, oFX_DELETEPROP, oFX_ARRAYLIT :: Op
oFE_LITERAL = 1
oFE_VAR = 2
oFE_LET = 3
oFE_LETREC = 4
oFE_LAMBDA = 5
oFE_APPLY = 6
oFE_EMBEDEFF = 7

oFE_IF = 8

oFE_OPTIONCASE = 9

oFE_RESOK = 10

oFE_RESERR = 11

oFE_RESCASE = 12

oFE_INDEX = 13

oFE_U8INDEX = 14

oFE_ERROR = 15

oFE_FIXED = 16

oFE_FNLIT = 17

oFE_FROZEN = 18

oFE_GETFIELD = 19

oFE_UNSAFENULL = 20

oFE_KCONCAT = 21

oFE_KPLUS = 22

oFE_KTIMES = 23

oFE_KMINUS = 24

oFE_KNEG = 25

oFE_KDIV = 26

oFE_KREM = 27

oFE_KBITAND = 28

oFE_KBITOR = 29

oFE_KBITXOR = 30

oFE_KSHL = 31

oFE_KSHR = 32

oFE_KUSHR = 33

oFE_KBIG = 34

oFE_KBIGNEG = 35

oFE_KAND = 36

oFE_KOR = 37

oFE_KEQ = 38

oFE_KNEQ = 39

oFE_KGTH = 40

oFE_KLTH = 41

oFE_KGTEQ = 42

oFE_KLTEQ = 43

oFE_KSHOW = 44

oFE_KTYPEOF = 45

oFE_MMAP = 46

oFE_MFILTER = 47

oFE_MREDUCE = 48

oFE_MREDUCER = 49

oFE_MTOSORTED = 50

oFE_MFROM = 51

oFE_HVM2REF = 52

oFX_LIFT = 100

oFX_FFI = 101

oFX_UNSAFEOBJ = 102

oFX_UNSAFEOBJGET = 103

oFX_UNSAFEOBJSET = 104

oFX_CALLMETHOD = 105

oFX_BIND = 106

oFX_THENE = 107

oFX_BINDREC = 108

oFX_LAMBDAE = 109

oFX_APPLYE = 110

oFX_IFE = 111

oFX_WHILE = 112

oFX_FORRANGE = 113

oFX_U8SET = 114

oFX_U8FILL = 115

oFX_OPTCASEE = 116

oFX_RESCASEE = 117

oFX_STRCASEE = 118

oFX_THROW = 119

oFX_TRY = 120

oFX_OBJLIT = 121

oFX_DELETEPROP = 122

oFX_ARRAYLIT = 123

data FlatSoA = FlatSoA
  { fsaOpcodes :: !(VU.Vector Op)
  , fsaA :: !(VU.Vector Int32)
  , fsaB :: !(VU.Vector Int32)
  , fsaC :: !(VU.Vector Int32)
  , fsaD :: !(VU.Vector Int32)
  , fsaE :: !(VU.Vector Int32)
  , fsaPure :: !(VU.Vector Word8)
  , fsaFixed :: !(V.Vector FlatFixed)
  , fsaFnLit :: !(V.Vector ([Int], NodeId))
  , fsaArrayGroups :: !(V.Vector (V.Vector NodeId))
  , fsaLits :: !(V.Vector FlatLit)
  , fsaTexts :: !(V.Vector Text)
  , fsaFFIs :: !(V.Vector FFIForm)
  , fsaStrCases :: !(V.Vector [(Text, NodeId)])
  , fsaFieldGroups :: !(V.Vector [FlatField])
  , fsaArgGroups :: !(V.Vector [FlatArg])
  , fsaRoot :: !NodeId
  }

packEffectProgramSoA :: IrEffect u -> FlatSoA
packEffectProgramSoA e = fst (packEffectProgramDirect e)

-- | Pack IR to SoA + 'FlatProgram' in one encode pass (no 'validateFlatProgram').
packEffectProgramDirect :: IrEffect u -> (FlatSoA, FlatProgram)
packEffectProgramDirect e =
  let
    (root, st) = packEffectProgramState e
    prog = buildFlatProgram root st
   in
    (fromProgram prog, prog)

-- | SoA optimizer passes without re-packing from 'FlatProgram'.
optimizeFlatPack :: FlatSoA -> FlatProgram -> (FlatProgram, FlatSoA)
optimizeFlatPack soa0 prog0 =
  let
    (soa1, folded) = optConstantFoldNumWithChangedPar soa0
    soa2 = propagatePureFlagsPar soa1
    pureV = V.fromList (VU.toList (fsaPure soa2))
   in
    if folded
      then (toProgram soa2, soa2)
      else (prog0 {fpPure = pureV}, soa2)

i32 :: Int -> Int32
i32 = fromIntegral

data Enc = Enc !Op !Int32 !Int32 !Int32 !Int32 !Int32

fromProgram :: FlatProgram -> FlatSoA
fromProgram p =
  let
    nodes = fpNodes p
    n = V.length nodes
    pureV = pureColumn (fpPure p) n
   in
    runST $ do
      opM <- MVU.new n
      aM <- MVU.new n
      bM <- MVU.new n
      cM <- MVU.new n
      dM <- MVU.new n
      eM <- MVU.new n
      let
        writeEnc !i (Enc op a b c d e) = do
          MVU.write opM i op
          MVU.write aM i a
          MVU.write bM i b
          MVU.write cM i c
          MVU.write dM i d
          MVU.write eM i e
        go !i !fx !fl !ag
          | i >= n = pure (fx, fl, ag)
          | otherwise = do
              let
                (enc, fx', fl', ag') =
                  encodeNode (nodes V.! i) fx fl ag
              writeEnc i enc
              go (i + 1) fx' fl' ag'
      (fx, fl, ag) <- go 0 V.empty V.empty V.empty
      opF <- VU.unsafeFreeze opM
      aF <- VU.unsafeFreeze aM
      bF <- VU.unsafeFreeze bM
      cF <- VU.unsafeFreeze cM
      dF <- VU.unsafeFreeze dM
      eF <- VU.unsafeFreeze eM
      pure
        FlatSoA
          { fsaOpcodes = opF
          , fsaA = aF
          , fsaB = bF
          , fsaC = cF
          , fsaD = dF
          , fsaE = eF
          , fsaPure = pureV
          , fsaFixed = fx
          , fsaFnLit = fl
          , fsaArrayGroups = ag
          , fsaLits = fpLits p
          , fsaTexts = fpTexts p
          , fsaFFIs = fpFFIs p
          , fsaStrCases = fpStrCases p
          , fsaFieldGroups = fpFieldGroups p
          , fsaArgGroups = fpArgGroups p
          , fsaRoot = fpRoot p
          }
 where
  pureColumn fp n
    | V.length fp == n = VU.generate n (\i -> fp V.! i)
    | otherwise = VU.replicate n 0

  encodeNode node fx fl ag = case node of
    FE_Literal li -> (Enc oFE_LITERAL (i32 li) 0 0 0 0, fx, fl, ag)
    FE_Var v -> (Enc oFE_VAR (i32 v) 0 0 0 0, fx, fl, ag)
    FE_Let tag x b -> (Enc oFE_LET (i32 tag) (i32 x) (i32 b) 0 0, fx, fl, ag)
    FE_LetRec tag r b -> (Enc oFE_LETREC (i32 tag) (i32 r) (i32 b) 0 0, fx, fl, ag)
    FE_Lambda tag b -> (Enc oFE_LAMBDA (i32 tag) (i32 b) 0 0 0, fx, fl, ag)
    FE_Apply f x -> (Enc oFE_APPLY (i32 f) (i32 x) 0 0 0, fx, fl, ag)
    FE_EmbedEff e -> (Enc oFE_EMBEDEFF (i32 e) 0 0 0 0, fx, fl, ag)
    FE_If c t e -> (Enc oFE_IF (i32 c) (i32 t) (i32 e) 0 0, fx, fl, ag)
    FE_OptionCase o n tag s ->
      (Enc oFE_OPTIONCASE (i32 o) (i32 n) (i32 tag) (i32 s) 0, fx, fl, ag)
    FE_ResultOk x -> (Enc oFE_RESOK (i32 x) 0 0 0 0, fx, fl, ag)
    FE_ResultErr x -> (Enc oFE_RESERR (i32 x) 0 0 0 0, fx, fl, ag)
    FE_ResultCase o tagE er tagO ok ->
      ( Enc oFE_RESCASE (i32 o) (i32 tagE) (i32 er) (i32 tagO) (i32 ok)
      , fx
      , fl
      , ag
      )
    FE_Index a idx -> (Enc oFE_INDEX (i32 a) (i32 idx) 0 0 0, fx, fl, ag)
    FE_U8Index b idx -> (Enc oFE_U8INDEX (i32 b) (i32 idx) 0 0 0, fx, fl, ag)
    FE_Error m -> (Enc oFE_ERROR (i32 m) 0 0 0 0, fx, fl, ag)
    FE_Fixed fix ->
      let
        fi = V.length fx
       in
        (Enc oFE_FIXED (i32 fi) 0 0 0 0, fx `V.snoc` fix, fl, ag)
    FE_FnLit tags b ->
      let
        fi = V.length fl
       in
        (Enc oFE_FNLIT (i32 fi) (i32 b) 0 0 0, fx, fl `V.snoc` (tags, b), ag)
    FE_UnsafeNullable x -> (Enc oFE_UNSAFENULL (i32 x) 0 0 0 0, fx, fl, ag)
    FE_FrozenLit gi -> (Enc oFE_FROZEN (i32 gi) 0 0 0 0, fx, fl, ag)
    FE_GetField ti o -> (Enc oFE_GETFIELD (i32 ti) (i32 o) 0 0 0, fx, fl, ag)
    FE_Hvm2Ref ti -> (Enc oFE_HVM2REF (i32 ti) 0 0 0 0, fx, fl, ag)
    FE_KConcat x y -> (Enc oFE_KCONCAT (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KPlus x y -> (Enc oFE_KPLUS (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KTimes x y -> (Enc oFE_KTIMES (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KMinus x y -> (Enc oFE_KMINUS (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KNegate x -> (Enc oFE_KNEG (i32 x) 0 0 0 0, fx, fl, ag)
    FE_KFracDiv x y -> (Enc oFE_KDIV (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KRem x y -> (Enc oFE_KREM (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KBitAnd x y -> (Enc oFE_KBITAND (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KBitOr x y -> (Enc oFE_KBITOR (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KBitXor x y -> (Enc oFE_KBITXOR (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KShl x y -> (Enc oFE_KSHL (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KShr x y -> (Enc oFE_KSHR (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KUShr x y -> (Enc oFE_KUSHR (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KBig op x y -> (Enc oFE_KBIG (bigOpTag op) (i32 x) (i32 y) 0 0, fx, fl, ag)
    FE_KBigNeg x -> (Enc oFE_KBIGNEG (i32 x) 0 0 0 0, fx, fl, ag)
    FE_KAnd x y -> (Enc oFE_KAND (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KOr x y -> (Enc oFE_KOR (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KEq s x y -> (Enc oFE_KEQ (if s then 1 else 0) (i32 x) (i32 y) 0 0, fx, fl, ag)
    FE_KNEq s x y -> (Enc oFE_KNEQ (if s then 1 else 0) (i32 x) (i32 y) 0 0, fx, fl, ag)
    FE_KGTh x y -> (Enc oFE_KGTH (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KLTh x y -> (Enc oFE_KLTH (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KGTEq x y -> (Enc oFE_KGTEQ (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KLTEq x y -> (Enc oFE_KLTEQ (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FE_KShow x -> (Enc oFE_KSHOW (i32 x) 0 0 0 0, fx, fl, ag)
    FE_KTypeOf x -> (Enc oFE_KTYPEOF (i32 x) 0 0 0 0, fx, fl, ag)
    FE_MethMap a tag b -> (Enc oFE_MMAP (i32 a) (i32 tag) (i32 b) 0 0, fx, fl, ag)
    FE_MethFilter a tag b -> (Enc oFE_MFILTER (i32 a) (i32 tag) (i32 b) 0 0, fx, fl, ag)
    FE_MethReduce a z ta tb body ->
      (Enc oFE_MREDUCE (i32 a) (i32 z) (i32 ta) (i32 tb) (i32 body), fx, fl, ag)
    FE_MethReduceRight a z ta tb body ->
      (Enc oFE_MREDUCER (i32 a) (i32 z) (i32 ta) (i32 tb) (i32 body), fx, fl, ag)
    FE_MethToSorted a ta tb b ->
      (Enc oFE_MTOSORTED (i32 a) (i32 ta) (i32 tb) (i32 b) 0, fx, fl, ag)
    FE_MethFrom n tag b -> (Enc oFE_MFROM (i32 n) (i32 tag) (i32 b) 0 0, fx, fl, ag)
    FX_Lift x -> (Enc oFX_LIFT (i32 x) 0 0 0 0, fx, fl, ag)
    FX_FFI fi ai -> (Enc oFX_FFI (i32 fi) (i32 ai) 0 0 0, fx, fl, ag)
    FX_UnsafeObject t -> (Enc oFX_UNSAFEOBJ (i32 t) 0 0 0 0, fx, fl, ag)
    FX_UnsafeObjectGet x t -> (Enc oFX_UNSAFEOBJGET (i32 x) (i32 t) 0 0 0, fx, fl, ag)
    FX_UnsafeObjectAssign x y ->
      (Enc oFX_UNSAFEOBJSET (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FX_CallMethod r m ai -> (Enc oFX_CALLMETHOD (i32 r) (i32 m) (i32 ai) 0 0, fx, fl, ag)
    FX_Bind tag x b -> (Enc oFX_BIND (i32 tag) (i32 x) (i32 b) 0 0, fx, fl, ag)
    FX_ThenE x y -> (Enc oFX_THENE (i32 x) (i32 y) 0 0 0, fx, fl, ag)
    FX_BindRec tag r b -> (Enc oFX_BINDREC (i32 tag) (i32 r) (i32 b) 0 0, fx, fl, ag)
    FX_LambdaE tag b -> (Enc oFX_LAMBDAE (i32 tag) (i32 b) 0 0 0, fx, fl, ag)
    FX_ApplyE f x -> (Enc oFX_APPLYE (i32 f) (i32 x) 0 0 0, fx, fl, ag)
    FX_IfE c t e -> (Enc oFX_IFE (i32 c) (i32 t) (i32 e) 0 0, fx, fl, ag)
    FX_While c b -> (Enc oFX_WHILE (i32 c) (i32 b) 0 0 0, fx, fl, ag)
    FX_ForRange s e tag b ->
      (Enc oFX_FORRANGE (i32 s) (i32 e) (i32 tag) (i32 b) 0, fx, fl, ag)
    FX_U8Set b i v -> (Enc oFX_U8SET (i32 b) (i32 i) (i32 v) 0 0, fx, fl, ag)
    FX_U8Fill b v -> (Enc oFX_U8FILL (i32 b) (i32 v) 0 0 0, fx, fl, ag)
    FX_OptionCaseE o n tag s ->
      (Enc oFX_OPTCASEE (i32 o) (i32 n) (i32 tag) (i32 s) 0, fx, fl, ag)
    FX_ResultCaseE o tagE er tagO ok ->
      (Enc oFX_RESCASEE (i32 o) (i32 tagE) (i32 er) (i32 tagO) (i32 ok), fx, fl, ag)
    FX_StringCaseE s ai d -> (Enc oFX_STRCASEE (i32 s) (i32 ai) (i32 d) 0 0, fx, fl, ag)
    FX_Throw x -> (Enc oFX_THROW (i32 x) 0 0 0 0, fx, fl, ag)
    FX_Try a tag k -> (Enc oFX_TRY (i32 a) (i32 tag) (i32 k) 0 0, fx, fl, ag)
    FX_ObjectLit gi -> (Enc oFX_OBJLIT (i32 gi) 0 0 0 0, fx, fl, ag)
    FX_DeleteProp o k -> (Enc oFX_DELETEPROP (i32 o) (i32 k) 0 0 0, fx, fl, ag)
    FX_ArrayLit ns ->
      let
        ai = V.length ag
       in
        (Enc oFX_ARRAYLIT (i32 ai) 0 0 0 0, fx, fl, ag `V.snoc` V.fromList ns)

  bigOpTag :: BigBinOp -> Int32
  bigOpTag =
    fromIntegral . \case
      BPlus -> (0 :: Int)
      BMinus -> 1
      BTimes -> 2
      BQuot -> 3
      BRem -> 4
      BBitAnd -> 5
      BBitOr -> 6
      BBitXor -> 7
      BShl -> 8
      BShr -> 9

toProgram :: FlatSoA -> FlatProgram
toProgram soa =
  FlatProgram
    { fpNodes = V.generate (VU.length (fsaOpcodes soa)) decodeNode
    , fpLits = fsaLits soa
    , fpTexts = fsaTexts soa
    , fpFFIs = fsaFFIs soa
    , fpStrCases = fsaStrCases soa
    , fpFieldGroups = fsaFieldGroups soa
    , fpArgGroups = fsaArgGroups soa
    , fpPure = V.fromList (VU.toList (fsaPure soa))
    , fpRoot = fsaRoot soa
    }
 where
  decodeNode idx =
    let
      op = fsaOpcodes soa VU.! idx
      ix = fromIntegral (fsaA soa VU.! idx) :: Int
      iy = fromIntegral (fsaB soa VU.! idx) :: Int
      iz = fromIntegral (fsaC soa VU.! idx) :: Int
      iw = fromIntegral (fsaD soa VU.! idx) :: Int
      iv = fromIntegral (fsaE soa VU.! idx) :: Int
     in
      decodeOp soa op ix iy iz iw iv

decodeOp :: FlatSoA -> Op -> Int -> Int -> Int -> Int -> Int -> FlatNode
decodeOp soa op ix iy iz iw iv
  | op == oFE_LITERAL = FE_Literal ix
  | op == oFE_VAR = FE_Var ix
  | op == oFE_LET = FE_Let ix iy iz
  | op == oFE_LETREC = FE_LetRec ix iy iz
  | op == oFE_LAMBDA = FE_Lambda ix iy
  | op == oFE_APPLY = FE_Apply ix iy
  | op == oFE_EMBEDEFF = FE_EmbedEff ix
  | op == oFE_IF = FE_If ix iy iz
  | op == oFE_OPTIONCASE = FE_OptionCase ix iy iz iw
  | op == oFE_RESOK = FE_ResultOk ix
  | op == oFE_RESERR = FE_ResultErr ix
  | op == oFE_RESCASE = FE_ResultCase ix iy iz iw iv
  | op == oFE_INDEX = FE_Index ix iy
  | op == oFE_U8INDEX = FE_U8Index ix iy
  | op == oFE_ERROR = FE_Error ix
  | op == oFE_FIXED = FE_Fixed (fsaFixed soa V.! ix)
  | op == oFE_FNLIT = FE_FnLit (fst (fsaFnLit soa V.! ix)) iy
  | op == oFE_UNSAFENULL = FE_UnsafeNullable ix
  | op == oFE_FROZEN = FE_FrozenLit ix
  | op == oFE_GETFIELD = FE_GetField ix iy
  | op == oFE_HVM2REF = FE_Hvm2Ref ix
  | op == oFE_KCONCAT = FE_KConcat ix iy
  | op == oFE_KPLUS = FE_KPlus ix iy
  | op == oFE_KTIMES = FE_KTimes ix iy
  | op == oFE_KMINUS = FE_KMinus ix iy
  | op == oFE_KNEG = FE_KNegate ix
  | op == oFE_KDIV = FE_KFracDiv ix iy
  | op == oFE_KREM = FE_KRem ix iy
  | op == oFE_KBITAND = FE_KBitAnd ix iy
  | op == oFE_KBITOR = FE_KBitOr ix iy
  | op == oFE_KBITXOR = FE_KBitXor ix iy
  | op == oFE_KSHL = FE_KShl ix iy
  | op == oFE_KSHR = FE_KShr ix iy
  | op == oFE_KUSHR = FE_KUShr ix iy
  | op == oFE_KBIG = FE_KBig (tagBigOp (fromIntegral ix)) iy iz
  | op == oFE_KBIGNEG = FE_KBigNeg ix
  | op == oFE_KAND = FE_KAnd ix iy
  | op == oFE_KOR = FE_KOr ix iy
  | op == oFE_KEQ = FE_KEq (ix /= 0) iy iz
  | op == oFE_KNEQ = FE_KNEq (ix /= 0) iy iz
  | op == oFE_KGTH = FE_KGTh ix iy
  | op == oFE_KLTH = FE_KLTh ix iy
  | op == oFE_KGTEQ = FE_KGTEq ix iy
  | op == oFE_KLTEQ = FE_KLTEq ix iy
  | op == oFE_KSHOW = FE_KShow ix
  | op == oFE_KTYPEOF = FE_KTypeOf ix
  | op == oFE_MMAP = FE_MethMap ix iy iz
  | op == oFE_MFILTER = FE_MethFilter ix iy iz
  | op == oFE_MREDUCE = FE_MethReduce ix iy iz iw iv
  | op == oFE_MREDUCER = FE_MethReduceRight ix iy iz iw iv
  | op == oFE_MTOSORTED = FE_MethToSorted ix iy iz iw
  | op == oFE_MFROM = FE_MethFrom ix iy iz
  | op == oFX_LIFT = FX_Lift ix
  | op == oFX_FFI = FX_FFI ix iy
  | op == oFX_UNSAFEOBJ = FX_UnsafeObject ix
  | op == oFX_UNSAFEOBJGET = FX_UnsafeObjectGet ix iy
  | op == oFX_UNSAFEOBJSET = FX_UnsafeObjectAssign ix iy
  | op == oFX_CALLMETHOD = FX_CallMethod ix iy iz
  | op == oFX_BIND = FX_Bind ix iy iz
  | op == oFX_THENE = FX_ThenE ix iy
  | op == oFX_BINDREC = FX_BindRec ix iy iz
  | op == oFX_LAMBDAE = FX_LambdaE ix iy
  | op == oFX_APPLYE = FX_ApplyE ix iy
  | op == oFX_IFE = FX_IfE ix iy iz
  | op == oFX_WHILE = FX_While ix iy
  | op == oFX_FORRANGE = FX_ForRange ix iy iz iw
  | op == oFX_U8SET = FX_U8Set ix iy iz
  | op == oFX_U8FILL = FX_U8Fill ix iy
  | op == oFX_OPTCASEE = FX_OptionCaseE ix iy iz iw
  | op == oFX_RESCASEE = FX_ResultCaseE ix iy iz iw iv
  | op == oFX_STRCASEE = FX_StringCaseE ix iy iz
  | op == oFX_THROW = FX_Throw ix
  | op == oFX_TRY = FX_Try ix iy iz
  | op == oFX_OBJLIT = FX_ObjectLit ix
  | op == oFX_DELETEPROP = FX_DeleteProp ix iy
  | op == oFX_ARRAYLIT =
      FX_ArrayLit (V.toList (fsaArrayGroups soa V.! ix))
  | otherwise =
      unknownFlatOpcode op ix iy iz iw iv

unknownFlatOpcode :: Op -> Int -> Int -> Int -> Int -> Int -> FlatNode
unknownFlatOpcode op _ _ _ _ _ =
  error $
    "JShark.FlatSoA.decodeOp: unknown opcode "
      <> show (fromIntegral op :: Int)
      <> " (add a decode arm and test)"

tagBigOp :: Int32 -> BigBinOp
tagBigOp = \case
  0 -> BPlus
  1 -> BMinus
  2 -> BTimes
  3 -> BQuot
  4 -> BRem
  5 -> BBitAnd
  6 -> BBitOr
  7 -> BBitXor
  8 -> BShl
  9 -> BShr
  _ -> BPlus

exprMask :: FlatSoA -> VU.Vector Word8
exprMask soa =
  VU.map (\op -> if op < oFX_LIFT then 1 else 0) (fsaOpcodes soa)

propagatePureFlags :: FlatSoA -> FlatSoA
propagatePureFlags soa0 =
  let
    go soa =
      let
        (soa', changed) = propagatePureFlagsPass soa
       in
        if changed then go soa' else soa'
   in
    go soa0

propagatePureFlagsPass :: FlatSoA -> (FlatSoA, Bool)
propagatePureFlagsPass soa =
  let
    n = VU.length (fsaOpcodes soa)
    (pureV, changed) = runST $ do
      mp <- VU.unsafeThaw (fsaPure soa)
      changedRef <- newSTRef False
      let
        ch j = do
          let
            i = fromIntegral j :: Int
          if i >= 0 && i < n
            then MVU.read mp i
            else pure (0 :: Word8)
        bin j k = do
          x <- ch j
          y <- ch k
          pure (x .&. y)
        tri j k l = do
          x <- bin j k
          y <- ch l
          pure (x .&. y)
        andNodes ns =
          foldM
            (\acc j -> do p <- ch (i32 j); pure (acc .&. p))
            (1 :: Word8)
            ns
        pureFixed fi =
          case fsaFixed soa V.! fromIntegral fi of
            FlatFixedU _ j -> ch (i32 j)
            FlatFixedB _ j k -> bin (i32 j) (i32 k)
            FlatFixedT _ j k l -> tri (i32 j) (i32 k) (i32 l)
        pureArray gi =
          andNodes (V.toList (fsaArrayGroups soa V.! fromIntegral gi))
        pureFields gi =
          let
            fieldNode = \case
              FlatField _ j -> j
              FlatFieldEff _ j -> j
              FlatFieldExtra _ j -> j
              FlatFieldExtraEff _ j -> j
            ns = map fieldNode (fsaFieldGroups soa V.! fromIntegral gi)
           in
            andNodes ns

        pureForOp op a b c d e
          | op == oFE_LITERAL = pure 1
          | op == oFE_VAR = pure 1
          | op == oFE_FROZEN = pure 1
          | op == oFE_RESOK = ch a
          | op == oFE_RESERR = ch a
          | op == oFE_LET = bin b c
          | op == oFE_LETREC = bin b c
          | op == oFE_LAMBDA = ch b
          | op == oFE_APPLY = bin a b
          | op == oFE_IF = tri a b c
          | op == oFE_OPTIONCASE = tri a b d
          | op == oFE_RESCASE = tri a c e
          | op == oFE_INDEX = bin a b
          | op == oFE_U8INDEX = bin a b
          | op == oFE_FIXED = pureFixed a
          | op == oFE_FNLIT = ch b
          | op == oFE_GETFIELD = ch b
          | op == oFE_HVM2REF = pure 1
          | op == oFE_UNSAFENULL = ch a
          | op == oFE_KNEG = ch a
          | op == oFE_KBIGNEG = ch a
          | op == oFE_KSHOW = ch a
          | op == oFE_KTYPEOF = ch a
          | op == oFE_KEQ = bin b c
          | op == oFE_KNEQ = bin b c
          | op == oFE_KBIG = bin b c
          | op == oFE_MMAP = bin a c
          | op == oFE_MFILTER = bin a c
          | op == oFE_MREDUCE = tri a b e
          | op == oFE_MREDUCER = tri a b e
          | op == oFE_MTOSORTED = bin a d
          | op == oFE_MFROM = bin a c
          | op == oFX_LIFT = ch a
          | op == oFX_BIND = bin b c
          | op == oFX_THENE = bin a b
          | op == oFX_BINDREC = bin b c
          | op == oFX_LAMBDAE = ch b
          | op == oFX_IFE = tri a b c
          | op == oFX_FORRANGE = tri a b d
          | op == oFX_U8SET = tri a b c
          | op == oFX_U8FILL = bin a b
          | op == oFX_OPTCASEE = tri a b d
          | op == oFX_RESCASEE = tri a c e
          | op == oFX_STRCASEE = bin a c
          | op == oFX_THROW = ch a
          | op == oFX_TRY = bin a c
          | op == oFX_OBJLIT = pureFields a
          | op == oFX_DELETEPROP = bin a b
          | op == oFX_ARRAYLIT = pureArray a
          | op < oFX_LIFT = bin a b
          | otherwise = pure 0

        pureAt idx = do
          let
            op = fsaOpcodes soa VU.! idx
            a = fsaA soa VU.! idx
            b = fsaB soa VU.! idx
            c = fsaC soa VU.! idx
            d = fsaD soa VU.! idx
            e = fsaE soa VU.! idx
          if impureOp op
            then pure (0 :: Word8)
            else pureForOp op a b c d e
       in
        forM_ [0 .. n - 1] $ \idx -> do
          p <- pureAt idx
          old <- MVU.read mp idx
          when (p /= old) (writeSTRef changedRef True)
          MVU.write mp idx p
      ch <- readSTRef changedRef
      pureV' <- VU.unsafeFreeze mp
      pure (pureV', ch)
   in
    (soa {fsaPure = pureV}, changed)
 where
  impureOp :: Op -> Bool
  impureOp op
    | op >= oFX_LIFT =
        op
          `elem` [ oFX_FFI
                 , oFX_UNSAFEOBJ
                 , oFX_UNSAFEOBJGET
                 , oFX_UNSAFEOBJSET
                 , oFX_CALLMETHOD
                 , oFX_APPLYE
                 , oFX_WHILE
                 , oFX_FORRANGE
                 , oFX_U8SET
                 , oFX_U8FILL
                 , oFX_THROW
                 , oFX_DELETEPROP
                 ]
    | otherwise = op `elem` [oFE_ERROR, oFE_EMBEDEFF]

soaPureCount :: FlatSoA -> Int
soaPureCount = fromIntegral . VU.sum . fsaPure

litAsNumber :: FlatLit -> Maybe Double
litAsNumber (FLit (ValueNumber d)) = Just d
litAsNumber _ = Nothing

optConstantFoldNumOnce :: FlatSoA -> (FlatSoA, Bool)
optConstantFoldNumOnce soa0 = runST $ do
  let
    n = VU.length (fsaOpcodes soa0)
  opM <- VU.unsafeThaw (fsaOpcodes soa0)
  aM <- VU.unsafeThaw (fsaA soa0)
  bM <- VU.unsafeThaw (fsaB soa0)
  litsRef <- newSTRef =<< V.unsafeThaw (fsaLits soa0)
  changedRef <- newSTRef False
  let
    readOp i = MVU.read opM i
    readA i = MVU.read aM i
    readLitM li = do
      litsM <- readSTRef litsRef
      v <- GM.read litsM (fromIntegral (li :: Int32))
      pure (litAsNumber v)
    addLit d = do
      litsM <- readSTRef litsRef
      let
        li = GM.length litsM
      litsM' <- GM.unsafeGrow litsM 1
      GM.unsafeWrite litsM' li (FLit (ValueNumber d))
      writeSTRef litsRef litsM'
      pure (i32 li)
    tryFold i = do
      op <- readOp i
      case () of
        _ | op == oFE_KPLUS -> foldAt i (+)
        _ | op == oFE_KTIMES -> foldAt i (*)
        _ | op == oFE_KMINUS -> foldAt i (-)
        _ -> pure ()
    foldAt i f = do
      x <- readA i
      y <- MVU.read bM i
      ox <- readOp (fromIntegral x)
      oy <- readOp (fromIntegral y)
      when (ox == oFE_LITERAL && oy == oFE_LITERAL) $ do
        lix <- readA (fromIntegral x)
        liy <- readA (fromIntegral y)
        mdx <- readLitM lix
        mdy <- readLitM liy
        case (mdx, mdy) of
          (Just dx, Just dy) -> do
            liNew <- addLit (f dx dy)
            MVU.write opM i oFE_LITERAL
            MVU.write aM i liNew
            MVU.write bM i 0
            writeSTRef changedRef True
          _ -> pure ()
  forM_ [0 .. n - 1] tryFold
  opF <- VU.unsafeFreeze opM
  aF <- VU.unsafeFreeze aM
  litsM <- readSTRef litsRef
  litsF <- V.unsafeFreeze litsM
  changed <- readSTRef changedRef
  pure (soa0 {fsaOpcodes = opF, fsaA = aF, fsaLits = litsF}, changed)

optConstantFoldNum :: FlatSoA -> FlatSoA
optConstantFoldNum soa0 =
  fst (optConstantFoldNumWithChanged soa0)

optConstantFoldNumWithChanged :: FlatSoA -> (FlatSoA, Bool)
optConstantFoldNumWithChanged soa0 =
  let
    go soa didFold =
      let
        (soa', changed) = optConstantFoldNumOnce soa
       in
        if changed then go soa' True else (soa, didFold)
   in
    go soa0 False

soaColumnsEqual :: FlatSoA -> FlatSoA -> Bool
soaColumnsEqual a b =
  soaUnboxedEqual a b
    && soaSideLengthsEqual a b
    && fsaTexts a == fsaTexts b
    && fsaStrCases a == fsaStrCases b
    && fsaFnLit a == fsaFnLit b
    && fsaArrayGroups a == fsaArrayGroups b
    && fsaArgGroups a == fsaArgGroups b
    && fsaFieldGroups a == fsaFieldGroups b
    && soaUnboxedEqual a (fromProgram (toProgram a))
    && soaUnboxedEqual b (fromProgram (toProgram b))

soaUnboxedEqual :: FlatSoA -> FlatSoA -> Bool
soaUnboxedEqual a b =
  fsaOpcodes a == fsaOpcodes b
    && fsaA a == fsaA b
    && fsaB a == fsaB b
    && fsaC a == fsaC b
    && fsaD a == fsaD b
    && fsaE a == fsaE b
    && fsaPure a == fsaPure b
    && fsaRoot a == fsaRoot b

soaSideLengthsEqual :: FlatSoA -> FlatSoA -> Bool
soaSideLengthsEqual a b =
  V.length (fsaFixed a) == V.length (fsaFixed b)
    && V.length (fsaLits a) == V.length (fsaLits b)
    && V.length (fsaFFIs a) == V.length (fsaFFIs b)

-- | Node count above which SoA passes use chunked 'mapConcurrently'.
flatSoaParallelThreshold :: Int
flatSoaParallelThreshold = 4096

chunkRanges :: Int -> Int -> [[Int]]
chunkRanges n chunk =
  let
    go lo acc
      | lo >= n = reverse acc
      | otherwise =
          let
            hi = min n (lo + chunk)
           in
            go hi ([lo .. hi - 1] : acc)
   in
    go 0 []

propagatePureFlagsPassPar :: FlatSoA -> (FlatSoA, Bool)
propagatePureFlagsPassPar soa
  | VU.length (fsaOpcodes soa) < flatSoaParallelThreshold =
      propagatePureFlagsPass soa
  | otherwise =
      unsafePerformIO $
        propagatePureFlagsPassIO soa
{-# NOINLINE propagatePureFlagsPassPar #-}

propagatePureFlagsPassIO :: FlatSoA -> IO (FlatSoA, Bool)
propagatePureFlagsPassIO soa = do
  let
    n = VU.length (fsaOpcodes soa)
  caps <- max 1 <$> getNumCapabilities
  let
    chunk = max 256 (n `div` (caps * 4))
    ranges = chunkRanges n chunk
  mp <- VU.unsafeThaw (fsaPure soa)
  changedRef <- newIORef False
  let
    ch j = do
      let
        i = fromIntegral j :: Int
      if i >= 0 && i < n
        then MVU.read mp i
        else pure (0 :: Word8)
    bin j k = do
      x <- ch j
      y <- ch k
      pure (x .&. y)
    tri j k l = do
      x <- bin j k
      y <- ch l
      pure (x .&. y)
    andNodes ns =
      foldM
        (\acc j -> do p <- ch (i32 j); pure (acc .&. p))
        (1 :: Word8)
        ns
    pureFixed fi =
      case fsaFixed soa V.! fromIntegral fi of
        FlatFixedU _ j -> ch (i32 j)
        FlatFixedB _ j k -> bin (i32 j) (i32 k)
        FlatFixedT _ j k l -> tri (i32 j) (i32 k) (i32 l)
    pureArray gi =
      andNodes (V.toList (fsaArrayGroups soa V.! fromIntegral gi))
    pureFields gi =
      let
        fieldNode = \case
          FlatField _ j -> j
          FlatFieldEff _ j -> j
          FlatFieldExtra _ j -> j
          FlatFieldExtraEff _ j -> j
        ns = map fieldNode (fsaFieldGroups soa V.! fromIntegral gi)
       in
        andNodes ns
    pureForOp op a b c d e
      | op == oFE_LITERAL = pure 1
      | op == oFE_VAR = pure 1
      | op == oFE_FROZEN = pure 1
      | op == oFE_RESOK = ch a
      | op == oFE_RESERR = ch a
      | op == oFE_LET = bin b c
      | op == oFE_LETREC = bin b c
      | op == oFE_LAMBDA = ch b
      | op == oFE_APPLY = bin a b
      | op == oFE_IF = tri a b c
      | op == oFE_OPTIONCASE = tri a b d
      | op == oFE_RESCASE = tri a c e
      | op == oFE_INDEX = bin a b
      | op == oFE_U8INDEX = bin a b
      | op == oFE_FIXED = pureFixed a
      | op == oFE_FNLIT = ch b
      | op == oFE_GETFIELD = ch b
      | op == oFE_HVM2REF = pure 1
      | op == oFE_UNSAFENULL = ch a
      | op == oFE_KNEG = ch a
      | op == oFE_KBIGNEG = ch a
      | op == oFE_KSHOW = ch a
      | op == oFE_KTYPEOF = ch a
      | op == oFE_KEQ = bin b c
      | op == oFE_KNEQ = bin b c
      | op == oFE_KBIG = bin b c
      | op == oFE_MMAP = bin a c
      | op == oFE_MFILTER = bin a c
      | op == oFE_MREDUCE = tri a b e
      | op == oFE_MREDUCER = tri a b e
      | op == oFE_MTOSORTED = bin a d
      | op == oFE_MFROM = bin a c
      | op == oFX_LIFT = ch a
      | op == oFX_BIND = bin b c
      | op == oFX_THENE = bin a b
      | op == oFX_BINDREC = bin b c
      | op == oFX_LAMBDAE = ch b
      | op == oFX_IFE = tri a b c
      | op == oFX_FORRANGE = tri a b d
      | op == oFX_U8SET = tri a b c
      | op == oFX_U8FILL = bin a b
      | op == oFX_OPTCASEE = tri a b d
      | op == oFX_RESCASEE = tri a c e
      | op == oFX_STRCASEE = bin a c
      | op == oFX_THROW = ch a
      | op == oFX_TRY = bin a c
      | op == oFX_OBJLIT = pureFields a
      | op == oFX_DELETEPROP = bin a b
      | op == oFX_ARRAYLIT = pureArray a
      | op < oFX_LIFT = bin a b
      | otherwise = pure 0
    runIdx idx = do
      let
        op = fsaOpcodes soa VU.! idx
        a = fsaA soa VU.! idx
        b = fsaB soa VU.! idx
        c = fsaC soa VU.! idx
        d = fsaD soa VU.! idx
        e = fsaE soa VU.! idx
      p <-
        if impureOp op
          then pure (0 :: Word8)
          else pureForOp op a b c d e
      old <- MVU.read mp idx
      when (p /= old) (writeIORef changedRef True)
      MVU.write mp idx p
  _ <- mapConcurrently (\ixs -> forM_ ixs runIdx) ranges
  changed <- readIORef changedRef
  pureV' <- VU.unsafeFreeze mp
  pure (soa {fsaPure = pureV'}, changed)
 where
  impureOp op
    | op >= oFX_LIFT =
        op
          `elem` [ oFX_FFI
                 , oFX_UNSAFEOBJ
                 , oFX_UNSAFEOBJGET
                 , oFX_UNSAFEOBJSET
                 , oFX_CALLMETHOD
                 , oFX_APPLYE
                 , oFX_WHILE
                 , oFX_FORRANGE
                 , oFX_U8SET
                 , oFX_U8FILL
                 , oFX_THROW
                 , oFX_DELETEPROP
                 ]
    | otherwise = op `elem` [oFE_ERROR, oFE_EMBEDEFF]

propagatePureFlagsPar :: FlatSoA -> FlatSoA
propagatePureFlagsPar soa0 =
  let
    go soa =
      let
        (soa', changed) = propagatePureFlagsPassPar soa
       in
        if changed then go soa' else soa'
   in
    go soa0

optConstantFoldNumOncePar :: FlatSoA -> (FlatSoA, Bool)
optConstantFoldNumOncePar soa0
  | VU.length (fsaOpcodes soa0) < flatSoaParallelThreshold =
      optConstantFoldNumOnce soa0
  | otherwise =
      unsafePerformIO $
        optConstantFoldNumOnceIO soa0
{-# NOINLINE optConstantFoldNumOncePar #-}

optConstantFoldNumOnceIO :: FlatSoA -> IO (FlatSoA, Bool)
optConstantFoldNumOnceIO soa0 = do
  let
    n = VU.length (fsaOpcodes soa0)
  caps <- max 1 <$> getNumCapabilities
  let
    chunk = max 256 (n `div` (caps * 4))
    ranges = chunkRanges n chunk
    readLit li = litAsNumber (fsaLits soa0 V.! fromIntegral (li :: Int32))
    scanFold i = do
      let
        op = fsaOpcodes soa0 VU.! i
        x = fsaA soa0 VU.! i
        y = fsaB soa0 VU.! i
      case op of
        o
          | o == oFE_KPLUS ->
              tryPair i x y (+)
        o
          | o == oFE_KTIMES ->
              tryPair i x y (*)
        o
          | o == oFE_KMINUS ->
              tryPair i x y (-)
        _ -> pure Nothing
     where
      tryPair idx xa ya f = do
        let
          ox = fsaOpcodes soa0 VU.! fromIntegral xa
          oy = fsaOpcodes soa0 VU.! fromIntegral ya
        if ox == oFE_LITERAL && oy == oFE_LITERAL
          then case (readLit xa, readLit ya) of
            (Just dx, Just dy) -> pure (Just (idx, f dx dy))
            _ -> pure Nothing
          else pure Nothing
  hitsNested <- mapConcurrently (\ixs -> mapM scanFold ixs) ranges
  let
    hits = catMaybes (concat hitsNested)
  if null hits
    then pure (soa0, False)
    else do
      opM <- VU.unsafeThaw (fsaOpcodes soa0)
      aM <- VU.unsafeThaw (fsaA soa0)
      bM <- VU.unsafeThaw (fsaB soa0)
      litsRef <- newIORef (fsaLits soa0)
      forM_ hits $ \(i, d) -> do
        lits <- readIORef litsRef
        let
          liNew = i32 (V.length lits)
          lits' = lits V.// [(V.length lits, FLit (ValueNumber d))]
        writeIORef litsRef lits'
        MVU.write opM i oFE_LITERAL
        MVU.write aM i liNew
        MVU.write bM i 0
      opF <- VU.unsafeFreeze opM
      aF <- VU.unsafeFreeze aM
      litsF <- readIORef litsRef
      pure (soa0 {fsaOpcodes = opF, fsaA = aF, fsaLits = litsF}, True)

optConstantFoldNumWithChangedPar :: FlatSoA -> (FlatSoA, Bool)
optConstantFoldNumWithChangedPar soa0 =
  let
    go soa didFold =
      let
        (soa', changed) = optConstantFoldNumOncePar soa
       in
        if changed then go soa' True else (soa, didFold)
   in
    go soa0 False

-- | Run SoA opts and attach 'fpPure' (staging metadata; emit ignores for now).
optimizeFlatProgram :: FlatProgram -> FlatProgram
optimizeFlatProgram p =
  let
    soa0 = fromProgram p
    (soa1, folded) = optConstantFoldNumWithChangedPar soa0
    soa2 = propagatePureFlagsPar soa1
    pureV = V.fromList (VU.toList (fsaPure soa2))
   in
    if folded
      then toProgram soa2
      else p {fpPure = pureV}

optimizeSoA :: FlatSoA -> FlatSoA
optimizeSoA soa =
  propagatePureFlags (optConstantFoldNum soa)
