{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

-- | Untyped flat IR: pack + frozen SoA view + bulk passes.
--
-- Internal to the JShark compiler; this module is exposed for tests and
-- tooling and its API may change between 0.x releases.
module JShark.Compiler.Flat
  ( NodeId
  , FlatNode (..)
  , FlatArg (..)
  , FlatField (..)
  , FlatFixed (..)
  , FlatLit (FLit)
  , FlatOp (..)
  , Op
  , opCode
  , flatOpOf
  , Enc (..)
  , runPack
  , freezePackColumns
  , encodeFlatNode
  , emptySoaSideAcc
  , flatOpIsEffect
  , irNodeIsEffect
  , flatNodeChildRefs
  , flatArgRef
  , flatFieldRef
  , packStateNodeCount
  , packStateSoaSide
  , packStateSideTables
  , packStateHoistTags
  , packStateParamNames
  , PackState
  , sideAccToVectors
  , FlatSoA (..)
  , packProgramDirect
  , optimizeFlatPack
  , flatSoaNodeCount
  , flatSoaNode
  , withFlatLitValue
  , flatSoaText
  , flatSoaFFI
  , flatSoaStrCases
  , flatSoaFieldGroup
  , flatSoaArgGroup
  , flatSoaNodePackRefs
  , flatSoaHoistTag
  , flatSoaParamName
  , flatSoaLayerBuckets
  , soaPureCount
  , soaPureVector
  , constantFoldWithStats
  , optConstantFoldNumOnce
  , soaColumnsEqual
  )
where

import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (StateT, get, modify, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Bits ((.&.))
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Word (Word16, Word8)
import JShark.Api.Types
  ( BigBinOp (..)
  , FFIForm (..)
  , FixedOp
  , LamInfo (..)
  , Value (..)
  )
import JShark.Compiler.Ir

type NodeId = Int

-- | One opcode per 'FlatNode' constructor. Expression ops sort below
-- effect ops ('FX_LIFT' is the first effect opcode); range checks rely
-- on that ordering.
data FlatOp
  = FE_LITERAL
  | FE_VAR
  | FE_LET
  | FE_LETREC
  | FE_LAMBDA
  | FE_APPLY
  | FE_EMBEDEFF
  | FE_IF
  | FE_OPTIONCASE
  | FE_RESOK
  | FE_RESERR
  | FE_RESCASE
  | FE_INDEX
  | FE_U8INDEX
  | FE_ERROR
  | FE_FIXED
  | FE_FNLIT
  | FE_FROZEN
  | FE_GETFIELD
  | FE_UNSAFENULL
  | FE_KCONCAT
  | FE_KPLUS
  | FE_KTIMES
  | FE_KMINUS
  | FE_KNEG
  | FE_KDIV
  | FE_KREM
  | FE_KBITAND
  | FE_KBITOR
  | FE_KBITXOR
  | FE_KSHL
  | FE_KSHR
  | FE_KUSHR
  | FE_KBIG
  | FE_KBIGNEG
  | FE_KAND
  | FE_KOR
  | FE_KEQ
  | FE_KNEQ
  | FE_KGTH
  | FE_KLTH
  | FE_KGTEQ
  | FE_KLTEQ
  | FE_KSHOW
  | FE_KTYPEOF
  | FE_MMAP
  | FE_MFILTER
  | FE_MREDUCE
  | FE_MREDUCER
  | FE_MTOSORTED
  | FE_MFROM
  | FX_LIFT
  | FX_EXTERN
  | FX_UNSAFEOBJ
  | FX_UNSAFEOBJGET
  | FX_UNSAFEOBJSET
  | FX_CALLMETHOD
  | FX_BIND
  | FX_THENE
  | FX_BINDREC
  | FX_LAMBDAE
  | FX_APPLYE
  | FX_IFE
  | FX_WHILE
  | FX_FORRANGE
  | FX_U8SET
  | FX_U8FILL
  | FX_OPTCASEE
  | FX_RESCASEE
  | FX_STRCASEE
  | FX_THROW
  | FX_TRY
  | FX_OBJLIT
  | FX_DELETEPROP
  | FX_ARRAYLIT
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Opcodes are stored in an unboxed 'Word16' column.
type Op = Word16

opCode :: FlatOp -> Op
opCode = fromIntegral . fromEnum

flatOpOf :: Op -> FlatOp
flatOpOf = toEnum . fromIntegral

-- | Transient encoded row: the opcode plus five operand slots.
data Enc = Enc !FlatOp !Int32 !Int32 !Int32 !Int32 !Int32

data FlatLit where
  FLit :: Value u -> FlatLit

data FlatArg
  = FlatArgExpr NodeId
  | FlatArgEffect NodeId
  deriving Eq

data FlatField
  = FlatField Text NodeId
  | FlatFieldEff Text NodeId
  | FlatFieldExtra Text NodeId
  | FlatFieldExtraEff Text NodeId
  deriving Eq

data FlatFixed where
  FlatFixedU :: FixedOp a b c u -> NodeId -> FlatFixed
  FlatFixedB :: FixedOp a b c u -> NodeId -> NodeId -> FlatFixed
  FlatFixedT :: FixedOp a b c u -> NodeId -> NodeId -> NodeId -> FlatFixed

data FlatNode
  = -- expr (IrExpr)
    FE_Literal Int
  | FE_Var Int
  | FE_Let Int NodeId NodeId
  | FE_LetRec Int NodeId NodeId
  | FE_Lambda Int NodeId
  | FE_Apply NodeId NodeId
  | FE_EmbedEff NodeId
  | FE_If NodeId NodeId NodeId
  | FE_OptionCase NodeId NodeId Int NodeId
  | FE_ResultOk NodeId
  | FE_ResultErr NodeId
  | FE_ResultCase NodeId Int NodeId Int NodeId
  | FE_Index NodeId NodeId
  | FE_U8Index NodeId NodeId
  | FE_Error NodeId
  | FE_Fixed FlatFixed
  | FE_KConcat NodeId NodeId
  | FE_KPlus NodeId NodeId
  | FE_KTimes NodeId NodeId
  | FE_KMinus NodeId NodeId
  | FE_KNegate NodeId
  | FE_KFracDiv NodeId NodeId
  | FE_KRem NodeId NodeId
  | FE_KBitAnd NodeId NodeId
  | FE_KBitOr NodeId NodeId
  | FE_KBitXor NodeId NodeId
  | FE_KShl NodeId NodeId
  | FE_KShr NodeId NodeId
  | FE_KUShr NodeId NodeId
  | FE_KBig BigBinOp NodeId NodeId
  | FE_KBigNeg NodeId
  | FE_KAnd NodeId NodeId
  | FE_KOr NodeId NodeId
  | FE_KEq Bool NodeId NodeId
  | FE_KNEq Bool NodeId NodeId
  | FE_KGTh NodeId NodeId
  | FE_KLTh NodeId NodeId
  | FE_KGTEq NodeId NodeId
  | FE_KLTEq NodeId NodeId
  | FE_KShow NodeId
  | FE_KTypeOf NodeId
  | FE_MethMap NodeId Int NodeId
  | FE_MethFilter NodeId Int NodeId
  | FE_MethReduce NodeId NodeId Int Int NodeId
  | FE_MethReduceRight NodeId NodeId Int Int NodeId
  | FE_MethToSorted NodeId Int Int NodeId
  | FE_MethFrom NodeId Int NodeId
  | FE_FnLit [Int] [Maybe Text] NodeId
  | FE_UnsafeNullable NodeId
  | FE_FrozenLit Int
  | FE_GetField Int NodeId
  | -- effect (IrEffect)
    FX_Lift NodeId
  | FX_FFI Int Int
  | FX_UnsafeObject Int
  | FX_UnsafeObjectGet NodeId Int
  | FX_UnsafeObjectAssign NodeId NodeId
  | FX_CallMethod NodeId Int Int
  | FX_Bind Int NodeId NodeId
  | FX_ThenE NodeId NodeId
  | FX_BindRec Int NodeId NodeId
  | FX_LambdaE Int NodeId
  | FX_ApplyE NodeId NodeId
  | FX_IfE NodeId NodeId NodeId
  | FX_While NodeId NodeId
  | FX_ForRange NodeId NodeId Int NodeId
  | FX_U8Set NodeId NodeId NodeId
  | FX_U8Fill NodeId NodeId
  | FX_OptionCaseE NodeId NodeId Int NodeId
  | FX_ResultCaseE NodeId Int NodeId Int NodeId
  | FX_StringCaseE NodeId Int NodeId
  | FX_Throw NodeId
  | FX_Try NodeId Int NodeId
  | FX_ObjectLit Int
  | FX_DeleteProp NodeId NodeId
  | FX_ArrayLit [NodeId]

data SoaSideAcc = SoaSideAcc
  { saFixed :: !(Seq FlatFixed)
  , saFixedCount :: !Int
  , saFnLit :: !(Seq ([Int], [Maybe Text]))
  , saFnLitCount :: !Int
  , saArrays :: !(Seq [NodeId])
  , saArrayCount :: !Int
  }

emptySoaSideAcc :: SoaSideAcc
emptySoaSideAcc =
  SoaSideAcc
    { saFixed = Seq.empty
    , saFixedCount = 0
    , saFnLit = Seq.empty
    , saFnLitCount = 0
    , saArrays = Seq.empty
    , saArrayCount = 0
    }

sideAccToVectors ::
  SoaSideAcc
  -> ( Vector FlatFixed
     , Vector ([Int], [Maybe Text])
     , Vector (Vector NodeId)
     )
sideAccToVectors side =
  ( V.fromList (toList (saFixed side))
  , V.fromList (toList (saFnLit side))
  , V.fromList (map V.fromList (toList (saArrays side)))
  )

encI32 :: Int -> Int32
encI32 = fromIntegral

encBigOp :: BigBinOp -> Int32
encBigOp =
  encI32 . \case
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

encodeFlatNode :: FlatNode -> SoaSideAcc -> (Enc, SoaSideAcc)
encodeFlatNode node side = case node of
  FE_Literal li -> (Enc FE_LITERAL (encI32 li) 0 0 0 0, side)
  FE_Var v -> (Enc FE_VAR (encI32 v) 0 0 0 0, side)
  FE_Let tag x b -> (Enc FE_LET (encI32 tag) (encI32 x) (encI32 b) 0 0, side)
  FE_LetRec tag r b -> (Enc FE_LETREC (encI32 tag) (encI32 r) (encI32 b) 0 0, side)
  FE_Lambda tag b -> (Enc FE_LAMBDA (encI32 tag) (encI32 b) 0 0 0, side)
  FE_Apply f x -> (Enc FE_APPLY (encI32 f) (encI32 x) 0 0 0, side)
  FE_EmbedEff e -> (Enc FE_EMBEDEFF (encI32 e) 0 0 0 0, side)
  FE_If c t e -> (Enc FE_IF (encI32 c) (encI32 t) (encI32 e) 0 0, side)
  FE_OptionCase o n tag s ->
    (Enc FE_OPTIONCASE (encI32 o) (encI32 n) (encI32 tag) (encI32 s) 0, side)
  FE_ResultOk x -> (Enc FE_RESOK (encI32 x) 0 0 0 0, side)
  FE_ResultErr x -> (Enc FE_RESERR (encI32 x) 0 0 0 0, side)
  FE_ResultCase o tagE er tagO ok ->
    ( Enc
        FE_RESCASE
        (encI32 o)
        (encI32 tagE)
        (encI32 er)
        (encI32 tagO)
        (encI32 ok)
    , side
    )
  FE_Index a idx -> (Enc FE_INDEX (encI32 a) (encI32 idx) 0 0 0, side)
  FE_U8Index b idx -> (Enc FE_U8INDEX (encI32 b) (encI32 idx) 0 0 0, side)
  FE_Error m -> (Enc FE_ERROR (encI32 m) 0 0 0 0, side)
  FE_Fixed fix ->
    let
      fi = saFixedCount side
     in
      ( Enc FE_FIXED (encI32 fi) 0 0 0 0
      , side
          { saFixed = saFixed side Seq.|> fix
          , saFixedCount = fi + 1
          }
      )
  FE_FnLit tags names b ->
    let
      fi = saFnLitCount side
     in
      ( Enc FE_FNLIT (encI32 fi) (encI32 b) 0 0 0
      , side
          { saFnLit = saFnLit side Seq.|> (tags, names)
          , saFnLitCount = fi + 1
          }
      )
  FE_UnsafeNullable x -> (Enc FE_UNSAFENULL (encI32 x) 0 0 0 0, side)
  FE_FrozenLit gi -> (Enc FE_FROZEN (encI32 gi) 0 0 0 0, side)
  FE_GetField ti o -> (Enc FE_GETFIELD (encI32 ti) (encI32 o) 0 0 0, side)
  FE_KConcat x y -> (Enc FE_KCONCAT (encI32 x) (encI32 y) 0 0 0, side)
  FE_KPlus x y -> (Enc FE_KPLUS (encI32 x) (encI32 y) 0 0 0, side)
  FE_KTimes x y -> (Enc FE_KTIMES (encI32 x) (encI32 y) 0 0 0, side)
  FE_KMinus x y -> (Enc FE_KMINUS (encI32 x) (encI32 y) 0 0 0, side)
  FE_KNegate x -> (Enc FE_KNEG (encI32 x) 0 0 0 0, side)
  FE_KFracDiv x y -> (Enc FE_KDIV (encI32 x) (encI32 y) 0 0 0, side)
  FE_KRem x y -> (Enc FE_KREM (encI32 x) (encI32 y) 0 0 0, side)
  FE_KBitAnd x y -> (Enc FE_KBITAND (encI32 x) (encI32 y) 0 0 0, side)
  FE_KBitOr x y -> (Enc FE_KBITOR (encI32 x) (encI32 y) 0 0 0, side)
  FE_KBitXor x y -> (Enc FE_KBITXOR (encI32 x) (encI32 y) 0 0 0, side)
  FE_KShl x y -> (Enc FE_KSHL (encI32 x) (encI32 y) 0 0 0, side)
  FE_KShr x y -> (Enc FE_KSHR (encI32 x) (encI32 y) 0 0 0, side)
  FE_KUShr x y -> (Enc FE_KUSHR (encI32 x) (encI32 y) 0 0 0, side)
  FE_KBig op x y -> (Enc FE_KBIG (encBigOp op) (encI32 x) (encI32 y) 0 0, side)
  FE_KBigNeg x -> (Enc FE_KBIGNEG (encI32 x) 0 0 0 0, side)
  FE_KAnd x y -> (Enc FE_KAND (encI32 x) (encI32 y) 0 0 0, side)
  FE_KOr x y -> (Enc FE_KOR (encI32 x) (encI32 y) 0 0 0, side)
  FE_KEq s x y -> (Enc FE_KEQ (if s then 1 else 0) (encI32 x) (encI32 y) 0 0, side)
  FE_KNEq s x y -> (Enc FE_KNEQ (if s then 1 else 0) (encI32 x) (encI32 y) 0 0, side)
  FE_KGTh x y -> (Enc FE_KGTH (encI32 x) (encI32 y) 0 0 0, side)
  FE_KLTh x y -> (Enc FE_KLTH (encI32 x) (encI32 y) 0 0 0, side)
  FE_KGTEq x y -> (Enc FE_KGTEQ (encI32 x) (encI32 y) 0 0 0, side)
  FE_KLTEq x y -> (Enc FE_KLTEQ (encI32 x) (encI32 y) 0 0 0, side)
  FE_KShow x -> (Enc FE_KSHOW (encI32 x) 0 0 0 0, side)
  FE_KTypeOf x -> (Enc FE_KTYPEOF (encI32 x) 0 0 0 0, side)
  FE_MethMap a tag b -> (Enc FE_MMAP (encI32 a) (encI32 tag) (encI32 b) 0 0, side)
  FE_MethFilter a tag b -> (Enc FE_MFILTER (encI32 a) (encI32 tag) (encI32 b) 0 0, side)
  FE_MethReduce a z ta tb body ->
    ( Enc FE_MREDUCE (encI32 a) (encI32 z) (encI32 ta) (encI32 tb) (encI32 body)
    , side
    )
  FE_MethReduceRight a z ta tb body ->
    ( Enc FE_MREDUCER (encI32 a) (encI32 z) (encI32 ta) (encI32 tb) (encI32 body)
    , side
    )
  FE_MethToSorted a ta tb b ->
    (Enc FE_MTOSORTED (encI32 a) (encI32 ta) (encI32 tb) (encI32 b) 0, side)
  FE_MethFrom n tag b -> (Enc FE_MFROM (encI32 n) (encI32 tag) (encI32 b) 0 0, side)
  FX_Lift x -> (Enc FX_LIFT (encI32 x) 0 0 0 0, side)
  FX_FFI fi ai -> (Enc FX_EXTERN (encI32 fi) (encI32 ai) 0 0 0, side)
  FX_UnsafeObject t -> (Enc FX_UNSAFEOBJ (encI32 t) 0 0 0 0, side)
  FX_UnsafeObjectGet x t -> (Enc FX_UNSAFEOBJGET (encI32 x) (encI32 t) 0 0 0, side)
  FX_UnsafeObjectAssign x y ->
    (Enc FX_UNSAFEOBJSET (encI32 x) (encI32 y) 0 0 0, side)
  FX_CallMethod r m ai -> (Enc FX_CALLMETHOD (encI32 r) (encI32 m) (encI32 ai) 0 0, side)
  FX_Bind tag x b -> (Enc FX_BIND (encI32 tag) (encI32 x) (encI32 b) 0 0, side)
  FX_ThenE x y -> (Enc FX_THENE (encI32 x) (encI32 y) 0 0 0, side)
  FX_BindRec tag r b -> (Enc FX_BINDREC (encI32 tag) (encI32 r) (encI32 b) 0 0, side)
  FX_LambdaE tag b -> (Enc FX_LAMBDAE (encI32 tag) (encI32 b) 0 0 0, side)
  FX_ApplyE f x -> (Enc FX_APPLYE (encI32 f) (encI32 x) 0 0 0, side)
  FX_IfE c t e -> (Enc FX_IFE (encI32 c) (encI32 t) (encI32 e) 0 0, side)
  FX_While c b -> (Enc FX_WHILE (encI32 c) (encI32 b) 0 0 0, side)
  FX_ForRange s e tag b ->
    (Enc FX_FORRANGE (encI32 s) (encI32 e) (encI32 tag) (encI32 b) 0, side)
  FX_U8Set b i v -> (Enc FX_U8SET (encI32 b) (encI32 i) (encI32 v) 0 0, side)
  FX_U8Fill b v -> (Enc FX_U8FILL (encI32 b) (encI32 v) 0 0 0, side)
  FX_OptionCaseE o n tag s ->
    (Enc FX_OPTCASEE (encI32 o) (encI32 n) (encI32 tag) (encI32 s) 0, side)
  FX_ResultCaseE o tagE er tagO ok ->
    ( Enc
        FX_RESCASEE
        (encI32 o)
        (encI32 tagE)
        (encI32 er)
        (encI32 tagO)
        (encI32 ok)
    , side
    )
  FX_StringCaseE s ai d -> (Enc FX_STRCASEE (encI32 s) (encI32 ai) (encI32 d) 0 0, side)
  FX_Throw x -> (Enc FX_THROW (encI32 x) 0 0 0 0, side)
  FX_Try a tag k -> (Enc FX_TRY (encI32 a) (encI32 tag) (encI32 k) 0 0, side)
  FX_ObjectLit gi -> (Enc FX_OBJLIT (encI32 gi) 0 0 0 0, side)
  FX_DeleteProp o k -> (Enc FX_DELETEPROP (encI32 o) (encI32 k) 0 0 0, side)
  FX_ArrayLit ns ->
    let
      ai = saArrayCount side
     in
      ( Enc FX_ARRAYLIT (encI32 ai) 0 0 0 0
      , side
          { saArrays = saArrays side Seq.|> ns
          , saArrayCount = ai + 1
          }
      )

-- | Growable unboxed node columns plus boxed side tables. Rows are packed
-- straight into the columns (no intermediate 'Enc' sequence), so the node
-- table is never retained boxed.
data PackState s = PackState
  { psOpCol :: !(MVU.MVector s Op)
  , psACol :: !(MVU.MVector s Int32)
  , psBCol :: !(MVU.MVector s Int32)
  , psCCol :: !(MVU.MVector s Int32)
  , psDCol :: !(MVU.MVector s Int32)
  , psECol :: !(MVU.MVector s Int32)
  , psCap :: !Int
  , psNodeCount :: !Int
  , psSoaSide :: !SoaSideAcc
  , psLits :: !(Seq FlatLit)
  , psLitCount :: !Int
  , psTexts :: !(Seq Text)
  , psTextCount :: !Int
  , psFFIs :: !(Seq FFIForm)
  , psFFICount :: !Int
  , psStrCases :: !(Seq [(Text, NodeId)])
  , psStrCaseCount :: !Int
  , psFieldGroups :: !(Seq [FlatField])
  , psFieldGroupCount :: !Int
  , psArgGroups :: !(Seq [FlatArg])
  , psArgGroupCount :: !Int
  , psFFICache :: !(Map FFIForm Int)
  , psHoistTags :: !(Map NodeId Text)
  , psParamNames :: !(Map NodeId Text)
  }

type PackM s = StateT (PackState s) (ST s)

packStateNodeCount :: PackState s -> Int
packStateNodeCount = psNodeCount

packStateSoaSide :: PackState s -> SoaSideAcc
packStateSoaSide = psSoaSide

packStateHoistTags :: PackState s -> Map NodeId Text
packStateHoistTags = psHoistTags

packStateParamNames :: PackState s -> Map NodeId Text
packStateParamNames = psParamNames

-- | Initial column capacity; doubled on demand.
initColCap :: Int
initColCap = 16

emptyPackState :: ST s (PackState s)
emptyPackState = do
  -- Start empty; the first node grows to 'initColCap', so a tiny program
  -- never zero-fills a full initial buffer.
  opCol <- MVU.new 0
  aCol <- MVU.new 0
  bCol <- MVU.new 0
  cCol <- MVU.new 0
  dCol <- MVU.new 0
  eCol <- MVU.new 0
  pure
    PackState
      { psOpCol = opCol
      , psACol = aCol
      , psBCol = bCol
      , psCCol = cCol
      , psDCol = dCol
      , psECol = eCol
      , psCap = 0
      , psNodeCount = 0
      , psSoaSide = emptySoaSideAcc
      , psLits = Seq.empty
      , psLitCount = 0
      , psTexts = Seq.empty
      , psTextCount = 0
      , psFFIs = Seq.empty
      , psFFICount = 0
      , psStrCases = Seq.empty
      , psStrCaseCount = 0
      , psFieldGroups = Seq.empty
      , psFieldGroupCount = 0
      , psArgGroups = Seq.empty
      , psArgGroupCount = 0
      , psFFICache = Map.empty
      , psHoistTags = Map.empty
      , psParamNames = Map.empty
      }

addHoistTag :: NodeId -> Text -> PackM s ()
addHoistTag nid tag = modify $ \st -> st {psHoistTags = Map.insert nid tag (psHoistTags st)}

addParamName :: NodeId -> Text -> PackM s ()
addParamName nid name =
  modify $ \st -> st {psParamNames = Map.insert nid name (psParamNames st)}

packStateSideTables ::
  PackState s
  -> ( Vector FlatLit
     , Vector Text
     , Vector FFIForm
     , Vector [(Text, NodeId)]
     , Vector [FlatField]
     , Vector [FlatArg]
     )
packStateSideTables st =
  ( V.fromList (toList (psLits st))
  , V.fromList (toList (psTexts st))
  , V.fromList (toList (psFFIs st))
  , V.fromList (toList (psStrCases st))
  , V.fromList (toList (psFieldGroups st))
  , V.fromList (toList (psArgGroups st))
  )

addNode :: FlatNode -> PackM s NodeId
addNode node = do
  st <- get
  let
    n = psNodeCount st
    (Enc o a b c d e, side') = encodeFlatNode node (psSoaSide st)
  if n >= psCap st
    then growCols >> addNodeWrite n o a b c d e side'
    else addNodeWrite n o a b c d e side'
 where
  addNodeWrite ::
    Int
    -> FlatOp
    -> Int32
    -> Int32
    -> Int32
    -> Int32
    -> Int32
    -> SoaSideAcc
    -> PackM s NodeId
  addNodeWrite n o a b c d e side' = do
    st <- get
    lift $ do
      MVU.write (psOpCol st) n (opCode o)
      MVU.write (psACol st) n a
      MVU.write (psBCol st) n b
      MVU.write (psCCol st) n c
      MVU.write (psDCol st) n d
      MVU.write (psECol st) n e
    put st {psNodeCount = n + 1, psSoaSide = side'}
    pure n

-- | Double the column capacity, copying the rows already written.
growCols :: PackM s ()
growCols = do
  st <- get
  let
    newCap = max initColCap (psCap st * 2)
  (opC, aC, bC, cC, dC, eC) <- lift $ do
    opV <- MVU.new newCap
    MVU.copy (MVU.take (MVU.length (psOpCol st)) opV) (psOpCol st)
    aV <- MVU.new newCap
    MVU.copy (MVU.take (MVU.length (psACol st)) aV) (psACol st)
    bV <- MVU.new newCap
    MVU.copy (MVU.take (MVU.length (psBCol st)) bV) (psBCol st)
    cV <- MVU.new newCap
    MVU.copy (MVU.take (MVU.length (psCCol st)) cV) (psCCol st)
    dV <- MVU.new newCap
    MVU.copy (MVU.take (MVU.length (psDCol st)) dV) (psDCol st)
    eV <- MVU.new newCap
    MVU.copy (MVU.take (MVU.length (psECol st)) eV) (psECol st)
    pure (opV, aV, bV, cV, dV, eV)
  put
    st
      { psOpCol = opC
      , psACol = aC
      , psBCol = bC
      , psCCol = cC
      , psDCol = dC
      , psECol = eC
      , psCap = newCap
      }

addLit :: FlatLit -> PackM s Int
addLit lit = do
  st <- get
  let
    i = psLitCount st
  put st {psLits = psLits st Seq.|> lit, psLitCount = i + 1}
  pure i

addText :: Text -> PackM s Int
addText txt = do
  st <- get
  let
    i = psTextCount st
  put st {psTexts = psTexts st Seq.|> txt, psTextCount = i + 1}
  pure i

addFFI :: FFIForm -> PackM s Int
addFFI form = do
  st <- get
  case Map.lookup form (psFFICache st) of
    Just i -> pure i
    Nothing -> do
      let
        i = psFFICount st
      put
        st
          { psFFIs = psFFIs st Seq.|> form
          , psFFICount = i + 1
          , psFFICache = Map.insert form i (psFFICache st)
          }
      pure i

addStrCases :: [(Text, NodeId)] -> PackM s Int
addStrCases cases = do
  st <- get
  let
    i = psStrCaseCount st
  put st {psStrCases = psStrCases st Seq.|> cases, psStrCaseCount = i + 1}
  pure i

addFieldGroup :: [FlatField] -> PackM s Int
addFieldGroup fs = do
  st <- get
  let
    i = psFieldGroupCount st
  put st {psFieldGroups = psFieldGroups st Seq.|> fs, psFieldGroupCount = i + 1}
  pure i

addArgGroup :: [FlatArg] -> PackM s Int
addArgGroup args = do
  st <- get
  let
    i = psArgGroupCount st
  put st {psArgGroups = psArgGroups st Seq.|> args, psArgGroupCount = i + 1}
  pure i

runPack :: IrNode -> ST s (NodeId, PackState s)
runPack e = emptyPackState >>= runStateT (packExpr e)

-- | Freeze the written prefix of each column (capacity may exceed the row
-- count after growth).
freezePackColumns ::
  PackState s
  -> ST
       s
       ( VU.Vector Op
       , VU.Vector Int32
       , VU.Vector Int32
       , VU.Vector Int32
       , VU.Vector Int32
       , VU.Vector Int32
       )
freezePackColumns st = do
  let
    n = psNodeCount st
  opF <- VU.unsafeFreeze (psOpCol st)
  aF <- VU.unsafeFreeze (psACol st)
  bF <- VU.unsafeFreeze (psBCol st)
  cF <- VU.unsafeFreeze (psCCol st)
  dF <- VU.unsafeFreeze (psDCol st)
  eF <- VU.unsafeFreeze (psECol st)
  pure
    ( VU.take n opF
    , VU.take n aF
    , VU.take n bF
    , VU.take n cF
    , VU.take n dF
    , VU.take n eF
    )

fixedRefs :: FlatFixed -> [NodeId]
fixedRefs = \case
  FlatFixedU _ x -> [x]
  FlatFixedB _ x y -> [x, y]
  FlatFixedT _ x y z -> [x, y, z]

flatNodeChildRefs :: FlatNode -> [NodeId]
flatNodeChildRefs = \case
  FE_Literal _ -> []
  FE_Var _ -> []
  FE_Let _ x b -> [x, b]
  FE_LetRec _ r b -> [r, b]
  FE_Lambda _ b -> [b]
  FE_Apply f x -> [f, x]
  FE_EmbedEff e -> [e]
  FE_If c t e -> [c, t, e]
  FE_OptionCase o n _ s -> [o, n, s]
  FE_ResultOk x -> [x]
  FE_ResultErr x -> [x]
  FE_ResultCase o _ er _ ok -> [o, er, ok]
  FE_Index a idx -> [a, idx]
  FE_U8Index b idx -> [b, idx]
  FE_Error m -> [m]
  FE_Fixed fix -> fixedRefs fix
  FE_KConcat x y -> [x, y]
  FE_KPlus x y -> [x, y]
  FE_KTimes x y -> [x, y]
  FE_KMinus x y -> [x, y]
  FE_KNegate x -> [x]
  FE_KFracDiv x y -> [x, y]
  FE_KRem x y -> [x, y]
  FE_KBitAnd x y -> [x, y]
  FE_KBitOr x y -> [x, y]
  FE_KBitXor x y -> [x, y]
  FE_KShl x y -> [x, y]
  FE_KShr x y -> [x, y]
  FE_KUShr x y -> [x, y]
  FE_KBig _ x y -> [x, y]
  FE_KBigNeg x -> [x]
  FE_KAnd x y -> [x, y]
  FE_KOr x y -> [x, y]
  FE_KEq _ x y -> [x, y]
  FE_KNEq _ x y -> [x, y]
  FE_KGTh x y -> [x, y]
  FE_KLTh x y -> [x, y]
  FE_KGTEq x y -> [x, y]
  FE_KLTEq x y -> [x, y]
  FE_KShow x -> [x]
  FE_KTypeOf x -> [x]
  FE_MethMap a _ b -> [a, b]
  FE_MethFilter a _ b -> [a, b]
  FE_MethReduce a z _ _ body -> [a, z, body]
  FE_MethReduceRight a z _ _ body -> [a, z, body]
  FE_MethToSorted a _ _ b -> [a, b]
  FE_MethFrom n _ b -> [n, b]
  FE_FnLit _ _ b -> [b]
  FE_UnsafeNullable x -> [x]
  FE_FrozenLit _ -> []
  FE_GetField _ o -> [o]
  FX_Lift x -> [x]
  FX_FFI _ _ -> []
  FX_UnsafeObject _ -> []
  FX_UnsafeObjectGet x _ -> [x]
  FX_UnsafeObjectAssign x y -> [x, y]
  FX_CallMethod r _ _ -> [r]
  FX_Bind _ x b -> [x, b]
  FX_ThenE x y -> [x, y]
  FX_BindRec _ r b -> [r, b]
  FX_LambdaE _ b -> [b]
  FX_ApplyE f x -> [f, x]
  FX_IfE c t e -> [c, t, e]
  FX_While c b -> [c, b]
  FX_ForRange s e _ b -> [s, e, b]
  FX_U8Set b i v -> [b, i, v]
  FX_U8Fill b v -> [b, v]
  FX_OptionCaseE o n _ s -> [o, n, s]
  FX_ResultCaseE o _ er _ ok -> [o, er, ok]
  FX_StringCaseE s _ d -> [s, d]
  FX_Throw x -> [x]
  FX_Try a _ k -> [a, k]
  FX_ObjectLit _ -> []
  FX_DeleteProp o k -> [o, k]
  FX_ArrayLit ns -> ns

flatArgRef :: FlatArg -> NodeId
flatArgRef = \case
  FlatArgExpr j -> j
  FlatArgEffect j -> j

flatFieldRef :: FlatField -> NodeId
flatFieldRef = \case
  FlatField _ j -> j
  FlatFieldEff _ j -> j
  FlatFieldExtra _ j -> j
  FlatFieldExtraEff _ j -> j

-- | Effectness by opcode range: @FX_LIFT@ is the first effect opcode, so
-- no node decode is needed.
flatOpIsEffect :: FlatOp -> Bool
flatOpIsEffect op = op >= FX_LIFT

-- | Classify a subtree by its top constructor: expression-producing or
-- effect-producing. Mirrors the 'FlatNode' @FE_\/FX_@ split (and
-- 'flatOpIsEffect').
irNodeIsEffect :: IrNode -> Bool
irNodeIsEffect = \case
  IrLift {} -> True
  IrFFI {} -> True
  IrUnsafeObject {} -> True
  IrUnsafeObjectGet {} -> True
  IrUnsafeObjectAssign {} -> True
  IrCallMethod {} -> True
  IrBind {} -> True
  IrThenE {} -> True
  IrBindRec {} -> True
  IrLambdaE {} -> True
  IrApplyE {} -> True
  IrIfE {} -> True
  IrWhile {} -> True
  IrForRange {} -> True
  IrU8Set {} -> True
  IrU8Fill {} -> True
  IrOptionCaseE {} -> True
  IrResultCaseE {} -> True
  IrStringCaseE {} -> True
  IrThrow {} -> True
  IrTry {} -> True
  IrObjectLit {} -> True
  IrDeleteProp {} -> True
  IrArrayLit {} -> True
  _ -> False

-- | Pack a node used in expression position. An effect subtree there is the
-- optimizer splicing an effect into an expression slot: pack the effect then
-- wrap it with 'FE_EmbedEff' (the row the old type-bridge emitted). A stray
-- 'IrLift' unwraps to its body (a pure expression was spliced).
packExpr :: IrNode -> PackM s NodeId
packExpr n = case n of
  IrLift x -> packExpr x
  _
    | irNodeIsEffect n -> do
        i <- packNode n
        addNode (FE_EmbedEff i)
    | otherwise -> packNode n

-- | Pack a node used in effect position. Lowering always wraps pure values
-- in 'IrLift'; a bare expression here is defensive only.
packEffect :: IrNode -> PackM s NodeId
packEffect n
  | irNodeIsEffect n = packNode n
  | otherwise = packNode n >>= addNode . FX_Lift

packArgs :: [IrNode] -> PackM s [FlatArg]
packArgs = traverse packArg
 where
  packArg a
    | irNodeIsEffect a = FlatArgEffect <$> packNode a
    | otherwise = FlatArgExpr <$> packExpr a

packField :: IrField -> PackM s FlatField
packField = \case
  IrField k c -> FlatField k <$> packExpr c
  IrFieldEff k c -> FlatFieldEff k <$> packEffect c
  IrFieldExtra k c -> FlatFieldExtra k <$> packExpr c
  IrFieldExtraEff k c -> FlatFieldExtraEff k <$> packEffect c

packFields :: [IrField] -> PackM s Int
packFields fs = addFieldGroup =<< traverse packField fs

packStrCases :: [(Text, IrNode)] -> PackM s Int
packStrCases arms = addStrCases =<< traverse (\(k, e) -> (k,) <$> packEffect e) arms

packFixedOp :: SomeFixedOp -> [IrNode] -> PackM s NodeId
packFixedOp (SomeFixedOp op) args = case args of
  [x] -> do
    n <- packExpr x
    addNode (FE_Fixed (FlatFixedU op n))
  [x, y] -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_Fixed (FlatFixedB op nx ny))
  [x, y, z] -> do
    nx <- packExpr x
    ny <- packExpr y
    nz <- packExpr z
    addNode (FE_Fixed (FlatFixedT op nx ny nz))
  _ -> error "JShark.Flat.packFixedOp: unexpected fixed arity"

packNode :: IrNode -> PackM s NodeId
packNode node = case node of
  IrLiteral v -> do
    li <- addLit (FLit v)
    addNode (FE_Literal li)
  IrVar i -> addNode (FE_Var i)
  IrLet tag hint x body -> do
    nx <- packExpr x
    nb <- packExpr body
    nid <- addNode (FE_Let tag nx nb)
    case hint of
      Just pn -> addParamName nid pn
      Nothing -> pure ()
    pure nid
  IrLetRec tag r b -> do
    nr <- packNode r
    nb <- packNode b
    addNode (FE_LetRec tag nr nb)
  IrLambda tag info body -> do
    nb <- packNode body
    nid <- addNode (FE_Lambda tag nb)
    case lamTag info of
      Just name -> addHoistTag nid name
      Nothing -> pure ()
    case lamParam info of
      Just pn -> addParamName nid pn
      Nothing -> pure ()
    pure nid
  IrApply f x -> do
    nf <- packExpr f
    nx <- packExpr x
    addNode (FE_Apply nf nx)
  IrIf c t e -> do
    nc <- packExpr c
    nt <- packExpr t
    ne <- packExpr e
    addNode (FE_If nc nt ne)
  IrOptionCase o n tag s -> do
    no <- packExpr o
    nn <- packExpr n
    ns <- packExpr s
    addNode (FE_OptionCase no nn tag ns)
  IrResultOk x -> do
    n <- packExpr x
    addNode (FE_ResultOk n)
  IrResultErr x -> do
    n <- packExpr x
    addNode (FE_ResultErr n)
  IrResultCase o tagE er tagO ok -> do
    no <- packExpr o
    ner <- packExpr er
    nok <- packExpr ok
    addNode (FE_ResultCase no tagE ner tagO nok)
  IrIndex arr idx -> do
    nArr <- packExpr arr
    nIdx <- packExpr idx
    addNode (FE_Index nArr nIdx)
  IrU8Index buf idx -> do
    nBuf <- packExpr buf
    nIdx <- packExpr idx
    addNode (FE_U8Index nBuf nIdx)
  IrError msg -> do
    n <- packExpr msg
    addNode (FE_Error n)
  IrFixed op args -> packFixedOp op args
  IrFnLit tags names body -> do
    nb <- packExpr body
    addNode (FE_FnLit tags names nb)
  IrUnsafeNullable x -> do
    n <- packExpr x
    addNode (FE_UnsafeNullable n)
  IrFrozenLit fs -> do
    gi <- packFields fs
    addNode (FE_FrozenLit gi)
  IrGetField key o -> do
    ti <- addText key
    n <- packExpr o
    addNode (FE_GetField ti n)
  KConcat x y -> packBin2 FE_KConcat x y
  KPlus x y -> packBin2 FE_KPlus x y
  KTimes x y -> packBin2 FE_KTimes x y
  KMinus x y -> packBin2 FE_KMinus x y
  KNegate x -> packBin1 FE_KNegate x
  KFracDiv x y -> packBin2 FE_KFracDiv x y
  KRem x y -> packBin2 FE_KRem x y
  KBitAnd x y -> packBin2 FE_KBitAnd x y
  KBitOr x y -> packBin2 FE_KBitOr x y
  KBitXor x y -> packBin2 FE_KBitXor x y
  KShl x y -> packBin2 FE_KShl x y
  KShr x y -> packBin2 FE_KShr x y
  KUShr x y -> packBin2 FE_KUShr x y
  KBig op x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KBig op nx ny)
  KBigNeg x -> packBin1 FE_KBigNeg x
  KAnd x y -> packBin2 FE_KAnd x y
  KOr x y -> packBin2 FE_KOr x y
  KEq s x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KEq s nx ny)
  KNEq s x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KNEq s nx ny)
  KGTh x y -> packBin2 FE_KGTh x y
  KLTh x y -> packBin2 FE_KLTh x y
  KGTEq x y -> packBin2 FE_KGTEq x y
  KLTEq x y -> packBin2 FE_KLTEq x y
  KShow x -> packBin1 FE_KShow x
  KTypeOf x -> packBin1 FE_KTypeOf x
  IrMethMap a tag b -> do
    na <- packExpr a
    nb <- packExpr b
    addNode (FE_MethMap na tag nb)
  IrMethFilter a tag b -> do
    na <- packExpr a
    nb <- packExpr b
    addNode (FE_MethFilter na tag nb)
  IrMethReduce a z ta tb body -> do
    na <- packExpr a
    nz <- packExpr z
    nbody <- packExpr body
    addNode (FE_MethReduce na nz ta tb nbody)
  IrMethReduceRight a z ta tb body -> do
    na <- packExpr a
    nz <- packExpr z
    nbody <- packExpr body
    addNode (FE_MethReduceRight na nz ta tb nbody)
  IrMethToSorted a ta tb b -> do
    na <- packExpr a
    nb <- packExpr b
    addNode (FE_MethToSorted na ta tb nb)
  IrMethFrom n tag b -> do
    nn <- packExpr n
    nb <- packExpr b
    addNode (FE_MethFrom nn tag nb)
  IrLift x -> do
    n <- packExpr x
    addNode (FX_Lift n)
  IrFFI form args -> do
    fi <- addFFI form
    ai <- addArgGroup =<< packArgs args
    addNode (FX_FFI fi ai)
  IrUnsafeObject o -> do
    ti <- addText o
    addNode (FX_UnsafeObject ti)
  IrUnsafeObjectGet x s -> do
    nx <- packEffect x
    ti <- addText s
    addNode (FX_UnsafeObjectGet nx ti)
  IrUnsafeObjectAssign x y -> do
    nx <- packEffect x
    ny <- packEffect y
    addNode (FX_UnsafeObjectAssign nx ny)
  IrCallMethod x method args -> do
    nx <- packEffect x
    ti <- addText method
    ai <- addArgGroup =<< packArgs args
    addNode (FX_CallMethod nx ti ai)
  IrBind tag hint x body -> do
    nx <- packEffect x
    nb <- packEffect body
    nid <- addNode (FX_Bind tag nx nb)
    case hint of
      Just name -> addParamName nid name
      Nothing -> pure ()
    pure nid
  IrThenE x y -> do
    nx <- packEffect x
    ny <- packEffect y
    addNode (FX_ThenE nx ny)
  IrBindRec tag r b -> do
    nr <- packEffect r
    nb <- packEffect b
    addNode (FX_BindRec tag nr nb)
  IrLambdaE tag body -> do
    nb <- packEffect body
    addNode (FX_LambdaE tag nb)
  IrApplyE f x -> do
    nf <- packEffect f
    nx <- packEffect x
    addNode (FX_ApplyE nf nx)
  IrIfE c t e -> do
    nc <- packEffect c
    nt <- packEffect t
    ne <- packEffect e
    addNode (FX_IfE nc nt ne)
  IrWhile c b -> do
    nc <- packEffect c
    nb <- packEffect b
    addNode (FX_While nc nb)
  IrForRange s e tag body -> do
    ns <- packExpr s
    ne <- packExpr e
    nb <- packEffect body
    addNode (FX_ForRange ns ne tag nb)
  IrU8Set b i v -> do
    nb <- packExpr b
    ni <- packExpr i
    nv <- packExpr v
    addNode (FX_U8Set nb ni nv)
  IrU8Fill b v -> do
    nb <- packExpr b
    nv <- packExpr v
    addNode (FX_U8Fill nb nv)
  IrOptionCaseE o n tag s -> do
    no <- packExpr o
    nn <- packEffect n
    ns <- packEffect s
    addNode (FX_OptionCaseE no nn tag ns)
  IrResultCaseE o tagE er tagO ok -> do
    no <- packExpr o
    ner <- packEffect er
    nok <- packEffect ok
    addNode (FX_ResultCaseE no tagE ner tagO nok)
  IrStringCaseE s arms d -> do
    ns <- packExpr s
    ai <- packStrCases arms
    nd <- packEffect d
    addNode (FX_StringCaseE ns ai nd)
  IrThrow x -> do
    n <- packExpr x
    addNode (FX_Throw n)
  IrTry a tag k -> do
    na <- packEffect a
    nk <- packEffect k
    addNode (FX_Try na tag nk)
  IrObjectLit fs -> do
    gi <- packFields fs
    addNode (FX_ObjectLit gi)
  IrDeleteProp o k -> do
    no <- packEffect o
    nk <- packExpr k
    addNode (FX_DeleteProp no nk)
  IrArrayLit es -> do
    ns <- traverse packEffect es
    addNode (FX_ArrayLit ns)
 where
  packBin1 kon x = do
    nx <- packExpr x
    addNode (kon nx)
  packBin2 kon x y = do
    nx <- packExpr x
    ny <- packExpr y
    addNode (kon nx ny)

data FlatSoA = FlatSoA
  { fsaOpcodes :: !(VU.Vector Op)
  , fsaA :: !(VU.Vector Int32)
  , fsaB :: !(VU.Vector Int32)
  , fsaC :: !(VU.Vector Int32)
  , fsaD :: !(VU.Vector Int32)
  , fsaE :: !(VU.Vector Int32)
  , fsaFixed :: !(V.Vector FlatFixed)
  , fsaFnLit :: !(V.Vector ([Int], [Maybe Text]))
  , fsaArrayGroups :: !(V.Vector (V.Vector NodeId))
  , fsaLits :: !(V.Vector FlatLit)
  , fsaTexts :: !(V.Vector Text)
  , fsaFFIs :: !(V.Vector FFIForm)
  , fsaStrCases :: !(V.Vector [(Text, NodeId)])
  , fsaFieldGroups :: !(V.Vector [FlatField])
  , fsaArgGroups :: !(V.Vector [FlatArg])
  , fsaHoistTags :: !(V.Vector (Maybe Text))
  , fsaParamNames :: !(V.Vector (Maybe Text))
  , fsaRoot :: !NodeId
  }

flatSoaNodeCount :: FlatSoA -> Int
flatSoaNodeCount soa = VU.length (fsaOpcodes soa)

freezeSoaFromPackState :: NodeId -> PackState s -> ST s FlatSoA
freezeSoaFromPackState root st = do
  (opF, aF, bF, cF, dF, eF) <- freezePackColumns st
  let
    side = packStateSoaSide st
    n = packStateNodeCount st
    (fx, fl, ag) = sideAccToVectors side
    (lits, texts, ffis, strCases, fieldGroups, argGroups) =
      packStateSideTables st
    hoistMap = packStateHoistTags st
    hoistTags =
      V.generate n (\i -> Map.lookup i hoistMap)
    paramMap = packStateParamNames st
    paramNames =
      V.generate n (\i -> Map.lookup i paramMap)
    soa0 =
      FlatSoA
        { fsaOpcodes = opF
        , fsaA = aF
        , fsaB = bF
        , fsaC = cF
        , fsaD = dF
        , fsaE = eF
        , fsaFixed = fx
        , fsaFnLit = fl
        , fsaArrayGroups = ag
        , fsaLits = lits
        , fsaTexts = texts
        , fsaFFIs = ffis
        , fsaStrCases = strCases
        , fsaFieldGroups = fieldGroups
        , fsaArgGroups = argGroups
        , fsaHoistTags = hoistTags
        , fsaParamNames = paramNames
        , fsaRoot = root
        }
   in
    pure soa0

-- | Pack an IR tree directly to SoA columns (no intermediate node vector).
packProgramDirect :: IrNode -> FlatSoA
packProgramDirect e = runST $ do
  (root, st) <- runPack e
  freezeSoaFromPackState root st

-- | SoA optimizer pass; returns optimized SoA (emit decodes nodes on demand).
-- Purity is already computed at pack ('computeFlatSoaPure'); the fold only
-- turns numeric kernels into literals, which keeps purity flags correct
-- (both forms are pure). The fold is not redundant with the tree
-- optimizer: let-inlining there can create @lit op lit@ nodes after the
-- kernel was visited, and the single bottom-up pass never revisits.
optimizeFlatPack :: FlatSoA -> FlatSoA
optimizeFlatPack soa0 =
  let
    !(soa1, _, _) = constantFoldWithStats soa0
   in
    soa1

unboxedToBoxedPure :: VU.Vector Word8 -> V.Vector Word8
unboxedToBoxedPure = GV.convert
{-# INLINE unboxedToBoxedPure #-}

decodeOp :: FlatSoA -> FlatOp -> Int -> Int -> Int -> Int -> Int -> FlatNode
decodeOp soa op ix iy iz iw iv = case op of
  FE_LITERAL -> FE_Literal ix
  FE_VAR -> FE_Var ix
  FE_LET -> FE_Let ix iy iz
  FE_LETREC -> FE_LetRec ix iy iz
  FE_LAMBDA -> FE_Lambda ix iy
  FE_APPLY -> FE_Apply ix iy
  FE_EMBEDEFF -> FE_EmbedEff ix
  FE_IF -> FE_If ix iy iz
  FE_OPTIONCASE -> FE_OptionCase ix iy iz iw
  FE_RESOK -> FE_ResultOk ix
  FE_RESERR -> FE_ResultErr ix
  FE_RESCASE -> FE_ResultCase ix iy iz iw iv
  FE_INDEX -> FE_Index ix iy
  FE_U8INDEX -> FE_U8Index ix iy
  FE_ERROR -> FE_Error ix
  FE_FIXED -> FE_Fixed (fsaFixed soa V.! ix)
  FE_FNLIT ->
    let
      (tags, names) = fsaFnLit soa V.! ix
     in
      FE_FnLit tags names iy
  FE_UNSAFENULL -> FE_UnsafeNullable ix
  FE_FROZEN -> FE_FrozenLit ix
  FE_GETFIELD -> FE_GetField ix iy
  FE_KCONCAT -> FE_KConcat ix iy
  FE_KPLUS -> FE_KPlus ix iy
  FE_KTIMES -> FE_KTimes ix iy
  FE_KMINUS -> FE_KMinus ix iy
  FE_KNEG -> FE_KNegate ix
  FE_KDIV -> FE_KFracDiv ix iy
  FE_KREM -> FE_KRem ix iy
  FE_KBITAND -> FE_KBitAnd ix iy
  FE_KBITOR -> FE_KBitOr ix iy
  FE_KBITXOR -> FE_KBitXor ix iy
  FE_KSHL -> FE_KShl ix iy
  FE_KSHR -> FE_KShr ix iy
  FE_KUSHR -> FE_KUShr ix iy
  FE_KBIG -> FE_KBig (tagBigOp (fromIntegral ix)) iy iz
  FE_KBIGNEG -> FE_KBigNeg ix
  FE_KAND -> FE_KAnd ix iy
  FE_KOR -> FE_KOr ix iy
  FE_KEQ -> FE_KEq (ix /= 0) iy iz
  FE_KNEQ -> FE_KNEq (ix /= 0) iy iz
  FE_KGTH -> FE_KGTh ix iy
  FE_KLTH -> FE_KLTh ix iy
  FE_KGTEQ -> FE_KGTEq ix iy
  FE_KLTEQ -> FE_KLTEq ix iy
  FE_KSHOW -> FE_KShow ix
  FE_KTYPEOF -> FE_KTypeOf ix
  FE_MMAP -> FE_MethMap ix iy iz
  FE_MFILTER -> FE_MethFilter ix iy iz
  FE_MREDUCE -> FE_MethReduce ix iy iz iw iv
  FE_MREDUCER -> FE_MethReduceRight ix iy iz iw iv
  FE_MTOSORTED -> FE_MethToSorted ix iy iz iw
  FE_MFROM -> FE_MethFrom ix iy iz
  FX_LIFT -> FX_Lift ix
  FX_EXTERN -> FX_FFI ix iy
  FX_UNSAFEOBJ -> FX_UnsafeObject ix
  FX_UNSAFEOBJGET -> FX_UnsafeObjectGet ix iy
  FX_UNSAFEOBJSET -> FX_UnsafeObjectAssign ix iy
  FX_CALLMETHOD -> FX_CallMethod ix iy iz
  FX_BIND -> FX_Bind ix iy iz
  FX_THENE -> FX_ThenE ix iy
  FX_BINDREC -> FX_BindRec ix iy iz
  FX_LAMBDAE -> FX_LambdaE ix iy
  FX_APPLYE -> FX_ApplyE ix iy
  FX_IFE -> FX_IfE ix iy iz
  FX_WHILE -> FX_While ix iy
  FX_FORRANGE -> FX_ForRange ix iy iz iw
  FX_U8SET -> FX_U8Set ix iy iz
  FX_U8FILL -> FX_U8Fill ix iy
  FX_OPTCASEE -> FX_OptionCaseE ix iy iz iw
  FX_RESCASEE -> FX_ResultCaseE ix iy iz iw iv
  FX_STRCASEE -> FX_StringCaseE ix iy iz
  FX_THROW -> FX_Throw ix
  FX_TRY -> FX_Try ix iy iz
  FX_OBJLIT -> FX_ObjectLit ix
  FX_DELETEPROP -> FX_DeleteProp ix iy
  FX_ARRAYLIT ->
    FX_ArrayLit (V.toList (fsaArrayGroups soa V.! ix))

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

flatSoaNode :: FlatSoA -> NodeId -> FlatNode
flatSoaNode soa idx =
  let
    op = flatOpOf (fsaOpcodes soa VU.! idx)
    ix = fromIntegral (fsaA soa VU.! idx) :: Int
    iy = fromIntegral (fsaB soa VU.! idx) :: Int
    iz = fromIntegral (fsaC soa VU.! idx) :: Int
    iw = fromIntegral (fsaD soa VU.! idx) :: Int
    iv = fromIntegral (fsaE soa VU.! idx) :: Int
   in
    decodeOp soa op ix iy iz iw iv

flatSoaLit :: FlatSoA -> Int -> FlatLit
flatSoaLit soa i = fsaLits soa V.! i

withFlatLitValue :: FlatSoA -> Int -> (forall u. Value u -> r) -> r
withFlatLitValue soa i k = case flatSoaLit soa i of
  FLit v -> k v

flatSoaText :: FlatSoA -> Int -> Text
flatSoaText soa i = fsaTexts soa V.! i

flatSoaFFI :: FlatSoA -> Int -> FFIForm
flatSoaFFI soa i = fsaFFIs soa V.! i

flatSoaStrCases :: FlatSoA -> Int -> [(Text, NodeId)]
flatSoaStrCases soa i = fsaStrCases soa V.! i

flatSoaFieldGroup :: FlatSoA -> Int -> [FlatField]
flatSoaFieldGroup soa i = fsaFieldGroups soa V.! i

flatSoaArgGroup :: FlatSoA -> Int -> [FlatArg]
flatSoaArgGroup soa i = fsaArgGroups soa V.! i

flatSoaNodeSideRefs :: FlatSoA -> FlatNode -> [NodeId]
flatSoaNodeSideRefs soa = \case
  FE_Fixed _ -> []
  FE_FnLit _ _ _ -> []
  FE_FrozenLit gi -> map flatFieldRef (flatSoaFieldGroup soa gi)
  FX_FFI _ ai -> map flatArgRef (flatSoaArgGroup soa ai)
  FX_CallMethod _ _ ai -> map flatArgRef (flatSoaArgGroup soa ai)
  FX_StringCaseE _ ai _ -> map snd (flatSoaStrCases soa ai)
  FX_ObjectLit gi -> map flatFieldRef (flatSoaFieldGroup soa gi)
  FX_ArrayLit _ -> []
  _ -> []

flatSoaNodePackRefs :: FlatSoA -> FlatNode -> [NodeId]
flatSoaNodePackRefs soa node =
  flatNodeChildRefs node ++ flatSoaNodeSideRefs soa node

-- | Named-lambda hoist tag for a node, if any (bounds-checked).
flatSoaHoistTag :: FlatSoA -> NodeId -> Maybe Text
flatSoaHoistTag soa i =
  if i >= 0 && i < V.length (fsaHoistTags soa)
    then fsaHoistTags soa V.! i
    else Nothing

-- | Source-hint param name for a node, if any (bounds-checked).
flatSoaParamName :: FlatSoA -> NodeId -> Maybe Text
flatSoaParamName soa i =
  if i >= 0 && i < V.length (fsaParamNames soa)
    then fsaParamNames soa V.! i
    else Nothing

flatSoaReachableDepths :: FlatSoA -> NodeId -> V.Vector Int
flatSoaReachableDepths soa root =
  let
    n = flatSoaNodeCount soa
   in
    runST $ do
      md <- MV.new n
      MV.set md (-1)
      let
        go i =
          MV.read md i >>= \case
            d | d >= 0 -> pure d
            _ -> do
              let
                refs = flatSoaNodePackRefs soa (flatSoaNode soa i)
              d <-
                if null refs
                  then pure 0
                  else (1 +) . maximum <$> mapM go refs
              MV.write md i d
              pure d
      _ <- go root
      V.unsafeFreeze md

flatSoaLayerBuckets :: FlatSoA -> NodeId -> V.Vector (V.Vector NodeId)
flatSoaLayerBuckets soa root =
  let
    depths = flatSoaReachableDepths soa root
    n = V.length depths
    maxD = V.foldl' max 0 depths
    bucket d =
      V.fromList
        [ i
        | i <- [0 .. n - 1]
        , depths V.! i == d
        , depths V.! i >= 0
        ]
   in
    V.fromList [bucket d | d <- [0 .. maxD]]

-- | Per-node purity in one backward sweep. Pack order keeps every child
-- ref (side-table rows included) below its parent, so children are final
-- when the parent is written; no fixpoint iteration is needed.
computeFlatSoaPure :: FlatSoA -> VU.Vector Word8
computeFlatSoaPure soa =
  let
    n = VU.length (fsaOpcodes soa)
   in
    if n <= 0
      then VU.empty
      else runST $ do
        mp <- MVU.new n
        let
          ch j =
            let
              i = fromIntegral j :: Int
             in
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
              (\acc j -> do p <- ch (encI32 j); pure (acc .&. p))
              (1 :: Word8)
              ns
          pureFixed fi =
            case fsaFixed soa V.! fromIntegral fi of
              FlatFixedU _ j -> ch (encI32 j)
              FlatFixedB _ j k -> bin (encI32 j) (encI32 k)
              FlatFixedT _ j k l -> tri (encI32 j) (encI32 k) (encI32 l)
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
            | op == FE_LITERAL = pure 1
            | op == FE_VAR = pure 1
            | op == FE_FROZEN = pure 1
            | op == FE_RESOK = ch a
            | op == FE_RESERR = ch a
            | op == FE_LET = bin b c
            | op == FE_LETREC = bin b c
            | op == FE_LAMBDA = ch b
            | op == FE_APPLY = bin a b
            | op == FE_IF = tri a b c
            | op == FE_OPTIONCASE = tri a b d
            | op == FE_RESCASE = tri a c e
            | op == FE_INDEX = bin a b
            | op == FE_U8INDEX = bin a b
            | op == FE_FIXED = pureFixed a
            | op == FE_FNLIT = ch b
            | op == FE_GETFIELD = ch b
            | op == FE_UNSAFENULL = ch a
            | op == FE_KNEG = ch a
            | op == FE_KBIGNEG = ch a
            | op == FE_KSHOW = ch a
            | op == FE_KTYPEOF = ch a
            | op == FE_KEQ = bin b c
            | op == FE_KNEQ = bin b c
            | op == FE_KBIG = bin b c
            | op == FE_MMAP = bin a c
            | op == FE_MFILTER = bin a c
            | op == FE_MREDUCE = tri a b e
            | op == FE_MREDUCER = tri a b e
            | op == FE_MTOSORTED = bin a d
            | op == FE_MFROM = bin a c
            | op == FX_LIFT = ch a
            | op == FX_BIND = bin b c
            | op == FX_THENE = bin a b
            | op == FX_BINDREC = bin b c
            | op == FX_LAMBDAE = ch b
            | op == FX_IFE = tri a b c
            | op == FX_FORRANGE = tri a b d
            | op == FX_U8SET = tri a b c
            | op == FX_U8FILL = bin a b
            | op == FX_OPTCASEE = tri a b d
            | op == FX_RESCASEE = tri a c e
            | op == FX_STRCASEE = bin a c
            | op == FX_THROW = ch a
            | op == FX_TRY = bin a c
            | op == FX_OBJLIT = pureFields a
            | op == FX_DELETEPROP = bin a b
            | op == FX_ARRAYLIT = pureArray a
            | op < FX_LIFT = bin a b
            | otherwise = pure 0

          writeAt idx = do
            let
              op = flatOpOf (fsaOpcodes soa VU.! idx)
              a = fsaA soa VU.! idx
              b = fsaB soa VU.! idx
              c = fsaC soa VU.! idx
              d = fsaD soa VU.! idx
              e = fsaE soa VU.! idx
            p <-
              if impureOp op
                then pure (0 :: Word8)
                else pureForOp op a b c d e
            MVU.write mp idx p
          go idx
            | idx < 0 = pure ()
            | otherwise = writeAt idx >> go (idx - 1)
        go (n - 1)
        VU.unsafeFreeze mp
 where
  impureOp :: FlatOp -> Bool
  impureOp op
    | op >= FX_LIFT =
        op
          `elem` [ FX_EXTERN
                 , FX_UNSAFEOBJ
                 , FX_UNSAFEOBJGET
                 , FX_UNSAFEOBJSET
                 , FX_CALLMETHOD
                 , FX_APPLYE
                 , FX_WHILE
                 , FX_FORRANGE
                 , FX_U8SET
                 , FX_U8FILL
                 , FX_THROW
                 , FX_DELETEPROP
                 ]
    | otherwise = op `elem` [FE_ERROR, FE_EMBEDEFF]

soaPureCount :: FlatSoA -> Int
soaPureCount = fromIntegral . VU.sum . computeFlatSoaPure

soaPureVector :: FlatSoA -> V.Vector Word8
soaPureVector soa = unboxedToBoxedPure (computeFlatSoaPure soa)

litAsNumber :: FlatLit -> Maybe Double
litAsNumber (FLit (ValueNumber d)) = Just d
litAsNumber _ = Nothing

optConstantFoldNumOnce :: FlatSoA -> (FlatSoA, Bool)
optConstantFoldNumOnce soa0 = runST $ do
  let
    n = VU.length (fsaOpcodes soa0)
  -- 'thaw' copies. The input 'soa0' is immutable and may be shared by the
  -- caller (e.g. a stable-input test compares it after optimizing), so
  -- mutating its backing store with 'unsafeThaw' would corrupt it.
  opM <- VU.thaw (fsaOpcodes soa0)
  aM <- VU.thaw (fsaA soa0)
  bM <- VU.thaw (fsaB soa0)
  litsRef <- newSTRef =<< V.thaw (fsaLits soa0)
  litCountRef <- newSTRef (V.length (fsaLits soa0))
  changedRef <- newSTRef False
  let
    readOp i = flatOpOf <$> MVU.read opM i
    readA i = MVU.read aM i
    readLitM li = do
      litsM <- readSTRef litsRef
      v <- GM.read litsM (fromIntegral (li :: Int32))
      pure (litAsNumber v)
    -- Grow geometrically: appending by one would copy the whole literal
    -- column on every fold.
    addFoldLit d = do
      litsM <- readSTRef litsRef
      li <- readSTRef litCountRef
      litsM' <-
        if li < GM.length litsM
          then pure litsM
          else GM.grow litsM (max 8 (GM.length litsM))
      GM.write litsM' li (FLit (ValueNumber d))
      writeSTRef litsRef litsM'
      writeSTRef litCountRef (li + 1)
      pure (encI32 li)
    tryFold i = do
      op <- readOp i
      case () of
        _ | op == FE_KPLUS -> foldAt i (+)
        _ | op == FE_KTIMES -> foldAt i (*)
        _ | op == FE_KMINUS -> foldAt i (-)
        _ -> pure ()
    foldAt i f = do
      x <- readA i
      y <- MVU.read bM i
      ox <- readOp (fromIntegral x)
      oy <- readOp (fromIntegral y)
      when (ox == FE_LITERAL && oy == FE_LITERAL) $ do
        lix <- readA (fromIntegral x)
        liy <- readA (fromIntegral y)
        mdx <- readLitM lix
        mdy <- readLitM liy
        case (mdx, mdy) of
          (Just dx, Just dy) -> do
            liNew <- addFoldLit (f dx dy)
            MVU.write opM i (opCode FE_LITERAL)
            MVU.write aM i liNew
            MVU.write bM i 0
            writeSTRef changedRef True
          _ -> pure ()
  forM_ [0 .. n - 1] tryFold
  opF <- VU.unsafeFreeze opM
  aF <- VU.unsafeFreeze aM
  bF <- VU.unsafeFreeze bM
  litsM <- readSTRef litsRef
  litCount <- readSTRef litCountRef
  litsF <- V.unsafeFreeze (GM.slice 0 litCount litsM)
  changed <- readSTRef changedRef
  pure
    ( soa0
        { fsaOpcodes = opF
        , fsaA = aF
        , fsaB = bF
        , fsaLits = litsF
        }
    , changed
    )

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

soaUnboxedEqual :: FlatSoA -> FlatSoA -> Bool
soaUnboxedEqual a b =
  fsaOpcodes a == fsaOpcodes b
    && fsaA a == fsaA b
    && fsaB a == fsaB b
    && fsaC a == fsaC b
    && fsaD a == fsaD b
    && fsaE a == fsaE b
    && fsaRoot a == fsaRoot b

soaSideLengthsEqual :: FlatSoA -> FlatSoA -> Bool
soaSideLengthsEqual a b =
  V.length (fsaFixed a) == V.length (fsaFixed b)
    && V.length (fsaLits a) == V.length (fsaLits b)
    && V.length (fsaFFIs a) == V.length (fsaFFIs b)

-- | Bottom-up numeric fold. Packing keeps children below parents and the
-- scan runs in ascending id order, so a folded child is visible to its
-- parent later in the same pass: one scan reaches the fixed point.
-- Sequential 'runST' scan; a parallel IO-per-node walk was ~100s on Life.
constantFoldWithStats :: FlatSoA -> (FlatSoA, Int, Bool)
constantFoldWithStats soa0 =
  let
    (soa', changed) = optConstantFoldNumOnce soa0
   in
    (soa', if changed then 1 else 0, changed)
{-# NOINLINE constantFoldWithStats #-}
