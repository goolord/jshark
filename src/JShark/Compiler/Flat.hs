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

-- | Untyped flat IR: optimized 'Ir' trees packed into vectors for codegen.
module JShark.Compiler.Flat
  ( NodeId
  , FlatNode (..)
  , FlatArg (..)
  , FlatField (..)
  , FlatFixed (..)
  , FlatLit (FLit)
  , packEffectProgramState
  , flatNodeIsEffect
  , flatNodeChildRefs
  , flatArgRef
  , flatFieldRef
  , packStateEncs
  , packStateNodeCount
  , packStateSoaSide
  , packStateSideTables
  , packStateHoistTags
  , packStateParamNames
  , PackState
  , sideAccToVectors
  )
where

import Control.Monad.State.Strict (State, get, modify, put, runState)
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.TypeLits (KnownSymbol, symbolVal)
import JShark.Api.Rec (Rec (..))
import JShark.Api.Types
  ( BigBinOp (..)
  , FFIForm (..)
  , FixedOp
  , LamInfo (..)
  , Value (..)
  )
import JShark.Compiler.FlatEnc (Enc (..))
import qualified JShark.Compiler.FlatEnc as FE
import JShark.Compiler.Ir

type NodeId = Int

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
  | FE_Hvm2Ref Int
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
  FE_Literal li -> (Enc FE.oFE_LITERAL (encI32 li) 0 0 0 0, side)
  FE_Var v -> (Enc FE.oFE_VAR (encI32 v) 0 0 0 0, side)
  FE_Let tag x b -> (Enc FE.oFE_LET (encI32 tag) (encI32 x) (encI32 b) 0 0, side)
  FE_LetRec tag r b -> (Enc FE.oFE_LETREC (encI32 tag) (encI32 r) (encI32 b) 0 0, side)
  FE_Lambda tag b -> (Enc FE.oFE_LAMBDA (encI32 tag) (encI32 b) 0 0 0, side)
  FE_Apply f x -> (Enc FE.oFE_APPLY (encI32 f) (encI32 x) 0 0 0, side)
  FE_EmbedEff e -> (Enc FE.oFE_EMBEDEFF (encI32 e) 0 0 0 0, side)
  FE_If c t e -> (Enc FE.oFE_IF (encI32 c) (encI32 t) (encI32 e) 0 0, side)
  FE_OptionCase o n tag s ->
    (Enc FE.oFE_OPTIONCASE (encI32 o) (encI32 n) (encI32 tag) (encI32 s) 0, side)
  FE_ResultOk x -> (Enc FE.oFE_RESOK (encI32 x) 0 0 0 0, side)
  FE_ResultErr x -> (Enc FE.oFE_RESERR (encI32 x) 0 0 0 0, side)
  FE_ResultCase o tagE er tagO ok ->
    ( Enc
        FE.oFE_RESCASE
        (encI32 o)
        (encI32 tagE)
        (encI32 er)
        (encI32 tagO)
        (encI32 ok)
    , side
    )
  FE_Index a idx -> (Enc FE.oFE_INDEX (encI32 a) (encI32 idx) 0 0 0, side)
  FE_U8Index b idx -> (Enc FE.oFE_U8INDEX (encI32 b) (encI32 idx) 0 0 0, side)
  FE_Error m -> (Enc FE.oFE_ERROR (encI32 m) 0 0 0 0, side)
  FE_Fixed fix ->
    let
      fi = saFixedCount side
     in
      ( Enc FE.oFE_FIXED (encI32 fi) 0 0 0 0
      , side
          { saFixed = saFixed side Seq.|> fix
          , saFixedCount = fi + 1
          }
      )
  FE_FnLit tags names b ->
    let
      fi = saFnLitCount side
     in
      ( Enc FE.oFE_FNLIT (encI32 fi) (encI32 b) 0 0 0
      , side
          { saFnLit = saFnLit side Seq.|> (tags, names)
          , saFnLitCount = fi + 1
          }
      )
  FE_UnsafeNullable x -> (Enc FE.oFE_UNSAFENULL (encI32 x) 0 0 0 0, side)
  FE_FrozenLit gi -> (Enc FE.oFE_FROZEN (encI32 gi) 0 0 0 0, side)
  FE_GetField ti o -> (Enc FE.oFE_GETFIELD (encI32 ti) (encI32 o) 0 0 0, side)
  FE_Hvm2Ref ti -> (Enc FE.oFE_HVM2REF (encI32 ti) 0 0 0 0, side)
  FE_KConcat x y -> (Enc FE.oFE_KCONCAT (encI32 x) (encI32 y) 0 0 0, side)
  FE_KPlus x y -> (Enc FE.oFE_KPLUS (encI32 x) (encI32 y) 0 0 0, side)
  FE_KTimes x y -> (Enc FE.oFE_KTIMES (encI32 x) (encI32 y) 0 0 0, side)
  FE_KMinus x y -> (Enc FE.oFE_KMINUS (encI32 x) (encI32 y) 0 0 0, side)
  FE_KNegate x -> (Enc FE.oFE_KNEG (encI32 x) 0 0 0 0, side)
  FE_KFracDiv x y -> (Enc FE.oFE_KDIV (encI32 x) (encI32 y) 0 0 0, side)
  FE_KRem x y -> (Enc FE.oFE_KREM (encI32 x) (encI32 y) 0 0 0, side)
  FE_KBitAnd x y -> (Enc FE.oFE_KBITAND (encI32 x) (encI32 y) 0 0 0, side)
  FE_KBitOr x y -> (Enc FE.oFE_KBITOR (encI32 x) (encI32 y) 0 0 0, side)
  FE_KBitXor x y -> (Enc FE.oFE_KBITXOR (encI32 x) (encI32 y) 0 0 0, side)
  FE_KShl x y -> (Enc FE.oFE_KSHL (encI32 x) (encI32 y) 0 0 0, side)
  FE_KShr x y -> (Enc FE.oFE_KSHR (encI32 x) (encI32 y) 0 0 0, side)
  FE_KUShr x y -> (Enc FE.oFE_KUSHR (encI32 x) (encI32 y) 0 0 0, side)
  FE_KBig op x y -> (Enc FE.oFE_KBIG (encBigOp op) (encI32 x) (encI32 y) 0 0, side)
  FE_KBigNeg x -> (Enc FE.oFE_KBIGNEG (encI32 x) 0 0 0 0, side)
  FE_KAnd x y -> (Enc FE.oFE_KAND (encI32 x) (encI32 y) 0 0 0, side)
  FE_KOr x y -> (Enc FE.oFE_KOR (encI32 x) (encI32 y) 0 0 0, side)
  FE_KEq s x y -> (Enc FE.oFE_KEQ (if s then 1 else 0) (encI32 x) (encI32 y) 0 0, side)
  FE_KNEq s x y -> (Enc FE.oFE_KNEQ (if s then 1 else 0) (encI32 x) (encI32 y) 0 0, side)
  FE_KGTh x y -> (Enc FE.oFE_KGTH (encI32 x) (encI32 y) 0 0 0, side)
  FE_KLTh x y -> (Enc FE.oFE_KLTH (encI32 x) (encI32 y) 0 0 0, side)
  FE_KGTEq x y -> (Enc FE.oFE_KGTEQ (encI32 x) (encI32 y) 0 0 0, side)
  FE_KLTEq x y -> (Enc FE.oFE_KLTEQ (encI32 x) (encI32 y) 0 0 0, side)
  FE_KShow x -> (Enc FE.oFE_KSHOW (encI32 x) 0 0 0 0, side)
  FE_KTypeOf x -> (Enc FE.oFE_KTYPEOF (encI32 x) 0 0 0 0, side)
  FE_MethMap a tag b -> (Enc FE.oFE_MMAP (encI32 a) (encI32 tag) (encI32 b) 0 0, side)
  FE_MethFilter a tag b -> (Enc FE.oFE_MFILTER (encI32 a) (encI32 tag) (encI32 b) 0 0, side)
  FE_MethReduce a z ta tb body ->
    ( Enc FE.oFE_MREDUCE (encI32 a) (encI32 z) (encI32 ta) (encI32 tb) (encI32 body)
    , side
    )
  FE_MethReduceRight a z ta tb body ->
    ( Enc FE.oFE_MREDUCER (encI32 a) (encI32 z) (encI32 ta) (encI32 tb) (encI32 body)
    , side
    )
  FE_MethToSorted a ta tb b ->
    (Enc FE.oFE_MTOSORTED (encI32 a) (encI32 ta) (encI32 tb) (encI32 b) 0, side)
  FE_MethFrom n tag b -> (Enc FE.oFE_MFROM (encI32 n) (encI32 tag) (encI32 b) 0 0, side)
  FX_Lift x -> (Enc FE.oFX_LIFT (encI32 x) 0 0 0 0, side)
  FX_FFI fi ai -> (Enc FE.oFX_FFI (encI32 fi) (encI32 ai) 0 0 0, side)
  FX_UnsafeObject t -> (Enc FE.oFX_UNSAFEOBJ (encI32 t) 0 0 0 0, side)
  FX_UnsafeObjectGet x t -> (Enc FE.oFX_UNSAFEOBJGET (encI32 x) (encI32 t) 0 0 0, side)
  FX_UnsafeObjectAssign x y ->
    (Enc FE.oFX_UNSAFEOBJSET (encI32 x) (encI32 y) 0 0 0, side)
  FX_CallMethod r m ai -> (Enc FE.oFX_CALLMETHOD (encI32 r) (encI32 m) (encI32 ai) 0 0, side)
  FX_Bind tag x b -> (Enc FE.oFX_BIND (encI32 tag) (encI32 x) (encI32 b) 0 0, side)
  FX_ThenE x y -> (Enc FE.oFX_THENE (encI32 x) (encI32 y) 0 0 0, side)
  FX_BindRec tag r b -> (Enc FE.oFX_BINDREC (encI32 tag) (encI32 r) (encI32 b) 0 0, side)
  FX_LambdaE tag b -> (Enc FE.oFX_LAMBDAE (encI32 tag) (encI32 b) 0 0 0, side)
  FX_ApplyE f x -> (Enc FE.oFX_APPLYE (encI32 f) (encI32 x) 0 0 0, side)
  FX_IfE c t e -> (Enc FE.oFX_IFE (encI32 c) (encI32 t) (encI32 e) 0 0, side)
  FX_While c b -> (Enc FE.oFX_WHILE (encI32 c) (encI32 b) 0 0 0, side)
  FX_ForRange s e tag b ->
    (Enc FE.oFX_FORRANGE (encI32 s) (encI32 e) (encI32 tag) (encI32 b) 0, side)
  FX_U8Set b i v -> (Enc FE.oFX_U8SET (encI32 b) (encI32 i) (encI32 v) 0 0, side)
  FX_U8Fill b v -> (Enc FE.oFX_U8FILL (encI32 b) (encI32 v) 0 0 0, side)
  FX_OptionCaseE o n tag s ->
    (Enc FE.oFX_OPTCASEE (encI32 o) (encI32 n) (encI32 tag) (encI32 s) 0, side)
  FX_ResultCaseE o tagE er tagO ok ->
    ( Enc
        FE.oFX_RESCASEE
        (encI32 o)
        (encI32 tagE)
        (encI32 er)
        (encI32 tagO)
        (encI32 ok)
    , side
    )
  FX_StringCaseE s ai d -> (Enc FE.oFX_STRCASEE (encI32 s) (encI32 ai) (encI32 d) 0 0, side)
  FX_Throw x -> (Enc FE.oFX_THROW (encI32 x) 0 0 0 0, side)
  FX_Try a tag k -> (Enc FE.oFX_TRY (encI32 a) (encI32 tag) (encI32 k) 0 0, side)
  FX_ObjectLit gi -> (Enc FE.oFX_OBJLIT (encI32 gi) 0 0 0 0, side)
  FX_DeleteProp o k -> (Enc FE.oFX_DELETEPROP (encI32 o) (encI32 k) 0 0 0, side)
  FX_ArrayLit ns ->
    let
      ai = saArrayCount side
     in
      ( Enc FE.oFX_ARRAYLIT (encI32 ai) 0 0 0 0
      , side
          { saArrays = saArrays side Seq.|> ns
          , saArrayCount = ai + 1
          }
      )

emptyPackState :: PackState
emptyPackState =
  PackState
    { psEncs = Seq.empty
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

data PackState = PackState
  { psEncs :: !(Seq Enc)
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

packStateEncs :: PackState -> Seq Enc
packStateEncs = psEncs

packStateNodeCount :: PackState -> Int
packStateNodeCount = psNodeCount

packStateSoaSide :: PackState -> SoaSideAcc
packStateSoaSide = psSoaSide

packStateHoistTags :: PackState -> Map NodeId Text
packStateHoistTags = psHoistTags

packStateParamNames :: PackState -> Map NodeId Text
packStateParamNames = psParamNames

addHoistTag :: NodeId -> Text -> State PackState ()
addHoistTag nid tag = modify $ \st -> st {psHoistTags = Map.insert nid tag (psHoistTags st)}

addParamName :: NodeId -> Text -> State PackState ()
addParamName nid name =
  modify $ \st -> st {psParamNames = Map.insert nid name (psParamNames st)}

packStateSideTables ::
  PackState
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

fieldKeyText :: forall k. KnownSymbol k => Text
fieldKeyText = T.pack (symbolVal (Proxy @k))

addNode :: FlatNode -> State PackState NodeId
addNode node = do
  st <- get
  let
    n = psNodeCount st
    (enc, side') = encodeFlatNode node (psSoaSide st)
  put
    st
      { psEncs = psEncs st Seq.|> enc
      , psNodeCount = n + 1
      , psSoaSide = side'
      }
  pure n

addLit :: FlatLit -> State PackState Int
addLit lit = do
  st <- get
  let
    i = psLitCount st
  put st {psLits = psLits st Seq.|> lit, psLitCount = i + 1}
  pure i

addText :: Text -> State PackState Int
addText txt = do
  st <- get
  let
    i = psTextCount st
  put st {psTexts = psTexts st Seq.|> txt, psTextCount = i + 1}
  pure i

addFFI :: FFIForm -> State PackState Int
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

addStrCases :: [(Text, NodeId)] -> State PackState Int
addStrCases cases = do
  st <- get
  let
    i = psStrCaseCount st
  put st {psStrCases = psStrCases st Seq.|> cases, psStrCaseCount = i + 1}
  pure i

addFieldGroup :: [FlatField] -> State PackState Int
addFieldGroup fs = do
  st <- get
  let
    i = psFieldGroupCount st
  put st {psFieldGroups = psFieldGroups st Seq.|> fs, psFieldGroupCount = i + 1}
  pure i

addArgGroup :: [FlatArg] -> State PackState Int
addArgGroup args = do
  st <- get
  let
    i = psArgGroupCount st
  put st {psArgGroups = psArgGroups st Seq.|> args, psArgGroupCount = i + 1}
  pure i

packEffectProgramState :: IrEffect u -> (NodeId, PackState)
packEffectProgramState e = runState (packEffect e) emptyPackState

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
  FE_Hvm2Ref _ -> []
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

flatNodeIsEffect :: FlatNode -> Bool
flatNodeIsEffect = \case
  FX_Lift {} -> True
  FX_FFI {} -> True
  FX_UnsafeObject {} -> True
  FX_UnsafeObjectGet {} -> True
  FX_UnsafeObjectAssign {} -> True
  FX_CallMethod {} -> True
  FX_Bind {} -> True
  FX_ThenE {} -> True
  FX_BindRec {} -> True
  FX_LambdaE {} -> True
  FX_ApplyE {} -> True
  FX_IfE {} -> True
  FX_While {} -> True
  FX_ForRange {} -> True
  FX_U8Set {} -> True
  FX_U8Fill {} -> True
  FX_OptionCaseE {} -> True
  FX_ResultCaseE {} -> True
  FX_StringCaseE {} -> True
  FX_Throw {} -> True
  FX_Try {} -> True
  FX_ObjectLit {} -> True
  FX_DeleteProp {} -> True
  FX_ArrayLit {} -> True
  _ -> False

packRecArgs :: Rec IrArg us -> State PackState Int
packRecArgs rec = addArgGroup =<< packRecArgsGo rec

packRecArgsGo :: Rec IrArg us -> State PackState [FlatArg]
packRecArgsGo RecNil = pure []
packRecArgsGo (RecCons (IrArgExpr e) rs) = do
  n <- packExpr e
  rest <- packRecArgsGo rs
  pure (FlatArgExpr n : rest)
packRecArgsGo (RecCons (IrArgEffect e) rs) = do
  n <- packEffect e
  rest <- packRecArgsGo rs
  pure (FlatArgEffect n : rest)

packFieldLit :: IrFieldLit r -> State PackState FlatField
packFieldLit = \case
  IrFieldLit @k e -> do
    n <- packExpr e
    pure (FlatField (fieldKeyText @k) n)
  IrFieldLitEffect @k e -> do
    n <- packEffect e
    pure (FlatFieldEff (fieldKeyText @k) n)
  IrFieldLitExtra @k e -> do
    n <- packExpr e
    pure (FlatFieldExtra (fieldKeyText @k) n)
  IrFieldLitExtraEffect @k e -> do
    n <- packEffect e
    pure (FlatFieldExtraEff (fieldKeyText @k) n)

packFieldLits :: [IrFieldLit r] -> State PackState Int
packFieldLits fs = addFieldGroup =<< traverse packFieldLit fs

packFnBody :: IrFnBody us r -> State PackState ([Int], [Maybe Text], NodeId)
packFnBody = \case
  IrJfNil e -> ([],[],) <$> packExpr e
  IrJfCons t pn rest -> do
    (ts, pns, body) <- packFnBody rest
    pure (t : ts, pn : pns, body)

packFixed ::
  FixedOp a b c u -> IrFixedArgs a b c -> State PackState NodeId
packFixed op = \case
  IrArgsU x -> do
    n <- packExpr x
    addNode (FE_Fixed (FlatFixedU op n))
  IrArgsB x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_Fixed (FlatFixedB op nx ny))
  IrArgsT x y z -> do
    nx <- packExpr x
    ny <- packExpr y
    nz <- packExpr z
    addNode (FE_Fixed (FlatFixedT op nx ny nz))

packKernel :: IrKernel u -> State PackState NodeId
packKernel = \case
  KConcat x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KConcat nx ny)
  KPlus x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KPlus nx ny)
  KTimes x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KTimes nx ny)
  KMinus x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KMinus nx ny)
  KNegate x -> do
    n <- packExpr x
    addNode (FE_KNegate n)
  KFracDiv x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KFracDiv nx ny)
  KRem x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KRem nx ny)
  KBitAnd x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KBitAnd nx ny)
  KBitOr x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KBitOr nx ny)
  KBitXor x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KBitXor nx ny)
  KShl x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KShl nx ny)
  KShr x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KShr nx ny)
  KUShr x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KUShr nx ny)
  KBig op x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KBig op nx ny)
  KBigNeg x -> do
    n <- packExpr x
    addNode (FE_KBigNeg n)
  KAnd x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KAnd nx ny)
  KOr x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KOr nx ny)
  KEq s x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KEq s nx ny)
  KNEq s x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KNEq s nx ny)
  KGTh x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KGTh nx ny)
  KLTh x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KLTh nx ny)
  KGTEq x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KGTEq nx ny)
  KLTEq x y -> do
    nx <- packExpr x
    ny <- packExpr y
    addNode (FE_KLTEq nx ny)
  KShow x -> do
    n <- packExpr x
    addNode (FE_KShow n)
  KTypeOf x -> do
    n <- packExpr x
    addNode (FE_KTypeOf n)

packMethod :: IrMethod u -> State PackState NodeId
packMethod = \case
  IrMethMap arr tag body -> do
    nArr <- packExpr arr
    nBody <- packExpr body
    addNode (FE_MethMap nArr tag nBody)
  IrMethFilter arr tag body -> do
    nArr <- packExpr arr
    nBody <- packExpr body
    addNode (FE_MethFilter nArr tag nBody)
  IrMethReduce arr z tagA tagB body -> do
    nArr <- packExpr arr
    nz <- packExpr z
    nBody <- packExpr body
    addNode (FE_MethReduce nArr nz tagA tagB nBody)
  IrMethReduceRight arr z tagA tagB body -> do
    nArr <- packExpr arr
    nz <- packExpr z
    nBody <- packExpr body
    addNode (FE_MethReduceRight nArr nz tagA tagB nBody)
  IrMethToSorted arr tagA tagB body -> do
    nArr <- packExpr arr
    nBody <- packExpr body
    addNode (FE_MethToSorted nArr tagA tagB nBody)
  IrMethFrom n tag body -> do
    nn <- packExpr n
    nBody <- packExpr body
    addNode (FE_MethFrom nn tag nBody)

packExpr :: IrExpr u -> State PackState NodeId
packExpr = \case
  IrLiteral v -> do
    li <- addLit (FLit v)
    addNode (FE_Literal li)
  IrVar i -> addNode (FE_Var i)
  IrLet tag x body -> do
    nx <- packExpr x
    nb <- packExpr body
    addNode (FE_Let tag nx nb)
  IrLetRec tag r b -> do
    nr <- packExpr r
    nb <- packExpr b
    addNode (FE_LetRec tag nr nb)
  IrLambda tag info body -> do
    nb <- packExpr body
    n <- addNode (FE_Lambda tag nb)
    case lamTag info of
      Just name -> addHoistTag n name
      Nothing -> pure ()
    case lamParam info of
      Just pn -> addParamName n pn
      Nothing -> pure ()
    pure n
  IrApply f x -> do
    nf <- packExpr f
    nx <- packExpr x
    addNode (FE_Apply nf nx)
  IrEmbedEff e -> do
    ne <- packEffect e
    addNode (FE_EmbedEff ne)
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
  IrFixed op args -> packFixed op args
  IrKernelK k -> packKernel k
  IrMethod m -> packMethod m
  IrFnLit body -> do
    (tags, names, nBody) <- packFnBody body
    addNode (FE_FnLit tags names nBody)
  IrUnsafeNullable x -> do
    n <- packExpr x
    addNode (FE_UnsafeNullable n)
  IrFrozenLit fs -> do
    gi <- packFieldLits fs
    addNode (FE_FrozenLit gi)
  IrGetField @k o -> do
    ti <- addText (fieldKeyText @k)
    n <- packExpr o
    addNode (FE_GetField ti n)
  IrHvm2Ref name -> do
    ti <- addText name
    addNode (FE_Hvm2Ref ti)

packEffectArms :: [(Text, IrEffect v)] -> State PackState Int
packEffectArms arms =
  addStrCases =<< traverse (\(k, e) -> (k,) <$> packEffect e) arms

packEffects :: [IrEffect u] -> State PackState [NodeId]
packEffects = traverse packEffect

packEffect :: IrEffect u -> State PackState NodeId
packEffect = \case
  IrLift x -> do
    n <- packExpr x
    addNode (FX_Lift n)
  IrFFI form args -> do
    fi <- addFFI form
    ai <- packRecArgs args
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
    ai <- packRecArgs args
    addNode (FX_CallMethod nx ti ai)
  IrBind tag x body -> do
    nx <- packEffect x
    nb <- packEffect body
    addNode (FX_Bind tag nx nb)
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
    ai <- packEffectArms arms
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
    gi <- packFieldLits fs
    addNode (FX_ObjectLit gi)
  IrDeleteProp o k -> do
    no <- packEffect o
    nk <- packExpr k
    addNode (FX_DeleteProp no nk)
  IrArrayLit es -> do
    ns <- packEffects es
    addNode (FX_ArrayLit ns)
