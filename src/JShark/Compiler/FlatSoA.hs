{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Struct-of-arrays flat IR and bulk-friendly optimizer passes.
module JShark.Compiler.FlatSoA
  ( FlatSoA (..)
  , flatSoaParallelThreshold
  , packEffectProgramDirect
  , optimizeFlatPack
  , flatSoaNodeCount
  , flatSoaNode
  , flatSoaLitValue
  , flatSoaText
  , flatSoaFFI
  , flatSoaStrCases
  , flatSoaFieldGroup
  , flatSoaArgGroup
  , flatSoaNodePackRefs
  , flatSoaIdentBudget
  , flatSoaLayerBuckets
  , soaPureCount
  , soaPureVector
  , constantFoldWithStats
  , propagatePureWithStats
  , optConstantFoldNumOnce
  , soaColumnsEqual
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (runST)
import Data.Bits ((.&.))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Word (Word8)
import GHC.IO.Unsafe (unsafePerformIO)
import JShark.Api.Types (BigBinOp (..), FFIForm (..), Value (..))
import JShark.Compiler.Flat
  ( FlatArg (..)
  , FlatField (..)
  , FlatFixed (..)
  , FlatLit (..)
  , FlatNode (..)
  , NodeId
  , PackState
  , flatArgRef
  , flatFieldRef
  , flatNodeChildRefs
  , packEffectProgramState
  , packStateEncs
  , packStateHoistTags
  , packStateNodeCount
  , packStateParamNames
  , packStateSideTables
  , packStateSoaSide
  , sideAccToVectors
  )
import JShark.Compiler.FlatEnc
  ( Op
  , freezeEncSeq
  , oFE_APPLY
  , oFE_EMBEDEFF
  , oFE_ERROR
  , oFE_FIXED
  , oFE_FNLIT
  , oFE_FROZEN
  , oFE_GETFIELD
  , oFE_HVM2REF
  , oFE_IF
  , oFE_INDEX
  , oFE_KAND
  , oFE_KBIG
  , oFE_KBIGNEG
  , oFE_KBITAND
  , oFE_KBITOR
  , oFE_KBITXOR
  , oFE_KCONCAT
  , oFE_KDIV
  , oFE_KEQ
  , oFE_KGTEQ
  , oFE_KGTH
  , oFE_KLTEQ
  , oFE_KLTH
  , oFE_KMINUS
  , oFE_KNEG
  , oFE_KNEQ
  , oFE_KOR
  , oFE_KPLUS
  , oFE_KREM
  , oFE_KSHL
  , oFE_KSHOW
  , oFE_KSHR
  , oFE_KTIMES
  , oFE_KTYPEOF
  , oFE_KUSHR
  , oFE_LAMBDA
  , oFE_LET
  , oFE_LETREC
  , oFE_LITERAL
  , oFE_MFILTER
  , oFE_MFROM
  , oFE_MMAP
  , oFE_MREDUCE
  , oFE_MREDUCER
  , oFE_MTOSORTED
  , oFE_OPTIONCASE
  , oFE_RESCASE
  , oFE_RESERR
  , oFE_RESOK
  , oFE_U8INDEX
  , oFE_UNSAFENULL
  , oFE_VAR
  , oFX_APPLYE
  , oFX_ARRAYLIT
  , oFX_BIND
  , oFX_BINDREC
  , oFX_CALLMETHOD
  , oFX_DELETEPROP
  , oFX_FFI
  , oFX_FORRANGE
  , oFX_IFE
  , oFX_LAMBDAE
  , oFX_LIFT
  , oFX_OBJLIT
  , oFX_OPTCASEE
  , oFX_RESCASEE
  , oFX_STRCASEE
  , oFX_THENE
  , oFX_THROW
  , oFX_TRY
  , oFX_U8FILL
  , oFX_U8SET
  , oFX_UNSAFEOBJ
  , oFX_UNSAFEOBJGET
  , oFX_UNSAFEOBJSET
  , oFX_WHILE
  )
import JShark.Compiler.Ir (IrEffect)
import Unsafe.Coerce (unsafeCoerce)

data FlatSoA = FlatSoA
  { fsaOpcodes :: !(VU.Vector Op)
  , fsaA :: !(VU.Vector Int32)
  , fsaB :: !(VU.Vector Int32)
  , fsaC :: !(VU.Vector Int32)
  , fsaD :: !(VU.Vector Int32)
  , fsaE :: !(VU.Vector Int32)
  , fsaPure :: !(VU.Vector Word8)
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
  , fsaSubtreeSizes :: !(V.Vector Int)
  }

flatSoaNodeCount :: FlatSoA -> Int
flatSoaNodeCount soa = VU.length (fsaOpcodes soa)

freezeSoaFromPackState :: NodeId -> PackState -> FlatSoA
freezeSoaFromPackState root st =
  let
    side = packStateSoaSide st
    n = packStateNodeCount st
    (opF, aF, bF, cF, dF, eF) = freezeEncSeq (packStateEncs st)
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
        , fsaPure = VU.replicate n 0
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
        , fsaSubtreeSizes = V.empty
        }
   in
    attachFlatSoaSubtreeSizes soa0

-- | Pack IR directly to SoA columns (no intermediate node vector).
packEffectProgramDirect :: IrEffect u -> FlatSoA
packEffectProgramDirect e =
  let
    (root, st) = packEffectProgramState e
   in
    freezeSoaFromPackState root st

-- | SoA optimizer passes; returns optimized SoA (emit decodes nodes on demand).
optimizeFlatPack :: FlatSoA -> FlatSoA
optimizeFlatPack soa0 =
  let
    !(soa1, _folded) = optConstantFoldNumWithChanged soa0
    !soa2 = propagatePureFlagsPar soa1
   in
    attachFlatSoaSubtreeSizes soa2

i32 :: Int -> Int32
i32 = fromIntegral

unboxedToBoxedPure :: VU.Vector Word8 -> V.Vector Word8
unboxedToBoxedPure = GV.convert
{-# INLINE unboxedToBoxedPure #-}

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
  | op == oFE_FNLIT =
      let
        (tags, names) = fsaFnLit soa V.! ix
       in
        FE_FnLit tags names iy
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
    "JShark.Compiler.FlatSoA.decodeOp: unknown opcode "
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

flatSoaNode :: FlatSoA -> NodeId -> FlatNode
flatSoaNode soa idx =
  let
    op = fsaOpcodes soa VU.! idx
    ix = fromIntegral (fsaA soa VU.! idx) :: Int
    iy = fromIntegral (fsaB soa VU.! idx) :: Int
    iz = fromIntegral (fsaC soa VU.! idx) :: Int
    iw = fromIntegral (fsaD soa VU.! idx) :: Int
    iv = fromIntegral (fsaE soa VU.! idx) :: Int
   in
    decodeOp soa op ix iy iz iw iv

flatSoaLit :: FlatSoA -> Int -> FlatLit
flatSoaLit soa i = fsaLits soa V.! i

flatSoaLitValue :: FlatSoA -> Int -> Value u
flatSoaLitValue soa i = case flatSoaLit soa i of
  FLit v -> unsafeCoerce v

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

flatSoaSubtreeSizes :: FlatSoA -> V.Vector Int
flatSoaSubtreeSizes soa =
  if V.null (fsaSubtreeSizes soa)
    then computeFlatSoaSubtreeSizes soa
    else fsaSubtreeSizes soa

attachFlatSoaSubtreeSizes :: FlatSoA -> FlatSoA
attachFlatSoaSubtreeSizes soa =
  soa {fsaSubtreeSizes = computeFlatSoaSubtreeSizes soa}

-- | Pack order: child refs are below @i@; one backward pass, no vector copies.
computeFlatSoaSubtreeSizes :: FlatSoA -> V.Vector Int
computeFlatSoaSubtreeSizes soa =
  let
    n = flatSoaNodeCount soa
   in
    if n <= 0
      then V.empty
      else runST $ do
        ms <- MV.new n
        let
          writeSz i sz = MV.write ms i sz
          go i
            | i < 0 = pure ()
            | otherwise = do
                let
                  refs = flatSoaNodePackRefs soa (flatSoaNode soa i)
                acc <-
                  foldM
                    ( \acc r -> do
                        s <- MV.read ms r
                        pure (acc + s)
                    )
                    (0 :: Int)
                    refs
                writeSz i (1 + acc)
                go (i - 1)
        go (n - 1)
        V.unsafeFreeze ms

flatSoaIdentBudget :: FlatSoA -> NodeId -> Int
flatSoaIdentBudget soa i =
  let
    sizes = flatSoaSubtreeSizes soa
   in
    if i >= 0 && i < V.length sizes then sizes V.! i else 1

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

soaPureVector :: FlatSoA -> V.Vector Word8
soaPureVector soa = unboxedToBoxedPure (fsaPure soa)

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

-- Sequential 'runST' scan. A parallel IO-per-node walk was ~100s on Life.
optConstantFoldNumWithChanged :: FlatSoA -> (FlatSoA, Bool)
optConstantFoldNumWithChanged soa =
  let
    (soa', _, folded) = constantFoldWithStats soa
   in
    (soa', folded)
{-# NOINLINE optConstantFoldNumWithChanged #-}

constantFoldWithStats :: FlatSoA -> (FlatSoA, Int, Bool)
constantFoldWithStats soa0 =
  let
    go soa passes didFold =
      let
        (soa', changed) = optConstantFoldNumOnce soa
       in
        if changed
          then go soa' (passes + 1) True
          else (soa, passes, didFold)
   in
    go soa0 0 False
{-# NOINLINE constantFoldWithStats #-}

propagatePureWithStats :: FlatSoA -> (FlatSoA, Int)
propagatePureWithStats soa0 =
  let
    go soa passes =
      let
        (soa', changed) = propagatePureFlagsPassPar soa
       in
        if changed then go soa' (passes + 1) else (soa, passes + 1)
   in
    go soa0 0
{-# NOINLINE propagatePureWithStats #-}
