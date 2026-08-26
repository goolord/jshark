{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

-- | Untyped flat IR: optimized 'Ir' trees packed into vectors for codegen.
module JShark.Flat
  ( NodeId
  , FlatProgram (..)
  , FlatNode (..)
  , FlatArg (..)
  , FlatField (..)
  , FlatFixed (..)
  , FlatLit (FLit)
  , packExpr
  , packEffect
  , packEffectProgram
  , fpRootEffect
  , flatNode
  , flatLit
  , flatLitValue
  , flatPure
  , flatText
  , flatFFI
  , flatStrCases
  , flatFieldGroup
  , flatArgGroup
  )
where

import Control.Monad.State.Strict (State, get, put, runState)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import GHC.TypeLits (KnownSymbol, symbolVal)
import JShark.Ir
import JShark.Rec (Rec (..))
import JShark.Types (BigBinOp, FFIForm (..), FixedOp, Value (..))
import Unsafe.Coerce (unsafeCoerce)

type NodeId = Int

data FlatLit where
  FLit :: Value u -> FlatLit

data FlatArg
  = FlatArgExpr NodeId
  | FlatArgEffect NodeId
  deriving (Eq)

data FlatField
  = FlatField Text NodeId
  | FlatFieldEff Text NodeId
  | FlatFieldExtra Text NodeId
  | FlatFieldExtraEff Text NodeId
  deriving (Eq)

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
  | FE_FnLit [Int] NodeId
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

data FlatProgram = FlatProgram
  { fpNodes :: Vector FlatNode
  , fpLits :: Vector FlatLit
  , fpTexts :: Vector Text
  , fpFFIs :: Vector FFIForm
  , fpStrCases :: Vector [(Text, NodeId)]
  , fpFieldGroups :: Vector [FlatField]
  , fpArgGroups :: Vector [FlatArg]
  , fpPure :: Vector Word8
  , fpRoot :: NodeId
  }

fpRootEffect :: FlatProgram -> NodeId
fpRootEffect = fpRoot

emptyPackState :: PackState
emptyPackState =
  PackState
    { psNodes = []
    , psLits = []
    , psTexts = []
    , psFFIs = []
    , psStrCases = []
    , psFieldGroups = []
    , psArgGroups = []
    }

packEffectProgram :: IrEffect u -> FlatProgram
packEffectProgram e =
  let
    (root, st) = runState (packEffect e) emptyPackState
   in
    finalizePack root st

flatNode :: FlatProgram -> NodeId -> FlatNode
flatNode p i = fpNodes p V.! i

flatLit :: FlatProgram -> Int -> FlatLit
flatLit p i = fpLits p V.! i

-- | Recover a typed literal after PHOAS typechecking erased 'u'.
-- INVARIANT: only call on literals produced by 'packEffectProgram'.
flatLitValue :: FlatProgram -> Int -> Value u
flatLitValue p i = case flatLit p i of
  FLit v -> unsafeCoerce v

flatPure :: FlatProgram -> NodeId -> Word8
flatPure p i
  | i >= 0 && i < V.length (fpPure p) = fpPure p V.! i
  | otherwise = 0

flatText :: FlatProgram -> Int -> Text
flatText p i = fpTexts p V.! i

flatFFI :: FlatProgram -> Int -> FFIForm
flatFFI p i = fpFFIs p V.! i

flatStrCases :: FlatProgram -> Int -> [(Text, NodeId)]
flatStrCases p i = fpStrCases p V.! i

flatFieldGroup :: FlatProgram -> Int -> [FlatField]
flatFieldGroup p i = fpFieldGroups p V.! i

flatArgGroup :: FlatProgram -> Int -> [FlatArg]
flatArgGroup p i = fpArgGroups p V.! i

data PackState = PackState
  { psNodes :: ![FlatNode]
  , psLits :: ![FlatLit]
  , psTexts :: ![Text]
  , psFFIs :: ![FFIForm]
  , psStrCases :: ![[(Text, NodeId)]]
  , psFieldGroups :: ![[FlatField]]
  , psArgGroups :: ![[FlatArg]]
  }

fieldKeyText :: forall k. KnownSymbol k => Text
fieldKeyText = T.pack (symbolVal (Proxy @k))

addNode :: FlatNode -> State PackState NodeId
addNode node = do
  st <- get
  let n = length (psNodes st)
  put st {psNodes = node : psNodes st}
  pure n

addLit :: FlatLit -> State PackState Int
addLit lit = do
  st <- get
  let i = length (psLits st)
  put st {psLits = lit : psLits st}
  pure i

addText :: Text -> State PackState Int
addText txt = do
  st <- get
  let i = length (psTexts st)
  put st {psTexts = txt : psTexts st}
  pure i

addFFI :: FFIForm -> State PackState Int
addFFI form = do
  st <- get
  let i = length (psFFIs st)
  put st {psFFIs = form : psFFIs st}
  pure i

addStrCases :: [(Text, NodeId)] -> State PackState Int
addStrCases cases = do
  st <- get
  let i = length (psStrCases st)
  put st {psStrCases = cases : psStrCases st}
  pure i

addFieldGroup :: [FlatField] -> State PackState Int
addFieldGroup fs = do
  st <- get
  let i = length (psFieldGroups st)
  put st {psFieldGroups = fs : psFieldGroups st}
  pure i

addArgGroup :: [FlatArg] -> State PackState Int
addArgGroup args = do
  st <- get
  let i = length (psArgGroups st)
  put st {psArgGroups = args : psArgGroups st}
  pure i

finalizePack :: NodeId -> PackState -> FlatProgram
finalizePack root st =
  let
    prog =
      FlatProgram
        { fpNodes = V.fromList (reverse (psNodes st))
        , fpLits = V.fromList (reverse (psLits st))
        , fpTexts = V.fromList (reverse (psTexts st))
        , fpFFIs = V.fromList (reverse (psFFIs st))
        , fpStrCases = V.fromList (reverse (psStrCases st))
        , fpFieldGroups = V.fromList (reverse (psFieldGroups st))
        , fpArgGroups = V.fromList (reverse (psArgGroups st))
        , fpPure = V.empty
        , fpRoot = root
        }
   in
    validateFlatProgram prog `seq` prog

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
  FE_FnLit _ b -> [b]
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

flatNodeSideRefs :: FlatProgram -> FlatNode -> [NodeId]
flatNodeSideRefs p = \case
  FX_FFI _ ai -> sideArgRefs p ai
  FX_CallMethod _ _ ai -> sideArgRefs p ai
  FX_StringCaseE _ ai _ -> sideStrCaseRefs p ai
  FX_ObjectLit gi -> sideFieldRefs p gi
  _ -> []
 where
  sideArgRefs prog ai
    | ai >= 0 && ai < V.length (fpArgGroups prog) =
        map flatArgRef (fpArgGroups prog V.! ai)
    | otherwise =
        error "JShark.Flat.flatNodeSideRefs: arg group index out of range"
  sideStrCaseRefs prog ai
    | ai >= 0 && ai < V.length (fpStrCases prog) =
        map snd (fpStrCases prog V.! ai)
    | otherwise =
        error "JShark.Flat.flatNodeSideRefs: str case group index out of range"
  sideFieldRefs prog gi
    | gi >= 0 && gi < V.length (fpFieldGroups prog) =
        map flatFieldRef (fpFieldGroups prog V.! gi)
    | otherwise =
        error "JShark.Flat.flatNodeSideRefs: field group index out of range"

flatNodePackRefs :: FlatProgram -> FlatNode -> [NodeId]
flatNodePackRefs p node =
  flatNodeChildRefs node ++ flatNodeSideRefs p node

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

validateFlatProgram :: FlatProgram -> ()
validateFlatProgram p =
  let
    nodes = fpNodes p
    n = V.length nodes
    inRange ref
      | ref >= 0 && ref < n = ()
      | otherwise =
          error $
            "JShark.Flat.validateFlatProgram: node ref "
              <> show ref
              <> " out of range [0,"
              <> show n
              <> ")"
    packOrder i ref
      | ref < i = ()
      | otherwise =
          error $
            "JShark.Flat.validateFlatProgram: node "
              <> show i
              <> " refs "
              <> show ref
              <> " (pack order violated)"
   in
    foldr
      ( \i acc ->
          foldr
            (\ref a -> packOrder i ref `seq` a)
            acc
            (flatNodePackRefs p (nodes V.! i))
      )
      ()
      [0 .. n - 1]
      `seq` foldr (\r () -> inRange r) () (map flatArgRef (concat (V.toList (fpArgGroups p))))
      `seq` foldr (\r () -> inRange r) () (map flatFieldRef (concat (V.toList (fpFieldGroups p))))
      `seq` foldr (\r () -> inRange r) () (map snd (concat (V.toList (fpStrCases p))))
      `seq` if fpRoot p >= 0 && fpRoot p < n
        then ()
        else error "JShark.Flat.validateFlatProgram: invalid root"

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

packFnBody :: IrFnBody us r -> State PackState ([Int], NodeId)
packFnBody = \case
  IrJfNil e -> (, ) [] <$> packExpr e
  IrJfCons t rest -> do
    (ts, body) <- packFnBody rest
    pure (t : ts, body)

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
  IrLambda tag body -> do
    nb <- packExpr body
    addNode (FE_Lambda tag nb)
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
    (tags, nBody) <- packFnBody body
    addNode (FE_FnLit tags nBody)
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
