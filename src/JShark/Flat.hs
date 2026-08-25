{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

data FlatField
  = FlatField Text NodeId
  | FlatFieldEff Text NodeId
  | FlatFieldExtra Text NodeId
  | FlatFieldExtraEff Text NodeId

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

flatLitValue :: FlatProgram -> Int -> Value u
flatLitValue p i = case flatLit p i of
  FLit v -> unsafeCoerce v

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
  FlatProgram
    { fpNodes = V.fromList (reverse (psNodes st))
    , fpLits = V.fromList (reverse (psLits st))
    , fpTexts = V.fromList (reverse (psTexts st))
    , fpFFIs = V.fromList (reverse (psFFIs st))
    , fpStrCases = V.fromList (reverse (psStrCases st))
    , fpFieldGroups = V.fromList (reverse (psFieldGroups st))
    , fpArgGroups = V.fromList (reverse (psArgGroups st))
    , fpRoot = root
    }

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
