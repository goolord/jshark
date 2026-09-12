{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier -Wno-missing-export-lists -Wno-missing-signatures -Wno-type-defaults #-}

-- | Flat IR → JavaScript (effectful compile path).
--
-- Internal to the JShark compiler; this module is exposed for tests and
-- tooling and its API may change between 0.x releases.
module JShark.Compiler.Codegen.Flat where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.STRef (modifySTRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import GHC.IO.Unsafe (unsafePerformIO)
import qualified JShark.Api.Prim as Prim
import JShark.Api.Types
import JShark.Compiler.Binder
  ( pattern Name
  )
import JShark.Compiler.Codegen.Core
import JShark.Compiler.Codegen.Stmt
  ( asStmt
  , assignResult
  , emitBranching
  , ifAssignOrStmt
  , ifElseStmt
  , letResult
  , recBindStmt
  , renderFFIInvoke
  , tryCatchStmt
  )
import JShark.Compiler.Emit
  ( JS
  , blockBody
  , braces
  , brackets
  , colon
  , hcat
  , jsString
  , jsText
  , parens
  , punctuate
  , semi
  , vcat
  , vcatNonEmpty
  , ($$)
  , (<+>)
  )
import JShark.Compiler.Evaluate
  ( bigOpJS
  , jsBigIntLit
  , jsQuote
  , jsUint8ArrayLit
  )
import qualified JShark.Compiler.Flat as Flat
import JShark.Compiler.Lower (lowerOptEffectIrWith, lowerOptExprIr)

resultPayloadRef :: Maybe JS -> JS
resultPayloadRef = fromMaybe "undefined"

resultObject :: Bool -> Maybe JS -> JS
resultObject isOk payload =
  let
    flag = if isOk then "true" else "false"
   in
    braces ((("ok:" <+> flag) <> ",") <+> ("value:" <+> resultPayloadRef payload))

flatRenderResultLit :: Bool -> CG -> Value u -> (CG, Code)
flatRenderResultLit isOk s0 x =
  let
    (s1, MkCode d r _) = flatRenderLiteral (IM.empty) s0 x
   in
    (s1, MkCode d (Just (resultObject isOk r)) False)

flatRenderLiteral ::
  Env -> CG -> Value u -> (CG, Code)
flatRenderLiteral env s0 = \case
  ValueNumber d -> (s0, Code mempty (jsNumber (cgStyle s0) d))
  ValueBigInt n -> (s0, Code mempty (jsBigIntLit n))
  ValueArray xs ->
    let
      (s1, exprs) =
        mapAccumL (\s x -> flatRenderLiteral env s x) s0 xs
     in
      ( s1
      , Code
          (codesDecls exprs)
          (brackets (hcat (punctuate ", " (codesRefs exprs))))
      )
  ValueString s -> (s0, Code mempty (jsQuote s))
  ValueFunction _ -> error "JShark.flatPureAST: ValueFunction is eval-only"
  ValueUnit -> (s0, mempty)
  ValueOption (Just x) -> flatRenderLiteral env s0 x
  ValueOption Nothing -> (s0, Code mempty "null")
  ValueResult (Right x) -> flatRenderResultLit True s0 x
  ValueResult (Left x) -> flatRenderResultLit False s0 x
  ValueRegex s ->
    (s0, Code mempty ("new RegExp" <> parens (jsQuote s)))
  ValueUint8Array ba -> (s0, Code mempty (jsUint8ArrayLit ba))
  ValueBool True -> (s0, Code mempty "true")
  ValueBool False -> (s0, Code mempty "false")
  ValueFrozen {} -> error "JShark.flatPureAST: ValueFrozen is eval-only"

flatIsUnitExpr view nid = case Flat.flatSoaNode view nid of
  Flat.FE_Literal li ->
    Flat.withFlatLitValue view li $ \case
      ValueUnit -> True
      _ -> False
  Flat.FE_Var {} -> False
  _ -> False

flatIsUnitEffect view nid = case Flat.flatSoaNode view nid of
  Flat.FX_Lift eid -> flatIsUnitExpr view eid
  Flat.FX_Throw _ -> True
  Flat.FX_While _ b -> flatIsUnitEffect view b
  Flat.FX_ForRange _ _ _ b -> flatIsUnitEffect view b
  Flat.FX_ThenE _ y -> flatIsUnitEffect view y
  Flat.FX_Bind _ _ y -> flatIsUnitEffect view y
  Flat.FX_BindRec _ _ y -> flatIsUnitEffect view y
  Flat.FX_IfE _ t e -> flatIsUnitEffect view t && flatIsUnitEffect view e
  Flat.FX_OptionCaseE _ n _ s ->
    flatIsUnitEffect view n && flatIsUnitEffect view s
  Flat.FX_ResultCaseE _ _ er _ ok ->
    flatIsUnitEffect view er && flatIsUnitEffect view ok
  Flat.FX_StringCaseE _ ai d ->
    all (flatIsUnitEffect view . snd) (Flat.flatSoaStrCases view ai)
      && flatIsUnitEffect view d
  Flat.FX_Try a _ k -> flatIsUnitEffect view a && flatIsUnitEffect view k
  _ -> False

flatIsSimpleEffectNode view nid = case Flat.flatSoaNode view nid of
  Flat.FX_Lift eid -> flatIsSimpleNode view eid
  Flat.FX_FFI {} -> True
  Flat.FX_CallMethod {} -> True
  Flat.FX_UnsafeObject {} -> True
  Flat.FX_UnsafeObjectGet {} -> True
  Flat.FX_ArrayLit es -> all (flatIsSimpleEffectNode view) es
  _ -> False

flatIsSimpleNode view nid = case Flat.flatSoaNode view nid of
  Flat.FE_Literal _ -> True
  Flat.FE_Var _ -> True
  Flat.FE_EmbedEff eid -> flatIsSimpleEffectNode view eid
  Flat.FE_KShow _ -> True
  Flat.FE_KTypeOf _ -> True
  Flat.FE_KNegate _ -> True
  Flat.FE_KBigNeg _ -> True
  Flat.FE_Fixed {} -> True
  Flat.FE_FnLit {} -> True
  Flat.FE_Index {} -> True
  Flat.FE_U8Index {} -> True
  Flat.FE_UnsafeNullable x -> flatIsSimpleNode view x
  Flat.FE_FrozenLit {} -> True
  Flat.FE_GetField {} -> True
  _ -> False

flatWrapOperand view nid d =
  if flatIsSimpleNode view nid then d else parens d

flatRenderBin ctx op s0 view xId yId =
  let
    (s1, Code xDecl xRef) = flatChild ctx s0 xId
    (s2, Code yDecl yRef) = flatChild ctx s1 yId
   in
    ( s2
    , Code
        (xDecl $$ yDecl)
        ( flatWrapOperand view xId xRef
            <+> jsText op
            <+> flatWrapOperand view yId yRef
        )
    )

-- | @&&@ / @||@: the left operand always evaluates, the right only when
-- the operator selects it. When the right side needs declarations they
-- must be guarded, not hoisted above the operator.
flatRenderShortCircuit ctx s0 view isAnd xId yId =
  let
    (s1, MkCode xDecl xRef _) = flatChild ctx s0 xId
   in
    case flatChild ctx s1 yId of
      (s2, MkCode Nothing yRef _) ->
        ( s2
        , MkCode
            xDecl
            ( Just
                ( flatWrapOperand view xId (fromMaybe "undefined" xRef)
                    <+> (if isAnd then "&&" else "||")
                    <+> flatWrapOperand view yId (fromMaybe "undefined" yRef)
                )
            )
            False
        )
      (s2, MkCode (Just yDecl) yRef _) ->
        let
          (n, s3) = allocIdent s2
          rv = identName s3 n
          cond = if isAnd then jsText rv else "!" <> parens (jsText rv)
          guard =
            yDecl
              $$ (jsText rv <+> "=" <+> fromMaybe "undefined" yRef)
              <> semi
          xInit =
            ( "let"
                <+> jsText rv
                <+> "="
                <+> flatWrapOperand view xId (fromMaybe "undefined" xRef)
            )
              <> semi
         in
          ( s3
          , MkCode
              ( Just
                  (fromMaybe mempty xDecl $$ xInit $$ ("if" <+> parens cond <+> blockBody guard))
              )
              (Just (jsText rv))
              False
          )

flatRenderArgListSeq ctx s0 args =
  let
    go s = \case
      [] -> (s, [])
      a : rest ->
        let
          (s', c) = flatChild ctx s (Flat.flatArgRef a)
          (s'', cs') = go s' rest
         in
          (s'', c : cs')
    (s1, cs) = go s0 args
   in
    (s1, codesDecls cs, hcat (punctuate ", " (codesRefs cs)))

flatRenderArgList ctx s0 view ai =
  flatRenderArgListSeq ctx s0 (Flat.flatSoaArgGroup view ai)

flatRenderField ctx s f =
  let
    k = case f of
      Flat.FlatField n _ -> n
      Flat.FlatFieldEff n _ -> n
      Flat.FlatFieldExtra n _ -> n
      Flat.FlatFieldExtraEff n _ -> n
    (s', Code d r) = flatChild ctx s (Flat.flatFieldRef f)
   in
    (s', (d, (jsPropKey (cgStyle s') k <> ":") <+> r))

flatRenderObjectLit ctx s0 view gi =
  let
    fs = Flat.flatSoaFieldGroup view gi
    (s1, parts) = mapAccumL (flatRenderField ctx) s0 fs
    (declList, pairs) = unzip parts
   in
    (s1, Code (vcatNonEmpty declList) (braces (hcat (punctuate ", " pairs))))

flatRenderArrayLit ctx s0 es =
  let
    (s1, cs) =
      mapAccumL
        ( \st nid ->
            let
              (st', code) = flatChild ctx st nid
             in
              (st', code)
        )
        s0
        es
   in
    ( s1
    , Code
        (codesDecls cs)
        (brackets (hcat (punctuate ", " (codesRefs cs))))
    )

flatRenderFixed ctx s0 view = \case
  Flat.FlatFixedU op xId
    | Just name <- Prim.math1Name op ->
        let
          (s1, Code xDecl xRef) = flatChild ctx s0 xId
         in
          (s1, Code xDecl ("Math." <> jsText name <> parens xRef))
  Flat.FlatFixedB op xId yId
    | Just name <- Prim.math2Name op ->
        let
          (s1, Code xDecl xRef) = flatChild ctx s0 xId
          (s2, Code yDecl yRef) = flatChild ctx s1 yId
         in
          ( s2
          , Code
              (xDecl $$ yDecl)
              ( "Math."
                  <> jsText name
                  <> parens (xRef <> ", " <> yRef)
              )
          )
  Flat.FlatFixedU op xId ->
    let
      (s1, Code rDecl rRef) = flatChild ctx s0 xId
     in
      (s1, Code rDecl (Prim.fixedUnaryJS op (flatWrapOperand view xId rRef)))
  Flat.FlatFixedB op xId yId ->
    let
      (s1, Code rDecl rRef) = flatChild ctx s0 xId
      (s2, Code aDecl aRef) = flatChild ctx s1 yId
     in
      ( s2
      , Code
          (rDecl $$ aDecl)
          (Prim.fixedBinaryJS op (flatWrapOperand view xId rRef) aRef)
      )
  Flat.FlatFixedT op xId yId zId ->
    let
      (s1, Code rDecl rRef) = flatChild ctx s0 xId
      (s2, Code aDecl aRef) = flatChild ctx s1 yId
      (s3, Code bDecl bRef) = flatChild ctx s2 zId
     in
      ( s3
      , Code
          (rDecl $$ aDecl $$ bDecl)
          ( Prim.fixedTernaryJS
              op
              (flatWrapOperand view xId rRef)
              aRef
              bRef
          )
      )

flatRenderKernel ctx s0 view = \case
  Flat.FE_KConcat x y -> flatRenderBin ctx "+" s0 view x y
  Flat.FE_KPlus x y -> flatRenderBin ctx "+" s0 view x y
  Flat.FE_KMinus x y -> flatRenderBin ctx "-" s0 view x y
  Flat.FE_KTimes x y -> flatRenderBin ctx "*" s0 view x y
  Flat.FE_KFracDiv x y -> flatRenderBin ctx "/" s0 view x y
  Flat.FE_KRem x y -> flatRenderBin ctx "%" s0 view x y
  Flat.FE_KBitAnd x y -> flatRenderBin ctx "&" s0 view x y
  Flat.FE_KBitOr x y -> flatRenderBin ctx "|" s0 view x y
  Flat.FE_KBitXor x y -> flatRenderBin ctx "^" s0 view x y
  Flat.FE_KShl x y -> flatRenderBin ctx "<<" s0 view x y
  Flat.FE_KShr x y -> flatRenderBin ctx ">>" s0 view x y
  Flat.FE_KUShr x y -> flatRenderBin ctx ">>>" s0 view x y
  Flat.FE_KBig op x y -> flatRenderBin ctx (bigOpJS op) s0 view x y
  Flat.FE_KBigNeg x ->
    let
      (s1, Code xDecl xRef) = flatChild ctx s0 x
     in
      (s1, Code xDecl $ "-" <> parens xRef)
  Flat.FE_KShow x ->
    let
      (s1, Code xDecl xRef) = flatChild ctx s0 x
     in
      (s1, Code xDecl $ "String" <> parens xRef)
  Flat.FE_KTypeOf x ->
    let
      (s1, Code xDecl xRef) = flatChild ctx s0 x
     in
      (s1, Code xDecl $ "typeof" <+> xRef)
  Flat.FE_KNegate x ->
    let
      (s1, Code xDecl xRef) = flatChild ctx s0 x
     in
      (s1, Code xDecl $ "-" <> parens xRef)
  Flat.FE_KAnd x y -> flatRenderShortCircuit ctx s0 view True x y
  Flat.FE_KOr x y -> flatRenderShortCircuit ctx s0 view False x y
  Flat.FE_KEq structural x y
    | structural ->
        let
          (s1, Code xDecl xRef) = flatChild ctx s0 x
          (s2, Code yDecl yRef) = flatChild ctx s1 y
          (s3, eqJs) =
            emitValueEq
              s2
              (flatWrapOperand view x xRef)
              (flatWrapOperand view y yRef)
         in
          (s3, Code (xDecl $$ yDecl) eqJs)
    | otherwise ->
        flatRenderBin ctx "===" s0 view x y
  Flat.FE_KNEq structural x y
    | structural ->
        let
          (s1, Code xDecl xRef) = flatChild ctx s0 x
          (s2, Code yDecl yRef) = flatChild ctx s1 y
          (s3, neJs) =
            emitValueNEq
              s2
              (flatWrapOperand view x xRef)
              (flatWrapOperand view y yRef)
         in
          (s3, Code (xDecl $$ yDecl) neJs)
    | otherwise ->
        flatRenderBin ctx "!==" s0 view x y
  Flat.FE_KGTh x y -> flatRenderBin ctx ">" s0 view x y
  Flat.FE_KLTh x y -> flatRenderBin ctx "<" s0 view x y
  Flat.FE_KGTEq x y -> flatRenderBin ctx ">=" s0 view x y
  Flat.FE_KLTEq x y -> flatRenderBin ctx "<=" s0 view x y
  _ -> error "JShark.flatRenderKernel: unexpected node"

flatEnvTag env tag =
  case IM.lookup tag env of
    Just n -> n
    Nothing -> error "JShark.flatEnvTag: missing binding"

flatRenderCallbackMethod ctx env name s0 view arrId tag bodyId =
  let
    (s1, Code rDecl rRef) = flatChild ctx s0 arrId
    (s2, Code exDecl exRef) = flatChild ctx s1 bodyId
    nParam = flatEnvTag env tag
    call =
      flatWrapOperand view arrId rRef
        <> "."
        <> jsString name
        <> parens (jsCallback s2 [nJS s2 nParam] exDecl exRef)
   in
    (s2, Code rDecl call)

flatRenderFold ctx env method s0 view arrId zId tagA tagB bodyId =
  let
    (s1, Code rDecl rRef) = flatChild ctx s0 arrId
    (s2, Code zDecl zRef) = flatChild ctx s1 zId
    (s3, Code exDecl exRef) = flatChild ctx s2 bodyId
    nAcc = flatEnvTag env tagA
    nElem = flatEnvTag env tagB
    cb = jsCallback s3 [nJS s3 nAcc, nJS s3 nElem] exDecl exRef
    call =
      flatWrapOperand view arrId rRef
        <> jsString method
        <> parens (cb <> ", " <> zRef)
   in
    (s3, Code (rDecl $$ zDecl) call)

flatRenderMethod ctx env s0 view = \case
  Flat.FE_MethMap arr tag body ->
    flatRenderCallbackMethod ctx env "map" s0 view arr tag body
  Flat.FE_MethFilter arr tag body ->
    flatRenderCallbackMethod ctx env "filter" s0 view arr tag body
  Flat.FE_MethReduce arr z tagA tagB body ->
    flatRenderFold ctx env ".reduce" s0 view arr z tagA tagB body
  Flat.FE_MethReduceRight arr z tagA tagB body ->
    flatRenderFold ctx env ".reduceRight" s0 view arr z tagA tagB body
  Flat.FE_MethToSorted arr tagA tagB body ->
    let
      (s1, Code rDecl rRef) = flatChild ctx s0 arr
      nA = flatEnvTag env tagA
      nB = flatEnvTag env tagB
      (s2, Code exDecl exRef) = flatChild ctx s1 body
      cb = jsCallback s2 [nJS s2 nA, nJS s2 nB] exDecl exRef
     in
      ( s2
      , Code rDecl (flatWrapOperand view arr rRef <> ".toSorted" <> parens cb)
      )
  Flat.FE_MethFrom n tag body ->
    let
      (s1, Code nDecl nRef) = flatChild ctx s0 n
      nI = flatEnvTag env tag
      (s2, Code exDecl exRef) = flatChild ctx s1 body
      cb = jsCallback s2 [jsText "_", nJS s2 nI] exDecl exRef
     in
      (s2, Code nDecl ("Array.from({length: " <> nRef <> "}, " <> cb <> ")"))
  _ -> error "JShark.flatRenderMethod: unexpected node"

flatRenderFnLit ctx env s0 tags bodyId =
  let
    ids = map (flatEnvTag env) tags
    (s1, Code d r) = flatChild ctx s0 bodyId
   in
    (s1, Code mempty (jsCallback s1 (map (nJS s1) ids) d r))

flatResultUnwrapIdent env s tag = (flatEnvTag env tag, s)

-- | Shared result-case prelude: bind the scrutinee, then its @.value@.
flatResultPrelude ctx env s0 resId tagE =
  let
    (s1, MkCode rDecl rRef _) = flatChild ctx s0 resId
    (nObj, s2) = allocIdent s1
    (nUnw, s3) = flatResultUnwrapIdent env s2 tagE
    obj = identName s3 nObj
    prelude =
      fromMaybe mempty rDecl
        $$ constBind s3 nObj (fromMaybe mempty rRef)
        $$ constBind s3 nUnw (jsText obj <> ".value")
   in
    (s3, obj, prelude)

flatRenderResultCase ctx env s0 resId tagE errId _tagO okId =
  let
    (s3, obj, prelude) = flatResultPrelude ctx env s0 resId tagE
    (s4, Code eDecl eRef) = flatChild ctx s3 errId
    (s5, Code oDecl oRef) = flatChild ctx s4 okId
   in
    ( s5
    , Code
        (prelude $$ eDecl $$ oDecl)
        (parens ((jsText obj <> ".ok") <+> "?" <+> oRef <+> ":" <+> eRef))
    )

flatSeqEffect ctx s0 xId yId =
  let
    (s1, MkCode xDecl xRef xFX) = flatChild ctx s0 xId
    (s2, MkCode yDecl yRef yFX) = flatChild ctx s1 yId
    stmt
      | isNothing xRef = fromMaybe mempty xDecl
      | not xFX && isJust xDecl = fromMaybe mempty xDecl
      | otherwise = asStmt xDecl xRef
   in
    (s2, MkCode (Just (stmt $$ fromMaybe mempty yDecl)) yRef yFX)

flatBindEffect ctx s0 view nid tag xId bodyId =
  case Flat.flatSoaNode view bodyId of
    Flat.FX_Lift eId
      | Flat.FE_Var i <- Flat.flatSoaNode view eId
      , i == tag ->
          flatChild ctx s0 xId
    _ ->
      flatBindEffectKeep ctx s0 view nid tag xId bodyId

flatBindEffectKeep ctx s0 view nid _tag xId bodyId =
  let
    (s1, MkCode xDecl xRef xFX) = flatChild ctx s0 xId
    hint = Flat.flatSoaParamName view nid
    (nBind, s2) = flatPlanIdentHint ctx s1 nid hint
    (s3, MkCode yDecl yRef yFX) = flatChild ctx s2 bodyId
    stmtX
      | isNothing xRef = fromMaybe mempty xDecl
      | not xFX && isJust xDecl = fromMaybe mempty xDecl
      | otherwise = asStmt xDecl xRef
   in
    case xRef of
      Nothing ->
        (s3, MkCode (Just (stmtX $$ fromMaybe mempty yDecl)) yRef yFX)
      Just _ ->
        ( s3
        , MkCode
            ( Just
                ( fromMaybe mempty xDecl
                    $$ constBind s3 nBind (fromMaybe mempty xRef)
                    $$ fromMaybe mempty yDecl
                )
            )
            yRef
            yFX
        )

flatRenderResultCaseE ctx env s0 view nid resId tagE errId _tagO okId =
  if flatIsUnitEffect view errId && flatIsUnitEffect view okId
    then
      let
        (s3, obj, prelude) = flatResultPrelude ctx env s0 resId tagE
        (s4, MkCode eDecl eRef _) = flatChild ctx s3 errId
        (s5, MkCode oDecl oRef _) = flatChild ctx s4 okId
       in
        ( s5
        , Code
            (prelude $$ ifElseStmt (jsText obj <> ".ok") oDecl oRef eDecl eRef)
            mempty
        )
    else
      let
        (s3, obj, prelude) = flatResultPrelude ctx env s0 resId tagE
        (resultN, s4) = flatPlanIdent ctx s3 nid
        resultVar = identName s4 resultN
        (s5, MkCode eDecl eRef _) = flatChild ctx s4 errId
        (s6, MkCode oDecl oRef _) = flatChild ctx s5 okId
        stmt =
          prelude
            $$ letResult resultVar
            $$ ifElseStmt
              (jsText obj <> ".ok")
              (Just (fromMaybe mempty oDecl $$ assignResult resultVar oRef))
              Nothing
              (Just (fromMaybe mempty eDecl $$ assignResult resultVar eRef))
              Nothing
       in
        (s6, Code stmt (jsText resultVar))

flatRenderStringCaseE ctx s0 view nid scrutId ai defId =
  let
    arms = Flat.flatSoaStrCases view ai
    unit =
      all (flatIsUnitEffect view . snd) arms
        && flatIsUnitEffect view defId
    (s1, Code oDecl oRef) = flatChild ctx s0 scrutId
    (resultN, s2) =
      if unit then (0, s1) else flatPlanIdent ctx s1 nid
    resultVar = identName s2 resultN
    renderArm s e =
      let
        (s', MkCode mDecl mRef _) = flatChild ctx s e
        body =
          if unit
            then asStmt mDecl mRef
            else fromMaybe mempty mDecl $$ assignResult resultVar mRef
       in
        (s', body)
    (s3, caseJSs) =
      mapAccumL
        ( \s (k, e) ->
            let
              (s', body) = renderArm s e
              line =
                "case"
                  <+> (jsQuote k <> colon)
                  <+> blockBody (body <+> ("break" <> semi))
             in
              (s', line)
        )
        s2
        arms
    (s4, defBody) = renderArm s3 defId
    defJS = "default:" <+> blockBody defBody
    switchStmt = "switch" <+> parens oRef <+> blockBody (vcat (caseJSs ++ [defJS]))
    prelude =
      if unit then oDecl else oDecl $$ letResult resultVar
    ref = if unit then Nothing else Just (jsText resultVar)
   in
    (s4, MkCode (Just (prelude $$ switchStmt)) ref False)

type FlatCodeTable = V.Vector Code

newtype FlatTableRead = FlatTableRead (MV.IOVector Code)

flatTableLookup (FlatTableRead mv) i =
  unsafePerformIO (MV.read mv i)
{-# NOINLINE flatTableLookup #-}

data FlatEmitPlan = FlatEmitPlan
  { fepEnv :: !(V.Vector (Maybe Env))
  , fepBind :: !(V.Vector (Maybe Int))
  , fepLayers :: !(V.Vector (V.Vector Flat.NodeId))
  }

data FlatEmitCtx = FlatEmitCtx
  { fecTable :: FlatTableRead
  , fecPlan :: FlatEmitPlan
  }

flatPlanIdent :: FlatEmitCtx -> CG -> Flat.NodeId -> (Int, CG)
flatPlanIdent ctx s nid =
  case fepBind (fecPlan ctx) V.!? nid of
    Just (Just i) -> (i, s)
    _ -> allocIdent s

flatPlanEnv :: FlatEmitPlan -> Flat.NodeId -> Env
flatPlanEnv plan nid =
  case fepEnv plan V.!? nid of
    Just (Just env) -> env
    _ -> error ("JShark.flatPlanEnv: missing env for node " ++ show nid)

-- | Binders are pre-allocated in the plan; the hint is already baked in.
flatPlanIdentHint ctx s nid _hint = flatPlanIdent ctx s nid

-- | Emit a nested chain of lambdas as one n-ary function.
-- @step@ matches the pure ('Flat.FE_Lambda') or effect
-- ('Flat.FX_LambdaE') constructor.
flatEmitLambdaSpineWith step ctx env0 s0 view nid0 tag0 bodyId0 =
  withHintScope s0 $ \sScoped -> go sScoped env0 nid0 tag0 bodyId0 []
 where
  go s env nid tag bodyId acc =
    let
      hint = Flat.flatSoaParamName view nid
      (nParam, s1) = flatPlanIdentHint ctx s nid hint
      env' = IM.insert tag nParam env
     in
      case step (Flat.flatSoaNode view bodyId) of
        Just (tag2, body2)
          | isNothing (Flat.flatSoaHoistTag view bodyId) ->
              go s1 env' bodyId tag2 body2 (nParam : acc)
        _ ->
          let
            (s2, MkCode d r _) = flatChild ctx s1 bodyId
            ids = reverse (nParam : acc)
           in
            (s2, renderFn s2 (map (nJS s2) ids) d r)

flatEmitLambdaSpine ctx =
  flatEmitLambdaSpineWith
    (\case Flat.FE_Lambda t b -> Just (t, b); _ -> Nothing)
    ctx

flatEmitLambdaESpine ctx =
  flatEmitLambdaSpineWith
    (\case Flat.FX_LambdaE t b -> Just (t, b); _ -> Nothing)
    ctx

-- | Apply-spine length that matches a hoisted peeled lambda. Opaque
-- heads (params, @id@, FFI) stay curried: @f(a)(b)@, not @f(a, b)@.
flatCallArity view nid = case Flat.flatSoaNode view nid of
  Flat.FE_Lambda _ bodyId
    | isJust (Flat.flatSoaHoistTag view nid) ->
        1 + flatUntaggedLambdaChain view bodyId
  Flat.FX_LambdaE _ bodyId
    | isJust (Flat.flatSoaHoistTag view nid) ->
        1 + flatUntaggedLambdaEChain view bodyId
  _ -> 0

flatUntaggedLambdaChain view nid = case Flat.flatSoaNode view nid of
  Flat.FE_Lambda _ bodyId
    | isNothing (Flat.flatSoaHoistTag view nid) ->
        1 + flatUntaggedLambdaChain view bodyId
  _ -> 0

flatUntaggedLambdaEChain view nid = case Flat.flatSoaNode view nid of
  Flat.FX_LambdaE _ bodyId
    | isNothing (Flat.flatSoaHoistTag view nid) ->
        1 + flatUntaggedLambdaEChain view bodyId
  _ -> 0

flatCollectApply view fId argIds = case Flat.flatSoaNode view fId of
  Flat.FE_Apply f2 x2 -> flatCollectApply view f2 (x2 : argIds)
  _ -> (fId, argIds)

flatCollectApplyE view fId argIds = case Flat.flatSoaNode view fId of
  Flat.FX_ApplyE f2 x2 -> flatCollectApplyE view f2 (x2 : argIds)
  _ -> (fId, argIds)

-- | Emit an application spine. @wrap@ is 'Code' in expression position
-- and 'fxCode' in effect position; @collect@ picks the matching apply
-- constructor ('Flat.FE_Apply' vs 'Flat.FX_ApplyE').
flatEmitApplyWith wrap collect ctx s0 view fId argIds =
  let
    (headId, args) = collect view fId argIds
    n = length args
   in
    if n > 1 && flatCallArity view headId == n
      then
        let
          (s1, Code fDecl fRef) = flatChild ctx s0 headId
          (s2, argDecl, argRefs) = flatEmitApplyArgs ctx s1 args
         in
          (s2, wrap (fDecl $$ argDecl) (jsCallN fRef argRefs))
      else
        let
          xId = case argIds of
            (x : _) -> x
            [] -> error "JShark.flatEmitApply: missing argument"
          (s1, Code fDecl fRef) = flatChild ctx s0 fId
          (s2, Code xDecl xRef) = flatChild ctx s1 xId
         in
          (s2, wrap (fDecl $$ xDecl) (jsCall fRef xRef))

flatEmitApply ctx = flatEmitApplyWith Code flatCollectApply ctx

flatEmitApplyE ctx = flatEmitApplyWith fxCode flatCollectApplyE ctx

flatEmitApplyArgs ctx s0 xs =
  foldl'
    ( \(s, d, rs) xId ->
        let
          (s', Code xd xr) = flatChild ctx s xId
         in
          (s', d $$ xd, rs ++ [xr])
    )
    (s0, mempty, [])
    xs

-- | Emit a child node: a table lookup keyed by node id (layer order was
-- fixed by 'Flat.flatSoaLayerBuckets', so children are already emitted).
flatChild ctx s cId = (s, flatTableLookup (fecTable ctx) cId)

flatNodeKindEffect view nid =
  Flat.flatOpIsEffect (Flat.flatOpOf (Flat.fsaOpcodes view VU.! nid))

buildFlatEmitPlan ::
  Flat.FlatSoA -> Flat.NodeId -> CG -> (FlatEmitPlan, CG)
buildFlatEmitPlan view root s0 =
  let
    n = Flat.flatSoaNodeCount view
   in
    runST $ do
      envAt <- MV.replicate n Nothing
      bindAt <- MV.replicate n Nothing
      sRef <- newSTRef s0
      let
        writeEnv i e = MV.write envAt i (Just e)
        planAlloc i = do
          s <- readSTRef sRef
          let
            hint = Flat.flatSoaParamName view i
            (ident, s') = allocIdentHint s hint
          writeSTRef sRef s'
          MV.write bindAt i (Just ident)
          pure ident
        planInScope act = do
          modifySTRef sRef pushHintScope
          act
          modifySTRef sRef popHintScope
        -- @v <- e; pure v@: codegen flattens the bind into its RHS
        -- ('flatBindEffect'), so the binder never emits a name.
        isTransparentBind tag bodyId =
          case Flat.flatSoaNode view bodyId of
            Flat.FX_Lift eId ->
              case Flat.flatSoaNode view eId of
                Flat.FE_Var i -> i == tag
                _ -> False
            _ -> False
        planGo env nid
          | nid < 0 || nid >= n = pure ()
          | otherwise = do
              writeEnv nid env
              -- Method-callback bodies: allocate @k@ idents in a hint scope,
              -- bind @tags@ to the last @length tags@ of them (earlier slots
              -- are reserved but unbound, e.g. 'FE_MethFrom'’s array ident),
              -- and emit the body under that env.
              let
                scopedBody tags k bodyId =
                  planInScope $ do
                    s <- readSTRef sRef
                    let
                      (ids, s') = allocNIdents s k
                    writeSTRef sRef s'
                    let
                      bound = drop (k - length tags) ids
                      env' = foldr (uncurry IM.insert) env (zip tags bound)
                    writeEnv nid env'
                    planGo env' bodyId
                -- Both result-case shapes share one plan: the result ident is
                -- shared by the error and ok branches (err binds first).
                resultCase resId tagE errId tagO okId = do
                  planGo env resId
                  _ <- planAlloc nid
                  s <- readSTRef sRef
                  let
                    (identUnw, s') = allocIdent s
                  writeSTRef sRef s'
                  let
                    envE = IM.insert tagE identUnw env
                    envO = IM.insert tagO identUnw envE
                  writeEnv nid envO
                  planGo envE errId
                  planGo envO okId
              case Flat.flatSoaNode view nid of
                Flat.FE_Let tag xId bodyId -> do
                  planGo env xId
                  ident <- planAlloc nid
                  planGo (IM.insert tag ident env) bodyId
                Flat.FE_LetRec tag rId bId -> do
                  ident <- planAlloc nid
                  let
                    env' = IM.insert tag ident env
                  planGo env' rId
                  planGo env' bId
                Flat.FE_Lambda tag bodyId ->
                  planInScope $ do
                    ident <- planAlloc nid
                    planGo (IM.insert tag ident env) bodyId
                Flat.FE_OptionCase oId nId tag sId -> do
                  planGo env oId
                  ident <- planAlloc nid
                  let
                    env' = IM.insert tag ident env
                  planGo env nId
                  planGo env' sId
                Flat.FE_ResultCase resId tagE errId tagO okId ->
                  resultCase resId tagE errId tagO okId
                Flat.FE_FnLit tags names bodyId ->
                  planInScope $ do
                    s <- readSTRef sRef
                    let
                      (ids, s') = allocNIdentsHints s names
                    writeSTRef sRef s'
                    let
                      env' = foldr (\(tag, i) -> IM.insert tag i) env (zip tags ids)
                    writeEnv nid env'
                    planGo env' bodyId
                Flat.FE_MethMap arr tag bodyId ->
                  planGo env arr Prelude.>> scopedBody [tag] 1 bodyId
                Flat.FE_MethFilter arr tag bodyId ->
                  planGo env arr Prelude.>> scopedBody [tag] 1 bodyId
                Flat.FE_MethReduce arr z tagA tagB bodyId ->
                  planGo env arr
                    Prelude.>> planGo env z
                    Prelude.>> scopedBody [tagA, tagB] 2 bodyId
                Flat.FE_MethReduceRight arr z tagA tagB bodyId ->
                  planGo env arr
                    Prelude.>> planGo env z
                    Prelude.>> scopedBody [tagA, tagB] 2 bodyId
                Flat.FE_MethToSorted arr tagA tagB bodyId ->
                  planGo env arr Prelude.>> scopedBody [tagA, tagB] 2 bodyId
                Flat.FE_MethFrom lenId tag bodyId ->
                  planGo env lenId Prelude.>> scopedBody [tag] 2 bodyId
                Flat.FX_Bind tag xId bodyId -> do
                  planGo env xId
                  -- A bind whose body is just @Lift (Var tag)@ (@v <- e; pure v@)
                  -- is flattened by 'flatBindEffect' into its RHS and never
                  -- emits a name; do not reserve one for it.
                  if isTransparentBind tag bodyId
                    then planGo env bodyId
                    else do
                      ident <- planAlloc nid
                      planGo (IM.insert tag ident env) bodyId
                Flat.FX_BindRec tag rId bId -> do
                  ident <- planAlloc nid
                  let
                    env' = IM.insert tag ident env
                  planGo env' rId
                  planGo env' bId
                Flat.FX_LambdaE tag bodyId ->
                  planInScope $ do
                    ident <- planAlloc nid
                    planGo (IM.insert tag ident env) bodyId
                Flat.FX_ForRange startId endId tag bodyId -> do
                  planGo env startId
                  planGo env endId
                  ident <- planAlloc nid
                  planGo (IM.insert tag ident env) bodyId
                Flat.FX_OptionCaseE oId nId tag sId -> do
                  planGo env oId
                  ident <- planAlloc nid
                  let
                    env' = IM.insert tag ident env
                  planGo env nId
                  planGo env' sId
                Flat.FX_ResultCaseE resId tagE errId tagO okId ->
                  resultCase resId tagE errId tagO okId
                Flat.FX_Try aId tag kId -> do
                  planGo env aId
                  ident <- planAlloc nid
                  planGo (IM.insert tag ident env) kId
                Flat.FX_StringCaseE scrutId ai defId -> do
                  planGo env scrutId
                  _ <- planAlloc nid
                  mapM_
                    (planGo env . snd)
                    (Flat.flatSoaStrCases view ai)
                  planGo env defId
                node -> do
                  let
                    refs = Flat.flatSoaNodePackRefs view node
                  mapM_ (planGo env) refs
      planGo IM.empty root
      envF <- V.unsafeFreeze envAt
      bindF <- V.unsafeFreeze bindAt
      sFinal <- readSTRef sRef
      pure
        ( FlatEmitPlan
            { fepEnv = envF
            , fepBind = bindF
            , fepLayers = Flat.flatSoaLayerBuckets view root
            }
        , sFinal
        )

flatEmitLayered view root plan s0 =
  unsafePerformIO $ do
    let
      n = Flat.flatSoaNodeCount view
      emitOrder = concatMap V.toList (V.toList (fepLayers plan))
    tableMV <- MV.new n
    MV.set tableMV (Code mempty mempty)
    let
      tableRead = FlatTableRead tableMV
    sRef <- newIORef s0
    forM_ emitOrder $ \nid -> do
      s <- readIORef sRef
      let
        ctx = FlatEmitCtx {fecTable = tableRead, fecPlan = plan}
        env = flatPlanEnv plan nid
        (s', code) =
          if flatNodeKindEffect view nid
            then flatEffectfulASTGo ctx env s view nid
            else flatPureASTGo ctx env s view nid
      MV.write tableMV nid code
      writeIORef sRef s'
    sFinal <- readIORef sRef
    rootCode <- MV.read tableMV root
    pure (sFinal, rootCode)
{-# NOINLINE flatEmitLayered #-}

flatPureASTGo !ctx !env !sIn view nid =
  let
    s0 = sIn
   in
    case Flat.flatSoaNode view nid of
      Flat.FE_Literal li ->
        Flat.withFlatLitValue view li (flatRenderLiteral env s0)
      Flat.FE_Var i ->
        (s0, Code mempty (varStampJS s0 env (Name i)))
      Flat.FE_Let tag xId bodyId ->
        case Flat.flatSoaNode view bodyId of
          Flat.FE_Var i
            | i == tag ->
                flatChild ctx s0 xId
          _ ->
            let
              (nBind, s1) = flatPlanIdent ctx s0 nid
              (s2, MkCode xDecl xRef _) = flatChild ctx s1 xId
              (s3, yCode) = flatChild ctx s2 bodyId
             in
              ( s3
              , keepRef
                  ( fromMaybe mempty xDecl
                      $$ constBind s3 nBind (fromMaybe mempty xRef)
                      $$ fromMaybe mempty (codeDecl yCode)
                  )
                  yCode
              )
      Flat.FE_LetRec _tag rId bId ->
        let
          (nBind, s1) = flatPlanIdent ctx s0 nid
          n = nJS s1 nBind
          (s2, MkCode rDecl rRef _) = flatChild ctx s1 rId
          (s3, bCode) = flatChild ctx s2 bId
         in
          ( s3
          , keepRef (recBindStmt n rDecl rRef $$ fromMaybe mempty (codeDecl bCode)) bCode
          )
      Flat.FE_Lambda tag bodyId ->
        case Flat.flatSoaHoistTag view nid of
          Just _ ->
            let
              (s1, fnJs) = flatEmitLambdaSpine ctx env s0 view nid tag bodyId
              (s2, hoisted) = emitHoistedFnValue s1 view nid fnJs
             in
              (s2, Code mempty hoisted)
          Nothing ->
            withHintScope s0 $ \sScoped ->
              let
                hint = Flat.flatSoaParamName view nid
                (nParam, s1) = flatPlanIdentHint ctx sScoped nid hint
                (s2, MkCode d r _) = flatChild ctx s1 bodyId
               in
                (s2, Code mempty (renderFunction s2 nParam d r))
      Flat.FE_Apply fId xId ->
        flatEmitApply ctx s0 view fId [xId]
      Flat.FE_EmbedEff eId -> flatChild ctx s0 eId
      Flat.FE_If cId tId eId ->
        let
          (s1, MkCode cDecl cRef _) = flatChild ctx s0 cId
          (s2, MkCode tDecl tRef tFX) = flatChild ctx s1 tId
          (s3, MkCode eDecl eRef eFX) = flatChild ctx s2 eId
          cJs = fromMaybe mempty cRef
         in
          if isNothing tDecl && isNothing eDecl && not tFX && not eFX
            then
              ( s3
              , MkCode
                  cDecl
                  ( Just
                      ( parens
                          ( cJs
                              <+> "?"
                              <+> fromMaybe "undefined" tRef
                              <+> ":"
                              <+> fromMaybe "undefined" eRef
                          )
                      )
                  )
                  False
              )
            else
              -- Branch-local declarations must not run until their branch is
              -- selected: hoisting both would evaluate (and possibly throw
              -- from) the untaken side.
              let
                (n, s4) = allocIdent s3
                rv = identName s4 n
               in
                ( s4
                , MkCode
                    ( Just
                        ( fromMaybe mempty cDecl
                            $$ letResult rv
                            $$ ifAssignOrStmt (Just rv) cJs tDecl tRef eDecl eRef
                        )
                    )
                    (Just (jsText rv))
                    False
                )
      Flat.FE_OptionCase oId nId _tag sId ->
        let
          (s1, MkCode optDecl optRef _) = flatChild ctx s0 oId
          (nBind, s2) = flatPlanIdent ctx s1 nid
          optVar = identName s2 nBind
          (s3, MkCode noneDecl noneRef nFX) = flatChild ctx s2 nId
          (s4, MkCode someDecl someRef sFX) = flatChild ctx s3 sId
          optBind =
            constBind s4 nBind (fromMaybe mempty optRef)
          cond = jsText optVar <+> "===" <+> "null"
         in
          if isNothing noneDecl && isNothing someDecl && not nFX && not sFX
            then
              ( s4
              , MkCode
                  (Just (fromMaybe mempty optDecl $$ optBind))
                  ( Just
                      ( parens
                          ( cond
                              <+> "?"
                              <+> fromMaybe "undefined" noneRef
                              <+> ":"
                              <+> fromMaybe "undefined" someRef
                          )
                      )
                  )
                  False
              )
            else
              -- Branch-local declarations must stay inside their branch.
              let
                (n, s5) = allocIdent s4
                rv = identName s5 n
               in
                ( s5
                , MkCode
                    ( Just
                        ( fromMaybe mempty optDecl
                            $$ optBind
                            $$ letResult rv
                            $$ ifAssignOrStmt (Just rv) cond noneDecl noneRef someDecl someRef
                        )
                    )
                    (Just (jsText rv))
                    False
                )
      Flat.FE_ResultOk xId ->
        let
          (s1, MkCode d r _) = flatChild ctx s0 xId
         in
          (s1, MkCode d (Just (resultObject True r)) False)
      Flat.FE_ResultErr xId ->
        let
          (s1, MkCode d r _) = flatChild ctx s0 xId
         in
          (s1, MkCode d (Just (resultObject False r)) False)
      Flat.FE_ResultCase resId tagE errId tagO okId ->
        flatRenderResultCase ctx env s0 resId tagE errId tagO okId
      Flat.FE_Index arrId idxId ->
        let
          (s1, Code aDecl aRef) = flatChild ctx s0 arrId
          (s2, Code iDecl iRef) = flatChild ctx s1 idxId
          (s3, call) = emitCheckedIndex s2 aRef iRef
         in
          (s3, Code (aDecl $$ iDecl) call)
      Flat.FE_U8Index bufId idxId ->
        let
          (s1, Code bDecl bRef) = flatChild ctx s0 bufId
          (s2, Code iDecl iRef) = flatChild ctx s1 idxId
         in
          (s2, Code (bDecl $$ iDecl) (bRef <> brackets iRef))
      Flat.FE_Error msgId ->
        let
          (s1, Code d r) = flatChild ctx s0 msgId
         in
          (s1, Code d ("(function(){throw new Error(" <> r <> ");}())"))
      Flat.FE_Fixed fixed -> flatRenderFixed ctx s0 view fixed
      Flat.FE_FnLit tags _names bodyId ->
        flatRenderFnLit ctx env s0 tags bodyId
      Flat.FE_UnsafeNullable xId -> flatChild ctx s0 xId
      Flat.FE_FrozenLit gi -> flatRenderObjectLit ctx s0 view gi
      Flat.FE_GetField ti oId ->
        let
          (s1, Code d r) = flatChild ctx s0 oId
         in
          (s1, Code d (jsDotOrBracket r (Flat.flatSoaText view ti)))
      knode ->
        case knode of
          Flat.FE_MethMap {} -> flatRenderMethod ctx env s0 view knode
          Flat.FE_MethFilter {} -> flatRenderMethod ctx env s0 view knode
          Flat.FE_MethReduce {} -> flatRenderMethod ctx env s0 view knode
          Flat.FE_MethReduceRight {} -> flatRenderMethod ctx env s0 view knode
          Flat.FE_MethToSorted {} -> flatRenderMethod ctx env s0 view knode
          Flat.FE_MethFrom {} -> flatRenderMethod ctx env s0 view knode
          _ -> flatRenderKernel ctx s0 view knode

flatEffectfulASTGo !ctx !env !sIn view nid =
  let
    s0 = sIn
   in
    case Flat.flatSoaNode view nid of
      Flat.FX_Lift eId -> flatChild ctx s0 eId
      Flat.FX_FFI fi ai ->
        let
          (s1, argDecl, argRefs) = flatRenderArgList ctx s0 view ai
         in
          ( s1
          , fxCode
              argDecl
              (renderFFIInvoke (Flat.flatSoaFFI view fi) argRefs)
          )
      Flat.FX_UnsafeObject ti ->
        (s0, Code mempty (jsText (Flat.flatSoaText view ti)))
      Flat.FX_UnsafeObjectGet xId sId ->
        let
          (s1, Code xDecl xRef) = flatChild ctx s0 xId
         in
          (s1, Code xDecl $ jsDotOrBracket xRef (Flat.flatSoaText view sId))
      Flat.FX_UnsafeObjectAssign xId yId ->
        let
          (s1, Code xDecl xRef) = flatChild ctx s0 xId
          (s2, Code yDecl yRef) = flatChild ctx s1 yId
         in
          (s2, fxCode (xDecl $$ yDecl) $ xRef <> " = " <> yRef)
      Flat.FX_CallMethod recvId methodIdx ai ->
        let
          method = Flat.flatSoaText view methodIdx
          (s1, Code rDecl rRef) = flatChild ctx s0 recvId
          (s2, argDecl, argRefs) = flatRenderArgList ctx s1 view ai
         in
          ( s2
          , fxCode
              (rDecl $$ argDecl)
              (rRef <> "." <> jsText method <> parens argRefs)
          )
      Flat.FX_Bind tag xId bodyId ->
        flatBindEffect ctx s0 view nid tag xId bodyId
      Flat.FX_ThenE xId yId -> flatSeqEffect ctx s0 xId yId
      Flat.FX_BindRec _tag rId bId ->
        let
          (nBind, s1) = flatPlanIdent ctx s0 nid
          n = nJS s1 nBind
          (s2, MkCode rDecl rRef _) = flatChild ctx s1 rId
          (s3, MkCode bDecl bRef bFX) = flatChild ctx s2 bId
         in
          ( s3
          , MkCode (Just (recBindStmt n rDecl rRef $$ fromMaybe mempty bDecl)) bRef bFX
          )
      Flat.FX_LambdaE tag bodyId ->
        case Flat.flatSoaHoistTag view nid of
          Just _ ->
            let
              (s1, fnJs) = flatEmitLambdaESpine ctx env s0 view nid tag bodyId
              (s2, hoisted) = emitHoistedFnValue s1 view nid fnJs
             in
              (s2, Code mempty hoisted)
          Nothing ->
            withHintScope s0 $ \sScoped ->
              let
                hint = Flat.flatSoaParamName view nid
                (nParam, s1) = flatPlanIdentHint ctx sScoped nid hint
                (s2, MkCode exprXDecl exprXRef _) =
                  flatChild ctx s1 bodyId
                (s3, fnJs) =
                  emitHoistedFnValue
                    s2
                    view
                    nid
                    (renderFunction s2 nParam exprXDecl exprXRef)
               in
                (s3, Code mempty fnJs)
      Flat.FX_ApplyE fId xId ->
        flatEmitApplyE ctx s0 view fId [xId]
      Flat.FX_IfE cId tId eId ->
        let
          unit =
            flatIsUnitEffect view tId && flatIsUnitEffect view eId
          (s1, MkCode cDecl cRef _) = flatChild ctx s0 cId
          (s2, MkCode tDecl tRef tFX) = flatChild ctx s1 tId
          (s3, MkCode eDecl eRef eFX) = flatChild ctx s2 eId
          cJs = fromMaybe mempty cRef
         in
          if unit
            then
              ( s3
              , MkCode
                  (Just (fromMaybe mempty cDecl $$ ifElseStmt cJs tDecl tRef eDecl eRef))
                  Nothing
                  False
              )
            else
              if isNothing tDecl && isNothing eDecl && not tFX && not eFX
                then
                  ( s3
                  , Code
                      (fromMaybe mempty cDecl)
                      ( parens
                          ( cJs
                              <+> "?"
                              <+> fromMaybe "undefined" tRef
                              <+> ":"
                              <+> fromMaybe "undefined" eRef
                          )
                      )
                  )
                else
                  let
                    (n, s4) = allocIdent s3
                    rv = identName s4 n
                   in
                    ( s4
                    , MkCode
                        ( Just
                            ( fromMaybe mempty cDecl
                                $$ letResult rv
                                $$ ifAssignOrStmt (Just rv) cJs tDecl tRef eDecl eRef
                            )
                        )
                        (Just (jsText rv))
                        False
                    )
      Flat.FX_While cId bId ->
        let
          (s1, MkCode condDecl condRef _) = flatChild ctx s0 cId
          (s2, MkCode bodyDecl bodyRef _) = flatChild ctx s1 bId
          bodyStmt = asStmt bodyDecl bodyRef
          whileStmt =
            "while"
              <+> parens (fromMaybe mempty condRef)
              <+> blockBody bodyStmt
         in
          (s2, MkCode (Just (fromMaybe mempty condDecl $$ whileStmt)) Nothing False)
      Flat.FX_ForRange startId endId _tag bodyId ->
        let
          (s1, MkCode startDecl startRef _) = flatChild ctx s0 startId
          (s2, MkCode endDecl endRef _) = flatChild ctx s1 endId
          (loopN, s3) = flatPlanIdentHint ctx s2 nid (Just "i")
          loopVar = nJS s3 loopN
          (s4, MkCode bodyDecl bodyRef _) = flatChild ctx s3 bodyId
          bodyStmt = asStmt bodyDecl bodyRef
          forInit =
            "let" <+> loopVar <+> "=" <+> fromMaybe mempty startRef
          forCond = loopVar <+> "<" <+> fromMaybe mempty endRef
          forStep = loopVar <> "++"
          forHead =
            hcat [forInit, ";", " ", forCond, ";", " ", forStep]
          forStmt = "for" <+> parens forHead <+> blockBody bodyStmt
         in
          ( s4
          , MkCode
              ( Just
                  ( fromMaybe mempty startDecl
                      $$ fromMaybe mempty endDecl
                      $$ forStmt
                  )
              )
              Nothing
              False
          )
      Flat.FX_U8Set bufId idxId valId ->
        let
          (s1, Code bDecl bRef) = flatChild ctx s0 bufId
          (s2, Code iDecl iRef) = flatChild ctx s1 idxId
          (s3, Code vDecl vRef) = flatChild ctx s2 valId
          stmt = (bRef <> brackets iRef) <+> "=" <+> vRef
         in
          (s3, Code (bDecl $$ iDecl $$ vDecl $$ (stmt <> semi)) mempty)
      Flat.FX_U8Fill bufId valId ->
        let
          (s1, Code bDecl bRef) = flatChild ctx s0 bufId
          (s2, Code vDecl vRef) = flatChild ctx s1 valId
          stmt = bRef <> ".fill" <> parens vRef
         in
          (s2, Code (bDecl $$ vDecl $$ (stmt <> semi)) mempty)
      Flat.FX_OptionCaseE oId nId _tag sId ->
        emitBranching
          ( flatIsUnitEffect view nId
              && flatIsUnitEffect view sId
          )
          s0
          ( \s ->
              let
                (s1, Code oDecl oRef) = flatChild ctx s oId
                (nBind, s2) = flatPlanIdent ctx s1 nid
               in
                (s2, oDecl $$ constBind s2 nBind oRef, nBind)
          )
          ( \mRes nBind s ->
              let
                (s1, MkCode nDecl nRef _) = flatChild ctx s nId
                (s2, MkCode sDecl sRef _) = flatChild ctx s1 sId
                cond = nJS s nBind <+> "===" <+> "null"
               in
                (s2, ifAssignOrStmt mRes cond nDecl nRef sDecl sRef)
          )
      Flat.FX_ResultCaseE resId tagE errId tagO okId ->
        flatRenderResultCaseE ctx env s0 view nid resId tagE errId tagO okId
      Flat.FX_StringCaseE scrutId ai defId ->
        flatRenderStringCaseE ctx s0 view nid scrutId ai defId
      Flat.FX_Throw xId ->
        let
          (s1, Code xDecl xRef) = flatChild ctx s0 xId
         in
          (s1, Code (xDecl $$ (("throw" <+> xRef) <> semi)) mempty)
      Flat.FX_Try aId _tag kId ->
        emitBranching
          (flatIsUnitEffect view aId && flatIsUnitEffect view kId)
          s0
          (\s -> (s, mempty, ()))
          ( \mRes () s ->
              let
                (s1, MkCode aDecl aRef _) = flatChild ctx s aId
                (catchN, s2) = flatPlanIdent ctx s1 nid
                (s3, MkCode bDecl bRef _) = flatChild ctx s2 kId
               in
                (s3, tryCatchStmt mRes (nJS s3 catchN) aDecl aRef bDecl bRef)
          )
      Flat.FX_ObjectLit gi -> flatRenderObjectLit ctx s0 view gi
      Flat.FX_DeleteProp oId kId ->
        let
          (s1, Code oDecl oRef) = flatChild ctx s0 oId
          (s2, Code kDecl kRef) = flatChild ctx s1 kId
         in
          (s2, fxCode (oDecl $$ kDecl) (("delete" <+> oRef) <> brackets kRef))
      Flat.FX_ArrayLit es -> flatRenderArrayLit ctx s0 es
      _ -> error "JShark.flatEffectfulAST': unexpected node"

flatEffectfulCodegenFromView soa =
  flatEffectfulCodegenFromViewWith startCG soa

flatEffectfulCodegenFromViewWith sStart soa =
  let
    root = Flat.fsaRoot soa
    total = Flat.flatSoaNodeCount soa
   in
    if root < 0 || root >= total
      then error "JShark.flatEffectfulCodegen: invalid root node"
      else
        let
          (plan, s1) = buildFlatEmitPlan soa root sStart
         in
          flatEmitLayered soa root plan s1
{-# NOINLINE flatEffectfulCodegenFromViewWith #-}

flatEffectfulCodegen ::
  ClosedEffect u -> (CG, Code)
flatEffectfulCodegen = flatEffectfulCodegenWith minifiedStyle

flatEffectfulCodegenWith ::
  EmitStyle -> ClosedEffect u -> (CG, Code)
flatEffectfulCodegenWith style (e :: ClosedEffect u) =
  let
    !(soa, s0) =
      unsafePerformIO
        (prepareFlatProgramWith style (lowerOptEffectIrWith (esKeepLets style) e))
   in
    flatEffectfulCodegenFromViewWith s0 soa
{-# NOINLINE flatEffectfulCodegenWith #-}

-- | Pure expression through the same flat pipeline (lower -> IR opt ->
-- pack -> SoA opt -> emit), so pure and effectful programs share one
-- optimizer and one emitter.
flatPureCodegenWith ::
  EmitStyle -> ClosedExpr u -> (CG, Code)
flatPureCodegenWith style (e :: ClosedExpr u) =
  let
    !(soa, s0) =
      unsafePerformIO
        (prepareFlatProgramWith style (lowerOptExprIr (esKeepLets style) e))
   in
    flatEffectfulCodegenFromViewWith s0 soa
{-# NOINLINE flatPureCodegenWith #-}

flatPureCodegen :: ClosedExpr u -> (CG, Code)
flatPureCodegen = flatPureCodegenWith minifiedStyle

effectfulASTFromSoA :: Flat.FlatSoA -> JS
effectfulASTFromSoA soa =
  uncurry renderWithPreamble (flatEffectfulCodegenFromView soa)

effectfulAST :: ClosedEffect u -> JS
effectfulAST = effectfulASTWith idiomaticStyle

effectfulASTWith :: EmitStyle -> ClosedEffect u -> JS
effectfulASTWith style e =
  uncurry renderWithPreamble (flatEffectfulCodegenWith style e)

pureAST :: ClosedExpr u -> JS
pureAST = pureASTWith idiomaticStyle

pureASTWith :: EmitStyle -> ClosedExpr u -> JS
pureASTWith style e =
  uncurry renderWithPreamble (flatPureCodegenWith style e)
