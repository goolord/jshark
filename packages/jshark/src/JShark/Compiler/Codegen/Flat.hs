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
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier -Wno-missing-export-lists -Wno-missing-signatures -Wno-type-defaults -Wno-incomplete-patterns #-}

-- | Flat IR → JavaScript (effectful compile path).
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
import GHC.IO.Unsafe (unsafePerformIO)
import qualified JShark.Api.Prim as Prim
import JShark.Api.Types
import JShark.Compiler.Binder
  ( pattern Name
  )
import JShark.Compiler.Codegen.Core
import JShark.Compiler.Codegen.Phoas
  ( asStmt
  , assignResult
  , emitBranching
  , hvm2ExportRef
  , ifAssignOrStmt
  , ifElseStmt
  , letResult
  , recBindStmt
  , renderFFIInvoke
  , renderResultLit
  , resultObject
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
import qualified JShark.Compiler.FlatSoA as FlatSoA
import qualified JShark.Compiler.FlatView as FlatView
import JShark.Compiler.Hoist (emitHoistedFnValue)

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
  ValueResult (Right x) -> renderResultLit True s0 x
  ValueResult (Left x) -> renderResultLit False s0 x
  ValueRegex s ->
    (s0, Code mempty ("new RegExp" <> parens (jsQuote s)))
  ValueUint8Array ba -> (s0, Code mempty (jsUint8ArrayLit ba))
  ValueBool True -> (s0, Code mempty "true")
  ValueBool False -> (s0, Code mempty "false")
  ValueFrozen {} -> error "JShark.flatPureAST: ValueFrozen is eval-only"

flatIsUnitExpr view nid = case FlatView.firNode view nid of
  Flat.FE_Literal li ->
    FlatView.withLitValue view li $ \case
      ValueUnit -> True
      _ -> False
  Flat.FE_Var {} -> False
  _ -> False

flatIsUnitEffect view nid = case FlatView.firNode view nid of
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
    all (flatIsUnitEffect view . snd) (FlatView.firStrCases view ai)
      && flatIsUnitEffect view d
  Flat.FX_Try a _ k -> flatIsUnitEffect view a && flatIsUnitEffect view k
  _ -> False

flatIsSimpleEffectNode view nid = case FlatView.firNode view nid of
  Flat.FX_Lift eid -> flatIsSimpleNode view eid
  Flat.FX_FFI {} -> True
  Flat.FX_CallMethod {} -> True
  Flat.FX_UnsafeObject {} -> True
  Flat.FX_UnsafeObjectGet {} -> True
  Flat.FX_ArrayLit es -> all (flatIsSimpleEffectNode view) es
  _ -> False

flatIsSimpleNode view nid = case FlatView.firNode view nid of
  Flat.FE_Literal _ -> True
  Flat.FE_Var _ -> True
  Flat.FE_EmbedEff eid -> flatIsSimpleEffectNode view eid
  Flat.FE_KShow _ -> True
  Flat.FE_KTypeOf _ -> True
  Flat.FE_KNegate _ -> True
  Flat.FE_KBigNeg _ -> True
  Flat.FE_KConcat {} -> False
  Flat.FE_KPlus {} -> False
  Flat.FE_KTimes {} -> False
  Flat.FE_KMinus {} -> False
  Flat.FE_KFracDiv {} -> False
  Flat.FE_KRem {} -> False
  Flat.FE_KBitAnd {} -> False
  Flat.FE_KBitOr {} -> False
  Flat.FE_KBitXor {} -> False
  Flat.FE_KShl {} -> False
  Flat.FE_KShr {} -> False
  Flat.FE_KUShr {} -> False
  Flat.FE_KBig {} -> False
  Flat.FE_KAnd {} -> False
  Flat.FE_KOr {} -> False
  Flat.FE_KEq {} -> False
  Flat.FE_KNEq {} -> False
  Flat.FE_KGTh {} -> False
  Flat.FE_KLTh {} -> False
  Flat.FE_KGTEq {} -> False
  Flat.FE_KLTEq {} -> False
  Flat.FE_Fixed {} -> True
  Flat.FE_MethMap {} -> False
  Flat.FE_MethFilter {} -> False
  Flat.FE_MethReduce {} -> False
  Flat.FE_MethReduceRight {} -> False
  Flat.FE_MethToSorted {} -> False
  Flat.FE_MethFrom {} -> False
  Flat.FE_FnLit {} -> True
  Flat.FE_Index {} -> True
  Flat.FE_U8Index {} -> True
  Flat.FE_Error {} -> False
  Flat.FE_UnsafeNullable x -> flatIsSimpleNode view x
  Flat.FE_FrozenLit {} -> True
  Flat.FE_Hvm2Ref {} -> True
  Flat.FE_GetField {} -> True
  _ -> False

flatWrapOperand view nid d =
  if flatIsSimpleNode view nid then d else parens d

flatRenderBin ctx op s0 view xId yId =
  let
    (s1, Code xDecl xRef) = flatPureChild ctx s0 xId
    (s2, Code yDecl yRef) = flatPureChild ctx s1 yId
   in
    ( s2
    , Code
        (xDecl $$ yDecl)
        ( flatWrapOperand view xId xRef
            <+> jsText op
            <+> flatWrapOperand view yId yRef
        )
    )

emitFlatSiblings emit s0 nids =
  mapAccumL
    ( \st nid ->
        let
          (st', code) = emit st nid
         in
          (st', code)
    )
    s0
    nids

flatRenderArgListSeq ctx s0 args =
  let
    go s = \case
      [] -> (s, [])
      Flat.FlatArgExpr eid : rest ->
        let
          (s', c) = flatPureChild ctx s eid
          (s'', cs') = go s' rest
         in
          (s'', c : cs')
      Flat.FlatArgEffect eid : rest ->
        let
          (s', c) = flatEffectChild ctx s eid
          (s'', cs') = go s' rest
         in
          (s'', c : cs')
    (s1, cs) = go s0 args
   in
    (s1, codesDecls cs, hcat (punctuate ", " (codesRefs cs)))

flatRenderArgList ctx s0 view ai =
  flatRenderArgListSeq ctx s0 (FlatView.firArgGroup view ai)

flatRenderField ctx s = \case
  Flat.FlatField k eid ->
    let
      (s', Code d r) = flatPureChild ctx s eid
     in
      (s', (d, (jsPropKey (cgStyle s') k <> ":") <+> r))
  Flat.FlatFieldExtra k eid ->
    let
      (s', Code d r) = flatPureChild ctx s eid
     in
      (s', (d, (jsPropKey (cgStyle s') k <> ":") <+> r))
  Flat.FlatFieldEff k eid ->
    let
      (s', MkCode d r _) = flatEffectChild ctx s eid
     in
      ( s'
      ,
        ( fromMaybe mempty d
        , (jsPropKey (cgStyle s') k <> ":") <+> fromMaybe mempty r
        )
      )
  Flat.FlatFieldExtraEff k eid ->
    let
      (s', MkCode d r _) = flatEffectChild ctx s eid
     in
      ( s'
      ,
        ( fromMaybe mempty d
        , (jsPropKey (cgStyle s') k <> ":") <+> fromMaybe mempty r
        )
      )

flatRenderObjectLit ctx s0 view gi =
  let
    fs = FlatView.firFieldGroup view gi
    (s1, parts) = mapAccumL (flatRenderField ctx) s0 fs
    (declList, pairs) = unzip parts
   in
    (s1, Code (vcatNonEmpty declList) (braces (hcat (punctuate ", " pairs))))

flatRenderArrayLit ctx s0 es =
  let
    (s1, cs) = emitFlatSiblings (flatEffectChild ctx) s0 es
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
          (s1, Code xDecl xRef) = flatPureChild ctx s0 xId
         in
          (s1, Code xDecl ("Math." <> jsText name <> parens xRef))
  Flat.FlatFixedB op xId yId
    | Just name <- Prim.math2Name op ->
        let
          (s1, Code xDecl xRef) = flatPureChild ctx s0 xId
          (s2, Code yDecl yRef) = flatPureChild ctx s1 yId
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
      (s1, Code rDecl rRef) = flatPureChild ctx s0 xId
     in
      (s1, Code rDecl (Prim.fixedUnaryJS op (flatWrapOperand view xId rRef)))
  Flat.FlatFixedB op xId yId ->
    let
      (s1, Code rDecl rRef) = flatPureChild ctx s0 xId
      (s2, Code aDecl aRef) = flatPureChild ctx s1 yId
     in
      ( s2
      , Code
          (rDecl $$ aDecl)
          (Prim.fixedBinaryJS op (flatWrapOperand view xId rRef) aRef)
      )
  Flat.FlatFixedT op xId yId zId ->
    let
      (s1, Code rDecl rRef) = flatPureChild ctx s0 xId
      (s2, Code aDecl aRef) = flatPureChild ctx s1 yId
      (s3, Code bDecl bRef) = flatPureChild ctx s2 zId
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
      (s1, Code xDecl xRef) = flatPureChild ctx s0 x
     in
      (s1, Code xDecl $ "-" <> parens xRef)
  Flat.FE_KShow x ->
    let
      (s1, Code xDecl xRef) = flatPureChild ctx s0 x
     in
      (s1, Code xDecl $ "String" <> parens xRef)
  Flat.FE_KTypeOf x ->
    let
      (s1, Code xDecl xRef) = flatPureChild ctx s0 x
     in
      (s1, Code xDecl $ "typeof" <+> xRef)
  Flat.FE_KNegate x ->
    let
      (s1, Code xDecl xRef) = flatPureChild ctx s0 x
     in
      (s1, Code xDecl $ "-" <> parens xRef)
  Flat.FE_KAnd x y -> flatRenderBin ctx "&&" s0 view x y
  Flat.FE_KOr x y -> flatRenderBin ctx "||" s0 view x y
  Flat.FE_KEq structural x y
    | structural ->
        let
          (s1, Code xDecl xRef) = flatPureChild ctx s0 x
          (s2, Code yDecl yRef) = flatPureChild ctx s1 y
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
          (s1, Code xDecl xRef) = flatPureChild ctx s0 x
          (s2, Code yDecl yRef) = flatPureChild ctx s1 y
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
    (s1, Code rDecl rRef) = flatPureChild ctx s0 arrId
    (s2, Code exDecl exRef) = flatPureChild ctx s1 bodyId
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
    (s1, Code rDecl rRef) = flatPureChild ctx s0 arrId
    (s2, Code zDecl zRef) = flatPureChild ctx s1 zId
    (s3, Code exDecl exRef) = flatPureChild ctx s2 bodyId
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
      (s1, Code rDecl rRef) = flatPureChild ctx s0 arr
      nA = flatEnvTag env tagA
      nB = flatEnvTag env tagB
      (s2, Code exDecl exRef) = flatPureChild ctx s1 body
      cb = jsCallback s2 [nJS s2 nA, nJS s2 nB] exDecl exRef
     in
      ( s2
      , Code rDecl (flatWrapOperand view arr rRef <> ".toSorted" <> parens cb)
      )
  Flat.FE_MethFrom n tag body ->
    let
      (s1, Code nDecl nRef) = flatPureChild ctx s0 n
      nI = flatEnvTag env tag
      (s2, Code exDecl exRef) = flatPureChild ctx s1 body
      cb = jsCallback s2 [jsText "_", nJS s2 nI] exDecl exRef
     in
      (s2, Code nDecl ("Array.from({length: " <> nRef <> "}, " <> cb <> ")"))
  _ -> error "JShark.flatRenderMethod: unexpected node"

flatRenderFnLit ctx env s0 tags bodyId =
  let
    ids = map (flatEnvTag env) tags
    (s1, Code d r) = flatPureChild ctx s0 bodyId
   in
    (s1, Code mempty (jsCallback s1 (map (nJS s1) ids) d r))

flatResultUnwrapIdent env s tag = (flatEnvTag env tag, s)

flatRenderResultCase ctx env s0 resId tagE errId _tagO okId =
  let
    (s1, MkCode rDecl rRef _) = flatPureChild ctx s0 resId
    (nObj, s2) = allocIdent s1
    (nUnw, s3) = flatResultUnwrapIdent env s2 tagE
    obj = identName s3 nObj
    prelude =
      fromMaybe mempty rDecl
        $$ constBind s3 nObj (fromMaybe mempty rRef)
        $$ constBind s3 nUnw (jsText obj <> ".value")
    (s4, Code eDecl eRef) = flatPureChild ctx s3 errId
    (s5, Code oDecl oRef) = flatPureChild ctx s4 okId
   in
    ( s5
    , Code
        (prelude $$ eDecl $$ oDecl)
        (parens ((jsText obj <> ".ok") <+> "?" <+> oRef <+> ":" <+> eRef))
    )

flatSeqEffect ctx s0 xId yId =
  let
    (s1, MkCode xDecl xRef xFX) = flatEffectChild ctx s0 xId
    (s2, MkCode yDecl yRef yFX) = flatEffectChild ctx s1 yId
    stmt
      | isNothing xRef = fromMaybe mempty xDecl
      | not xFX && isJust xDecl = fromMaybe mempty xDecl
      | otherwise = asStmt xDecl xRef
   in
    (s2, MkCode (Just (stmt $$ fromMaybe mempty yDecl)) yRef yFX)

flatBindEffect ctx s0 view nid tag xId bodyId =
  case FlatView.firNode view bodyId of
    Flat.FX_Lift eId
      | Flat.FE_Var i <- FlatView.firNode view eId
      , i == tag ->
          flatEffectChild ctx s0 xId
    _ ->
      flatBindEffectKeep ctx s0 view nid tag xId bodyId

flatBindEffectKeep ctx s0 view nid _tag xId bodyId =
  let
    (s1, MkCode xDecl xRef xFX) = flatEffectChild ctx s0 xId
    hint = FlatView.firParamName view nid
    (nBind, s2) = flatPlanIdentHint ctx s1 nid hint
    (s3, MkCode yDecl yRef yFX) = flatEffectChild ctx s2 bodyId
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
        (s1, MkCode rDecl rRef _) = flatPureChild ctx s0 resId
        (nObj, s2) = allocIdent s1
        (nUnw, s3) = flatResultUnwrapIdent env s2 tagE
        obj = identName s3 nObj
        prelude =
          fromMaybe mempty rDecl
            $$ constBind s3 nObj (fromMaybe mempty rRef)
            $$ constBind s3 nUnw (jsText obj <> ".value")
        (s4, MkCode eDecl eRef _) = flatEffectChild ctx s3 errId
        (s5, MkCode oDecl oRef _) = flatEffectChild ctx s4 okId
       in
        ( s5
        , Code
            (prelude $$ ifElseStmt (jsText obj <> ".ok") oDecl oRef eDecl eRef)
            mempty
        )
    else
      let
        (s1, MkCode rDecl rRef _) = flatPureChild ctx s0 resId
        (nObj, s2) = allocIdent s1
        (nUnw, s3) = flatResultUnwrapIdent env s2 tagE
        obj = identName s3 nObj
        prelude =
          fromMaybe mempty rDecl
            $$ constBind s3 nObj (fromMaybe mempty rRef)
            $$ constBind s3 nUnw (jsText obj <> ".value")
        (resultN, s4) = flatPlanIdent ctx s3 nid
        resultVar = identName s4 resultN
        (s5, MkCode eDecl eRef _) = flatEffectChild ctx s4 errId
        (s6, MkCode oDecl oRef _) = flatEffectChild ctx s5 okId
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
    arms = FlatView.firStrCases view ai
    unit =
      all (flatIsUnitEffect view . snd) arms
        && flatIsUnitEffect view defId
    (s1, Code oDecl oRef) = flatPureChild ctx s0 scrutId
    (resultN, s2) =
      if unit then (0, s1) else flatPlanIdent ctx s1 nid
    resultVar = identName s2 resultN
    renderArm s e =
      let
        (s', MkCode mDecl mRef _) = flatEffectChild ctx s e
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
  , fepReach :: !(V.Vector Bool)
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

flatEmitLambdaSpine ctx env0 s0 view nid0 tag0 bodyId0 =
  withHintScope s0 $ \sScoped -> go sScoped env0 nid0 tag0 bodyId0 []
 where
  go s env nid tag bodyId acc =
    let
      hint = FlatView.firParamName view nid
      (nParam, s1) = flatPlanIdentHint ctx s nid hint
      env' = IM.insert tag nParam env
     in
      case FlatView.firNode view bodyId of
        Flat.FE_Lambda tag2 body2
          | isNothing (FlatView.firHoistTag view bodyId) ->
              go s1 env' bodyId tag2 body2 (nParam : acc)
        _ ->
          let
            (s2, MkCode d r _) = flatPureChild ctx s1 bodyId
            ids = reverse (nParam : acc)
           in
            (s2, renderFn s2 (map (nJS s2) ids) d r)

flatEmitLambdaESpine ctx env0 s0 view nid0 tag0 bodyId0 =
  withHintScope s0 $ \sScoped -> go sScoped env0 nid0 tag0 bodyId0 []
 where
  go s env nid tag bodyId acc =
    let
      hint = FlatView.firParamName view nid
      (nParam, s1) = flatPlanIdentHint ctx s nid hint
      env' = IM.insert tag nParam env
     in
      case FlatView.firNode view bodyId of
        Flat.FX_LambdaE tag2 body2
          | isNothing (FlatView.firHoistTag view bodyId) ->
              go s1 env' bodyId tag2 body2 (nParam : acc)
        _ ->
          let
            (s2, MkCode d r _) = flatEffectChild ctx s1 bodyId
            ids = reverse (nParam : acc)
           in
            (s2, renderFn s2 (map (nJS s2) ids) d r)

-- | Apply-spine length that matches a hoisted peeled lambda. Opaque
-- heads (params, @id@, FFI) stay curried: @f(a)(b)@, not @f(a, b)@.
flatCallArity view nid = case FlatView.firNode view nid of
  Flat.FE_Lambda _ bodyId
    | isJust (FlatView.firHoistTag view nid) ->
        1 + flatUntaggedLambdaChain view bodyId
  Flat.FX_LambdaE _ bodyId
    | isJust (FlatView.firHoistTag view nid) ->
        1 + flatUntaggedLambdaEChain view bodyId
  _ -> 0

flatUntaggedLambdaChain view nid = case FlatView.firNode view nid of
  Flat.FE_Lambda _ bodyId
    | isNothing (FlatView.firHoistTag view nid) ->
        1 + flatUntaggedLambdaChain view bodyId
  _ -> 0

flatUntaggedLambdaEChain view nid = case FlatView.firNode view nid of
  Flat.FX_LambdaE _ bodyId
    | isNothing (FlatView.firHoistTag view nid) ->
        1 + flatUntaggedLambdaEChain view bodyId
  _ -> 0

flatCollectApply view fId argIds = case FlatView.firNode view fId of
  Flat.FE_Apply f2 x2 -> flatCollectApply view f2 (x2 : argIds)
  _ -> (fId, argIds)

flatCollectApplyE view fId argIds = case FlatView.firNode view fId of
  Flat.FX_ApplyE f2 x2 -> flatCollectApplyE view f2 (x2 : argIds)
  _ -> (fId, argIds)

flatEmitApply ctx s0 view fId argIds =
  let
    (headId, args) = flatCollectApply view fId argIds
    n = length args
   in
    if n > 1 && flatCallArity view headId == n
      then
        let
          (s1, Code fDecl fRef) = flatPureChild ctx s0 headId
          (s2, argDecl, argRefs) = flatEmitApplyArgs ctx s1 args
         in
          (s2, Code (fDecl $$ argDecl) (jsCallN fRef argRefs))
      else
        let
          xId = case argIds of
            (x : _) -> x
            [] -> error "JShark.flatEmitApply: missing argument"
          (s1, Code fDecl fRef) = flatPureChild ctx s0 fId
          (s2, Code xDecl xRef) = flatPureChild ctx s1 xId
         in
          (s2, Code (fDecl $$ xDecl) (jsCall fRef xRef))

flatEmitApplyE ctx s0 view fId argIds =
  let
    (headId, args) = flatCollectApplyE view fId argIds
    n = length args
   in
    if n > 1 && flatCallArity view headId == n
      then
        let
          (s1, Code fDecl fRef) = flatEffectChild ctx s0 headId
          (s2, argDecl, argRefs) = flatEmitApplyArgsE ctx s1 args
         in
          (s2, fxCode (fDecl $$ argDecl) (jsCallN fRef argRefs))
      else
        let
          xId = case argIds of
            (x : _) -> x
            [] -> error "JShark.flatEmitApplyE: missing argument"
          (s1, Code fDecl fRef) = flatEffectChild ctx s0 fId
          (s2, Code xDecl xRef) = flatEffectChild ctx s1 xId
         in
          (s2, fxCode (fDecl $$ xDecl) (jsCall fRef xRef))

flatEmitApplyArgs ctx s0 xs =
  foldl'
    ( \(s, d, rs) xId ->
        let
          (s', Code xd xr) = flatPureChild ctx s xId
         in
          (s', d $$ xd, rs ++ [xr])
    )
    (s0, mempty, [])
    xs

flatEmitApplyArgsE ctx s0 xs =
  foldl'
    ( \(s, d, rs) xId ->
        let
          (s', Code xd xr) = flatEffectChild ctx s xId
         in
          (s', d $$ xd, rs ++ [xr])
    )
    (s0, mempty, [])
    xs

flatPureChild ctx s cId = (s, flatTableLookup (fecTable ctx) cId)

flatEffectChild ctx s cId = (s, flatTableLookup (fecTable ctx) cId)

flatNodeKindEffect view nid =
  FlatView.firNodeIsEffect (FlatView.firNode view nid)

buildFlatEmitPlan ::
  FlatView.FlatIRView -> Flat.NodeId -> CG -> (FlatEmitPlan, CG)
buildFlatEmitPlan view root s0 =
  let
    n = FlatView.firNodeCount view
   in
    runST $ do
      envAt <- MV.replicate n Nothing
      bindAt <- MV.replicate n Nothing
      reach <- MV.replicate n False
      sRef <- newSTRef s0
      let
        writeEnv i e = MV.write envAt i (Just e)
        markReach i = MV.write reach i True
        planAlloc i = do
          s <- readSTRef sRef
          let
            hint = FlatView.firParamName view i
            (ident, s') = allocIdentHint s hint
          writeSTRef sRef s'
          MV.write bindAt i (Just ident)
          pure ident
        planInScope act = do
          modifySTRef sRef pushHintScope
          act
          modifySTRef sRef popHintScope
        planGo env nid
          | nid < 0 || nid >= n = pure ()
          | otherwise = do
              markReach nid
              writeEnv nid env
              case FlatView.firNode view nid of
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
                Flat.FE_ResultCase resId tagE errId tagO okId -> do
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
                Flat.FE_FnLit tags _names bodyId ->
                  planInScope $ do
                    s <- readSTRef sRef
                    let
                      (ids, s') = allocNIdents s (length tags)
                    writeSTRef sRef s'
                    let
                      env' = foldr (\(tag, i) -> IM.insert tag i) env (zip tags ids)
                    writeEnv nid env'
                    planGo env' bodyId
                Flat.FE_MethMap arr tag bodyId -> do
                  planGo env arr
                  planInScope $ do
                    s <- readSTRef sRef
                    let
                      (ident, s') = allocIdent s
                    writeSTRef sRef s'
                    let
                      env' = IM.insert tag ident env
                    writeEnv nid env'
                    planGo env' bodyId
                Flat.FE_MethFilter arr tag bodyId -> do
                  planGo env arr
                  planInScope $ do
                    s <- readSTRef sRef
                    let
                      (ident, s') = allocIdent s
                    writeSTRef sRef s'
                    let
                      env' = IM.insert tag ident env
                    writeEnv nid env'
                    planGo env' bodyId
                Flat.FE_MethReduce arr z tagA tagB bodyId -> do
                  planGo env arr
                  planGo env z
                  planInScope $ do
                    s <- readSTRef sRef
                    let
                      (ids, s') = allocNIdents s 2
                    writeSTRef sRef s'
                    let
                      nAcc = ids !! 0
                      nElem = ids !! 1
                      env' = IM.insert tagA nAcc $ IM.insert tagB nElem env
                    writeEnv nid env'
                    planGo env' bodyId
                Flat.FE_MethReduceRight arr z tagA tagB bodyId -> do
                  planGo env arr
                  planGo env z
                  planInScope $ do
                    s <- readSTRef sRef
                    let
                      (ids, s') = allocNIdents s 2
                    writeSTRef sRef s'
                    let
                      nAcc = ids !! 0
                      nElem = ids !! 1
                      env' = IM.insert tagA nAcc $ IM.insert tagB nElem env
                    writeEnv nid env'
                    planGo env' bodyId
                Flat.FE_MethToSorted arr tagA tagB bodyId -> do
                  planGo env arr
                  planInScope $ do
                    s <- readSTRef sRef
                    let
                      (ids, s') = allocNIdents s 2
                    writeSTRef sRef s'
                    let
                      nA = ids !! 0
                      nB = ids !! 1
                      env' = IM.insert tagA nA $ IM.insert tagB nB env
                    writeEnv nid env'
                    planGo env' bodyId
                Flat.FE_MethFrom lenId tag bodyId -> do
                  planGo env lenId
                  planInScope $ do
                    s <- readSTRef sRef
                    let
                      (ids, s') = allocNIdents s 2
                    writeSTRef sRef s'
                    let
                      nI = ids !! 1
                      env' = IM.insert tag nI env
                    writeEnv nid env'
                    planGo env' bodyId
                Flat.FX_Bind tag xId bodyId -> do
                  planGo env xId
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
                Flat.FX_ResultCaseE resId tagE errId tagO okId -> do
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
                Flat.FX_Try aId tag kId -> do
                  planGo env aId
                  ident <- planAlloc nid
                  planGo (IM.insert tag ident env) kId
                Flat.FX_StringCaseE scrutId ai defId -> do
                  planGo env scrutId
                  _ <- planAlloc nid
                  mapM_
                    (planGo env . snd)
                    (FlatView.firStrCases view ai)
                  planGo env defId
                node -> do
                  let
                    refs = FlatView.firNodePackRefs view node
                  mapM_ (planGo env) refs
      planGo IM.empty root
      envF <- V.unsafeFreeze envAt
      bindF <- V.unsafeFreeze bindAt
      reachF <- V.unsafeFreeze reach
      sFinal <- readSTRef sRef
      pure
        ( FlatEmitPlan
            { fepEnv = envF
            , fepBind = bindF
            , fepReach = reachF
            , fepLayers = FlatView.firLayerBuckets view root
            }
        , sFinal
        )

flatEmitLayered view root plan s0 =
  unsafePerformIO $ do
    let
      n = FlatView.firNodeCount view
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
    s0 = bumpEmitTick sIn
   in
    case FlatView.firNode view nid of
      Flat.FE_Literal li ->
        FlatView.withLitValue view li (flatRenderLiteral env s0)
      Flat.FE_Var i ->
        (s0, Code mempty (varStampJS s0 env (Name i)))
      Flat.FE_Let tag xId bodyId ->
        case FlatView.firNode view bodyId of
          Flat.FE_Var i
            | i == tag ->
                flatPureChild ctx s0 xId
          _ ->
            let
              (nBind, s1) = flatPlanIdent ctx s0 nid
              (s2, MkCode xDecl xRef _) = flatPureChild ctx s1 xId
              (s3, yCode) = flatPureChild ctx s2 bodyId
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
          (s2, MkCode rDecl rRef _) = flatPureChild ctx s1 rId
          (s3, bCode) = flatPureChild ctx s2 bId
         in
          ( s3
          , keepRef (recBindStmt n rDecl rRef $$ fromMaybe mempty (codeDecl bCode)) bCode
          )
      Flat.FE_Lambda tag bodyId ->
        case FlatView.firHoistTag view nid of
          Just _ ->
            let
              (s1, fnJs) = flatEmitLambdaSpine ctx env s0 view nid tag bodyId
              (s2, hoisted) = emitHoistedFnValue s1 view nid fnJs
             in
              (s2, Code mempty hoisted)
          Nothing ->
            withHintScope s0 $ \sScoped ->
              let
                hint = FlatView.firParamName view nid
                (nParam, s1) = flatPlanIdentHint ctx sScoped nid hint
                (s2, MkCode d r _) = flatPureChild ctx s1 bodyId
               in
                (s2, Code mempty (renderFunction s2 nParam d r))
      Flat.FE_Apply fId xId ->
        flatEmitApply ctx s0 view fId [xId]
      Flat.FE_EmbedEff eId -> flatEffectChild ctx s0 eId
      Flat.FE_If cId tId eId ->
        let
          (s1, Code cDecl cRef) = flatPureChild ctx s0 cId
          (s2, Code tDecl tRef) = flatPureChild ctx s1 tId
          (s3, Code eDecl eRef) = flatPureChild ctx s2 eId
         in
          ( s3
          , Code
              (cDecl $$ tDecl $$ eDecl)
               (parens (cRef <+> "?" <+> tRef <+> ":" <+> eRef))
           )
      Flat.FE_OptionCase oId nId _tag sId ->
        let
          (s1, Code optDecl optRef) = flatPureChild ctx s0 oId
          (nBind, s2) = flatPlanIdent ctx s1 nid
          optVar = identName s2 nBind
          (s3, Code noneDecl noneRef) = flatPureChild ctx s2 nId
          (s4, Code someDecl someRef) = flatPureChild ctx s3 sId
         in
          ( s4
          , Code
              (optDecl $$ constBind s2 nBind optRef $$ noneDecl $$ someDecl)
              ( parens
                  (jsText optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef)
              )
          )
      Flat.FE_ResultOk xId ->
        let
          (s1, MkCode d r _) = flatPureChild ctx s0 xId
         in
          (s1, MkCode d (Just (resultObject True r)) False)
      Flat.FE_ResultErr xId ->
        let
          (s1, MkCode d r _) = flatPureChild ctx s0 xId
         in
          (s1, MkCode d (Just (resultObject False r)) False)
      Flat.FE_ResultCase resId tagE errId tagO okId ->
        flatRenderResultCase ctx env s0 resId tagE errId tagO okId
      Flat.FE_Index arrId idxId ->
        let
          (s1, Code aDecl aRef) = flatPureChild ctx s0 arrId
          (s2, Code iDecl iRef) = flatPureChild ctx s1 idxId
          (s3, call) = emitCheckedIndex s2 aRef iRef
         in
          (s3, Code (aDecl $$ iDecl) call)
      Flat.FE_U8Index bufId idxId ->
        let
          (s1, Code bDecl bRef) = flatPureChild ctx s0 bufId
          (s2, Code iDecl iRef) = flatPureChild ctx s1 idxId
         in
          (s2, Code (bDecl $$ iDecl) (bRef <> brackets iRef))
      Flat.FE_Error msgId ->
        let
          (s1, Code d r) = flatPureChild ctx s0 msgId
         in
          (s1, Code d ("(function(){throw new Error(" <> r <> ");}())"))
      Flat.FE_Fixed fixed -> flatRenderFixed ctx s0 view fixed
      Flat.FE_FnLit tags _names bodyId ->
        flatRenderFnLit ctx env s0 tags bodyId
      Flat.FE_UnsafeNullable xId -> flatPureChild ctx s0 xId
      Flat.FE_FrozenLit gi -> flatRenderObjectLit ctx s0 view gi
      Flat.FE_GetField ti oId ->
        let
          (s1, Code d r) = flatPureChild ctx s0 oId
         in
          (s1, Code d (jsDotOrBracket r (FlatView.firText view ti)))
      Flat.FE_Hvm2Ref ti ->
        (s0, Code mempty (hvm2ExportRef (FlatView.firText view ti)))
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
    s0 = bumpEmitTick sIn
   in
    case FlatView.firNode view nid of
      Flat.FX_Lift eId -> flatPureChild ctx s0 eId
      Flat.FX_FFI fi ai ->
        let
          (s1, argDecl, argRefs) = flatRenderArgList ctx s0 view ai
         in
          ( s1
          , fxCode
              argDecl
              (renderFFIInvoke (FlatView.firFFI view fi) argRefs)
          )
      Flat.FX_UnsafeObject ti ->
        (s0, Code mempty (jsText (FlatView.firText view ti)))
      Flat.FX_UnsafeObjectGet xId sId ->
        let
          (s1, Code xDecl xRef) = flatEffectChild ctx s0 xId
         in
          (s1, Code xDecl $ jsDotOrBracket xRef (FlatView.firText view sId))
      Flat.FX_UnsafeObjectAssign xId yId ->
        let
          (s1, Code xDecl xRef) = flatEffectChild ctx s0 xId
          (s2, Code yDecl yRef) = flatEffectChild ctx s1 yId
         in
          (s2, fxCode (xDecl $$ yDecl) $ xRef <> " = " <> yRef)
      Flat.FX_CallMethod recvId methodIdx ai ->
        let
          method = FlatView.firText view methodIdx
          (s1, Code rDecl rRef) = flatEffectChild ctx s0 recvId
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
          (s2, MkCode rDecl rRef _) = flatEffectChild ctx s1 rId
          (s3, MkCode bDecl bRef bFX) = flatEffectChild ctx s2 bId
         in
          ( s3
          , MkCode (Just (recBindStmt n rDecl rRef $$ fromMaybe mempty bDecl)) bRef bFX
          )
      Flat.FX_LambdaE tag bodyId ->
        case FlatView.firHoistTag view nid of
          Just _ ->
            let
              (s1, fnJs) = flatEmitLambdaESpine ctx env s0 view nid tag bodyId
              (s2, hoisted) = emitHoistedFnValue s1 view nid fnJs
             in
              (s2, Code mempty hoisted)
          Nothing ->
            withHintScope s0 $ \sScoped ->
              let
                hint = FlatView.firParamName view nid
                (nParam, s1) = flatPlanIdentHint ctx sScoped nid hint
                (s2, MkCode exprXDecl exprXRef _) =
                  flatEffectChild ctx s1 bodyId
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
          (s1, MkCode cDecl cRef _) = flatEffectChild ctx s0 cId
          (s2, MkCode tDecl tRef tFX) = flatEffectChild ctx s1 tId
          (s3, MkCode eDecl eRef eFX) = flatEffectChild ctx s2 eId
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
          (s1, MkCode condDecl condRef _) = flatEffectChild ctx s0 cId
          (s2, MkCode bodyDecl bodyRef _) = flatEffectChild ctx s1 bId
          bodyStmt = asStmt bodyDecl bodyRef
          whileStmt =
            "while"
              <+> parens (fromMaybe mempty condRef)
              <+> blockBody bodyStmt
         in
          (s2, MkCode (Just (fromMaybe mempty condDecl $$ whileStmt)) Nothing False)
      Flat.FX_ForRange startId endId _tag bodyId ->
        let
          (s1, MkCode startDecl startRef _) = flatPureChild ctx s0 startId
          (s2, MkCode endDecl endRef _) = flatPureChild ctx s1 endId
          (loopN, s3) = flatPlanIdentHint ctx s2 nid (Just "i")
          loopVar = nJS s3 loopN
          (s4, MkCode bodyDecl bodyRef _) = flatEffectChild ctx s3 bodyId
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
          (s1, Code bDecl bRef) = flatPureChild ctx s0 bufId
          (s2, Code iDecl iRef) = flatPureChild ctx s1 idxId
          (s3, Code vDecl vRef) = flatPureChild ctx s2 valId
          stmt = (bRef <> brackets iRef) <+> "=" <+> vRef
         in
          (s3, Code (bDecl $$ iDecl $$ vDecl $$ (stmt <> semi)) mempty)
      Flat.FX_U8Fill bufId valId ->
        let
          (s1, Code bDecl bRef) = flatPureChild ctx s0 bufId
          (s2, Code vDecl vRef) = flatPureChild ctx s1 valId
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
                (s1, Code oDecl oRef) = flatPureChild ctx s oId
                (nBind, s2) = flatPlanIdent ctx s1 nid
               in
                (s2, oDecl $$ constBind s2 nBind oRef, nBind)
          )
           ( \mRes nBind s ->
               let
                (s1, MkCode nDecl nRef _) = flatEffectChild ctx s nId
                (s2, MkCode sDecl sRef _) = flatEffectChild ctx s1 sId
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
          (s1, Code xDecl xRef) = flatPureChild ctx s0 xId
         in
          (s1, Code (xDecl $$ (("throw" <+> xRef) <> semi)) mempty)
      Flat.FX_Try aId _tag kId ->
        emitBranching
          (flatIsUnitEffect view aId && flatIsUnitEffect view kId)
          s0
          (\s -> (s, mempty, ()))
          ( \mRes () s ->
              let
                (s1, MkCode aDecl aRef _) = flatEffectChild ctx s aId
                (catchN, s2) = flatPlanIdent ctx s1 nid
                (s3, MkCode bDecl bRef _) = flatEffectChild ctx s2 kId
               in
                (s3, tryCatchStmt mRes (nJS s3 catchN) aDecl aRef bDecl bRef)
          )
      Flat.FX_ObjectLit gi -> flatRenderObjectLit ctx s0 view gi
      Flat.FX_DeleteProp oId kId ->
        let
          (s1, Code oDecl oRef) = flatEffectChild ctx s0 oId
          (s2, Code kDecl kRef) = flatPureChild ctx s1 kId
         in
          (s2, fxCode (oDecl $$ kDecl) (("delete" <+> oRef) <> brackets kRef))
      Flat.FX_ArrayLit es -> flatRenderArrayLit ctx s0 es
      _ -> error "JShark.flatEffectfulAST': unexpected node"

flatEffectfulCodegenFromView soa =
  flatEffectfulCodegenFromViewWith startCG soa

flatEffectfulCodegenFromViewWith sStart soa =
  let
    root = FlatView.firRoot soa
    total = FlatView.firNodeCount soa
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
    !(soa, s0) = unsafePerformIO (prepareFlatEffectProgramWith style e)
   in
    flatEffectfulCodegenFromViewWith s0 soa
{-# NOINLINE flatEffectfulCodegenWith #-}

effectfulASTFromFlat :: ClosedEffect u -> JS
effectfulASTFromFlat e = uncurry renderWithPreamble (flatEffectfulCodegen e)

effectfulASTFromSoA :: FlatSoA.FlatSoA -> JS
effectfulASTFromSoA soa =
  uncurry renderWithPreamble (flatEffectfulCodegenFromView soa)

effectfulAST :: ClosedEffect u -> JS
effectfulAST = effectfulASTWith idiomaticStyle

effectfulASTWith :: EmitStyle -> ClosedEffect u -> JS
effectfulASTWith style e =
  uncurry renderWithPreamble (flatEffectfulCodegenWith style e)

effectfulASTIr = effectfulASTFromFlat

-- | Stmt-only codegen for branching effects (no shared @let result@).
