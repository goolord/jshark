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

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
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
  , renderFunction
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
  , dquotes
  , hcat
  , jsDouble
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
  ValueNumber d -> (s0, Code mempty (jsDouble d))
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
    case FlatView.firLitValue view li of
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

flatRenderBin mode env op s0 view xId yId =
  let
    (s1, Code xDecl xRef) = flatPureChild mode env s0 view xId
    (s2, Code yDecl yRef) = flatPureChild mode env s1 view yId
   in
    ( s2
    , Code
        (xDecl $$ yDecl)
        ( flatWrapOperand view xId xRef
            <+> jsText op
            <+> flatWrapOperand view yId yRef
        )
    )

flatParEmitMinSiblings = 4

flatParEmitMaxWorkers = 8

-- | Higher than 'FlatSoA.flatSoaParallelThreshold': parallel emit keeps
--   sibling 'CG' helper maps until merge; stream-merge caps peak RAM.
flatParEmitBudgetThreshold = 16384

shouldParFlatSiblings view nids =
  length nids >= flatParEmitMinSiblings
    && sum (map (FlatView.firIdentBudget view) nids)
      >= flatParEmitBudgetThreshold
{-# NOINLINE shouldParFlatSiblings #-}

parEmitChunked maxW f xs =
  let
    (chunk, rest) = splitAt maxW xs
   in
    (<>)
      <$> mapConcurrently f chunk
      <*> parEmitChunked maxW f rest

emitFlatSiblingsSeq emit env s0 view nids =
  mapAccumL
    ( \st nid ->
        let
          (st', code) = emit env st view nid
         in
          (st', code)
    )
    s0
    nids

parEmitSiblingsDirect emit env s0 view nids =
  unsafePerformIO $ do
    let
      budgets = map (FlatView.firIdentBudget view) nids
      starts = scanl (+) (cgIdent s0) (0 : init budgets)
      jobs = zip starts nids
    results <-
      parEmitChunked
        flatParEmitMaxWorkers
        ( \(start, nid) ->
            pure $
              emit env (s0 {cgIdent = start}) view nid
        )
        jobs
    let
      (cgs, codes) = unzip results
      !sMerged = mergeEmitCGs s0 cgs
    pure (sMerged, codes)
{-# NOINLINE parEmitSiblingsDirect #-}

emitFlatSiblings mode emit env s0 view nids =
  case mode of
    LayeredEmit {} -> emitFlatSiblingsSeq emit env s0 view nids
    DirectEmit
      | shouldParFlatSiblings view nids ->
          parEmitSiblingsDirect emit env s0 view nids
      | otherwise ->
          emitFlatSiblingsSeq emit env s0 view nids
{-# NOINLINE emitFlatSiblings #-}

flatRenderArgListSeq mode env s0 view args =
  let
    go s = \case
      [] -> (s, [])
      Flat.FlatArgExpr eid : rest ->
        let
          (s', c) = flatPureChild mode env s view eid
          (s'', cs') = go s' rest
         in
          (s'', c : cs')
      Flat.FlatArgEffect eid : rest ->
        let
          (s', c) = flatEffectChild mode env s view eid
          (s'', cs') = go s' rest
         in
          (s'', c : cs')
    (s1, cs) = go s0 args
   in
    (s1, codesDecls cs, hcat (punctuate ", " (codesRefs cs)))

flatRenderArgListPar env s0 view args =
  let
    emitArg s = \case
      Flat.FlatArgExpr eid -> flatPureChild DirectEmit env s view eid
      Flat.FlatArgEffect eid -> flatEffectChild DirectEmit env s view eid
    nids =
      [ case arg of
          Flat.FlatArgExpr eid -> eid
          Flat.FlatArgEffect eid -> eid
      | arg <- args
      ]
    starts = scanl (+) (cgIdent s0) (0 : init (map (FlatView.firIdentBudget view) nids))
    jobs = zip starts args
    (s1, cs) =
      unsafePerformIO $
        do
          let
            emitJob (start, arg) =
              pure $ emitArg (s0 {cgIdent = start}) arg
          results <- parEmitChunked flatParEmitMaxWorkers emitJob jobs
          let
            (cgs, codes) = unzip results
            !sMerged = mergeEmitCGs s0 cgs
          pure (sMerged, codes)
   in
    (s1, codesDecls cs, hcat (punctuate ", " (codesRefs cs)))
{-# NOINLINE flatRenderArgListPar #-}

flatRenderArgList mode env s0 view ai =
  let
    args = FlatView.firArgGroup view ai
   in
    case mode of
      LayeredEmit {} ->
        flatRenderArgListSeq mode env s0 view args
      DirectEmit ->
        let
          nids =
            [ case arg of
                Flat.FlatArgExpr eid -> eid
                Flat.FlatArgEffect eid -> eid
            | arg <- args
            ]
         in
          if shouldParFlatSiblings view nids
            then flatRenderArgListPar env s0 view args
            else flatRenderArgListSeq DirectEmit env s0 view args

flatRenderField mode env view s = \case
  Flat.FlatField k eid ->
    let
      (s', Code d r) = flatPureChild mode env s view eid
     in
      (s', (d, (dquotes (jsText k) <> ":") <+> r))
  Flat.FlatFieldExtra k eid ->
    let
      (s', Code d r) = flatPureChild mode env s view eid
     in
      (s', (d, (dquotes (jsText k) <> ":") <+> r))
  Flat.FlatFieldEff k eid ->
    let
      (s', MkCode d r _) = flatEffectChild mode env s view eid
     in
      ( s'
      ,
        ( fromMaybe mempty d
        , (dquotes (jsText k) <> ":") <+> fromMaybe mempty r
        )
      )
  Flat.FlatFieldExtraEff k eid ->
    let
      (s', MkCode d r _) = flatEffectChild mode env s view eid
     in
      ( s'
      ,
        ( fromMaybe mempty d
        , (dquotes (jsText k) <> ":") <+> fromMaybe mempty r
        )
      )

flatRenderObjectLit mode env s0 view gi =
  let
    fs = FlatView.firFieldGroup view gi
    (s1, parts) = mapAccumL (flatRenderField mode env view) s0 fs
    (declList, pairs) = unzip parts
   in
    (s1, Code (vcatNonEmpty declList) (braces (hcat (punctuate ", " pairs))))

flatRenderArrayLit mode env s0 view es =
  let
    (s1, cs) = emitFlatSiblings mode (flatEffectChild mode) env s0 view es
   in
    ( s1
    , Code
        (codesDecls cs)
        (brackets (hcat (punctuate ", " (codesRefs cs))))
    )

flatRenderFixed mode env s0 view = \case
  Flat.FlatFixedU op xId
    | Just name <- Prim.math1Name op ->
        let
          (s1, Code xDecl xRef) = flatPureChild mode env s0 view xId
         in
          (s1, Code xDecl ("Math." <> jsText name <> parens xRef))
  Flat.FlatFixedB op xId yId
    | Just name <- Prim.math2Name op ->
        let
          (s1, Code xDecl xRef) = flatPureChild mode env s0 view xId
          (s2, Code yDecl yRef) = flatPureChild mode env s1 view yId
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
      (s1, Code rDecl rRef) = flatPureChild mode env s0 view xId
     in
      (s1, Code rDecl (Prim.fixedUnaryJS op (flatWrapOperand view xId rRef)))
  Flat.FlatFixedB op xId yId ->
    let
      (s1, Code rDecl rRef) = flatPureChild mode env s0 view xId
      (s2, Code aDecl aRef) = flatPureChild mode env s1 view yId
     in
      ( s2
      , Code
          (rDecl $$ aDecl)
          (Prim.fixedBinaryJS op (flatWrapOperand view xId rRef) aRef)
      )
  Flat.FlatFixedT op xId yId zId ->
    let
      (s1, Code rDecl rRef) = flatPureChild mode env s0 view xId
      (s2, Code aDecl aRef) = flatPureChild mode env s1 view yId
      (s3, Code bDecl bRef) = flatPureChild mode env s2 view zId
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

flatRenderKernel mode env s0 view = \case
  Flat.FE_KConcat x y -> flatRenderBin mode env "+" s0 view x y
  Flat.FE_KPlus x y -> flatRenderBin mode env "+" s0 view x y
  Flat.FE_KMinus x y -> flatRenderBin mode env "-" s0 view x y
  Flat.FE_KTimes x y -> flatRenderBin mode env "*" s0 view x y
  Flat.FE_KFracDiv x y -> flatRenderBin mode env "/" s0 view x y
  Flat.FE_KRem x y -> flatRenderBin mode env "%" s0 view x y
  Flat.FE_KBitAnd x y -> flatRenderBin mode env "&" s0 view x y
  Flat.FE_KBitOr x y -> flatRenderBin mode env "|" s0 view x y
  Flat.FE_KBitXor x y -> flatRenderBin mode env "^" s0 view x y
  Flat.FE_KShl x y -> flatRenderBin mode env "<<" s0 view x y
  Flat.FE_KShr x y -> flatRenderBin mode env ">>" s0 view x y
  Flat.FE_KUShr x y -> flatRenderBin mode env ">>>" s0 view x y
  Flat.FE_KBig op x y -> flatRenderBin mode env (bigOpJS op) s0 view x y
  Flat.FE_KBigNeg x ->
    let
      (s1, Code xDecl xRef) = flatPureChild mode env s0 view x
     in
      (s1, Code xDecl $ "-" <> parens xRef)
  Flat.FE_KShow x ->
    let
      (s1, Code xDecl xRef) = flatPureChild mode env s0 view x
     in
      (s1, Code xDecl $ "String" <> parens xRef)
  Flat.FE_KTypeOf x ->
    let
      (s1, Code xDecl xRef) = flatPureChild mode env s0 view x
     in
      (s1, Code xDecl $ "typeof" <+> xRef)
  Flat.FE_KNegate x ->
    let
      (s1, Code xDecl xRef) = flatPureChild mode env s0 view x
     in
      (s1, Code xDecl $ "-" <> parens xRef)
  Flat.FE_KAnd x y -> flatRenderBin mode env "&&" s0 view x y
  Flat.FE_KOr x y -> flatRenderBin mode env "||" s0 view x y
  Flat.FE_KEq structural x y
    | structural ->
        let
          s1 = useEqHelpers s0
          (s2, Code xDecl xRef) = flatPureChild mode env s1 view x
          (s3, Code yDecl yRef) = flatPureChild mode env s2 view y
         in
          ( s3
          , Code
              (xDecl $$ yDecl)
              (jsValueEq (flatWrapOperand view x xRef) (flatWrapOperand view y yRef))
          )
    | otherwise ->
        flatRenderBin mode env "===" s0 view x y
  Flat.FE_KNEq structural x y
    | structural ->
        let
          s1 = useEqHelpers s0
          (s2, Code xDecl xRef) = flatPureChild mode env s1 view x
          (s3, Code yDecl yRef) = flatPureChild mode env s2 view y
         in
          ( s3
          , Code
              (xDecl $$ yDecl)
              (jsValueNEq (flatWrapOperand view x xRef) (flatWrapOperand view y yRef))
          )
    | otherwise ->
        flatRenderBin mode env "!==" s0 view x y
  Flat.FE_KGTh x y -> flatRenderBin mode env ">" s0 view x y
  Flat.FE_KLTh x y -> flatRenderBin mode env "<" s0 view x y
  Flat.FE_KGTEq x y -> flatRenderBin mode env ">=" s0 view x y
  Flat.FE_KLTEq x y -> flatRenderBin mode env "<=" s0 view x y
  _ -> error "JShark.flatRenderKernel: unexpected node"

flatEnvTag env tag =
  case IM.lookup tag env of
    Just n -> n
    Nothing -> error "JShark.flatEnvTag: missing binding"

flatRenderCallbackMethod mode env name s0 view arrId tag bodyId =
  let
    (s1, Code rDecl rRef) = flatPureChild mode env s0 view arrId
    (nParam, s2, exDecl, exRef) =
      case mode of
        LayeredEmit {} ->
          let
            (s', Code d r) = flatPureChild mode env s1 view bodyId
           in
            (flatEnvTag env tag, s', d, r)
        DirectEmit ->
          let
            (n, s') = allocIdent s1
            env' = IM.insert tag n env
            (s'', Code d r) = flatPureChild mode env' s' view bodyId
           in
            (n, s'', d, r)
    call =
      flatWrapOperand view arrId rRef
        <> "."
        <> jsString name
        <> parens (jsCallback [nJS nParam] exDecl exRef)
   in
    (s2, Code rDecl call)

flatRenderFold mode env method s0 view arrId zId tagA tagB bodyId =
  let
    (s1, Code rDecl rRef) = flatPureChild mode env s0 view arrId
    (s2, Code zDecl zRef) = flatPureChild mode env s1 view zId
    (nAcc, nElem, s3, exDecl, exRef) =
      case mode of
        LayeredEmit {} ->
          let
            (s', Code d r) = flatPureChild mode env s2 view bodyId
           in
            ( flatEnvTag env tagA
            , flatEnvTag env tagB
            , s'
            , d
            , r
            )
        DirectEmit ->
          let
            (nA, sA) = allocIdent s2
            (nE, sE) = allocIdent sA
            env' = IM.insert tagA nA $ IM.insert tagB nE env
            (s', Code d r) = flatPureChild mode env' sE view bodyId
           in
            (nA, nE, s', d, r)
    cb = jsCallback [nJS nAcc, nJS nElem] exDecl exRef
    call =
      flatWrapOperand view arrId rRef
        <> jsString method
        <> parens (cb <> ", " <> zRef)
   in
    (s3, Code (rDecl $$ zDecl) call)

flatRenderMethod mode env s0 view = \case
  Flat.FE_MethMap arr tag body ->
    flatRenderCallbackMethod mode env "map" s0 view arr tag body
  Flat.FE_MethFilter arr tag body ->
    flatRenderCallbackMethod mode env "filter" s0 view arr tag body
  Flat.FE_MethReduce arr z tagA tagB body ->
    flatRenderFold mode env ".reduce" s0 view arr z tagA tagB body
  Flat.FE_MethReduceRight arr z tagA tagB body ->
    flatRenderFold mode env ".reduceRight" s0 view arr z tagA tagB body
  Flat.FE_MethToSorted arr tagA tagB body ->
    case mode of
      LayeredEmit {} ->
        let
          (s1, Code rDecl rRef) = flatPureChild mode env s0 view arr
          nA = flatEnvTag env tagA
          nB = flatEnvTag env tagB
          (s2, Code exDecl exRef) = flatPureChild mode env s1 view body
          cb = jsCallback [nJS nA, nJS nB] exDecl exRef
         in
          ( s2
          , Code rDecl (flatWrapOperand view arr rRef <> ".toSorted" <> parens cb)
          )
      DirectEmit ->
        let
          (s1, Code rDecl rRef) = flatPureChild mode env s0 view arr
          (nA, s2) = allocIdent s1
          (nB, s3) = allocIdent s2
          env' = IM.insert tagA nA $ IM.insert tagB nB env
          (s4, Code exDecl exRef) = flatPureChild mode env' s3 view body
          cb = jsCallback [nJS nA, nJS nB] exDecl exRef
         in
          ( s4
          , Code rDecl (flatWrapOperand view arr rRef <> ".toSorted" <> parens cb)
          )
  Flat.FE_MethFrom n tag body ->
    case mode of
      LayeredEmit {} ->
        let
          (s1, Code nDecl nRef) = flatPureChild mode env s0 view n
          nI = flatEnvTag env tag
          (s2, Code exDecl exRef) = flatPureChild mode env s1 view body
          cb = jsCallback ["_", nJS nI] exDecl exRef
         in
          (s2, Code nDecl ("Array.from({length: " <> nRef <> "}, " <> cb <> ")"))
      DirectEmit ->
        let
          (s1, Code nDecl nRef) = flatPureChild mode env s0 view n
          (nHole, s2) = allocIdent s1
          (nI, s3) = allocIdent s2
          env' = IM.insert tag nI env
          (s4, Code exDecl exRef) = flatPureChild mode env' s3 view body
          cb = jsCallback [nJS nHole, nJS nI] exDecl exRef
         in
          (s4, Code nDecl ("Array.from({length: " <> nRef <> "}, " <> cb <> ")"))
  _ -> error "JShark.flatRenderMethod: unexpected node"

flatRenderFnLit mode env s0 view tags bodyId =
  case mode of
    LayeredEmit {} ->
      let
        ids = map (flatEnvTag env) tags
        (s1, Code d r) = flatPureChild mode env s0 view bodyId
       in
        (s1, Code mempty (jsCallback (map nJS ids) d r))
    DirectEmit ->
      let
        (ids, s1) = allocNIdents s0 (length tags)
        env' = foldr (\(tag, n) -> IM.insert tag n) env (zip tags ids)
        (s2, Code d r) = flatPureChild mode env' s1 view bodyId
       in
        (s2, Code mempty (jsCallback (map nJS ids) d r))

flatResultUnwrapIdent mode env s tag =
  case mode of
    LayeredEmit {} -> (flatEnvTag env tag, s)
    DirectEmit ->
      let
        (n, s') = allocIdent s
       in
        (n, s')

flatRenderResultCase mode env s0 view resId tagE errId tagO okId =
  let
    (s1, MkCode rDecl rRef _) = flatPureChild mode env s0 view resId
    (nObj, s2) = allocIdent s1
    (nUnw, s3) = flatResultUnwrapIdent mode env s2 tagE
    obj = nName nObj
    prelude =
      fromMaybe mempty rDecl
        $$ constBind nObj (fromMaybe mempty rRef)
        $$ constBind nUnw (jsText obj <> ".value")
    envE = IM.insert tagE nUnw env
    envO = IM.insert tagO nUnw envE
    (s4, Code eDecl eRef) = flatPureChild mode envE s3 view errId
    (s5, Code oDecl oRef) = flatPureChild mode envO s4 view okId
   in
    ( s5
    , Code
        (prelude $$ eDecl $$ oDecl)
        (parens ((jsText obj <> ".ok") <+> "?" <+> oRef <+> ":" <+> eRef))
    )

flatSeqEffect mode env s0 view xId yId =
  let
    (s1, MkCode xDecl xRef xFX) = flatEffectChild mode env s0 view xId
    (s2, MkCode yDecl yRef yFX) = flatEffectChild mode env s1 view yId
    stmt
      | isNothing xRef = fromMaybe mempty xDecl
      | not xFX && isJust xDecl = fromMaybe mempty xDecl
      | otherwise = asStmt xDecl xRef
   in
    (s2, MkCode (Just (stmt $$ fromMaybe mempty yDecl)) yRef yFX)

flatBindEffect mode env s0 view nid tag xId bodyId =
  let
    (s1, MkCode xDecl xRef xFX) = flatEffectChild mode env s0 view xId
    (nBind, s2) = flatPlanIdent mode s1 nid
    env' = IM.insert tag nBind env
    (s3, MkCode yDecl yRef yFX) = flatEffectChild mode env' s2 view bodyId
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
                    $$ constBind nBind (fromMaybe mempty xRef)
                    $$ fromMaybe mempty yDecl
                )
            )
            yRef
            yFX
        )

flatRenderResultCaseE mode env s0 view nid resId tagE errId tagO okId =
  if flatIsUnitEffect view errId && flatIsUnitEffect view okId
    then
      let
        (s1, MkCode rDecl rRef _) = flatPureChild mode env s0 view resId
        (nObj, s2) = allocIdent s1
        (nUnw, s3) = flatResultUnwrapIdent mode env s2 tagE
        obj = nName nObj
        prelude =
          fromMaybe mempty rDecl
            $$ constBind nObj (fromMaybe mempty rRef)
            $$ constBind nUnw (jsText obj <> ".value")
        envE = IM.insert tagE nUnw env
        envO = IM.insert tagO nUnw envE
        (s4, MkCode eDecl eRef _) = flatEffectChild mode envE s3 view errId
        (s5, MkCode oDecl oRef _) = flatEffectChild mode envO s4 view okId
       in
        ( s5
        , Code
            (prelude $$ ifElseStmt (jsText obj <> ".ok") oDecl oRef eDecl eRef)
            mempty
        )
    else
      let
        (s1, MkCode rDecl rRef _) = flatPureChild mode env s0 view resId
        (nObj, s2) = allocIdent s1
        (nUnw, s3) = flatResultUnwrapIdent mode env s2 tagE
        obj = nName nObj
        prelude =
          fromMaybe mempty rDecl
            $$ constBind nObj (fromMaybe mempty rRef)
            $$ constBind nUnw (jsText obj <> ".value")
        (resultN, s4) = flatPlanIdent mode s3 nid
        resultVar = nName resultN
        envE = IM.insert tagE nUnw env
        envO = IM.insert tagO nUnw envE
        (s5, MkCode eDecl eRef _) = flatEffectChild mode envE s4 view errId
        (s6, MkCode oDecl oRef _) = flatEffectChild mode envO s5 view okId
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

flatRenderStringCaseE mode env s0 view nid scrutId ai defId =
  let
    arms = FlatView.firStrCases view ai
    unit =
      all (flatIsUnitEffect view . snd) arms
        && flatIsUnitEffect view defId
    (s1, Code oDecl oRef) = flatPureChild mode env s0 view scrutId
    (resultN, s2) =
      if unit then (0, s1) else flatPlanIdent mode s1 nid
    resultVar = nName resultN
    renderArm s e =
      let
        (s', MkCode mDecl mRef _) = flatEffectChild mode env s view e
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

data FlatEmitMode where
  LayeredEmit :: FlatTableRead -> FlatEmitPlan -> FlatEmitMode
  DirectEmit :: FlatEmitMode

flatPlanIdent :: FlatEmitMode -> CG -> Flat.NodeId -> (Int, CG)
flatPlanIdent mode s nid =
  case mode of
    LayeredEmit _ plan ->
      case fepBind plan V.!? nid of
        Just (Just i) -> (i, s)
        _ -> allocIdent s
    DirectEmit -> allocIdent s

flatPlanEnv :: FlatEmitPlan -> Flat.NodeId -> Env
flatPlanEnv plan nid =
  case fepEnv plan V.!? nid of
    Just (Just env) -> env
    _ -> error ("JShark.flatPlanEnv: missing env for node " ++ show nid)

jsCallback :: [JS] -> JS -> JS -> JS
jsCallback params decl ref =
  "function"
    <+> parens (hcat (punctuate ", " params))
    <+> blockBody (decl $$ "return" <+> ref)

jsBinaryCallback ids decl ref =
  jsCallback (map nJS ids) decl ref

flatRenderBinaryHoistedLambda mode env s0 view nidOuter tagA nidInner tagB bodyId =
  let
    (nA, s1) = flatPlanIdent mode s0 nidOuter
    (nB, s2) = flatPlanIdent mode s1 nidInner
    env' = IM.insert tagA nA $ IM.insert tagB nB env
    (s3, Code d r) = flatPureChild mode env' s2 view bodyId
   in
    (s3, jsBinaryCallback [nA, nB] d r)

flatPureChild mode env s view cId =
  case mode of
    LayeredEmit table _ -> (s, flatTableLookup table cId)
    DirectEmit -> flatPureASTGo DirectEmit env s view cId

flatEffectChild mode env s view cId =
  case mode of
    LayeredEmit table _ -> (s, flatTableLookup table cId)
    DirectEmit -> flatEffectfulASTGo DirectEmit env s view cId

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
            (ident, s') = allocIdent s
          writeSTRef sRef s'
          MV.write bindAt i (Just ident)
          pure ident
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
                Flat.FE_Lambda tag bodyId -> do
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
                Flat.FE_FnLit tags bodyId -> do
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
                Flat.FX_LambdaE tag bodyId -> do
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
        mode = LayeredEmit tableRead plan
        env = flatPlanEnv plan nid
        (s', code) =
          if flatNodeKindEffect view nid
            then flatEffectfulASTGo mode env s view nid
            else flatPureASTGo mode env s view nid
      MV.write tableMV nid code
      writeIORef sRef s'
    sFinal <- readIORef sRef
    rootCode <- MV.read tableMV root
    pure (sFinal, rootCode)
{-# NOINLINE flatEmitLayered #-}

flatPureAST' env s view nid = flatPureASTGo DirectEmit env s view nid

flatPureASTGo !mode !env !sIn view nid =
  let
    s0 = bumpEmitTick sIn
   in
    case FlatView.firNode view nid of
      Flat.FE_Literal li ->
        flatRenderLiteral env s0 (FlatView.firLitValue view li)
      Flat.FE_Var i ->
        (s0, Code mempty (varStampJS env (Name i)))
      Flat.FE_Let tag xId bodyId ->
        let
          (nBind, s1) = flatPlanIdent mode s0 nid
          (s2, MkCode xDecl xRef _) = flatPureChild mode env s1 view xId
          env' = IM.insert tag nBind env
          (s3, yCode) = flatPureChild mode env' s2 view bodyId
         in
          ( s3
          , keepRef
              ( fromMaybe mempty xDecl
                  $$ constBind nBind (fromMaybe mempty xRef)
                  $$ fromMaybe mempty (codeDecl yCode)
              )
              yCode
          )
      Flat.FE_LetRec tag rId bId ->
        let
          (nBind, s1) = flatPlanIdent mode s0 nid
          n = nJS nBind
          env' = IM.insert tag nBind env
          (s2, MkCode rDecl rRef _) = flatPureChild mode env' s1 view rId
          (s3, bCode) = flatPureChild mode env' s2 view bId
         in
          ( s3
          , keepRef (recBindStmt n rDecl rRef $$ fromMaybe mempty (codeDecl bCode)) bCode
          )
      Flat.FE_Lambda tag bodyId ->
        case (FlatView.firHoistTag view nid, FlatView.firNode view bodyId) of
          (Just _, Flat.FE_Lambda tag2 body2Id) ->
            let
              (s1, fnJs) =
                flatRenderBinaryHoistedLambda
                  mode
                  env
                  s0
                  view
                  nid
                  tag
                  bodyId
                  tag2
                  body2Id
              (s2, hoisted) = emitHoistedFnValue s1 view nid fnJs
             in
              (s2, Code mempty hoisted)
          (Just _, _) ->
            let
              (nParam, s1) = flatPlanIdent mode s0 nid
              env' = IM.insert tag nParam env
              (s2, MkCode exprXDecl exprXRef _) = flatPureChild mode env' s1 view bodyId
              (s3, fnJs) =
                emitHoistedFnValue
                  s2
                  view
                  nid
                  (renderFunction nParam exprXDecl exprXRef)
             in
              (s3, Code mempty fnJs)
          (Nothing, _) ->
            let
              (nParam, s1) = flatPlanIdent mode s0 nid
              env' = IM.insert tag nParam env
              (s2, MkCode exprXDecl exprXRef _) = flatPureChild mode env' s1 view bodyId
             in
              (s2, Code mempty (renderFunction nParam exprXDecl exprXRef))
      Flat.FE_Apply fId xId ->
        let
          (s1, Code fDecl fRef) = flatPureChild mode env s0 view fId
          (s2, Code xDecl xRef) = flatPureChild mode env s1 view xId
         in
          (s2, Code (fDecl $$ xDecl) (jsCall fRef xRef))
      Flat.FE_EmbedEff eId -> flatEffectChild mode env s0 view eId
      Flat.FE_If cId tId eId ->
        let
          (s1, Code cDecl cRef) = flatPureChild mode env s0 view cId
          (s2, Code tDecl tRef) = flatPureChild mode env s1 view tId
          (s3, Code eDecl eRef) = flatPureChild mode env s2 view eId
         in
          ( s3
          , Code
              (cDecl $$ tDecl $$ eDecl)
              (parens (cRef <+> "?" <+> tRef <+> ":" <+> eRef))
          )
      Flat.FE_OptionCase oId nId tag sId ->
        let
          (s1, Code optDecl optRef) = flatPureChild mode env s0 view oId
          (nBind, s2) = flatPlanIdent mode s1 nid
          optVar = nName nBind
          env' = IM.insert tag nBind env
          (s3, Code noneDecl noneRef) = flatPureChild mode env s2 view nId
          (s4, Code someDecl someRef) = flatPureChild mode env' s3 view sId
         in
          ( s4
          , Code
              (optDecl $$ constBind nBind optRef $$ noneDecl $$ someDecl)
              ( parens
                  (jsText optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef)
              )
          )
      Flat.FE_ResultOk xId ->
        let
          (s1, MkCode d r _) = flatPureChild mode env s0 view xId
         in
          (s1, MkCode d (Just (resultObject True r)) False)
      Flat.FE_ResultErr xId ->
        let
          (s1, MkCode d r _) = flatPureChild mode env s0 view xId
         in
          (s1, MkCode d (Just (resultObject False r)) False)
      Flat.FE_ResultCase resId tagE errId tagO okId ->
        flatRenderResultCase mode env s0 view resId tagE errId tagO okId
      Flat.FE_Index arrId idxId ->
        let
          s1 = useHelperSrc "$checkedIndex" jsCheckedIndexSrc s0
          (s2, Code aDecl aRef) = flatPureChild mode env s1 view arrId
          (s3, Code iDecl iRef) = flatPureChild mode env s2 view idxId
         in
          (s3, Code (aDecl $$ iDecl) (jsCheckedIndex aRef iRef))
      Flat.FE_U8Index bufId idxId ->
        let
          (s1, Code bDecl bRef) = flatPureChild mode env s0 view bufId
          (s2, Code iDecl iRef) = flatPureChild mode env s1 view idxId
         in
          (s2, Code (bDecl $$ iDecl) (bRef <> brackets iRef))
      Flat.FE_Error msgId ->
        let
          (s1, Code d r) = flatPureChild mode env s0 view msgId
         in
          (s1, Code d ("(function(){throw new Error(" <> r <> ");}())"))
      Flat.FE_Fixed fixed -> flatRenderFixed mode env s0 view fixed
      Flat.FE_FnLit tags bodyId -> flatRenderFnLit mode env s0 view tags bodyId
      Flat.FE_UnsafeNullable xId -> flatPureChild mode env s0 view xId
      Flat.FE_FrozenLit gi -> flatRenderObjectLit mode env s0 view gi
      Flat.FE_GetField ti oId ->
        let
          (s1, Code d r) = flatPureChild mode env s0 view oId
         in
          (s1, Code d (jsDotOrBracket r (FlatView.firText view ti)))
      Flat.FE_Hvm2Ref ti ->
        (s0, Code mempty (hvm2ExportRef (FlatView.firText view ti)))
      knode ->
        case knode of
          Flat.FE_KConcat {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KPlus {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KTimes {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KMinus {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KNegate {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KFracDiv {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KRem {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KBitAnd {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KBitOr {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KBitXor {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KShl {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KShr {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KUShr {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KBig {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KBigNeg {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KAnd {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KOr {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KEq {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KNEq {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KGTh {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KLTh {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KGTEq {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KLTEq {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KShow {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_KTypeOf {} -> flatRenderKernel mode env s0 view knode
          Flat.FE_MethMap {} -> flatRenderMethod mode env s0 view knode
          Flat.FE_MethFilter {} -> flatRenderMethod mode env s0 view knode
          Flat.FE_MethReduce {} -> flatRenderMethod mode env s0 view knode
          Flat.FE_MethReduceRight {} -> flatRenderMethod mode env s0 view knode
          Flat.FE_MethToSorted {} -> flatRenderMethod mode env s0 view knode
          Flat.FE_MethFrom {} -> flatRenderMethod mode env s0 view knode
          _ -> error "JShark.flatPureAST': unexpected node"

flatEffectfulAST' env s view nid = flatEffectfulASTGo DirectEmit env s view nid

flatEffectfulASTGo !mode !env !sIn view nid =
  let
    s0 = bumpEmitTick sIn
   in
    case FlatView.firNode view nid of
      Flat.FX_Lift eId -> flatPureChild mode env s0 view eId
      Flat.FX_FFI fi ai ->
        let
          (s1, argDecl, argRefs) = flatRenderArgList mode env s0 view ai
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
          (s1, Code xDecl xRef) = flatEffectChild mode env s0 view xId
         in
          (s1, Code xDecl $ jsDotOrBracket xRef (FlatView.firText view sId))
      Flat.FX_UnsafeObjectAssign xId yId ->
        let
          (s1, Code xDecl xRef) = flatEffectChild mode env s0 view xId
          (s2, Code yDecl yRef) = flatEffectChild mode env s1 view yId
         in
          (s2, fxCode (xDecl $$ yDecl) $ xRef <> " = " <> yRef)
      Flat.FX_CallMethod recvId methodIdx ai ->
        let
          method = FlatView.firText view methodIdx
          (s1, Code rDecl rRef) = flatEffectChild mode env s0 view recvId
          (s2, argDecl, argRefs) = flatRenderArgList mode env s1 view ai
         in
          ( s2
          , fxCode
              (rDecl $$ argDecl)
              (rRef <> "." <> jsText method <> parens argRefs)
          )
      Flat.FX_Bind tag xId bodyId ->
        flatBindEffect mode env s0 view nid tag xId bodyId
      Flat.FX_ThenE xId yId -> flatSeqEffect mode env s0 view xId yId
      Flat.FX_BindRec tag rId bId ->
        let
          (nBind, s1) = flatPlanIdent mode s0 nid
          n = nJS nBind
          env' = IM.insert tag nBind env
          (s2, MkCode rDecl rRef _) = flatEffectChild mode env' s1 view rId
          (s3, MkCode bDecl bRef bFX) = flatEffectChild mode env' s2 view bId
         in
          ( s3
          , MkCode (Just (recBindStmt n rDecl rRef $$ fromMaybe mempty bDecl)) bRef bFX
          )
      Flat.FX_LambdaE tag bodyId ->
        let
          (nParam, s1) = flatPlanIdent mode s0 nid
          env' = IM.insert tag nParam env
          (s2, MkCode exprXDecl exprXRef _) = flatEffectChild mode env' s1 view bodyId
          (s3, fnJs) =
            emitHoistedFnValue s2 view nid (renderFunction nParam exprXDecl exprXRef)
         in
          (s3, Code mempty fnJs)
      Flat.FX_ApplyE fId xId ->
        let
          (s1, Code fDecl fRef) = flatEffectChild mode env s0 view fId
          (s2, Code xDecl xRef) = flatEffectChild mode env s1 view xId
         in
          (s2, fxCode (fDecl $$ xDecl) (jsCall fRef xRef))
      Flat.FX_IfE cId tId eId ->
        emitBranching
          ( flatIsUnitEffect view tId
              && flatIsUnitEffect view eId
          )
          s0
          ( \s ->
              let
                (s1, Code cDecl cRef) = flatEffectChild mode env s view cId
               in
                (s1, cDecl, cRef)
          )
          ( \mRes cRef s ->
              let
                (s1, MkCode tDecl tRef _) = flatEffectChild mode env s view tId
                (s2, MkCode eDecl eRef _) = flatEffectChild mode env s1 view eId
               in
                (s2, ifAssignOrStmt mRes cRef tDecl tRef eDecl eRef)
          )
      Flat.FX_While cId bId ->
        let
          (s1, MkCode condDecl condRef _) = flatEffectChild mode env s0 view cId
          (s2, MkCode bodyDecl bodyRef _) = flatEffectChild mode env s1 view bId
          bodyStmt = asStmt bodyDecl bodyRef
          whileStmt =
            "while"
              <+> parens (fromMaybe mempty condRef)
              <+> blockBody bodyStmt
         in
          (s2, MkCode (Just (fromMaybe mempty condDecl $$ whileStmt)) Nothing False)
      Flat.FX_ForRange startId endId tag bodyId ->
        let
          (s1, MkCode startDecl startRef _) = flatPureChild mode env s0 view startId
          (s2, MkCode endDecl endRef _) = flatPureChild mode env s1 view endId
          (loopN, s3) = flatPlanIdent mode s2 nid
          loopVar = nJS loopN
          env' = IM.insert tag loopN env
          (s4, MkCode bodyDecl bodyRef _) = flatEffectChild mode env' s3 view bodyId
          bodyStmt = asStmt bodyDecl bodyRef
          forHead =
            "let"
              <+> loopVar
              <+> "="
              <+> fromMaybe mempty startRef
              <+> ";"
              <+> loopVar
              <+> "<"
              <+> fromMaybe mempty endRef
              <+> ";"
              <+> loopVar
              <+> "++"
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
          (s1, Code bDecl bRef) = flatPureChild mode env s0 view bufId
          (s2, Code iDecl iRef) = flatPureChild mode env s1 view idxId
          (s3, Code vDecl vRef) = flatPureChild mode env s2 view valId
          stmt = (bRef <> brackets iRef) <+> "=" <+> vRef
         in
          (s3, Code (bDecl $$ iDecl $$ vDecl $$ (stmt <> semi)) mempty)
      Flat.FX_U8Fill bufId valId ->
        let
          (s1, Code bDecl bRef) = flatPureChild mode env s0 view bufId
          (s2, Code vDecl vRef) = flatPureChild mode env s1 view valId
          stmt = bRef <> ".fill" <> parens vRef
         in
          (s2, Code (bDecl $$ vDecl $$ (stmt <> semi)) mempty)
      Flat.FX_OptionCaseE oId nId tag sId ->
        emitBranching
          ( flatIsUnitEffect view nId
              && flatIsUnitEffect view sId
          )
          s0
          ( \s ->
              let
                (s1, Code oDecl oRef) = flatPureChild mode env s view oId
                (nBind, s2) = flatPlanIdent mode s1 nid
               in
                (s2, oDecl $$ constBind nBind oRef, nBind)
          )
          ( \mRes nBind s ->
              let
                env' = IM.insert tag nBind env
                (s1, MkCode nDecl nRef _) = flatEffectChild mode env s view nId
                (s2, MkCode sDecl sRef _) = flatEffectChild mode env' s1 view sId
                cond = nJS nBind <+> "===" <+> "null"
               in
                (s2, ifAssignOrStmt mRes cond nDecl nRef sDecl sRef)
          )
      Flat.FX_ResultCaseE resId tagE errId tagO okId ->
        flatRenderResultCaseE mode env s0 view nid resId tagE errId tagO okId
      Flat.FX_StringCaseE scrutId ai defId ->
        flatRenderStringCaseE mode env s0 view nid scrutId ai defId
      Flat.FX_Throw xId ->
        let
          (s1, Code xDecl xRef) = flatPureChild mode env s0 view xId
         in
          (s1, Code (xDecl $$ (("throw" <+> xRef) <> semi)) mempty)
      Flat.FX_Try aId tag kId ->
        emitBranching
          (flatIsUnitEffect view aId && flatIsUnitEffect view kId)
          s0
          (\s -> (s, mempty, ()))
          ( \mRes () s ->
              let
                (s1, MkCode aDecl aRef _) = flatEffectChild mode env s view aId
                (catchN, s2) = flatPlanIdent mode s1 nid
                env' = IM.insert tag catchN env
                (s3, MkCode bDecl bRef _) = flatEffectChild mode env' s2 view kId
               in
                (s3, tryCatchStmt mRes catchN aDecl aRef bDecl bRef)
          )
      Flat.FX_ObjectLit gi -> flatRenderObjectLit mode env s0 view gi
      Flat.FX_DeleteProp oId kId ->
        let
          (s1, Code oDecl oRef) = flatEffectChild mode env s0 view oId
          (s2, Code kDecl kRef) = flatPureChild mode env s1 view kId
         in
          (s2, fxCode (oDecl $$ kDecl) (("delete" <+> oRef) <> brackets kRef))
      Flat.FX_ArrayLit es -> flatRenderArrayLit mode env s0 view es
      _ -> error "JShark.flatEffectfulAST': unexpected node"

flatEffectfulCodegenFromView soa =
  let
    root = FlatView.firRoot soa
    total = FlatView.firNodeCount soa
   in
    if root < 0 || root >= total
      then error "JShark.flatEffectfulCodegen: invalid root node"
      else
        let
          (plan, s1) = buildFlatEmitPlan soa root startCG
         in
          flatEmitLayered soa root plan s1
{-# NOINLINE flatEffectfulCodegenFromView #-}

flatEffectfulCodegen ::
  ClosedEffect u -> (CG, Code)
flatEffectfulCodegen (e :: ClosedEffect u) =
  let
    !(soa, _s0) = unsafePerformIO (prepareFlatEffectProgram e)
   in
    flatEffectfulCodegenFromView soa
{-# NOINLINE flatEffectfulCodegen #-}

effectfulASTFromFlat :: ClosedEffect u -> JS
effectfulASTFromFlat e = uncurry renderWithHelpers (flatEffectfulCodegen e)

effectfulASTFromSoA :: FlatSoA.FlatSoA -> JS
effectfulASTFromSoA soa =
  uncurry renderWithHelpers (flatEffectfulCodegenFromView soa)

effectfulAST :: ClosedEffect u -> JS
effectfulAST = effectfulASTFromFlat

effectfulASTIr = effectfulASTFromFlat

-- | Stmt-only codegen for branching effects (no shared @let result@).
