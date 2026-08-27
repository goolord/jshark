{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds -Wno-pattern-namespace-specifier -Wno-missing-export-lists -Wno-missing-signatures -Wno-unused-imports -Wno-type-defaults -Wno-incomplete-patterns #-}

-- | Direct PHOAS → JavaScript (pure snippets and legacy paths).
module JShark.Codegen.Phoas where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Bits (xor, (.&.), (.|.))
import Data.Char (isDigit)
import qualified Data.Char as Char
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', mapAccumL, nub, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Monoid (All (..), Any (..), Sum (..))
import Data.Proxy (Proxy (..))
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable (Typeable, type (:~:) (Refl))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Clock (getMonotonicTime)
import qualified GHC.IO as GHCIO
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits (KnownSymbol, sameSymbol, symbolVal)
import JShark.Binder
  ( Stamp (..)
  , nestedDummy
  , nestedDummyId
  , peelBoolEffect
  , peelOption
  , peelResult
  , peelString
  , stampId
  , pattern Name
  )
import JShark.Codegen.Core
import JShark.CompileProgress
  ( EmitCtx
  , captureEmitCtx
  , initEmitCtxTotal
  , recordJobFlatPrepare
  , recordJobPhoasPrepare
  , reportFlatOptPhase
  , reportIrPreparePhase
  , reportPackPhase
  , tickEmitCtx
  )
import JShark.CompileTiming
  ( FlatOptProfile (..)
  , FlatPrepareTiming (..)
  , IrOptProfile (..)
  , LowerProfile (..)
  , PhoasPrepareTiming (..)
  , reportFlatPrepareTiming
  , reportPhoasPrepareTiming
  , seconds
  )
import JShark.Emit
  ( JS
  , blockBody
  , braces
  , brackets
  , colon
  , dquotes
  , hcat
  , iifeBody
  , jsDouble
  , jsString
  , jsText
  , nonEmpty
  , parens
  , punctuate
  , renderJS
  , renderJSCompact
  , semi
  , vcat
  , vcatNonEmpty
  , ($$)
  , (<+>)
  )
import JShark.Evaluate
  ( bigOpJS
  , eqFoldableValue
  , escapeJsString
  , evaluate
  , evaluateBigInt
  , evaluateCached
  , evaluateNumber
  , foldFixed
  , isFiniteDouble
  , isOrderableValue
  , jsBigIntLit
  , jsQuote
  , jsShow
  , jsUint8ArrayLit
  , mapFixedArgs
  , parseBigIntString
  , tryEvalBigBin
  , typeOfValue
  , valueCompare
  , valueEq
  )
import qualified JShark.Flat as Flat
import qualified JShark.FlatSoA as FlatSoA
import qualified JShark.FlatView as FlatView
import JShark.Flatten (flattenEff, flattenExpr)
import JShark.Hoist (registerHoistedTag)
import qualified JShark.Ir as Ir
import JShark.JsNum (jsBit2, jsRem, jsShl, jsShr, jsUShr)
import JShark.Optimize
  ( bindProbeTag
  , evalFnBody
  , fnArity
  , letProbeTag
  , optimize
  )
import JShark.Prim
  ( MathBinary (..)
  , MathUnary (..)
  , isPureFixed
  , matchMathBinary
  , matchMathUnary
  )
import qualified JShark.Prim as Prim
import JShark.Rec
import JShark.Types
import Unsafe.Coerce (unsafeCoerce)

probeContEff ::
  CG -> (Stamp u -> Effect Stamp v) -> (CG, Effect Stamp v, Int)
probeContEff s f =
  let
    (probeTag, s') = allocTag s
    probed = f (Stamp probeTag)
   in
    (s', probed, probeTag)

probeContExpr ::
  CG -> (Stamp u -> Expr Stamp v) -> (CG, Expr Stamp v, Int)
probeContExpr s g =
  let
    (probeTag, s') = allocTag s
    probed = g (Stamp probeTag)
   in
    (s', probed, probeTag)

liveBinder :: Effect Stamp u -> Maybe Int
liveBinder (Lift e) = liveBinderExpr e
liveBinder _ = Nothing

liveBinderExpr :: Expr Stamp u -> Maybe Int
liveBinderExpr (Var (EmbedEff e)) = liveBinder e
liveBinderExpr (Var (Stamp n)) | n >= 0 = Just n
liveBinderExpr (UnsafeNullable e) = liveBinderExpr e
liveBinderExpr _ = Nothing

seqEffectCode ::
  Env -> CG -> Effect Stamp u -> Effect Stamp v -> (CG, Code)
seqEffectCode env s0 x y =
  let
    (s1, MkCode xDecl xRef xFX) = effectfulAST' env s0 x
    (s2, MkCode yDecl yRef yFX) = effectfulAST' env s1 y
    -- Value-producing effects (ifE) put work in xDecl and leave a result
    -- ident in xRef (codeRefFX False). Assignments and calls keep the
    -- side effect in xRef (fxCode).
    stmt
      | isNothing xRef = fromMaybe mempty xDecl
      | not xFX && isJust xDecl = fromMaybe mempty xDecl
      | otherwise = asStmt xDecl xRef
   in
    (s2, MkCode (Just (stmt $$ fromMaybe mempty yDecl)) yRef yFX)

-- Bind of an Effect: when the continuation uses the binder once in a
-- strict position, splice the effect in place (so `x <- getEl; x.foo()`
-- becomes `getEl().foo()`); when never, keep it as a statement.
-- Apply `f` once at the optimizer tag; map that tag to the emitted ident
-- via `env` instead of `renameEff` (which copied the whole continuation).
bindEffectCode ::
  Env -> CG -> Effect Stamp u -> (Stamp u -> Effect Stamp v) -> (CG, Code)
bindEffectCode env s0 x f =
  let
    (sProbe, tagged, probeTag) = probeContEff s0 f
    (bTag, used) = bindProbeTag probeTag tagged
    (s1, MkCode xDecl xRef xFX) = effectfulAST' env sProbe x
    stmtX
      | isNothing xRef = fromMaybe mempty xDecl
      | not xFX && isJust xDecl = fromMaybe mempty xDecl
      | otherwise = asStmt xDecl xRef
    insertBinder env0 n =
      IM.insert bTag n $ if bTag == probeTag then env0 else IM.insert probeTag n env0
   in
    case xRef of
      Nothing ->
        if not used
          then
            let
              (s2, MkCode yDecl yRef yFX) = effectfulAST' env s1 tagged
             in
              (s2, MkCode (Just (stmtX $$ fromMaybe mempty yDecl)) yRef yFX)
          else
            let
              n = fromMaybe nestedDummyId (liveBinder x)
              (env', sBind, bindJS) =
                if n /= nestedDummyId
                  then (insertBinder env n, s1, mempty)
                  else
                    let
                      (nBind, s2) = allocIdent s1
                     in
                      (insertBinder env nBind, s2, mempty)
              (s3, MkCode yDecl yRef yFX) = effectfulAST' env' sBind tagged
             in
              ( s3
              , MkCode (Just (stmtX $$ bindJS $$ fromMaybe mempty yDecl)) yRef yFX
              )
      Just _ ->
        if not used
          then
            let
              (s2, MkCode yDecl yRef yFX) = effectfulAST' env s1 tagged
             in
              (s2, MkCode (Just (stmtX $$ fromMaybe mempty yDecl)) yRef yFX)
          else
            let
              (nBind, s2) = allocIdent s1
              env' = insertBinder env nBind
              (s3, MkCode yDecl yRef yFX) = effectfulAST' env' s2 tagged
             in
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

-- Flat codegen ('flatPureAST'' / 'flatEffectfulAST'') mirrors the PHOAS
-- emitters above; keep in sync — 'irParityTests' diff the two paths.
isUnitWitness = \case
  Lift (Literal ValueUnit) -> True
  Lift (Var (EmbedEff e)) -> isUnitWitness e
  Lift _ -> False
  While _ b -> isUnitWitness b
  ForRange _ _ b -> isUnitWitness (b nestedDummy)
  Bind _ f -> isUnitWitness (f nestedDummy)
  ThenE _ y -> isUnitWitness y
  BindRec _ f -> isUnitWitness (f nestedDummy)
  IfE _ t e -> isUnitWitness t && isUnitWitness e
  OptionCaseE _ n s -> isUnitWitness n && isUnitWitness (s nestedDummy)
  ResultCaseE _ e s ->
    isUnitWitness (e nestedDummy) && isUnitWitness (s nestedDummy)
  StringCaseE _ arms d -> all (isUnitWitness . snd) arms && isUnitWitness d
  Throw {} -> True
  Try a k -> isUnitWitness a && isUnitWitness (k nestedDummy)
  _ -> False

-- | Turn a rendered effect into a statement. Unit values may still have a
-- non-empty ref (@el.x = v@, @foo()@); those become statements, not
-- @let n = …@.
asStmt mDecl mRef = case mRef of
  Nothing -> fromMaybe mempty mDecl
  Just r -> fromMaybe mempty mDecl $$ (r <> semi)

ifElseStmt cRef tDecl tRef eDecl eRef
  | isNothing eDecl && isNothing eRef =
      "if" <+> parens cRef <+> blockBody (asStmt tDecl tRef)
  | otherwise =
      "if"
        <+> parens cRef
        <+> blockBody (asStmt tDecl tRef)
        $$ "else"
        <+> blockBody (asStmt eDecl eRef)

assignResult resultVar mRef = case mRef of
  Nothing -> mempty
  Just r -> (jsText resultVar <+> "=" <+> r) <> semi

letResult resultVar = ("let" <+> jsText resultVar) <> semi

recBindStmt n rDecl rRef =
  fromMaybe mempty rDecl
    $$ (("const" <+> n <+> "=" <+> fromMaybe mempty rRef) <> semi)

resultCasePrelude env s0 res =
  let
    (s1, MkCode rDecl rRef _) = pureAST' s0 env res
    (nObj, s2) = allocIdent s1
    (nUnw, s3) = allocIdent s2
    obj = nName nObj
    prelude =
      fromMaybe mempty rDecl
        $$ constBind nObj (fromMaybe mempty rRef)
        $$ constBind nUnw (jsText obj <> ".value")
   in
    (s3, prelude, obj, nUnw)

-- | Unit arms: prelude + stmt, empty ref. Value arms: prelude +
-- @let result@ + stmt, result ident.
emitBranching unit s0 prelude k
  | unit =
      let
        (s1, pre, extra) = prelude s0
        (s2, stmt) = k Nothing extra s1
       in
        (s2, MkCode (Just (pre $$ stmt)) Nothing False)
  | otherwise =
      let
        (s1, pre, extra) = prelude s0
        (n, s2) = allocIdent s1
        rv = nName n
        (s3, stmt) = k (Just rv) extra s2
       in
        (s3, MkCode (Just (pre $$ letResult rv $$ stmt)) (Just (jsText rv)) False)

ifAssignOrStmt ::
  Maybe Text
  -> JS
  -> Maybe JS
  -> Maybe JS
  -> Maybe JS
  -> Maybe JS
  -> JS
ifAssignOrStmt Nothing c tD tR eD eR = ifElseStmt c tD tR eD eR
ifAssignOrStmt (Just rv) c tD tR eD eR =
  "if"
    <+> parens c
    <+> blockBody (fromMaybe mempty tD $$ assignResult rv tR)
    $$ "else"
    <+> blockBody (fromMaybe mempty eD $$ assignResult rv eR)

tryCatchStmt mRes catchN aDecl aRef bDecl bRef =
  let
    catchHead = "catch" <+> parens (nJS catchN)
   in
    case mRes of
      Nothing ->
        "try"
          <+> blockBody (asStmt aDecl aRef)
          $$ (catchHead <+> blockBody (asStmt bDecl bRef))
      Just rv ->
        "try"
          <+> blockBody (fromMaybe mempty aDecl $$ assignResult rv aRef)
          $$ (catchHead <+> blockBody (fromMaybe mempty bDecl $$ assignResult rv bRef))

renderFunction nParam decl ref =
  "function"
    <+> parens (nJS nParam)
    <+> blockBody (fromMaybe mempty decl $$ ret)
 where
  -- Empty ref is Unit (event handlers, forEach of noOp). `return ()`
  -- is a SyntaxError; HughesPJ `parens` of empty is `()`.
  ret = case ref of
    Nothing -> "return"
    Just r -> "return" <+> parens r

-- | @function (n0, n1) { decls return ref }@ — callback style (bare return).
jsCallback params decl ref =
  "function"
    <+> parens (hcat (punctuate ", " params))
    <+> blockBody (decl $$ "return" <+> ref)

renderFFIForm = \case
  FFICall s -> jsText s
  FFILambda s -> parens (jsText s)

-- | Multi-parameter arrow lambdas are invalid IIFEs as @(...=>{...})(a,b)@;
--   wrap the lambda in an extra pair of parens so the call applies cleanly.
--   Parenthesized arrows from 'classifyFFI' become 'FFICall'; only wrap twice
--   when the callee is not already a whole parenthesized expression.
renderFFIInvoke fn argRefs = case fn of
  FFILambda s -> parens (jsText s) <> parens argRefs
  FFICall s ->
    let
      callee = jsText s
     in
      if "=>" `T.isInfixOf` s && not (isWholeParenthesized s)
        then parens callee <> parens argRefs
        else callee <> parens argRefs

-- | True when @t@ is @(… )@ with balanced outer parentheses only.
isWholeParenthesized t =
  case T.uncons t of
    Nothing -> False
    Just ('(', rest) ->
      case T.unsnoc rest of
        Nothing -> False
        Just (inner, ')') -> parenBalanced inner (0 :: Int)
        Just _ -> False
    _ -> False
 where
  parenBalanced txt depth =
    case T.uncons txt of
      Nothing -> depth == 0
      Just ('(', rest) -> parenBalanced rest (depth + 1)
      Just (')', rest)
        | depth == 0 -> False
        | otherwise -> parenBalanced rest (depth - 1)
      Just (_, rest) -> parenBalanced rest depth

effectfulAST' :: forall v. Env -> CG -> Effect Stamp v -> (CG, Code)
effectfulAST' !env !sIn eff =
  let
    s0 = bumpEmitTick sIn
   in
    case eff of
      Lift x -> pureAST' s0 env x
      FFI fn args ->
        let
          (s1, argDecl, argRefs) = renderArgList env s0 args
         in
          (s1, fxCode argDecl (renderFFIInvoke fn argRefs))
      IfE c t e ->
        -- Value-producing @if@: a shared result var is assigned in both
        -- arms. Do not use emptiness to pick a ternary — a Unit leftover
        -- ref is not a genuinely-empty Doc.
        emitBranching
          (isUnitWitness t && isUnitWitness e)
          s0
          ( \s ->
              let
                (s1, Code cDecl cRef) = effectfulAST' env s c
               in
                (s1, cDecl, cRef)
          )
          ( \mRes cRef s ->
              let
                (s1, MkCode tDecl tRef _) = effectfulAST' env s t
                (s2, MkCode eDecl eRef _) = effectfulAST' env s1 e
               in
                (s2, ifAssignOrStmt mRes cRef tDecl tRef eDecl eRef)
          )
      While cond body ->
        let
          (s1, MkCode condDecl condRef _) = effectfulAST' env s0 cond
          (s2, MkCode bodyDecl bodyRef _) = effectfulAST' env s1 body
          bodyStmt = asStmt bodyDecl bodyRef
          whileStmt = "while" <+> parens (fromMaybe mempty condRef) <+> blockBody bodyStmt
         in
          (s2, MkCode (Just (fromMaybe mempty condDecl $$ whileStmt)) Nothing False)
      ForRange start end body ->
        let
          (s1, MkCode startDecl startRef _) = pureAST' s0 env start
          (s2, MkCode endDecl endRef _) = pureAST' s1 env end
          (loopN, s3) = allocIdent s2
          loopVar = nJS loopN
          (s4, MkCode bodyDecl bodyRef _) = effectfulAST' env s3 (body (Name loopN))
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
              (Just (fromMaybe mempty startDecl $$ fromMaybe mempty endDecl $$ forStmt))
              Nothing
              False
          )
      U8Set buf idx val ->
        let
          (s1, Code bDecl bRef) = pureAST' s0 env buf
          (s2, Code iDecl iRef) = pureAST' s1 env idx
          (s3, Code vDecl vRef) = pureAST' s2 env val
          stmt = (bRef <> brackets iRef) <+> "=" <+> vRef
         in
          (s3, Code (bDecl $$ iDecl $$ vDecl $$ (stmt <> semi)) mempty)
      U8Fill buf val ->
        let
          (s1, Code bDecl bRef) = pureAST' s0 env buf
          (s2, Code vDecl vRef) = pureAST' s1 env val
          stmt = bRef <> ".fill" <> parens vRef
         in
          (s2, Code (bDecl $$ vDecl $$ (stmt <> semi)) mempty)
      OptionCaseE opt noneE someF ->
        let
          (sProbe, tagged, binderTag) = probeContEff s0 someF
         in
          emitBranching
            (isUnitWitness noneE && isUnitWitness (someF nestedDummy))
            sProbe
            ( \s ->
                let
                  (s1, Code oDecl oRef) = pureAST' s env opt
                  (nBind, s2) = allocIdent s1
                 in
                  (s2, oDecl $$ constBind nBind oRef, nBind)
            )
            ( \mRes nBind s ->
                let
                  env' = IM.insert binderTag nBind env
                  (s1, MkCode nDecl nRef _) = effectfulAST' env s noneE
                  (s2, MkCode sDecl sRef _) = effectfulAST' env' s1 tagged
                  cond = nJS nBind <+> "===" <+> "null"
                 in
                  (s2, ifAssignOrStmt mRes cond nDecl nRef sDecl sRef)
            )
      Try a k ->
        let
          (sProbe, tagged, binderTag) = probeContEff s0 k
         in
          emitBranching
            (isUnitWitness a && isUnitWitness (k nestedDummy))
            sProbe
            (\s -> (s, mempty, ()))
            ( \mRes () s ->
                let
                  (s1, MkCode aDecl aRef _) = effectfulAST' env s a
                  (catchN, s2) = allocIdent s1
                  env' = IM.insert binderTag catchN env
                  (s3, MkCode bDecl bRef _) = effectfulAST' env' s2 tagged
                 in
                  (s3, tryCatchStmt mRes catchN aDecl aRef bDecl bRef)
            )
      Bind x f -> bindEffectCode env s0 x f
      ThenE x y -> seqEffectCode env s0 x y
      BindRec r b ->
        let
          (nBind, s1) = allocIdent s0
          n = nJS nBind
          (s2, MkCode rDecl rRef _) = effectfulAST' env s1 (r (Name nBind))
          (s3, MkCode bDecl bRef bFX) = effectfulAST' env s2 (b (Name nBind))
         in
          ( s3
          , MkCode (Just (recBindStmt n rDecl rRef $$ fromMaybe mempty bDecl)) bRef bFX
          )
      Throw x ->
        let
          (s1, Code xDecl xRef) = pureAST' s0 env x
         in
          (s1, Code (xDecl $$ (("throw" <+> xRef) <> semi)) mempty)
      ObjectLit fs -> renderObjectLit env s0 fs
      ArrayLit es -> renderArrayLit env s0 es
      DeleteProp o k ->
        let
          (s1, Code oDecl oRef) = effectfulAST' env s0 o
          (s2, Code kDecl kRef) = pureAST' s1 env k
         in
          (s2, fxCode (oDecl $$ kDecl) (("delete" <+> oRef) <> brackets kRef))
      ResultCaseE res errF okF -> renderResultCaseE env s0 res errF okF
      StringCaseE scrut arms def -> renderStringCaseE env s0 scrut arms def
      UnsafeObject obj -> (s0, Code mempty (jsText obj))
      UnsafeObjectGet x string ->
        let
          (s1, Code x1Decl x1Ref) = effectfulAST' env s0 x
         in
          (s1, Code x1Decl $ jsDotOrBracket x1Ref string)
      UnsafeObjectAssign x y ->
        let
          (s1, Code x1Decl x1Ref) = effectfulAST' env s0 x
          (s2, Code y1Decl y1Ref) = effectfulAST' env s1 y
         in
          (s2, fxCode (x1Decl $$ y1Decl) $ x1Ref <> " = " <> y1Ref)
      CallMethod recv name args ->
        let
          (s1, Code rDecl rRef) = effectfulAST' env s0 recv
          (s2, argDecl, argRefs) = renderArgList env s1 args
         in
          (s2, fxCode (rDecl $$ argDecl) (rRef <> "." <> jsText name <> parens argRefs))
      LambdaE f -> emitEffectLambda env s0 f
      ApplyE fex ex ->
        let
          (s1, Code exprXDecl exprXRef) = effectfulAST' env s0 fex
          (s2, Code exprYDecl exprYRef) = effectfulAST' env s1 ex
         in
          (s2, fxCode (exprXDecl $$ exprYDecl) (jsCall exprXRef exprYRef))

letCode env s0 x g =
  let
    (sProbe, tagged, probeTag) = probeContExpr s0 g
    (binderTag, uses) = letProbeTag probeTag tagged
    (s1, MkCode xDecl xRef _) = pureAST' sProbe env x
   in
    case uses of
      0 ->
        let
          (s2, y) = pureAST' s1 env tagged
          stmt
            | isNothing xDecl && isJust xRef = fromMaybe mempty xRef <> semi
            | otherwise = fromMaybe mempty xDecl
         in
          (s2, keepRef (stmt $$ fromMaybe mempty (codeDecl y)) y)
      _ ->
        let
          (nBind, s2) = allocIdent s1
          env' = IM.insert binderTag nBind env
          (s3, y) = pureAST' s2 env' tagged
         in
          ( s3
          , keepRef
              ( fromMaybe mempty xDecl
                  $$ constBind nBind (fromMaybe mempty xRef)
                  $$ fromMaybe mempty (codeDecl y)
              )
              y
          )

pureAST :: ClosedExpr u -> JS
pureAST e = uncurry renderWithHelpers (pureAST' startCG IM.empty (optimize e))

pureAST' ::
  forall v.
  CG
  -> Env
  -> Expr Stamp v
  -> (CG, Code)
pureAST' !sIn env expr =
  let
    s0 = bumpEmitTick sIn
   in
    case expr of
      Literal v -> case v of
        ValueNumber d -> (s0, Code mempty (jsDouble d))
        ValueBigInt n -> (s0, Code mempty (jsBigIntLit n))
        ValueArray xs ->
          let
            (s1, exprs) = mapAccumL (\s x -> pureAST' s env (Literal x)) s0 xs
           in
            ( s1
            , Code
                (codesDecls exprs)
                (brackets (hcat (punctuate ", " (codesRefs exprs))))
            )
        ValueString s -> (s0, Code mempty (jsQuote s))
        ValueFunction _ -> error "JShark.pureAST: ValueFunction is eval-only"
        ValueUnit -> (s0, mempty)
        ValueOption (Just x) -> pureAST' s0 env (Literal x)
        ValueOption Nothing -> (s0, Code mempty "null")
        ValueResult (Right x) -> renderResultLit True s0 x
        ValueResult (Left x) -> renderResultLit False s0 x
        ValueRegex s ->
          (s0, Code mempty ("new RegExp" <> parens (jsQuote s)))
        ValueUint8Array ba -> (s0, Code mempty (jsUint8ArrayLit ba))
        ValueBool True -> (s0, Code mempty "true")
        ValueBool False -> (s0, Code mempty "false")
        ValueFrozen {} -> error "JShark.pureAST: ValueFrozen is eval-only"
      Lambda (Just tag) f ->
        let
          (s1, fnJs) = emitHoistedLambdaValue env s0 f
          (s2, name) = registerHoistedTag s1 tag (renderJS fnJs)
         in
          (s2, Code mempty (jsText name))
      Lambda Nothing f -> emitExprLambda env s0 f
      -- `const` when shared or used under a lambda/loop/short-circuit.
      Let x g -> letCode env s0 x g
      LetRec r b ->
        let
          (nBind, s1) = allocIdent s0
          n = nJS nBind
          (s2, MkCode rDecl rRef _) = pureAST' s1 env (r (Name nBind))
          (s3, bCode) = pureAST' s2 env (b (Name nBind))
         in
          ( s3
          , keepRef (recBindStmt n rDecl rRef $$ fromMaybe mempty (codeDecl bCode)) bCode
          )
      Apply fex ex ->
        let
          (s1, Code exprXDecl exprXRef) = pureAST' s0 env fex
          (s2, Code exprYDecl exprYRef) = pureAST' s1 env ex
         in
          (s2, Code (exprXDecl $$ exprYDecl) (jsCall exprXRef exprYRef))
      Var (Embed e) -> pureAST' s0 env (flattenExpr e)
      Var (EmbedEff e) -> effectfulAST' env s0 e
      Var s ->
        -- Tags and the unused-binder dummy are negative; map via `env`.
        (s0, Code mempty (varStampJS env s))
      If c t e ->
        let
          (s1, Code cDecl cRef) = pureAST' s0 env c
          (s2, Code tDecl tRef) = pureAST' s1 env t
          (s3, Code eDecl eRef) = pureAST' s2 env e
         in
          ( s3
          , Code
              (cDecl $$ tDecl $$ eDecl)
              (parens (cRef <+> "?" <+> tRef <+> ":" <+> eRef))
          )
      OptionCase opt none' someF ->
        case opt of
          Var (Embed e) -> pureAST' s0 env (OptionCase (flattenExpr e) none' someF)
          Var s ->
            let
              i = stampId s
              optVar = nName i
              (s2, Code noneDecl noneRef) = pureAST' s0 env none'
              (s3, Code someDecl someRef) = pureAST' s2 env (someF (Name i))
             in
              ( s3
              , Code
                  (noneDecl $$ someDecl)
                  ( parens
                      (jsText optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef)
                  )
              )
          _ ->
            let
              (s1, Code optDecl optRef) = pureAST' s0 env opt
              (nBind, s2) = allocIdent s1
              optVar = nName nBind
              (s3, Code noneDecl noneRef) = pureAST' s2 env none'
              (s4, Code someDecl someRef) = pureAST' s3 env (someF (Name nBind))
             in
              ( s4
              , Code
                  (optDecl $$ constBind nBind optRef $$ noneDecl $$ someDecl)
                  ( parens
                      (jsText optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef)
                  )
              )
      ResultOk x ->
        let
          (s1, MkCode d r _) = pureAST' s0 env x
         in
          (s1, MkCode d (Just (resultObject True r)) False)
      ResultErr x ->
        let
          (s1, MkCode d r _) = pureAST' s0 env x
         in
          (s1, MkCode d (Just (resultObject False r)) False)
      ResultCase res errF okF -> renderResultCase env s0 res errF okF
      Index arr idx ->
        let
          s1 = useHelperSrc "$checkedIndex" jsCheckedIndexSrc s0
          (s2, Code aDecl aRef) = pureAST' s1 env arr
          (s3, Code iDecl iRef) = pureAST' s2 env idx
         in
          (s3, Code (aDecl $$ iDecl) (jsCheckedIndex aRef iRef))
      U8Index buf idx ->
        let
          (s1, Code bDecl bRef) = pureAST' s0 env buf
          (s2, Code iDecl iRef) = pureAST' s1 env idx
         in
          (s2, Code (bDecl $$ iDecl) (bRef <> brackets iRef))
      Error msg ->
        let
          (s1, Code d r) = pureAST' s0 env msg
         in
          (s1, Code d ("(function(){throw new Error(" <> r <> ");}())"))
      Std s -> renderStd env s0 s
      FnLit body -> renderFn env s0 body
      UnsafeNullable x -> pureAST' s0 env x
      FrozenLit fs -> renderObjectLit env s0 fs
      GetField @k o ->
        let
          (s1, Code d r) = pureAST' s0 env o
         in
          (s1, Code d (jsDotOrBracket r (T.pack (symbolVal (Proxy @k)))))
      Hvm2Kernel name _ ->
        (s0, Code mempty (hvm2ExportRef name))

isSimple :: Expr Stamp u -> Bool
isSimple = \case
  Literal {} -> True
  Var (EmbedEff e) -> isSimpleEffect e
  Var {} -> True
  Std (Kernel (KShow {})) -> True
  Std (Kernel (KTypeOf {})) -> True
  Std (Kernel (KNegate {})) -> True
  Std (Kernel (KBigNeg {})) -> True
  Std (Kernel _) -> False
  Std {} -> True
  FnLit {} -> True
  Index {} -> True
  U8Index {} -> True
  Error {} -> False
  UnsafeNullable x -> isSimple x
  FrozenLit {} -> True
  GetField {} -> True
  Hvm2Kernel {} -> True
  _ -> False

isSimpleEffect :: Effect Stamp u -> Bool
isSimpleEffect = \case
  Lift x -> isSimple x
  FFI {} -> True
  CallMethod {} -> True
  UnsafeObject {} -> True
  UnsafeObjectGet {} -> True
  ArrayLit es -> all isSimpleEffect es
  _ -> False

wrapOperand :: Expr Stamp u -> JS -> JS
wrapOperand e d = if isSimple e then d else parens d

hvm2ExportRef :: Text -> JS
hvm2ExportRef name =
  let
    key = dquotes (jsString (escapeJsString (T.unpack name)))
    err =
      dquotes
        (jsString (escapeJsString ("HVM2 kernel not loaded: " ++ T.unpack name)))
   in
    "((function(){var f=globalThis.__jsharkHvm2?.exports?.["
      <> key
      <> "];if(typeof f!==\"function\")return function(){throw new Error("
      <> err
      <> ")};"
      <> "function toI64(x){var buf=new ArrayBuffer(8);"
      <> "var f64=new Float64Array(buf);var i64=new BigInt64Array(buf);"
      <> "f64[0]=+x;return i64[0];}"
      <> "function fromOut(r){return typeof r===\"bigint\"?Number(r):r;}"
      <> "if(f.length>=2){return function(a){return function(b){"
      <> "return fromOut(f(toI64(a),toI64(b)));};};}"
      <> "return function(a){return fromOut(f(toI64(a)));};})())"

renderFixed ::
  Env
  -> CG
  -> FixedOp a b c u
  -> FixedArgs Stamp a b c
  -> (CG, Code)
renderFixed env s0 op args = case (op, args) of
  (n, ArgsU x)
    | Just name <- Prim.math1Name n ->
        let
          (s1, Code xDecl xRef) = pureAST' s0 env x
         in
          (s1, Code xDecl ("Math." <> jsText name <> parens xRef))
  (n, ArgsB x y)
    | Just name <- Prim.math2Name n ->
        let
          (s1, Code xDecl xRef) = pureAST' s0 env x
          (s2, Code yDecl yRef) = pureAST' s1 env y
         in
          ( s2
          , Code
              (xDecl $$ yDecl)
              ( "Math."
                  <> jsText name
                  <> parens (xRef <> ", " <> yRef)
              )
          )
  (n, ArgsU recv) ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 env recv
     in
      (s1, Code rDecl (Prim.fixedUnaryJS n (wrapOperand recv rRef)))
  (n, ArgsB recv arg) ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 env recv
      (s2, Code aDecl aRef) = pureAST' s1 env arg
     in
      (s2, Code (rDecl $$ aDecl) (Prim.fixedBinaryJS n (wrapOperand recv rRef) aRef))
  (n, ArgsT recv a b) ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 env recv
      (s2, Code aDecl aRef) = pureAST' s1 env a
      (s3, Code bDecl bRef) = pureAST' s2 env b
     in
      ( s3
      , Code
          (rDecl $$ aDecl $$ bDecl)
          (Prim.fixedTernaryJS n (wrapOperand recv rRef) aRef bRef)
      )

resultPayloadRef = fromMaybe "undefined"

resultObject isOk payload =
  let
    flag = if isOk then "true" else "false"
   in
    braces ((("ok:" <+> flag) <> ",") <+> ("value:" <+> resultPayloadRef payload))

renderResultLit isOk s0 x =
  let
    (s1, MkCode d r _) = pureAST' s0 IM.empty (Literal x)
   in
    (s1, MkCode d (Just (resultObject isOk r)) False)

renderArrayLit env s0 es =
  let
    (s1, cs) = mapAccumL (\s e -> effectfulAST' env s e) s0 es
   in
    ( s1
    , Code
        (codesDecls cs)
        (brackets (hcat (punctuate ", " (codesRefs cs))))
    )

renderObjectLit env s0 fs =
  let
    (s1, parts) =
      mapAccumL
        ( \s fl ->
            case fl of
              FieldLit e ->
                let
                  (s', Code d r) = pureAST' s env e
                 in
                  (s', (d, (dquotes (jsText (fieldKey fl)) <> ":") <+> r))
              FieldLitExtra e ->
                let
                  (s', Code d r) = pureAST' s env e
                 in
                  (s', (d, (dquotes (jsText (fieldKey fl)) <> ":") <+> r))
              FieldLitEffect e ->
                let
                  (s', MkCode d r _) = effectfulAST' env s e
                 in
                  ( s'
                  ,
                    ( fromMaybe mempty d
                    , (dquotes (jsText (fieldKey fl)) <> ":") <+> fromMaybe mempty r
                    )
                  )
              FieldLitExtraEffect e ->
                let
                  (s', MkCode d r _) = effectfulAST' env s e
                 in
                  ( s'
                  ,
                    ( fromMaybe mempty d
                    , (dquotes (jsText (fieldKey fl)) <> ":") <+> fromMaybe mempty r
                    )
                  )
        )
        s0
        fs
    (declList, pairs) = unzip parts
   in
    (s1, Code (vcatNonEmpty declList) (braces (hcat (punctuate ", " pairs))))

renderResultCase env s0 res errF okF =
  let
    (s1, prelude, obj, nUnw) = resultCasePrelude env s0 res
    (s2, Code eDecl eRef) = pureAST' s1 IM.empty (errF (Name nUnw))
    (s3, Code oDecl oRef) = pureAST' s2 IM.empty (okF (Name nUnw))
   in
    ( s3
    , Code
        (prelude $$ eDecl $$ oDecl)
        (parens ((jsText obj <> ".ok") <+> "?" <+> oRef <+> ":" <+> eRef))
    )

renderStringCaseE env s0 scrut arms def =
  let
    unit = all (isUnitWitness . snd) arms && isUnitWitness def
    (s1, Code oDecl oRef) = pureAST' s0 env scrut
    (resultN, s2) =
      if unit then (0, s1) else allocIdent s1
    resultVar = nName resultN
    renderArm s e =
      let
        (s', MkCode mDecl mRef _) = effectfulAST' env s e
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
                "case" <+> (jsQuote k <> colon) <+> blockBody (body <+> ("break" <> semi))
             in
              (s', line)
        )
        s2
        arms
    (s4, defBody) = renderArm s3 def
    defJS = "default:" <+> blockBody defBody
    switchStmt = "switch" <+> parens oRef <+> blockBody (vcat (caseJSs ++ [defJS]))
    prelude =
      if unit then oDecl else oDecl $$ letResult resultVar
    ref = if unit then Nothing else Just (jsText resultVar)
   in
    (s4, MkCode (Just (prelude $$ switchStmt)) ref False)

renderResultCaseE env s0 res errF okF =
  let
    (s1, errTagged, tagE) = probeContEff s0 errF
    (s2, okTagged, tagO) = probeContEff s1 okF
   in
    if isUnitWitness (errF nestedDummy) && isUnitWitness (okF nestedDummy)
      then
        let
          (s3, prelude, obj, _) = resultCasePrelude env s2 res
          (s4, MkCode eDecl eRef _) = effectfulAST' env s3 errTagged
          (s5, MkCode oDecl oRef _) = effectfulAST' env s4 okTagged
         in
          ( s5
          , Code
              (prelude $$ ifElseStmt (jsText obj <> ".ok") oDecl oRef eDecl eRef)
              mempty
          )
      else
        let
          (s3, prelude, obj, nUnw) = resultCasePrelude env s2 res
          (resultN, s4) = allocIdent s3
          resultVar = nName resultN
          envE = IM.insert tagE nUnw env
          envO = IM.insert tagO nUnw envE
          (s5, MkCode eDecl eRef _) = effectfulAST' envE s4 errTagged
          (s6, MkCode oDecl oRef _) = effectfulAST' envO s5 okTagged
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

shouldFlattenBinaryHoist :: (Stamp u -> Expr Stamp v) -> Bool
shouldFlattenBinaryHoist f =
  case f nestedDummy of
    Lambda Nothing g ->
      case g nestedDummy of
        Lambda Nothing _ -> False
        _ -> True
    _ -> False

emitBinaryHoistedLambda ::
  Env -> CG -> (Stamp u -> Expr Stamp v) -> (CG, Code)
emitBinaryHoistedLambda env s0 f =
  let
    go a b =
      case f a of
        Lambda Nothing g -> unsafeCoerce g b
        _ -> error "JShark.emitBinaryHoistedLambda: expected inner lambda"
    fnBody = JfCons (\a -> JfCons (\b -> JfNil (go a b)))
    (s1, Code _ fnJs) = renderFn env s0 fnBody
   in
    (s1, Code mempty fnJs)

emitHoistedLambdaValue ::
  Env -> CG -> (Stamp u -> Expr Stamp v) -> (CG, JS)
emitHoistedLambdaValue env s0 f =
  if shouldFlattenBinaryHoist f
    then
      let
        (s1, Code _ fnJs) = emitBinaryHoistedLambda env s0 f
       in
        (s1, fnJs)
    else
      let
        (s1, Code _ fnJs) = emitExprLambda env s0 f
       in
        (s1, fnJs)

renderFn :: forall us r. Env -> CG -> FnBody Stamp us r -> (CG, Code)
renderFn env s0 body =
  let
    n = fnArity body
    (ids, s1) = allocNIdents s0 n
    (s2, Code d r) = pureAST' s1 env (evalFnBody body ids)
   in
    (s2, Code mempty (jsCallback (map nJS ids) d r))

emitExprLambda env = emitLambdaWith (\s e -> pureAST' s env e)

emitEffectLambda env = emitLambdaWith (effectfulAST' env)

emitLambdaWith walker s0 f =
  let
    (nParam, s1) = allocIdent s0
    (s2, MkCode exprXDecl exprXRef _) = walker s1 (f (Name nParam))
   in
    (s2, Code mempty (renderFunction nParam exprXDecl exprXRef))

renderBinaryFn env s0 f =
  let
    (s1, Code _ cb) = renderFn env s0 (JfCons $ \a -> JfCons $ \b -> JfNil (f a b))
   in
    (s1, cb)

renderStd :: Env -> CG -> Std Stamp u -> (CG, Code)
renderStd env s0 = \case
  Fixed op args -> renderFixed env s0 op args
  Method m -> renderMethod env s0 m
  Kernel k -> renderKernel env s0 k

renderKernel :: Env -> CG -> Kernel Stamp u -> (CG, Code)
renderKernel env s0 = \case
  KConcat x y -> renderBin env "+" s0 x y
  KPlus x y -> renderBin env "+" s0 x y
  KMinus x y -> renderBin env "-" s0 x y
  KTimes x y -> renderBin env "*" s0 x y
  KFracDiv x y -> renderBin env "/" s0 x y
  KRem x y -> renderBin env "%" s0 x y
  KBitAnd x y -> renderBin env "&" s0 x y
  KBitOr x y -> renderBin env "|" s0 x y
  KBitXor x y -> renderBin env "^" s0 x y
  KShl x y -> renderBin env "<<" s0 x y
  KShr x y -> renderBin env ">>" s0 x y
  KUShr x y -> renderBin env ">>>" s0 x y
  KBig op x y -> renderBin env (bigOpJS op) s0 x y
  KBigNeg x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 env x
     in
      (s1, Code x1Decl $ "-" <> parens x1Ref)
  KShow x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 env x
     in
      (s1, Code x1Decl $ "String" <> parens x1Ref)
  KTypeOf x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 env x
      wrapped = case x of
        FrozenLit {} -> parens x1Ref
        _ -> x1Ref
     in
      (s1, Code x1Decl $ "typeof" <+> wrapped)
  KNegate x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 env x
     in
      (s1, Code x1Decl $ "-" <> parens x1Ref)
  KAnd x y -> renderBin env "&&" s0 x y
  KOr x y -> renderBin env "||" s0 x y
  KEq structural x y
    | structural ->
        renderBinApp env jsValueEq (useEqHelpers s0) x y
    | otherwise ->
        renderBin env "===" s0 x y
  KNEq structural x y
    | structural ->
        renderBinApp env jsValueNEq (useEqHelpers s0) x y
    | otherwise ->
        renderBin env "!==" s0 x y
  KGTh x y -> renderBin env ">" s0 x y
  KLTh x y -> renderBin env "<" s0 x y
  KGTEq x y -> renderBin env ">=" s0 x y
  KLTEq x y -> renderBin env "<=" s0 x y

renderMethod env s0 = \case
  MethMap recv f -> renderCallbackMethod env "map" s0 recv f
  MethFilter recv f -> renderCallbackMethod env "filter" s0 recv f
  MethReduce recv z f -> renderFold env ".reduce" s0 recv z f
  MethReduceRight recv z f -> renderFold env ".reduceRight" s0 recv z f
  MethToSorted recv f ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 env recv
      (s2, cb) = renderBinaryFn env s1 f
      call = wrapOperand recv rRef <> ".toSorted" <> parens cb
     in
      (s2, Code rDecl call)
  MethFrom n f ->
    let
      (s1, Code nDecl nRef) = pureAST' s0 env n
      (nHole, s2) = allocIdent s1
      (nI, s3) = allocIdent s2
      (s4, Code exDecl exRef) = pureAST' s3 env (f (Name nI))
      cb = jsCallback [nJS nHole, nJS nI] exDecl exRef
     in
      (s4, Code nDecl ("Array.from({length: " <> nRef <> "}, " <> cb <> ")"))

renderFold env method s0 recv z f =
  let
    (s1, Code rDecl rRef) = pureAST' s0 env recv
    (s2, Code zDecl zRef) = pureAST' s1 env z
    (s3, cb) = renderBinaryFn env s2 f
    call = wrapOperand recv rRef <> jsString method <> parens (cb <> ", " <> zRef)
   in
    (s3, Code (rDecl $$ zDecl) call)

renderCallbackMethod env name s0 recv f =
  let
    (s1, Code rDecl rRef) = pureAST' s0 env recv
    (nParam, s2) = allocIdent s1
    (s3, Code exDecl exRef) = pureAST' s2 env (f (Name nParam))
    call =
      wrapOperand recv rRef
        <> "."
        <> jsString name
        <> parens (jsCallback [nJS nParam] exDecl exRef)
   in
    (s3, Code rDecl call)

renderBin :: Env -> Text -> CG -> Expr Stamp a -> Expr Stamp b -> (CG, Code)
renderBin env op s0 x y =
  renderBinApp
    env
    (\l r -> wrapOperand x l <+> jsText op <+> wrapOperand y r)
    s0
    x
    y

renderBinApp ::
  Env
  -> (JS -> JS -> JS)
  -> CG
  -> Expr Stamp a
  -> Expr Stamp b
  -> (CG, Code)
renderBinApp env join s0 x y =
  let
    (s1, Code xDecl xRef) = pureAST' s0 env x
    (s2, Code yDecl yRef) = pureAST' s1 env y
   in
    (s2, Code (xDecl $$ yDecl) (join xRef yRef))

argAST :: Env -> CG -> Arg Stamp u -> (CG, Code)
argAST env s (ArgExpr e) = pureAST' s env e
argAST env s (ArgEffect e) = effectfulAST' env s e

renderArgList env s0 args =
  let
    (s1, cs) = recCodes (argAST env) s0 args
   in
    (s1, codesDecls cs, hcat (punctuate ", " (codesRefs cs)))
