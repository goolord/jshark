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

-- | Direct PHOAS → JavaScript (pure snippets and legacy paths).
module JShark.Compiler.Codegen.Phoas where

import qualified Data.IntMap.Strict as IM
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (symbolVal)
import qualified JShark.Api.Prim as Prim
import JShark.Api.Rec
import JShark.Api.Types
import JShark.Compiler.Binder
  ( Stamp (..)
  , nestedDummy
  , nestedDummyId
  , stampId
  , pattern Name
  )
import JShark.Compiler.Codegen.Core
import JShark.Compiler.Emit
  ( JS
  , blockBody
  , braces
  , brackets
  , colon
  , dquotes
  , hcat
  , jsString
  , jsText
  , parens
  , punctuate
  , renderJS
  , semi
  , vcat
  , vcatNonEmpty
  , ($$)
  , (<+>)
  )
import JShark.Compiler.Evaluate
  ( bigOpJS
  , escapeJsString
  , jsBigIntLit
  , jsQuote
  , jsUint8ArrayLit
  )
import JShark.Compiler.Flatten (flattenExpr)
import JShark.Compiler.Hoist (registerHoistedTag)
import JShark.Compiler.Lower (evalFnBody)
import JShark.Compiler.Optimize
  ( bindProbeTag
  , letProbeTag
  , optimizeWith
  )

data SomeExpr where
  SomeExpr :: Expr Stamp u -> SomeExpr

data SomeEffect where
  SomeEffect :: Effect Stamp u -> SomeEffect

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
                          $$ constBind s3 nBind (fromMaybe mempty xRef)
                          $$ fromMaybe mempty yDecl
                      )
                  )
                  yRef
                  yFX
              )

-- Flat codegen mirrors these PHOAS emitters.
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
    obj = identName s3 nObj
    prelude =
      fromMaybe mempty rDecl
        $$ constBind s3 nObj (fromMaybe mempty rRef)
        $$ constBind s3 nUnw (jsText obj <> ".value")
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
        rv = identName s2 n
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

tryCatchStmt mRes catchJs aDecl aRef bDecl bRef =
  let
    catchHead = "catch" <+> parens catchJs
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

renderFFIForm = \case
  FFICall s -> jsText s
  FFILambda s -> parens (jsText s)
  FFIExpr s -> jsText s

-- | Multi-parameter arrow lambdas are invalid IIFEs as @(...=>{...})(a,b)@;
--   wrap the lambda in an extra pair of parens so the call applies cleanly.
--   Parenthesized arrows from 'classifyFFI' become 'FFICall'; only wrap twice
--   when the callee is not already a whole parenthesized expression.
renderFFIInvoke fn argRefs = case fn of
  FFILambda s -> parens (jsText s) <> parens argRefs
  FFIExpr s -> jsText s
  FFICall s ->
    let
      callee = jsText s
     in
      if "=>" `T.isInfixOf` s && not (isWholeParenthesized s)
        then parens callee <> parens argRefs
        else callee <> parens argRefs

wholeParenInner :: Text -> Maybe Text
wholeParenInner t =
  case T.uncons t of
    Just ('(', rest) ->
      case T.unsnoc rest of
        Just (inner, ')') | parenBalanced inner (0 :: Int) -> Just inner
        _ -> Nothing
    _ -> Nothing

-- | True when @t@ is @(… )@ with balanced outer parentheses only.
isWholeParenthesized t =
  case wholeParenInner t of
    Just _ -> True
    Nothing -> False

parenBalanced :: Text -> Int -> Bool
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
        let
          unit = isUnitWitness t && isUnitWitness e
          (s1, MkCode cDecl cRef _) = effectfulAST' env s0 c
          (s2, MkCode tDecl tRef tFX) = effectfulAST' env s1 t
          (s3, MkCode eDecl eRef eFX) = effectfulAST' env s2 e
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
          (loopN, s3) = allocIdentHint s2 (Just "i")
          loopVar = nJS s3 loopN
          (s4, MkCode bodyDecl bodyRef _) = effectfulAST' env s3 (body (Name loopN))
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
                  (s2, oDecl $$ constBind s2 nBind oRef, nBind)
            )
            ( \mRes nBind s ->
                let
                  env' = IM.insert binderTag nBind env
                  (s1, MkCode nDecl nRef _) = effectfulAST' env s noneE
                  (s2, MkCode sDecl sRef _) = effectfulAST' env' s1 tagged
                  cond = nJS s nBind <+> "===" <+> "null"
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
                  (s3, tryCatchStmt mRes (nJS s3 catchN) aDecl aRef bDecl bRef)
            )
      Bind x f -> bindEffectCode env s0 x f
      ThenE x y -> seqEffectCode env s0 x y
      BindRec r b ->
        let
          (nBind, s1) = allocIdent s0
          n = nJS s1 nBind
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
      ApplyE fex ex -> emitApplyE env s0 fex ex

isIdentityTagged :: Int -> Expr Stamp u -> Bool
isIdentityTagged tag = \case
  Var (Stamp i) -> i == tag
  Var (Embed e) -> isIdentityTagged tag e
  Var (EmbedEff (Lift e)) -> isIdentityTagged tag e
  _ -> False

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
      1
        | isIdentityTagged binderTag tagged ->
            (s1, MkCode xDecl xRef False)
      _ ->
        let
          (nBind, s2) = allocIdent s1
          env' = IM.insert binderTag nBind env
          (s3, y) = pureAST' s2 env' tagged
         in
          ( s3
          , keepRef
              ( fromMaybe mempty xDecl
                  $$ constBind s3 nBind (fromMaybe mempty xRef)
                  $$ fromMaybe mempty (codeDecl y)
              )
              y
          )

pureAST :: ClosedExpr u -> JS
pureAST = pureASTWith idiomaticStyle

pureASTWith :: EmitStyle -> ClosedExpr u -> JS
pureASTWith style e =
  uncurry
    renderWithPreamble
    (pureAST' (startCGWith style) IM.empty (optimizeWith (esKeepLets style) e))

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
        ValueNumber d -> (s0, Code mempty (jsNumber (cgStyle s0) d))
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
      Lambda (LamInfo (Just tag) p) f ->
        let
          (s1, fnJs) = emitHoistedLambdaValue env s0 p f
          (s2, name) = registerHoistedTag s1 tag (renderJS fnJs)
         in
          (s2, Code mempty (jsText name))
      Lambda (LamInfo Nothing p) f -> emitExprLambda env s0 p f
      -- `const` when shared or used under a lambda/loop/short-circuit.
      Let x g -> letCode env s0 x g
      LetRec r b ->
        let
          (nBind, s1) = allocIdent s0
          n = nJS s1 nBind
          (s2, MkCode rDecl rRef _) = pureAST' s1 env (r (Name nBind))
          (s3, bCode) = pureAST' s2 env (b (Name nBind))
         in
          ( s3
          , keepRef (recBindStmt n rDecl rRef $$ fromMaybe mempty (codeDecl bCode)) bCode
          )
      Apply fex ex -> emitApply env s0 fex ex
      Var (Embed e) -> pureAST' s0 env (flattenExpr e)
      Var (EmbedEff e) -> effectfulAST' env s0 e
      Var s ->
        -- Tags and the unused-binder dummy are negative; map via `env`.
        (s0, Code mempty (varStampJS s0 env s))
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
              optVar = identName s0 i
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
              optVar = identName s2 nBind
              (s3, Code noneDecl noneRef) = pureAST' s2 env none'
              (s4, Code someDecl someRef) = pureAST' s3 env (someF (Name nBind))
             in
              ( s4
              , Code
                  (optDecl $$ constBind s2 nBind optRef $$ noneDecl $$ someDecl)
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
          (s1, Code aDecl aRef) = pureAST' s0 env arr
          (s2, Code iDecl iRef) = pureAST' s1 env idx
          (s3, call) = emitCheckedIndex s2 aRef iRef
         in
          (s3, Code (aDecl $$ iDecl) call)
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
      FnLit body -> renderFnBody env s0 body
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
                  (s', (d, (jsPropKey (cgStyle s') (fieldKey fl) <> ":") <+> r))
              FieldLitExtra e ->
                let
                  (s', Code d r) = pureAST' s env e
                 in
                  (s', (d, (jsPropKey (cgStyle s') (fieldKey fl) <> ":") <+> r))
              FieldLitEffect e ->
                let
                  (s', MkCode d r _) = effectfulAST' env s e
                 in
                  ( s'
                  ,
                    ( fromMaybe mempty d
                    , (jsPropKey (cgStyle s') (fieldKey fl) <> ":") <+> fromMaybe mempty r
                    )
                  )
              FieldLitExtraEffect e ->
                let
                  (s', MkCode d r _) = effectfulAST' env s e
                 in
                  ( s'
                  ,
                    ( fromMaybe mempty d
                    , (jsPropKey (cgStyle s') (fieldKey fl) <> ":") <+> fromMaybe mempty r
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
    resultVar = identName s2 resultN
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
          resultVar = identName s4 resultN
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

emitHoistedLambdaValue ::
  Env -> CG -> Maybe Text -> (Stamp u -> Expr Stamp v) -> (CG, JS)
emitHoistedLambdaValue env s0 hint f =
  let
    (s1, Code _ fnJs) =
      emitLambdaSpine True (\s e -> pureAST' s env e) s0 hint f
   in
    (s1, fnJs)

renderFnBody :: forall us r. Env -> CG -> FnBody Stamp us r -> (CG, Code)
renderFnBody env s0 body =
  let
    hints = fnBodyHints body
    (ids, s1) = allocNIdentsHints s0 hints
    (s2, Code d r) = pureAST' s1 env (evalFnBody body ids)
   in
    (s2, Code mempty (jsCallback s2 (map (nJS s2) ids) d r))

fnBodyHints :: FnBody Stamp us r -> [Maybe Text]
fnBodyHints = \case
  JfNil _ -> []
  JfCons pn k -> pn : fnBodyHints (k nestedDummy)

emitExprLambda env s0 hint f =
  emitLambdaSpine False (\s e -> pureAST' s env e) s0 hint f

emitApply env s0 f0 x0 =
  let
    (spineHead, args) = collectApply (SomeExpr f0) [SomeExpr x0]
    n = length args
   in
    case spineHead of
      SomeExpr fn
        | n > 1 && exprCallArity fn == n ->
            let
              (s1, Code fDecl fRef) = pureAST' s0 env fn
              (s2, argDecl, argRefs) = emitApplyArgsSome env s1 args
             in
              (s2, Code (fDecl $$ argDecl) (jsCallN fRef argRefs))
      _ ->
        let
          (s1, Code fDecl fRef) = pureAST' s0 env f0
          (s2, Code xDecl xRef) = pureAST' s1 env x0
         in
          (s2, Code (fDecl $$ xDecl) (jsCall fRef xRef))

emitApplyE env s0 f0 x0 =
  let
    (spineHead, args) = collectApplyE (SomeEffect f0) [SomeEffect x0]
    n = length args
   in
    case spineHead of
      SomeEffect fn
        | n > 1 && effectCallArity fn == n ->
            let
              (s1, Code fDecl fRef) = effectfulAST' env s0 fn
              (s2, argDecl, argRefs) = emitApplyArgsSomeE env s1 args
             in
              (s2, fxCode (fDecl $$ argDecl) (jsCallN fRef argRefs))
      _ ->
        let
          (s1, Code fDecl fRef) = effectfulAST' env s0 f0
          (s2, Code xDecl xRef) = effectfulAST' env s1 x0
         in
          (s2, fxCode (fDecl $$ xDecl) (jsCall fRef xRef))

emitApplyArgsSome env s0 xs =
  foldl'
    ( \(s, d, rs) (SomeExpr x) ->
        let
          (s', Code xd xr) = pureAST' s env x
         in
          (s', d $$ xd, rs ++ [xr])
    )
    (s0, mempty, [])
    xs

emitApplyArgsSomeE env s0 xs =
  foldl'
    ( \(s, d, rs) (SomeEffect x) ->
        let
          (s', Code xd xr) = effectfulAST' env s x
         in
          (s', d $$ xd, rs ++ [xr])
    )
    (s0, mempty, [])
    xs

collectApply :: SomeExpr -> [SomeExpr] -> (SomeExpr, [SomeExpr])
collectApply f xs = case f of
  SomeExpr (Apply f' x') ->
    let
      (fn, args) = collectApply (SomeExpr f') (SomeExpr x' : xs)
     in
      (fn, args)
  _ ->
    (f, xs)

collectApplyE :: SomeEffect -> [SomeEffect] -> (SomeEffect, [SomeEffect])
collectApplyE f xs = case f of
  SomeEffect (ApplyE f' x') ->
    let
      (fn, args) = collectApplyE (SomeEffect f') (SomeEffect x' : xs)
     in
      (fn, args)
  _ ->
    (f, xs)

exprCallArity :: Expr Stamp u -> Int
exprCallArity = \case
  Lambda (LamInfo (Just _) _) f ->
    1 + untaggedLambdaChainBody f
  _ -> 0

untaggedLambdaChainBody :: (Stamp a -> Expr Stamp b) -> Int
untaggedLambdaChainBody f =
  case f nestedDummy of
    Lambda (LamInfo Nothing _) g ->
      1 + untaggedLambdaChainBody g
    _ -> 0

effectCallArity :: Effect Stamp u -> Int
effectCallArity = \case
  LambdaE f -> 1 + untaggedEffectLambdaChainBody f
  _ -> 0

untaggedEffectLambdaChainBody :: (Stamp a -> Effect Stamp b) -> Int
untaggedEffectLambdaChainBody f =
  case f nestedDummy of
    LambdaE g -> 1 + untaggedEffectLambdaChainBody g
    _ -> 0

emitApplyArgs env s0 xs =
  foldl'
    ( \(s, d, rs) x ->
        let
          (s', Code xd xr) = pureAST' s env x
         in
          (s', d $$ xd, rs ++ [xr])
    )
    (s0, mempty, [])
    xs

emitApplyArgsE env s0 xs =
  foldl'
    ( \(s, d, rs) x ->
        let
          (s', Code xd xr) = effectfulAST' env s x
         in
          (s', d $$ xd, rs ++ [xr])
    )
    (s0, mempty, [])
    xs

emitEffectLambda env s0 f =
  let
    (nParam, s1) = allocIdentHint s0 Nothing
    body = f (Name nParam)
    (s2, MkCode d r _) = effectfulAST' env s1 body
   in
    (s2, Code mempty (renderFn s2 [nJS s2 nParam] d r))

emitLambdaSpine ::
  Bool
  -> (forall v. CG -> Expr Stamp v -> (CG, Code))
  -> CG
  -> Maybe Text
  -> (Stamp a -> Expr Stamp u)
  -> (CG, Code)
emitLambdaSpine peelUntagged walker s0 hint f =
  peel s0 hint f []
 where
  peel s h g acc =
    let
      (nParam, s1) = allocIdentHint s h
      body = g (Name nParam)
     in
      peelBody s1 body (nParam : acc)

  peelBody :: forall v. CG -> Expr Stamp v -> [Int] -> (CG, Code)
  peelBody s body acc =
    case body of
      Lambda (LamInfo Nothing p) g
        | peelUntagged ->
            let
              (nParam, s1) = allocIdentHint s p
             in
              peelBody s1 (g (Name nParam)) (nParam : acc)
      _ ->
        let
          ids = reverse acc
          (s2, MkCode d r _) = walker s body
         in
          (s2, Code mempty (renderFn s2 (map (nJS s2) ids) d r))

renderBinaryFn env s0 f =
  let
    (s1, Code _ cb) =
      renderFnBody
        env
        s0
        (JfCons (Just "a") (\a -> JfCons (Just "b") (\b -> JfNil (f a b))))
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
        renderBinEmit env emitValueEq s0 x y
    | otherwise ->
        renderBin env "===" s0 x y
  KNEq structural x y
    | structural ->
        renderBinEmit env emitValueNEq s0 x y
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
      cb = jsCallback s4 [nJS s4 nHole, nJS s4 nI] exDecl exRef
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
    (nParam, s2) = allocIdentHint s1 (Just "x")
    (s3, Code exDecl exRef) = pureAST' s2 env (f (Name nParam))
    call =
      wrapOperand recv rRef
        <> "."
        <> jsString name
        <> parens (jsCallback s3 [nJS s3 nParam] exDecl exRef)
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

renderBinEmit ::
  Env
  -> (CG -> JS -> JS -> (CG, JS))
  -> CG
  -> Expr Stamp a
  -> Expr Stamp b
  -> (CG, Code)
renderBinEmit env emit s0 x y =
  let
    (s1, Code xDecl xRef) = pureAST' s0 env x
    (s2, Code yDecl yRef) = pureAST' s1 env y
    (s3, js) = emit s2 xRef yRef
   in
    (s3, Code (xDecl $$ yDecl) js)

argAST :: Env -> CG -> Arg Stamp u -> (CG, Code)
argAST env s (ArgExpr e) = pureAST' s env e
argAST env s (ArgEffect e) = effectfulAST' env s e

renderArgList env s0 args =
  let
    (s1, cs) = recCodes (argAST env) s0 args
   in
    (s1, codesDecls cs, hcat (punctuate ", " (codesRefs cs)))
