{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared JavaScript statement\/expression renderers for codegen.
--
-- Extracted from the former PHOAS emitter so 'JShark.Compiler.Codegen.Flat'
-- can build statement-form control flow without a PHOAS dependency.
module JShark.Compiler.Codegen.Stmt
  ( asStmt
  , ifElseStmt
  , assignResult
  , letResult
  , recBindStmt
  , emitBranching
  , ifAssignOrStmt
  , tryCatchStmt
  , renderFFIForm
  , renderFFIInvoke
  , isWholeParenthesized
  , hvm2ExportRef
  )
where

import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Api.Types (FFIForm (..))
import JShark.Compiler.Codegen.Core
import JShark.Compiler.Emit
  ( JS
  , blockBody
  , dquotes
  , jsString
  , jsText
  , parens
  , semi
  , ($$)
  , (<+>)
  )
import JShark.Compiler.Evaluate (escapeJsString)

-- | Turn a rendered effect into a statement. Unit values may still have a
-- non-empty ref (@el.x = v@, @foo()@); those become statements, not
-- @let n = …@.
asStmt :: Maybe JS -> Maybe JS -> JS
asStmt mDecl mRef = case mRef of
  Nothing -> fromMaybe mempty mDecl
  Just r -> fromMaybe mempty mDecl $$ (r <> semi)

ifElseStmt :: JS -> Maybe JS -> Maybe JS -> Maybe JS -> Maybe JS -> JS
ifElseStmt cRef tDecl tRef eDecl eRef
  | isNothing eDecl && isNothing eRef =
      "if" <+> parens cRef <+> blockBody (asStmt tDecl tRef)
  | otherwise =
      "if"
        <+> parens cRef
        <+> blockBody (asStmt tDecl tRef)
        $$ "else"
        <+> blockBody (asStmt eDecl eRef)

assignResult :: Text -> Maybe JS -> JS
assignResult resultVar mRef = case mRef of
  Nothing -> mempty
  Just r -> (jsText resultVar <+> "=" <+> r) <> semi

letResult :: Text -> JS
letResult resultVar = ("let" <+> jsText resultVar) <> semi

recBindStmt :: JS -> Maybe JS -> Maybe JS -> JS
recBindStmt n rDecl rRef =
  fromMaybe mempty rDecl
    $$ (("const" <+> n <+> "=" <+> fromMaybe mempty rRef) <> semi)

-- | Unit arms: prelude + stmt, empty ref. Value arms: prelude +
-- @let result@ + stmt, result ident.
emitBranching ::
  Bool
  -> CG
  -> (CG -> (CG, JS, a))
  -> (Maybe Text -> a -> CG -> (CG, JS))
  -> (CG, Code)
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

tryCatchStmt :: Maybe Text -> JS -> Maybe JS -> Maybe JS -> Maybe JS -> Maybe JS -> JS
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

renderFFIForm :: FFIForm -> JS
renderFFIForm = \case
  FFICall s -> jsText s
  FFILambda s -> parens (jsText s)
  FFIExpr s -> jsText s

-- | Multi-parameter arrow lambdas are invalid IIFEs as @(...=>{...})(a,b)@;
--   wrap the lambda in an extra pair of parens so the call applies cleanly.
--   Parenthesized arrows from 'classifyFFI' become 'FFICall'; only wrap twice
--   when the callee is not already a whole parenthesized expression.
renderFFIInvoke :: FFIForm -> JS -> JS
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
isWholeParenthesized :: Text -> Bool
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
