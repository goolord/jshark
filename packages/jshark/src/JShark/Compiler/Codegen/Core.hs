{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier -Wno-missing-export-lists -Wno-missing-signatures -Wno-type-defaults -Wno-missing-pattern-synonym-signatures #-}

-- | Codegen state ('CG'), snippet assembly ('Code'), and compile prep.
--
-- Internal to the JShark compiler; this module is exposed for tests and
-- tooling and its API may change between 0.x releases.
module JShark.Compiler.Codegen.Core where

import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as Char
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTime)
import qualified GHC.IO as GHCIO
import JShark.Api.Types
import JShark.Compiler.Binder
  ( stampId
  )
import JShark.Compiler.CompileProgress
  ( captureEmitCtx
  , initEmitCtxTotal
  , reportFlatOptPhase
  , reportIrPreparePhase
  , reportPackPhase
  )
import JShark.Compiler.CompileTiming
  ( FlatPrepareTiming (..)
  , reportFlatPrepareTiming
  , seconds
  )
import JShark.Compiler.Emit
  ( JS
  , blockBody
  , dquotes
  , hcat
  , iifeBody
  , jsDecimal
  , jsDouble
  , jsText
  , nonEmpty
  , parens
  , punctuate
  , renderJS
  , semi
  , vcat
  , ($$)
  , (<+>)
  )
import qualified JShark.Compiler.Flat as FlatSoA
import qualified JShark.Compiler.Ir as Ir
import JShark.Compiler.JsShim
  ( Builtin (CheckedIndex, GroupBy, ValueEq)
  , Preamble
  , emptyPreamble
  , hoistTagName
  , insertHoisted
  , renderPreambleStyled
  , useShim
  )
import JShark.Compiler.Lower
  ( lowerOptEffectIrWith
  , lowerOptExprIr
  )

preambleDecls s =
  renderPreambleStyled (cgPreamble s)

emitBuiltin :: CG -> Builtin -> [JS] -> (CG, JS)
emitBuiltin s b args =
  let
    (p, js) = useShim b args (cgPreamble s)
   in
    (s {cgPreamble = p}, js)

emitCheckedIndex :: CG -> JS -> JS -> (CG, JS)
emitCheckedIndex s arr idx = emitBuiltin s CheckedIndex [arr, idx]

emitGroupBy :: CG -> JS -> JS -> (CG, JS)
emitGroupBy s arr key = emitBuiltin s GroupBy [arr, key]

emitValueEq :: CG -> JS -> JS -> (CG, JS)
emitValueEq s a b = emitBuiltin s ValueEq [a, b]

emitValueNEq :: CG -> JS -> JS -> (CG, JS)
emitValueNEq s a b =
  let
    (s', js) = emitValueEq s a b
   in
    (s', "!" <> parens js)

-- | @o.foo@ when @foo@ is an identifier; @o.a.b@ for a dotted ident
-- path ('location.hash'); @o["0"]@ otherwise. A single key that is
-- not an ident must stay bracketed — @window["location.hash"]@ is
-- @undefined@, which made TodoMVC hash filters a no-op.
jsDotOrBracket obj key
  | jsIdent key = obj <> "." <> jsText key
  | (seg, rest) <- T.break (== '.') key
  , not (T.null rest)
  , jsIdent seg =
      jsDotOrBracket (jsDotOrBracket obj seg) (T.drop 1 rest)
  | otherwise = obj <> "[" <> dquotes (jsText key) <> "]"

jsIdent t = case T.uncons t of
  Nothing -> False
  Just (c, cs) -> jsIdStart c && T.all jsIdPart cs
 where
  jsIdStart x = Char.isAscii x && (Char.isLetter x || x == '_' || x == '$')
  jsIdPart x = jsIdStart x || Char.isDigit x

data Code = MkCode
  { codeDecl :: !(Maybe JS)
  , codeRef :: !(Maybe JS)
  , codeRefFX :: !Bool
  }

-- | Codegen presentation. Syntax flags are safe for minified output;
-- structure flags are 'Readable' only (keep source names and lets).
data EmitStyle = EmitStyle
  { esIntLiterals :: !Bool
  , esBareKeys :: !Bool
  , esSourceNames :: !Bool
  , esKeepLets :: !Bool
  }
  deriving (Eq, Show)

minifiedStyle :: EmitStyle
minifiedStyle =
  EmitStyle
    { esIntLiterals = True
    , esBareKeys = True
    , esSourceNames = False
    , esKeepLets = False
    }

idiomaticStyle :: EmitStyle
idiomaticStyle =
  EmitStyle
    { esIntLiterals = True
    , esBareKeys = True
    , esSourceNames = True
    , esKeepLets = True
    }

data CG = CG
  { cgIdent :: {-# UNPACK #-} !Int
  , cgPreamble :: !Preamble
  , cgStyle :: !EmitStyle
  , cgNames :: !(IM.IntMap Text)
  , cgScope :: ![Set Text]
  -- ^ Used source/synthetic names in each JS function, innermost first.
  -- Hoisted helpers push a fresh set so @$reduce@ params stay @seed@/@f@
  -- across emit sites (global uniqueness broke hoist dedup).
  }

type Env = IM.IntMap Int

pattern Code d r <- MkCode (fromMaybe mempty -> d) (fromMaybe mempty -> r) _
 where
  Code d r = MkCode (nonEmpty d) (nonEmpty r) False

{-# COMPLETE Code #-}

fxCode d r = MkCode (nonEmpty d) (nonEmpty r) True

-- | New decls, same ref and effectfulness as the source 'Code'.
keepRef d (MkCode _ r f) = MkCode (nonEmpty d) r f

instance Semigroup Code where
  MkCode a b f <> MkCode x y g = MkCode (a <> x) (b <> y) (f || g)

instance Monoid Code where
  mempty = MkCode Nothing Nothing False

renderCode (MkCode a b _) = fromMaybe mempty a $$ fromMaybe mempty b

-- | Wrap preamble + generated decls + result in an IIFE so a minifier treats
-- the result as live (plain expression statements get DCE'd).
renderIIFE s (MkCode decls ref _) =
  let
    stmts = preambleDecls s $$ fromMaybe mempty decls
    body = case ref of
      Nothing -> stmts
      Just r -> stmts $$ (("return" <+> r) <> semi)
   in
    "(() => {" <> iifeBody body <> "})()"

-- | Preamble (shims + hoisted @$name@) ahead of a snippet's declarations.
renderWithPreamble s code = preambleDecls s $$ renderCode code

codesDecls cs = vcat (mapMaybe (\(MkCode a _ _) -> a) cs)

codesRefs = map (\(MkCode _ b _) -> arrayElemRef b)

-- | 'ValueUnit' renders as nothing, since a unit statement emits nothing.
-- As an array element it still occupies a slot, so it has to print — a
-- dropped ref would shorten the literal.
arrayElemRef = fromMaybe "undefined"

-- Codegen counters: `cgIdent` is the next emitted JS name (`n0`, `n1`, …).
-- `cgPreamble` is runtime shims + hoisted @$name@ bodies used by this program.
startCG = startCGWith minifiedStyle

startCGWith :: EmitStyle -> CG
startCGWith style = CG 0 emptyPreamble style IM.empty [S.empty]

-- | Pack a lowered, optimized tree and start codegen. The effect and pure
-- entry points pass their lowered tree (the thunk carries the lowering, so
-- it runs inside 'flatPrepareScaffolding').
prepareFlatProgramWith ::
  EmitStyle -> (Ir.IrNode, Int) -> IO (FlatSoA.FlatSoA, CG)
prepareFlatProgramWith style lowered = do
  mCtx <- captureEmitCtx
  (soa, _timing, _irNodes, _ir) <- flatPrepareScaffolding lowered
  case mCtx of
    Nothing -> pure (soa, startCGWith style)
    Just ctx -> do
      initEmitCtxTotal ctx (FlatSoA.flatSoaNodeCount soa)
      pure (soa, startCGWith style)
{-# NOINLINE prepareFlatProgramWith #-}

flatPrepareFromIr :: Ir.IrNode -> IO (FlatSoA.FlatSoA, FlatPrepareTiming)
flatPrepareFromIr irOpt = do
  mCtx <- captureEmitCtx
  t0 <- getMonotonicTime
  case mCtx of
    Just ctx -> reportPackPhase ctx 0 1
    Nothing -> pure ()
  let
    !soa0 = FlatSoA.packProgramDirect irOpt
    !packNodes = FlatSoA.flatSoaNodeCount soa0
  t1 <- getMonotonicTime
  case mCtx of
    Just ctx -> reportPackPhase ctx 1 1
    Nothing -> pure ()
  let
    packSec = seconds t0 t1
  t2 <- getMonotonicTime
  case mCtx of
    Just ctx -> reportFlatOptPhase ctx 0 1
    Nothing -> pure ()
  let
    !soaOpt = FlatSoA.optimizeFlatPack soa0
  _ <-
    GHCIO.evaluate
      ( packNodes `seq`
          FlatSoA.flatSoaNodeCount soaOpt
      )
  t3 <- getMonotonicTime
  case mCtx of
    Just ctx -> reportFlatOptPhase ctx 1 1
    Nothing -> pure ()
  let
    timing =
      FlatPrepareTiming
        { fptIrPrepareSec = 0
        , fptPackSec = packSec
        , fptFlatOptSec = seconds t2 t3
        , fptTotalSec = seconds t0 t3
        }
  pure (soaOpt, timing)
{-# NOINLINE flatPrepareFromIr #-}

flatPrepareCore ::
  ClosedEffect u -> IO (FlatSoA.FlatSoA, FlatPrepareTiming, Int, Ir.IrNode)
flatPrepareCore = flatPrepareCoreWith False

-- | Lower, IR-opt, pack, and bulk-optimize a closed program onto the flat
-- SoA, with the shared phase/timing scaffolding. The effect and pure paths
-- differ only in the lowering call, passed as @lowered@ and forced inside
-- the ir-prepare timing window.
flatPrepareScaffolding ::
  (Ir.IrNode, Int) -> IO (FlatSoA.FlatSoA, FlatPrepareTiming, Int, Ir.IrNode)
flatPrepareScaffolding lowered = do
  mCtx <- captureEmitCtx
  tAll0 <- getMonotonicTime
  case mCtx of
    Just ctx -> reportIrPreparePhase ctx 0 1
    Nothing -> pure ()
  t0 <- getMonotonicTime
  !(irOpt, irNodes) <- evaluate lowered
  t1 <- getMonotonicTime
  case mCtx of
    Just ctx -> reportIrPreparePhase ctx 1 1
    Nothing -> pure ()
  (soa, packTiming) <- flatPrepareFromIr irOpt
  tAll1 <- getMonotonicTime
  let
    timing =
      FlatPrepareTiming
        { fptIrPrepareSec = seconds t0 t1
        , fptPackSec = fptPackSec packTiming
        , fptFlatOptSec = fptFlatOptSec packTiming
        , fptTotalSec = seconds tAll0 tAll1
        }
  reportFlatPrepareTiming timing
  pure (soa, timing, irNodes, irOpt)

allocIdent s = allocIdentHint s Nothing

-- | Fresh JS function scope. Params and inner @const@ uniquify only against
-- names in this function, so a later @$reduce@ still gets @seed@/@f@.
pushHintScope :: CG -> CG
pushHintScope s = s {cgScope = S.empty : cgScope s}

popHintScope :: CG -> CG
popHintScope s = case cgScope s of
  (_ : rest@(_ : _)) -> s {cgScope = rest}
  _ -> s

withHintScope :: CG -> (CG -> (CG, a)) -> (CG, a)
withHintScope s f =
  let
    (s', a) = f (pushHintScope s)
   in
    (popHintScope s', a)

hintScope :: CG -> Set Text
hintScope s = case cgScope s of
  (x : _) -> x
  [] -> S.empty

-- | Allocate a binder or param. Source hints uniquify within the current
-- JS function only (see 'cgScope').
allocIdentHint :: CG -> Maybe Text -> (Int, CG)
allocIdentHint s hint =
  let
    n = cgIdent s
    name = pickBinderName (cgStyle s) (uniqueBinderHint s hint) n
    s' =
      s
        { cgIdent = n + 1
        , cgNames = IM.insert n name (cgNames s)
        , cgScope = case cgScope s of
            (sc : rest) -> S.insert name sc : rest
            [] -> [S.singleton name]
        }
   in
    (n, s')

uniqueBinderHint :: CG -> Maybe Text -> Maybe Text
uniqueBinderHint s = \case
  Nothing -> Nothing
  Just t
    | t `S.member` hintScope s -> Nothing
    | otherwise -> Just t

pickBinderName :: EmitStyle -> Maybe Text -> Int -> Text
pickBinderName style hint n =
  case hint of
    Just t
      | esSourceNames style
      , jsSafeBinder t ->
          t
    _ -> nName n

jsSafeBinder t = jsIdent t && t `S.notMember` jsReserved

jsReserved :: Set Text
jsReserved =
  S.fromList
    [ "break"
    , "case"
    , "catch"
    , "class"
    , "const"
    , "continue"
    , "debugger"
    , "default"
    , "delete"
    , "do"
    , "else"
    , "export"
    , "extends"
    , "false"
    , "finally"
    , "for"
    , "function"
    , "if"
    , "import"
    , "in"
    , "instanceof"
    , "new"
    , "null"
    , "return"
    , "super"
    , "switch"
    , "this"
    , "throw"
    , "true"
    , "try"
    , "typeof"
    , "var"
    , "void"
    , "while"
    , "with"
    , "yield"
    , "enum"
    , "await"
    , "let"
    , "static"
    , "implements"
    , "interface"
    , "package"
    , "private"
    , "protected"
    , "public"
    ]

nName n = "n" <> T.pack (show n)

identName s n = fromMaybe (nName n) (IM.lookup n (cgNames s))

nJS s n = jsText (identName s n)

constBind s n ref = ("const" <+> nJS s n <+> "=" <+> ref) <> semi

-- | Optimizer tags (negative) map to emitted `n*` ids during codegen.
varStampJS cg env s =
  let
    i = stampId s
   in
    if i < 0
      then maybe mempty (nJS cg) (IM.lookup i env)
      else nJS cg i

jsCall f a = parens f <> parens a

jsCallN f args = parens f <> parens (hcat (punctuate ", " args))

jsNumber style d
  | esIntLiterals style
  , not (isNaN d || isInfinite d)
  , -- @-0.0@ compares equal to @0@ but is a distinct JS value, so it must
    -- not take the integer path and lose its sign.
    not (isNegativeZero d)
  , let
      n = round d :: Integer
  , fromInteger n == d
  , abs n <= 9007199254740991 =
      jsDecimal n
  | otherwise = jsDouble d

jsPropKey style k
  | esBareKeys style && jsIdent k = jsText k
  | otherwise = dquotes (jsText k)

-- | @n0 => …@ arrow functions (the only supported style).
renderFunction s nParam decl ref =
  renderFn s [nJS s nParam] decl ref

jsCallback s params decl ref = renderFn s params (nonEmpty decl) (Just ref)

renderFn :: CG -> [JS] -> Maybe JS -> Maybe JS -> JS
renderFn _s params mDecl mRef = renderArrow params mDecl mRef

renderArrow params mDecl mRef =
  let
    headJs = arrowParams params <+> "=>"
   in
    case (mDecl, mRef) of
      (Nothing, Nothing) -> headJs <+> blockBody mempty
      (Nothing, Just r) -> headJs <+> arrowExpr r
      (Just d, Nothing) -> headJs <+> blockBody (d $$ "return")
      (Just d, Just r) ->
        headJs <+> blockBody (d $$ ("return" <+> arrowExpr r))

arrowParams [p] = p
arrowParams ps = parens (hcat (punctuate ", " ps))

-- | Operand / return expression. Parenthesize only object-literal
-- returns \/ arrow bodies. Other expressions stay bare so @return n1 * 2@
-- is idiomatic.
arrowExpr r =
  let
    t = BC.strip (renderJS r)
   in
    if needsObjectParens t then parens r else r

needsObjectParens t = "{" `BS.isPrefixOf` t

allocNIdents :: CG -> Int -> ([Int], CG)
allocNIdents s 0 = ([], s)
allocNIdents s n =
  let
    (i, s1) = allocIdent s
    (is, s2) = allocNIdents s1 (n - 1)
   in
    (i : is, s2)

allocNIdentsHints :: CG -> [Maybe Text] -> ([Int], CG)
allocNIdentsHints s [] = ([], s)
allocNIdentsHints s (h : hs) =
  let
    (i, s1) = allocIdentHint s h
    (is, s2) = allocNIdentsHints s1 hs
   in
    (i : is, s2)

-- Named-lambda hoisting -------------------------------------------------------

-- | Register a shared @$tag@ binding in the preamble, deduplicating by
-- canonical (alpha-renamed) source.
registerHoistedTag :: CG -> Text -> Text -> (CG, Text)
registerHoistedTag s tag src =
  let
    name = hoistTagName tag
   in
    (s {cgPreamble = insertHoisted name src (cgPreamble s)}, name)

-- | If the node carries a hoist tag, render its function body as a shared
-- @$tag@ binding and reference the binding by name.
emitHoistedFnValue ::
  CG -> FlatSoA.FlatSoA -> FlatSoA.NodeId -> JS -> (CG, JS)
emitHoistedFnValue s view nid fnJs =
  case FlatSoA.flatSoaHoistTag view nid of
    Nothing -> (s, fnJs)
    Just tag ->
      let
        src = TE.decodeUtf8 (renderJS fnJs)
        (s', name) = registerHoistedTag s tag src
       in
        (s', jsText name)

flatPrepareCoreWith ::
  Bool
  -> ClosedEffect u
  -> IO (FlatSoA.FlatSoA, FlatPrepareTiming, Int, Ir.IrNode)
flatPrepareCoreWith keepLets e =
  flatPrepareScaffolding (lowerOptEffectIrWith keepLets e)
{-# NOINLINE flatPrepareCoreWith #-}

-- | Pure-program variant of 'flatPrepareCoreWith'.
flatPrepareExprCore ::
  Bool
  -> ClosedExpr u
  -> IO (FlatSoA.FlatSoA, FlatPrepareTiming, Int, Ir.IrNode)
flatPrepareExprCore keepLets e =
  flatPrepareScaffolding (lowerOptExprIr keepLets e)
{-# NOINLINE flatPrepareExprCore #-}
