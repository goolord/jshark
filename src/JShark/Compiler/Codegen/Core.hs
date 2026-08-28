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
module JShark.Compiler.Codegen.Core where

import qualified Data.Char as Char
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Clock (getMonotonicTime)
import qualified GHC.IO as GHCIO
import GHC.IO.Unsafe (unsafePerformIO)
import JShark.Api.Types
import JShark.Compiler.Binder
  ( Stamp (..)
  , stampId
  )
import JShark.Compiler.CompileProgress
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
import JShark.Compiler.CompileTiming
  ( FlatOptProfile (..)
  , FlatPrepareTiming (..)
  , IrOptProfile (..)
  , LowerProfile (..)
  , PhoasPrepareTiming (..)
  , reportFlatPrepareTiming
  , reportPhoasPrepareTiming
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
  , renderJSCompact
  , semi
  , vcat
  , ($$)
  , (<+>)
  )
import qualified JShark.Compiler.FlatSoA as FlatSoA
import qualified JShark.Compiler.Ir as Ir
import JShark.Compiler.JsShim
  ( Builtin (CheckedIndex, ValueEq)
  , Preamble
  , emptyPreamble
  , mergePreamble
  , renderPreamble
  , useShim
  )
import JShark.Compiler.Lower
  ( lowerEffectClosed
  , lowerOptEffectIrWith
  , optEffectClosed
  )
import JShark.Compiler.Optimize
  ( nodeCountExpr
  , optimizeWith
  )

printComputation computation = T.putStrLn (renderJSCompact computation)

preambleDecls s = renderPreamble (cgPreamble s)

emitBuiltin :: CG -> Builtin -> [JS] -> (CG, JS)
emitBuiltin s b args =
  let
    (p, js) = useShim b args (cgPreamble s)
   in
    (s {cgPreamble = p}, js)

emitCheckedIndex :: CG -> JS -> JS -> (CG, JS)
emitCheckedIndex s arr idx = emitBuiltin s CheckedIndex [arr, idx]

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
  { esArrowFns :: !Bool
  , esIntLiterals :: !Bool
  , esBareKeys :: !Bool
  , esSourceNames :: !Bool
  , esKeepLets :: !Bool
  }
  deriving (Eq, Show)

minifiedStyle :: EmitStyle
minifiedStyle =
  EmitStyle
    { esArrowFns = True
    , esIntLiterals = True
    , esBareKeys = True
    , esSourceNames = False
    , esKeepLets = False
    }

idiomaticStyle :: EmitStyle
idiomaticStyle =
  EmitStyle
    { esArrowFns = True
    , esIntLiterals = True
    , esBareKeys = True
    , esSourceNames = True
    , esKeepLets = True
    }

legacyStyle :: EmitStyle
legacyStyle =
  EmitStyle
    { esArrowFns = False
    , esIntLiterals = False
    , esBareKeys = False
    , esSourceNames = False
    , esKeepLets = False
    }

data CG = CG
  { cgIdent :: {-# UNPACK #-} !Int
  , cgTag :: {-# UNPACK #-} !Int
  , cgPreamble :: !Preamble
  , cgEmit :: {-# UNPACK #-} !Int
  , cgEmitCtx :: !(Maybe EmitCtx)
  , cgStyle :: !EmitStyle
  , cgNames :: !(IM.IntMap Text)
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

-- Codegen counters: `cgIdent` is the next emitted JS name (`n0`, `n1`, …);
-- `cgTag` is a decreasing negative id used only for use-counting/inlining
-- so nested Lets/Binds cannot collide (tags are never valid JS idents).
-- `cgPreamble` is runtime shims + hoisted @$name@ bodies used by this program.
startCG = startCGWith minifiedStyle

startCGWith :: EmitStyle -> CG
startCGWith style = CG 0 (-3) emptyPreamble 0 Nothing style IM.empty

-- | Prepare optimized pure AST and wire batch progress (pack then emit).
-- Requires 'withActiveJob' + 'configProgressSlot' when progress is enabled.
preparePureProgram :: ClosedExpr u -> IO (CG, Expr Stamp u)
preparePureProgram = preparePureProgramWith minifiedStyle

preparePureProgramWith :: EmitStyle -> ClosedExpr u -> IO (CG, Expr Stamp u)
preparePureProgramWith style e = do
  mCtx <- captureEmitCtx
  case mCtx of
    Nothing -> do
      t0 <- getMonotonicTime
      let
        !expr = optimizeWith (esKeepLets style) e
      t1 <- getMonotonicTime
      let
        timing =
          PhoasPrepareTiming
            { pptOptimizeSec = seconds t0 t1
            , pptTotalSec = seconds t0 t1
            }
      reportPhoasPrepareTiming timing
      recordJobPhoasPrepare timing
      pure (startCGWith style, expr)
    Just ctx -> do
      reportPackPhase ctx 0 1
      t0 <- getMonotonicTime
      let
        !expr = optimizeWith (esKeepLets style) e
      t1 <- getMonotonicTime
      let
        timing =
          PhoasPrepareTiming
            { pptOptimizeSec = seconds t0 t1
            , pptTotalSec = seconds t0 t1
            }
      reportPhoasPrepareTiming timing
      recordJobPhoasPrepare timing
      reportPackPhase ctx 1 1
      initEmitCtxTotal ctx (nodeCountExpr expr)
      pure ((startCGWith style) {cgEmitCtx = Just ctx}, expr)
{-# NOINLINE preparePureProgramWith #-}

prepareFlatEffectProgram :: ClosedEffect u -> IO (FlatSoA.FlatSoA, CG)
prepareFlatEffectProgram = prepareFlatEffectProgramWith minifiedStyle

prepareFlatEffectProgramWith ::
  EmitStyle -> ClosedEffect u -> IO (FlatSoA.FlatSoA, CG)
prepareFlatEffectProgramWith style e = do
  mCtx <- captureEmitCtx
  (soa, _timing, _irNodes, _ir) <- flatPrepareCoreWith (esKeepLets style) e
  case mCtx of
    Nothing -> pure (soa, startCGWith style)
    Just ctx -> do
      initEmitCtxTotal ctx (flatSoaNodeCount soa)
      pure (soa, (startCGWith style) {cgEmitCtx = Just ctx})
{-# NOINLINE prepareFlatEffectProgramWith #-}

flatPrepareFromIr :: Ir.IrEffect u -> IO (FlatSoA.FlatSoA, FlatPrepareTiming)
flatPrepareFromIr irOpt = do
  mCtx <- captureEmitCtx
  t0 <- getMonotonicTime
  case mCtx of
    Just ctx -> reportPackPhase ctx 0 1
    Nothing -> pure ()
  let
    !soa0 = FlatSoA.packEffectProgramDirect irOpt
    !packNodes = FlatSoA.flatSoaNodeCount soa0
    !_ = FlatSoA.soaPureCount soa0
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
      ( packNodes
          `seq` FlatSoA.soaPureCount soaOpt
          `seq` FlatSoA.flatSoaNodeCount soaOpt
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

profileFlatOptFromIr :: Ir.IrEffect u -> IO FlatOptProfile
profileFlatOptFromIr irOpt = do
  let
    !soa0 = FlatSoA.packEffectProgramDirect irOpt
    !nodeCount = FlatSoA.flatSoaNodeCount soa0
  _ <- GHCIO.evaluate (FlatSoA.soaPureCount soa0)
  tFold0 <- getMonotonicTime
  let
    !(soa1, foldPasses, folded) = FlatSoA.constantFoldWithStats soa0
  tFold1 <- getMonotonicTime
  tFoldSeq0 <- getMonotonicTime
  _ <- GHCIO.evaluate (FlatSoA.optConstantFoldNumOnce soa0)
  tFoldSeq1 <- getMonotonicTime
  tPure0 <- getMonotonicTime
  let
    !(soa2, purePasses) = FlatSoA.propagatePureWithStats soa1
    !pureCount = FlatSoA.soaPureCount soa2
  tPure1 <- getMonotonicTime
  tAttach0 <- getMonotonicTime
  let
    !_ = FlatSoA.soaPureVector soa2
  tAttach1 <- getMonotonicTime
  let
    foldSec = seconds tFold0 tFold1
    foldSeqSec = seconds tFoldSeq0 tFoldSeq1
    pureSec = seconds tPure0 tPure1
    attachSec = seconds tAttach0 tAttach1
    total = foldSec + pureSec + attachSec
  pure
    FlatOptProfile
      { fopNodeCount = nodeCount
      , fopFoldSec = foldSec
      , fopFoldSeqSec = foldSeqSec
      , fopFoldPasses = foldPasses
      , fopFolded = folded
      , fopPureSec = pureSec
      , fopPurePasses = purePasses
      , fopPureCount = pureCount
      , fopAttachSec = attachSec
      , fopTotalSec = total
      }
{-# NOINLINE profileFlatOptFromIr #-}

profileIrOptFromIr :: Ir.IrEffect u -> IO IrOptProfile
profileIrOptFromIr !irRaw = do
  tMetaRaw0 <- getMonotonicTime
  let
    !rawNodes = Ir.irSize (Ir.metaIrEffect irRaw)
  tMetaRaw1 <- getMonotonicTime
  tOpt0 <- getMonotonicTime
  let
    ?keepLets = False
   in
    do
      let
        !(_, !irOpt, !mdOpt) = Ir.optIrEffect (-2) irRaw
        !optNodes = Ir.irSize mdOpt
      tOpt1 <- getMonotonicTime
      tMetaOpt0 <- getMonotonicTime
      let
        !_ = Ir.metaIrEffect irOpt
      tMetaOpt1 <- getMonotonicTime
      _ <- GHCIO.evaluate irOpt
      let
        metaRawSec = seconds tMetaRaw0 tMetaRaw1
        optSec = seconds tOpt0 tOpt1
        metaOptSec = seconds tMetaOpt0 tMetaOpt1
       in
        pure
          IrOptProfile
            { iopRawNodes = rawNodes
            , iopOptNodes = optNodes
            , iopLowerSec = 0
            , iopMetaRawSec = metaRawSec
            , iopOptSec = optSec
            , iopMetaOptSec = metaOptSec
            , iopPrepareSec = 0
            , iopTotalSec = metaRawSec + optSec + metaOptSec
            }
{-# NOINLINE profileIrOptFromIr #-}

profileIrOptFromClosed :: ClosedEffect u -> IO IrOptProfile
profileIrOptFromClosed e = do
  tLower0 <- getMonotonicTime
  let
    !irRaw = lowerEffectClosed e
  tLower1 <- getMonotonicTime
  tPrep0 <- getMonotonicTime
  let
    !irOpt = optEffectClosed irRaw
    !optNodes = Ir.irSize (Ir.metaIrEffect irOpt)
  tPrep1 <- getMonotonicTime
  _ <- GHCIO.evaluate irOpt
  breakdown <- profileIrOptFromIr irRaw
  let
    lowerSec = seconds tLower0 tLower1
    prepareSec = seconds tPrep0 tPrep1
   in
    pure
      breakdown
        { iopOptNodes = optNodes
        , iopLowerSec = lowerSec
        , iopPrepareSec = prepareSec
        , iopTotalSec = lowerSec + prepareSec
        }
{-# NOINLINE profileIrOptFromClosed #-}

profileLowerFromClosed :: ClosedEffect u -> IO LowerProfile
profileLowerFromClosed e = do
  tLazy0 <- getMonotonicTime
  let
    !irRaw = lowerEffectClosed e
  tLazy1 <- getMonotonicTime
  tForce0 <- getMonotonicTime
  let
    !rawNodes = Ir.irSize (Ir.metaIrEffect irRaw)
  tForce1 <- getMonotonicTime
  let
    lazySec = seconds tLazy0 tLazy1
    forceSec = seconds tForce0 tForce1
   in
    pure
      LowerProfile
        { lopRawNodes = rawNodes
        , lopLazySec = lazySec
        , lopForceSec = forceSec
        , lopTotalSec = lazySec + forceSec
        }
{-# NOINLINE profileLowerFromClosed #-}

flatPrepareCore ::
  ClosedEffect u -> IO (FlatSoA.FlatSoA, FlatPrepareTiming, Int, Ir.IrEffect u)
flatPrepareCore = flatPrepareCoreWith False

flatPrepareCoreWith ::
  Bool
  -> ClosedEffect u
  -> IO (FlatSoA.FlatSoA, FlatPrepareTiming, Int, Ir.IrEffect u)
flatPrepareCoreWith keepLets (e :: ClosedEffect u) = do
  mCtx <- captureEmitCtx
  tAll0 <- getMonotonicTime
  case mCtx of
    Just ctx -> reportIrPreparePhase ctx 0 1
    Nothing -> pure ()
  t0 <- getMonotonicTime
  let
    !(irOpt, irNodes) = lowerOptEffectIrWith keepLets e
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
  recordJobFlatPrepare timing
  pure (soa, timing, irNodes, irOpt)
{-# NOINLINE flatPrepareCoreWith #-}

{-# NOINLINE tickEmitCtxUnit #-}
tickEmitCtxUnit ctx tag = unsafePerformIO (tag `seq` tickEmitCtx ctx)

bumpEmit s =
  let
    n = cgEmit s + 1
   in
    (n, s {cgEmit = n})

bumpEmitTick sIn =
  case cgEmitCtx sIn of
    Nothing -> sIn
    Just ctx ->
      let
        (tag, s0) = bumpEmit sIn
       in
        tag `seq` tickEmitCtxUnit ctx tag `seq` s0
{-# NOINLINE bumpEmitTick #-}

allocTag s = (cgTag s, s {cgTag = cgTag s - 2})

allocIdent s = allocIdentHint s Nothing

allocIdentHint :: CG -> Maybe Text -> (Int, CG)
allocIdentHint s hint =
  let
    n = cgIdent s
    name = pickBinderName (cgStyle s) hint n
    s' =
      s
        { cgIdent = n + 1
        , cgNames = IM.insert n name (cgNames s)
        }
   in
    (n, s')

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

-- | Ident already allocated for this effect (@Lift (Var n1)@). Not a
-- counter guess: only a binder that is already in the tree.
jsCall f a = parens f <> parens a

jsCallN f args = parens f <> parens (hcat (punctuate ", " args))

jsNumber style d
  | esIntLiterals style
  , not (isNaN d || isInfinite d)
  , let
      n = round d :: Integer
  , fromInteger n == d
  , abs n <= 9007199254740991 =
      jsDecimal n
  | otherwise = jsDouble d

jsPropKey style k
  | esBareKeys style && jsIdent k = jsText k
  | otherwise = dquotes (jsText k)

-- | @function (n0) {…}@ or @n0 => …@ depending on 'esArrowFns'.
renderFunction s nParam decl ref =
  renderFn s [nJS s nParam] decl ref

jsCallback s params decl ref = renderFn s params (nonEmpty decl) (Just ref)

renderFn :: CG -> [JS] -> Maybe JS -> Maybe JS -> JS
renderFn s params mDecl mRef
  | esArrowFns (cgStyle s) = renderArrow params mDecl mRef
  | otherwise = renderClassic params mDecl mRef

renderClassic params mDecl mRef =
  "function"
    <+> parens (hcat (punctuate ", " params))
    <+> blockBody (fromMaybe mempty mDecl $$ ret)
 where
  ret = case mRef of
    Nothing -> "return"
    Just r -> "return" <+> parens r

renderArrow params mDecl mRef =
  let
    headJs = arrowParams params <+> "=>"
   in
    case (mDecl, mRef) of
      (Nothing, Nothing) -> headJs <+> blockBody mempty
      (Nothing, Just r) -> headJs <+> arrowExpr r
      (Just d, Nothing) -> headJs <+> blockBody (d $$ "return")
      (Just d, Just r) ->
        headJs <+> blockBody (d $$ ("return" <+> parens r))

arrowParams [p] = p
arrowParams ps = parens (hcat (punctuate ", " ps))

arrowExpr r =
  let
    t = T.strip (renderJS r)
   in
    if "{" `T.isPrefixOf` t then parens r else r

-- | Needs no parentheses as an operand: already a primary JS expression.
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

mergeEmitCG a b =
  a
    { cgIdent = max (cgIdent a) (cgIdent b)
    , cgPreamble = mergePreamble (cgPreamble a) (cgPreamble b)
    , cgEmit = max (cgEmit a) (cgEmit b)
    , cgNames = cgNames a <> cgNames b
    }

mergeEmitCGs :: CG -> [CG] -> CG
mergeEmitCGs = foldl (\acc cg -> mergeEmitCG acc cg `seq` mergeEmitCG acc cg)

flatSoaNodeCount = FlatSoA.flatSoaNodeCount

flatSoaParallelThreshold = FlatSoA.flatSoaParallelThreshold
