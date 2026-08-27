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
{-# OPTIONS_GHC -fno-warn-unused-top-binds -Wno-pattern-namespace-specifier -Wno-missing-export-lists -Wno-missing-signatures -Wno-unused-imports -Wno-type-defaults -Wno-missing-pattern-synonym-signatures #-}

-- | Codegen state ('CG'), snippet assembly ('Code'), and compile prep.
module JShark.Compiler.Codegen.Core where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Bits (xor, (.&.), (.|.))
import qualified Data.Char as Char
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', mapAccumL)
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
import JShark.Api.Prim
  ( MathBinary (..)
  , MathUnary (..)
  , isPureFixed
  , matchMathBinary
  , matchMathUnary
  )
import qualified JShark.Api.Prim as Prim
import JShark.Api.Rec
import JShark.Api.Types
import JShark.Compiler.Binder
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
import JShark.Compiler.Evaluate
  ( bigOpJS
  , eqFoldableValue
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
import qualified JShark.Compiler.Flat as Flat
import qualified JShark.Compiler.FlatSoA as FlatSoA
import qualified JShark.Compiler.FlatView as FlatView
import JShark.Compiler.Flatten (flattenExpr, foldExpr)
import JShark.Compiler.Hoist.Canonical (canonicalHoistSrc)
import qualified JShark.Compiler.Ir as Ir
import JShark.Compiler.JsNum (jsBit2, jsRem, jsShl, jsShr, jsUShr)
import JShark.Compiler.Lower
  ( lowerEffectClosed
  , lowerOptEffectIr
  , optEffectClosed
  )
import JShark.Compiler.Optimize
  ( nodeCountEff
  , nodeCountExpr
  , optimize
  , optimizeEffectTree
  )
import Unsafe.Coerce (unsafeCoerce)

printComputation computation = T.putStrLn (renderJSCompact computation)

helperDecls s =
  vcat
    [ ("const" <+> jsText name <+> "=" <+> jsText src) <> semi
    | (name, src) <- M.toAscList (cgHelpers s)
    ]

jsValueEq a b = "$valueEq" <> parens (a <> ("," <+> b))

jsValueNEq a b = "!" <> parens (jsValueEq a b)

useEqHelpers s0 = foldr (uncurry useHelperSrc) s0 jsEqHelpers

-- | Integer slot + throw on a hole. Raw @a[i]@ would use the string key
-- (@a[1.9]@ is @undefined@) and invent @undefined@ at an arbitrary @u@.
-- Emitted once as @$checkedIndex@; inlining the lambda at every index
-- site blows up Life-sized programs (materializing huge emit trees never finishes).
jsCheckedIndexSrc :: Text
jsCheckedIndexSrc =
  "function(a,i){var n=Math.trunc(i);if(!(n>=0&&n<a.length))throw new Error(\"jshark: index\");return a[n];}"

jsCheckedIndex arr idx =
  "$checkedIndex" <> parens (arr <> ("," <+> idx))

valueNeedsStructuralEq = \case
  ValueArray _ -> True
  ValueFrozen _ -> True
  ValueUint8Array _ -> True
  _ -> False

stdNeedsStructuralEq = \case
  Fixed {} -> False
  Method {} -> True
  -- 'Kernel' itself never forces structural @===@; 'renderKernel' checks
  -- 'needsStructuralEq' on 'KEq'/'KNEq' operands via 'foldKernel'.
  Kernel {} -> False

needsStructuralEq e = case e of
  Var (Embed e') -> needsStructuralEq (flattenExpr e')
  Var _ -> True
  _ ->
    getAny
      ( foldExpr
          nestedDummy
          p
          (const mempty)
          (const mempty)
          e
      )
 where
  p x =
    Any
      ( case x of
          Literal v -> valueNeedsStructuralEq v
          Std s -> stdNeedsStructuralEq s
          Index {} -> True
          U8Index {} -> True
          FrozenLit {} -> True
          GetField {} -> True
          _ -> False
      )

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

data CG = CG
  { cgIdent :: {-# UNPACK #-} !Int
  , cgTag :: {-# UNPACK #-} !Int
  , cgHelpers :: !(M.Map Text Text)
  , cgEmit :: {-# UNPACK #-} !Int
  , cgEmitCtx :: !(Maybe EmitCtx)
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

-- | Wrap helpers + generated decls + result in an IIFE so a minifier treats
-- the result as live (plain expression statements get DCE'd).
renderIIFE s (MkCode decls ref _) =
  let
    stmts = helperDecls s $$ fromMaybe mempty decls
    body = case ref of
      Nothing -> stmts
      Just r -> stmts $$ (("return" <+> r) <> semi)
   in
    "(() => {" <> iifeBody body <> "})()"

-- | Helper definitions ahead of a snippet's own declarations.
renderWithHelpers s code = helperDecls s $$ renderCode code

codesDecls cs = vcat (mapMaybe (\(MkCode a _ _) -> a) cs)

codesRefs = map (\(MkCode _ b _) -> arrayElemRef b)

-- | 'ValueUnit' renders as nothing, since a unit statement emits nothing.
-- As an array element it still occupies a slot, so it has to print — a
-- dropped ref would shorten the literal.
arrayElemRef = fromMaybe "undefined"

-- Codegen counters: `cgIdent` is the next emitted JS name (`n0`, `n1`, …);
-- `cgTag` is a decreasing negative id used only for use-counting/inlining
-- so nested Lets/Binds cannot collide (tags are never valid JS idents).
-- `cgHelpers` is the set of runtime functions the program has called.
startCG = CG 0 (-3) M.empty 0 Nothing

-- | Prepare optimized pure AST and wire batch progress (pack then emit).
-- Requires 'withActiveJob' + 'configProgressSlot' when progress is enabled.
preparePureProgram :: ClosedExpr u -> IO (CG, Expr Stamp u)
preparePureProgram e = do
  mCtx <- captureEmitCtx
  case mCtx of
    Nothing -> do
      t0 <- getMonotonicTime
      let
        !expr = optimize e
      t1 <- getMonotonicTime
      let
        timing =
          PhoasPrepareTiming
            { pptOptimizeSec = seconds t0 t1
            , pptTotalSec = seconds t0 t1
            }
      reportPhoasPrepareTiming timing
      recordJobPhoasPrepare timing
      pure (startCG, expr)
    Just ctx -> do
      reportPackPhase ctx 0 1
      t0 <- getMonotonicTime
      let
        !expr = optimize e
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
      pure (startCG {cgEmitCtx = Just ctx}, expr)
{-# NOINLINE preparePureProgram #-}

prepareEffectProgram :: ClosedEffect u -> IO (CG, Effect Stamp u)
prepareEffectProgram e = do
  mCtx <- captureEmitCtx
  case mCtx of
    Nothing -> do
      t0 <- getMonotonicTime
      let
        !eff = optimizeEffectTree e
      t1 <- getMonotonicTime
      reportPhoasPrepareTiming
        PhoasPrepareTiming
          { pptOptimizeSec = seconds t0 t1
          , pptTotalSec = seconds t0 t1
          }
      pure (startCG, eff)
    Just ctx -> do
      reportPackPhase ctx 0 1
      t0 <- getMonotonicTime
      let
        !eff = optimizeEffectTree e
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
      initEmitCtxTotal ctx (nodeCountEff eff)
      pure (startCG {cgEmitCtx = Just ctx}, eff)
{-# NOINLINE prepareEffectProgram #-}

prepareFlatEffectProgram :: ClosedEffect u -> IO (FlatSoA.FlatSoA, CG)
prepareFlatEffectProgram e = do
  mCtx <- captureEmitCtx
  (soa, _timing, _irNodes, _ir) <- flatPrepareCore e
  case mCtx of
    Nothing -> pure (soa, startCG)
    Just ctx -> do
      initEmitCtxTotal ctx (flatSoaNodeCount soa)
      pure (soa, startCG {cgEmitCtx = Just ctx})
{-# NOINLINE prepareFlatEffectProgram #-}

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
    !rawNodes = Ir.irMetaSize (Ir.metaIrEffect irRaw)
  tMetaRaw1 <- getMonotonicTime
  tOpt0 <- getMonotonicTime
  let
    !(_, !irOpt, !mdOpt) = Ir.optIrEffect (-2) irRaw
    !optNodes = Ir.irMetaSize mdOpt
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
    !optNodes = Ir.irMetaSize (Ir.metaIrEffect irOpt)
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
    !rawNodes = Ir.irMetaSize (Ir.metaIrEffect irRaw)
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
flatPrepareCore (e :: ClosedEffect u) = do
  mCtx <- captureEmitCtx
  tAll0 <- getMonotonicTime
  case mCtx of
    Just ctx -> reportIrPreparePhase ctx 0 1
    Nothing -> pure ()
  t0 <- getMonotonicTime
  let
    !(irOpt, irNodes) = lowerOptEffectIr e
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
{-# NOINLINE flatPrepareCore #-}

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

allocIdent s = (cgIdent s, s {cgIdent = cgIdent s + 1})

useHelperSrc name src s = s {cgHelpers = M.insert name src (cgHelpers s)}

nName n = "n" <> T.pack (show n)

nJS n = jsText (nName n)

constBind n ref = ("const" <+> nJS n <+> "=" <+> ref) <> semi

-- | Optimizer tags (negative) map to emitted `n*` ids during codegen.
varStampJS env s =
  let
    i = stampId s
   in
    if i < 0
      then maybe mempty nJS (IM.lookup i env)
      else nJS i

-- | Ident already allocated for this effect (@Lift (Var n1)@). Not a
-- counter guess: only a binder that is already in the tree.
jsCall f a = parens f <> parens a

-- | Needs no parentheses as an operand: already a primary JS expression.
allocNIdents :: CG -> Int -> ([Int], CG)
allocNIdents s 0 = ([], s)
allocNIdents s n =
  let
    (i, s1) = allocIdent s
    (is, s2) = allocNIdents s1 (n - 1)
   in
    (i : is, s2)

mergeCgHelpers ::
  M.Map Text Text -> M.Map Text Text -> M.Map Text Text
mergeCgHelpers =
  M.unionWith
    ( \existing incoming ->
        if existing == incoming || canonicalHoistSrc existing == canonicalHoistSrc incoming
          then existing
          else
            error "JShark.mergeEmitCG: conflicting hoist helper definitions"
    )

mergeEmitCG a b =
  a
    { cgIdent = max (cgIdent a) (cgIdent b)
    , cgHelpers = mergeCgHelpers (cgHelpers a) (cgHelpers b)
    , cgEmit = max (cgEmit a) (cgEmit b)
    }

mergeEmitCGs :: CG -> [CG] -> CG
mergeEmitCGs = foldl (\acc cg -> mergeEmitCG acc cg `seq` mergeEmitCG acc cg)

flatSoaNodeCount = FlatSoA.flatSoaNodeCount

flatSoaParallelThreshold = FlatSoA.flatSoaParallelThreshold
