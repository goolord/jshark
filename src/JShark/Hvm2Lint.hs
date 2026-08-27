{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Find pure closed subtrees that compile to Bend/HVM2 kernels.
module JShark.Hvm2Lint
  ( Hvm2Candidate (..)
  , defaultHvm2MinCandidateSize
  , hvm2CandidatesFromEffect
  , hvm2CandidatesFromExpr
  , warnHvm2CandidatesEffect
  , warnHvm2CandidatesExpr
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import JShark
  ( irOptimizedEffectFromClosed
  , irOptimizedExprFromClosed
  )
import JShark.EmitBend (emitBendKernel, peelLambdas)
import JShark.Ir
  ( IrArg (..)
  , IrEffect (..)
  , IrExpr (..)
  , IrFieldLit (..)
  , irMetaPure
  , irMetaSize
  , metaIrExpr
  )
import qualified JShark.Ir as Ir
import JShark.Rec (Rec (..))
import JShark.Types (ClosedEffect, ClosedExpr)
import System.IO (hPutStrLn, stderr)

data Hvm2Candidate = Hvm2Candidate
  { hvm2CandidateName :: !Text
  , hvm2CandidateSize :: !Int
  , hvm2CandidateParams :: !Int
  , hvm2CandidatePreview :: !Text
  }
  deriving (Eq, Show)

defaultHvm2MinCandidateSize :: Int
defaultHvm2MinCandidateSize = 8

hvm2CandidatesFromExpr :: ClosedExpr u -> [Hvm2Candidate]
hvm2CandidatesFromExpr (e :: ClosedExpr u) =
  scanIrExprs defaultHvm2MinCandidateSize 0 (irOptimizedExprFromClosed e)

hvm2CandidatesFromEffect :: ClosedEffect u -> [Hvm2Candidate]
hvm2CandidatesFromEffect (e :: ClosedEffect u) =
  scanIrEffect defaultHvm2MinCandidateSize 0 (irOptimizedEffectFromClosed e)

warnHvm2CandidatesExpr :: ClosedExpr u -> IO ()
warnHvm2CandidatesExpr e = mapM_ printCandidate (hvm2CandidatesFromExpr e)

warnHvm2CandidatesEffect :: ClosedEffect u -> IO ()
warnHvm2CandidatesEffect e = mapM_ printCandidate (hvm2CandidatesFromEffect e)

printCandidate :: Hvm2Candidate -> IO ()
printCandidate c =
  hPutStrLn stderr
    $ T.unpack
    $ "hvm2-candidate: "
      <> hvm2CandidateName c
      <> " (size "
      <> T.pack (show (hvm2CandidateSize c))
      <> ", "
      <> T.pack (show (hvm2CandidateParams c))
      <> " param(s)): consider `hvm2Kernel "
      <> hvm2CandidateName c
      <> " (...)` — "
      <> T.take 72 (T.strip (hvm2CandidatePreview c))

scanIrEffect :: Int -> Int -> IrEffect u -> [Hvm2Candidate]
scanIrEffect minSize n = \case
  IrLift x ->
    scanIrExprs minSize n x
  IrFFI _ args ->
    scanIrArgs minSize n args
  IrUnsafeObjectGet x _ ->
    scanIrEffect minSize n x
  IrUnsafeObjectAssign x y ->
    scanIrEffect minSize n x <> scanIrEffect minSize n y
  IrCallMethod x _ args ->
    scanIrEffect minSize n x <> scanIrArgs minSize n args
  IrBind _ x y ->
    scanIrEffect minSize n x <> scanIrEffect minSize n y
  IrThenE x y ->
    scanIrEffect minSize n x <> scanIrEffect minSize n y
  IrBindRec _ x y ->
    scanIrEffect minSize n x <> scanIrEffect minSize n y
  IrLambdaE _ y ->
    scanIrEffect minSize n y
  IrApplyE x y ->
    scanIrEffect minSize n x <> scanIrEffect minSize n y
  IrIfE c t e ->
    scanIrEffect minSize n c <> scanIrEffect minSize n t <> scanIrEffect minSize n e
  IrWhile c b ->
    scanIrEffect minSize n c <> scanIrEffect minSize n b
  IrForRange s e _ b ->
    scanIrExprs minSize n s <> scanIrExprs minSize n e <> scanIrEffect minSize n b
  IrU8Set b i v ->
    scanIrExprs minSize n b <> scanIrExprs minSize n i <> scanIrExprs minSize n v
  IrU8Fill b v ->
    scanIrExprs minSize n b <> scanIrExprs minSize n v
  IrOptionCaseE o noneE _ someE ->
    scanIrExprs minSize n o
      <> scanIrEffect minSize n noneE
      <> scanIrEffect minSize n someE
  IrResultCaseE o _ er _ ok ->
    scanIrExprs minSize n o
      <> scanIrEffect minSize n er
      <> scanIrEffect minSize n ok
  IrStringCaseE o arms d ->
    scanIrExprs minSize n o
      <> concatMap (scanIrEffect minSize n . snd) arms
      <> scanIrEffect minSize n d
  IrThrow x ->
    scanIrExprs minSize n x
  IrTry x _ k ->
    scanIrEffect minSize n x <> scanIrEffect minSize n k
  IrObjectLit fs ->
    concatMap (scanIrFieldLit minSize n) fs
  IrDeleteProp o k ->
    scanIrEffect minSize n o <> scanIrExprs minSize n k
  IrArrayLit es ->
    concatMap (scanIrEffect minSize n) es
  IrUnsafeObject {} ->
    []

scanIrFieldLit :: Int -> Int -> IrFieldLit r -> [Hvm2Candidate]
scanIrFieldLit minSize n = \case
  IrFieldLitEffect e -> scanIrEffect minSize n e
  IrFieldLitExtraEffect e -> scanIrEffect minSize n e
  IrFieldLit x -> scanIrExprs minSize n x
  IrFieldLitExtra x -> scanIrExprs minSize n x

scanIrArgs :: Int -> Int -> Rec IrArg us -> [Hvm2Candidate]
scanIrArgs minSize n = \case
  RecNil -> []
  RecCons (IrArgExpr x) rest ->
    scanIrExprs minSize n x <> scanIrArgs minSize n rest
  RecCons (IrArgEffect x) rest ->
    scanIrEffect minSize n x <> scanIrArgs minSize n rest

scanIrExprs :: Int -> Int -> IrExpr u -> [Hvm2Candidate]
scanIrExprs minSize n e =
  let
    (here, n') = checkIrExpr minSize n e
    below = concatMap (scanSome minSize n') (irKids e)
   in
    here <> below

scanSome :: Int -> Int -> SomeIrExpr -> [Hvm2Candidate]
scanSome minSize n = \case
  SomeIrExpr x -> scanIrExprs minSize n x
  SomeIrEffect x -> scanIrEffect minSize n x
  SomeIrFnBody x -> scanIrFnBody minSize n x
  SomeIrFieldLit x -> scanIrFieldLit minSize n x

checkIrExpr :: Int -> Int -> IrExpr u -> ([Hvm2Candidate], Int)
checkIrExpr minSize n e
  | IrHvm2Ref {} <- e = ([], n)
  | not (irMetaPure (metaIrExpr e)) = ([], n)
  | irMetaSize (metaIrExpr e) < minSize = ([], n)
  | otherwise =
      case emitBendKernel (candidateName n) e of
        Left _ ->
          ([], n)
        Right bend ->
          let
            (tags, _) = peelLambdas e
            preview = T.takeWhile (/= '\n') bend
            cand =
              Hvm2Candidate
                { hvm2CandidateName = candidateName n
                , hvm2CandidateSize = irMetaSize (metaIrExpr e)
                , hvm2CandidateParams = length tags
                , hvm2CandidatePreview = preview
                }
           in
            ([cand], n + 1)

candidateName :: Int -> Text
candidateName i = "candidate_" <> T.pack (show i)

irKids :: IrExpr u -> [SomeIrExpr]
irKids = \case
  IrLiteral {} -> []
  IrVar {} -> []
  IrEmbedEff e -> [SomeIrEffect e]
  IrLet _ x b -> [SomeIrExpr x, SomeIrExpr b]
  IrLetRec _ r b -> [SomeIrExpr r, SomeIrExpr b]
  IrLambda _ _ b -> [SomeIrExpr b]
  IrApply f x -> [SomeIrExpr f, SomeIrExpr x]
  IrIf c t eF -> [SomeIrExpr c, SomeIrExpr t, SomeIrExpr eF]
  IrOptionCase o n _ s -> [SomeIrExpr o, SomeIrExpr n, SomeIrExpr s]
  IrResultOk x -> [SomeIrExpr x]
  IrResultErr x -> [SomeIrExpr x]
  IrResultCase o _ er _ ok -> [SomeIrExpr o, SomeIrExpr er, SomeIrExpr ok]
  IrIndex x i -> [SomeIrExpr x, SomeIrExpr i]
  IrU8Index x i -> [SomeIrExpr x, SomeIrExpr i]
  IrError x -> [SomeIrExpr x]
  IrFixed _ args -> irFixedKids args
  IrKernelK k -> irKernelKids k
  IrMethod m -> irMethodKids m
  IrFnLit b -> [SomeIrFnBody b]
  IrUnsafeNullable x -> [SomeIrExpr x]
  IrFrozenLit fs -> map SomeIrFieldLit fs
  IrGetField o -> [SomeIrExpr o]
  IrHvm2Ref {} -> []

data SomeIrExpr where
  SomeIrExpr :: IrExpr u -> SomeIrExpr
  SomeIrEffect :: IrEffect u -> SomeIrExpr
  SomeIrFnBody :: Ir.IrFnBody us r -> SomeIrExpr
  SomeIrFieldLit :: Ir.IrFieldLit r -> SomeIrExpr

scanIrFnBody :: Int -> Int -> Ir.IrFnBody us r -> [Hvm2Candidate]
scanIrFnBody minSize n = \case
  Ir.IrJfNil x -> scanIrExprs minSize n x
  Ir.IrJfCons _ b -> scanIrFnBody minSize n b

irFixedKids :: Ir.IrFixedArgs a b c -> [SomeIrExpr]
irFixedKids = \case
  Ir.IrArgsU x -> [SomeIrExpr x]
  Ir.IrArgsB x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.IrArgsT x y z -> [SomeIrExpr x, SomeIrExpr y, SomeIrExpr z]

irKernelKids :: Ir.IrKernel u -> [SomeIrExpr]
irKernelKids = \case
  Ir.KPlus x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KTimes x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KMinus x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KNegate x -> [SomeIrExpr x]
  Ir.KFracDiv x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KRem x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KBitAnd x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KBitOr x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KBitXor x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KShl x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KShr x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KUShr x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KBig _ x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KBigNeg x -> [SomeIrExpr x]
  Ir.KConcat x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KShow x -> [SomeIrExpr x]
  Ir.KTypeOf x -> [SomeIrExpr x]
  Ir.KAnd x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KOr x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KEq _ x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KNEq _ x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KGTh x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KLTh x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KGTEq x y -> [SomeIrExpr x, SomeIrExpr y]
  Ir.KLTEq x y -> [SomeIrExpr x, SomeIrExpr y]

irMethodKids :: Ir.IrMethod u -> [SomeIrExpr]
irMethodKids = \case
  Ir.IrMethMap x _ g -> [SomeIrExpr x, SomeIrExpr g]
  Ir.IrMethFilter x _ g -> [SomeIrExpr x, SomeIrExpr g]
  Ir.IrMethReduce x z _ _ g -> [SomeIrExpr x, SomeIrExpr z, SomeIrExpr g]
  Ir.IrMethReduceRight x z _ _ g -> [SomeIrExpr x, SomeIrExpr z, SomeIrExpr g]
  Ir.IrMethToSorted x _ _ g -> [SomeIrExpr x, SomeIrExpr g]
  Ir.IrMethFrom n _ g -> [SomeIrExpr n, SomeIrExpr g]
