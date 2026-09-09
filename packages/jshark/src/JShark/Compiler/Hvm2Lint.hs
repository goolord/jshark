{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Find pure closed subtrees that compile to Bend/HVM2 kernels.
module JShark.Compiler.Hvm2Lint
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
import JShark.Api.Types (ClosedEffect, ClosedExpr)
import JShark.Compiler.EmitBend (emitBendKernel, peelLambdas)
import JShark.Compiler.Ir
  ( IrNode (..)
  , irFieldChild
  , irPure
  , irSize
  , metaIr
  , data IrLiteral
  )
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
  scanIr defaultHvm2MinCandidateSize 0 (irOptimizedExprFromClosed e)

hvm2CandidatesFromEffect :: ClosedEffect u -> [Hvm2Candidate]
hvm2CandidatesFromEffect (e :: ClosedEffect u) =
  scanIr defaultHvm2MinCandidateSize 0 (irOptimizedEffectFromClosed e)

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

-- | Every node is visited once; only expression-kind subtrees are candidate
-- checked (effects were never candidates in the typed IR either).
scanIr :: Int -> Int -> IrNode -> [Hvm2Candidate]
scanIr minSize n e =
  let
    (here, n') = checkIr minSize n e
   in
    here <> concatMap (scanIr minSize n') (irKids e)

checkIr :: Int -> Int -> IrNode -> ([Hvm2Candidate], Int)
checkIr minSize n e
  | isEffectNode e = ([], n)
  | not (irPure (metaIr e)) = ([], n)
  | irSize (metaIr e) < minSize = ([], n)
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
                , hvm2CandidateSize = irSize (metaIr e)
                , hvm2CandidateParams = length tags
                , hvm2CandidatePreview = preview
                }
           in
            ([cand], n + 1)

candidateName :: Int -> Text
candidateName i = "candidate_" <> T.pack (show i)

-- | The node constructors that denote effect subtrees (mirrors the flat
-- @FX_@ split). Expression-kind nodes are everything else.
isEffectNode :: IrNode -> Bool
isEffectNode = \case
  IrLift {} -> True
  IrFFI {} -> True
  IrUnsafeObject {} -> True
  IrUnsafeObjectGet {} -> True
  IrUnsafeObjectAssign {} -> True
  IrCallMethod {} -> True
  IrBind {} -> True
  IrThenE {} -> True
  IrBindRec {} -> True
  IrLambdaE {} -> True
  IrApplyE {} -> True
  IrIfE {} -> True
  IrWhile {} -> True
  IrForRange {} -> True
  IrU8Set {} -> True
  IrU8Fill {} -> True
  IrOptionCaseE {} -> True
  IrResultCaseE {} -> True
  IrStringCaseE {} -> True
  IrThrow {} -> True
  IrTry {} -> True
  IrObjectLit {} -> True
  IrDeleteProp {} -> True
  IrArrayLit {} -> True
  _ -> False

irKids :: IrNode -> [IrNode]
irKids = \case
  IrLiteral {} -> []
  IrVar {} -> []
  IrLet _ _ x b -> [x, b]
  IrLetRec _ r b -> [r, b]
  IrLambda _ _ b -> [b]
  IrApply f x -> [f, x]
  IrIf c t eF -> [c, t, eF]
  IrOptionCase o n _ s -> [o, n, s]
  IrResultOk x -> [x]
  IrResultErr x -> [x]
  IrResultCase o _ er _ ok -> [o, er, ok]
  IrIndex x i -> [x, i]
  IrU8Index x i -> [x, i]
  IrError x -> [x]
  IrFixed _ args -> args
  IrFnLit _ _ b -> [b]
  IrUnsafeNullable x -> [x]
  IrFrozenLit fs -> map irFieldChild fs
  IrGetField _ o -> [o]
  IrHvm2Ref {} -> []
  KConcat x y -> [x, y]
  KPlus x y -> [x, y]
  KTimes x y -> [x, y]
  KMinus x y -> [x, y]
  KNegate x -> [x]
  KFracDiv x y -> [x, y]
  KRem x y -> [x, y]
  KBitAnd x y -> [x, y]
  KBitOr x y -> [x, y]
  KBitXor x y -> [x, y]
  KShl x y -> [x, y]
  KShr x y -> [x, y]
  KUShr x y -> [x, y]
  KBig _ x y -> [x, y]
  KBigNeg x -> [x]
  KAnd x y -> [x, y]
  KOr x y -> [x, y]
  KEq _ x y -> [x, y]
  KNEq _ x y -> [x, y]
  KGTh x y -> [x, y]
  KLTh x y -> [x, y]
  KGTEq x y -> [x, y]
  KLTEq x y -> [x, y]
  KShow x -> [x]
  KTypeOf x -> [x]
  IrMethMap x _ g -> [x, g]
  IrMethFilter x _ g -> [x, g]
  IrMethReduce x z _ _ g -> [x, z, g]
  IrMethReduceRight x z _ _ g -> [x, z, g]
  IrMethToSorted x _ _ g -> [x, g]
  IrMethFrom n _ g -> [n, g]
  IrLift x -> [x]
  IrFFI _ args -> args
  IrUnsafeObject {} -> []
  IrUnsafeObjectGet x _ -> [x]
  IrUnsafeObjectAssign x y -> [x, y]
  IrCallMethod x _ args -> x : args
  IrBind _ _ x b -> [x, b]
  IrThenE x y -> [x, y]
  IrBindRec _ r b -> [r, b]
  IrLambdaE _ b -> [b]
  IrApplyE f x -> [f, x]
  IrIfE c t eF -> [c, t, eF]
  IrWhile c b -> [c, b]
  IrForRange s e _ b -> [s, e, b]
  IrU8Set b i v -> [b, i, v]
  IrU8Fill b v -> [b, v]
  IrOptionCaseE o n _ s -> [o, n, s]
  IrResultCaseE o _ er _ ok -> [o, er, ok]
  IrStringCaseE s arms d -> s : map snd arms ++ [d]
  IrThrow x -> [x]
  IrTry a _ k -> [a, k]
  IrObjectLit fs -> map irFieldChild fs
  IrDeleteProp o k -> [o, k]
  IrArrayLit es -> es
