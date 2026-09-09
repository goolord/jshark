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
import JShark.Compiler.Flat (irNodeIsEffect)
import JShark.Compiler.Ir
  ( IrNode (..)
  , irNodeChildren
  , irPure
  , irSize
  , metaIr
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
    here <> concatMap (scanIr minSize n') (irNodeChildren e)

checkIr :: Int -> Int -> IrNode -> ([Hvm2Candidate], Int)
checkIr minSize n e
  | irNodeIsEffect e = ([], n)
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
