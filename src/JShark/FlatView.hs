{-# LANGUAGE LambdaCase #-}

-- | Unified flat IR view for emit: 'FlatProgram' nodes or on-demand SoA decode.
module JShark.FlatView
  ( FlatIRView (..)
  , firRoot
  , firNodeCount
  , firNode
  , firLitValue
  , firText
  , firFFI
  , firStrCases
  , firFieldGroup
  , firArgGroup
  , firNodePackRefs
  , firIdentBudget
  , firLayerBuckets
  , firNodeIsEffect
  )
where

import Data.Text (Text)
import qualified Data.Vector as V
import JShark.Flat
  ( FlatArg
  , FlatField
  , FlatNode (..)
  , FlatProgram (..)
  , NodeId
  , flatArgGroup
  , flatFFI
  , flatFieldGroup
  , flatIdentBudget
  , flatLayerBuckets
  , flatLitValue
  , flatNode
  , flatNodeIsEffect
  , flatNodePackRefs
  , flatStrCases
  , flatText
  , fpRootEffect
  )
import JShark.FlatSoA
  ( FlatSoA
  , flatSoaArgGroup
  , flatSoaFFI
  , flatSoaFieldGroup
  , flatSoaIdentBudget
  , flatSoaLayerBuckets
  , flatSoaLitValue
  , flatSoaNode
  , flatSoaNodeCount
  , flatSoaNodePackRefs
  , flatSoaStrCases
  , flatSoaText
  , fsaRoot
  )
import JShark.Types (FFIForm, Value)

data FlatIRView
  = FIRProgram !FlatProgram
  | FIRSoa !FlatSoA

firRoot :: FlatIRView -> NodeId
firRoot = \case
  FIRProgram p -> fpRootEffect p
  FIRSoa soa -> fsaRoot soa

firNodeCount :: FlatIRView -> Int
firNodeCount = \case
  FIRProgram p -> V.length (fpNodes p)
  FIRSoa soa -> flatSoaNodeCount soa

firNode :: FlatIRView -> NodeId -> FlatNode
firNode v i = case v of
  FIRProgram p -> flatNode p i
  FIRSoa soa -> flatSoaNode soa i

firLitValue :: FlatIRView -> Int -> Value u
firLitValue v i = case v of
  FIRProgram p -> flatLitValue p i
  FIRSoa soa -> flatSoaLitValue soa i

firText :: FlatIRView -> Int -> Text
firText v i = case v of
  FIRProgram p -> flatText p i
  FIRSoa soa -> flatSoaText soa i

firFFI :: FlatIRView -> Int -> FFIForm
firFFI v i = case v of
  FIRProgram p -> flatFFI p i
  FIRSoa soa -> flatSoaFFI soa i

firStrCases :: FlatIRView -> Int -> [(Text, NodeId)]
firStrCases v i = case v of
  FIRProgram p -> flatStrCases p i
  FIRSoa soa -> flatSoaStrCases soa i

firFieldGroup :: FlatIRView -> Int -> [FlatField]
firFieldGroup v i = case v of
  FIRProgram p -> flatFieldGroup p i
  FIRSoa soa -> flatSoaFieldGroup soa i

firArgGroup :: FlatIRView -> Int -> [FlatArg]
firArgGroup v i = case v of
  FIRProgram p -> flatArgGroup p i
  FIRSoa soa -> flatSoaArgGroup soa i

firNodePackRefs :: FlatIRView -> FlatNode -> [NodeId]
firNodePackRefs v node = case v of
  FIRProgram p -> flatNodePackRefs p node
  FIRSoa soa -> flatSoaNodePackRefs soa node

firIdentBudget :: FlatIRView -> NodeId -> Int
firIdentBudget v i = case v of
  FIRProgram p -> flatIdentBudget p i
  FIRSoa soa -> flatSoaIdentBudget soa i

firLayerBuckets :: FlatIRView -> NodeId -> V.Vector (V.Vector NodeId)
firLayerBuckets v root = case v of
  FIRProgram p -> flatLayerBuckets p root
  FIRSoa soa -> flatSoaLayerBuckets soa root

firNodeIsEffect :: FlatNode -> Bool
firNodeIsEffect = flatNodeIsEffect
