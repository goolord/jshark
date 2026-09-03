{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Emit-time view over packed flat SoA (on-demand node decode).
module JShark.Compiler.FlatView
  ( FlatIRView
  , firRoot
  , firNodeCount
  , firNode
  , withLitValue
  , firText
  , firFFI
  , firStrCases
  , firFieldGroup
  , firArgGroup
  , firNodePackRefs
  , firHoistTag
  , firParamName
  , firIdentBudget
  , firLayerBuckets
  , firNodeIsEffect
  )
where

import Data.Text (Text)
import qualified Data.Vector as V
import JShark.Api.Types (FFIForm, Value)
import JShark.Compiler.Flat
  ( FlatArg
  , FlatField
  , FlatNode (..)
  , NodeId
  , flatNodeIsEffect
  )
import JShark.Compiler.FlatSoA
  ( FlatSoA
  , flatSoaArgGroup
  , flatSoaFFI
  , flatSoaFieldGroup
  , flatSoaIdentBudget
  , flatSoaLayerBuckets
  , flatSoaNode
  , flatSoaNodeCount
  , flatSoaNodePackRefs
  , flatSoaStrCases
  , flatSoaText
  , fsaHoistTags
  , fsaParamNames
  , fsaRoot
  , withFlatLitValue
  )

type FlatIRView = FlatSoA

firRoot :: FlatIRView -> NodeId
firRoot = fsaRoot

firNodeCount :: FlatIRView -> Int
firNodeCount = flatSoaNodeCount

firNode :: FlatIRView -> NodeId -> FlatNode
firNode = flatSoaNode

withLitValue :: FlatIRView -> Int -> (forall u. Value u -> r) -> r
withLitValue = withFlatLitValue

firText :: FlatIRView -> Int -> Text
firText = flatSoaText

firFFI :: FlatIRView -> Int -> FFIForm
firFFI = flatSoaFFI

firStrCases :: FlatIRView -> Int -> [(Text, NodeId)]
firStrCases = flatSoaStrCases

firFieldGroup :: FlatIRView -> Int -> [FlatField]
firFieldGroup = flatSoaFieldGroup

firArgGroup :: FlatIRView -> Int -> [FlatArg]
firArgGroup = flatSoaArgGroup

firNodePackRefs :: FlatIRView -> FlatNode -> [NodeId]
firNodePackRefs = flatSoaNodePackRefs

firHoistTag :: FlatIRView -> NodeId -> Maybe Text
firHoistTag view i =
  if i >= 0 && i < V.length (fsaHoistTags view)
    then fsaHoistTags view V.! i
    else Nothing

firParamName :: FlatIRView -> NodeId -> Maybe Text
firParamName view i =
  if i >= 0 && i < V.length (fsaParamNames view)
    then fsaParamNames view V.! i
    else Nothing

firIdentBudget :: FlatIRView -> NodeId -> Int
firIdentBudget = flatSoaIdentBudget

firLayerBuckets :: FlatIRView -> NodeId -> V.Vector (V.Vector NodeId)
firLayerBuckets = flatSoaLayerBuckets

firNodeIsEffect :: FlatNode -> Bool
firNodeIsEffect = flatNodeIsEffect
