{-# LANGUAGE LambdaCase #-}

-- | Emit-time view over packed flat SoA (on-demand node decode).
module JShark.FlatView
  ( FlatIRView
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
  , NodeId
  , flatNodeIsEffect
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

type FlatIRView = FlatSoA

firRoot :: FlatIRView -> NodeId
firRoot = fsaRoot

firNodeCount :: FlatIRView -> Int
firNodeCount = flatSoaNodeCount

firNode :: FlatIRView -> NodeId -> FlatNode
firNode = flatSoaNode

firLitValue :: FlatIRView -> Int -> Value u
firLitValue = flatSoaLitValue

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

firIdentBudget :: FlatIRView -> NodeId -> Int
firIdentBudget = flatSoaIdentBudget

firLayerBuckets :: FlatIRView -> NodeId -> V.Vector (V.Vector NodeId)
firLayerBuckets = flatSoaLayerBuckets

firNodeIsEffect :: FlatNode -> Bool
firNodeIsEffect = flatNodeIsEffect
