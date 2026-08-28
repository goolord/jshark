{-# LANGUAGE OverloadedStrings #-}

-- | Named-lambda hoisting: register shared @$tag@ helpers in 'CG'.
--
-- 'registerHoistedTag' deduplicates by source text (see
-- 'JShark.Compiler.Hoist.Canonical.canonicalHoistSrc'). Flat IR uses
-- 'emitHoistedFnValue'; PHOAS codegen calls 'registerHoistedTag' after
-- rendering the lambda body.
module JShark.Compiler.Hoist
  ( canonicalHoistSrc
  , emitHoistedFnValue
  , hoistTagName
  , registerHoistedTag
  )
where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Compiler.Codegen.Core (CG (..))
import JShark.Compiler.Emit (JS, jsText, renderJS)
import qualified JShark.Compiler.Flat as Flat
import qualified JShark.Compiler.FlatView as FlatView
import JShark.Compiler.Hoist.Canonical
  ( canonicalHoistSrc
  , hoistTagName
  )

registerHoistedTag :: CG -> Text -> Text -> (CG, Text)
registerHoistedTag s tag src =
  let
    name = hoistTagName tag
    canon = canonicalHoistSrc src
   in
    case M.lookup name (cgHelpers s) of
      Just existing
        | existing == src || canonicalHoistSrc existing == canon -> (s, name)
        | otherwise ->
            error
              ( "JShark.registerHoistedTag: hoist tag "
                  <> T.unpack tag
                  <> " already registered with different body"
              )
      Nothing ->
        ( s {cgHelpers = M.insert name src (cgHelpers s)}
        , name
        )

emitHoistedFnValue ::
  CG -> FlatView.FlatIRView -> Flat.NodeId -> JS -> (CG, JS)
emitHoistedFnValue s view nid fnJs =
  case FlatView.firHoistTag view nid of
    Nothing -> (s, fnJs)
    Just tag ->
      let
        src = renderJS fnJs
        (s', name) = registerHoistedTag s tag src
       in
        (s', jsText name)
