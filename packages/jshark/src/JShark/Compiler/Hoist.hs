{-# LANGUAGE OverloadedStrings #-}

-- | Named-lambda hoisting: register shared @$tag@ bindings in 'CG'.
--
-- 'registerHoistedTag' delegates to 'insertHoisted', which deduplicates
-- by source text (see 'JShark.Compiler.Hoist.Canonical.canonicalHoistSrc').
-- Flat IR uses 'emitHoistedFnValue'; PHOAS codegen calls
-- 'registerHoistedTag' after rendering the lambda body.
module JShark.Compiler.Hoist
  ( emitHoistedFnValue
  , registerHoistedTag
  )
where

import Data.Text (Text)
import JShark.Compiler.Codegen.Core (CG (..))
import JShark.Compiler.Emit (JS, jsText, renderJS)
import qualified JShark.Compiler.Flat as Flat
import JShark.Compiler.Hoist.Canonical (hoistTagName)
import JShark.Compiler.JsShim (insertHoisted)

registerHoistedTag :: CG -> Text -> Text -> (CG, Text)
registerHoistedTag s tag src =
  let
    name = hoistTagName tag
   in
    (s {cgPreamble = insertHoisted name src (cgPreamble s)}, name)

emitHoistedFnValue ::
  CG -> Flat.FlatSoA -> Flat.NodeId -> JS -> (CG, JS)
emitHoistedFnValue s view nid fnJs =
  case Flat.flatSoaHoistTag view nid of
    Nothing -> (s, fnJs)
    Just tag ->
      let
        src = renderJS fnJs
        (s', name) = registerHoistedTag s tag src
       in
        (s', jsText name)
