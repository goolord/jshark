{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}
-- | Minimal @JSON@ wrapper.
module JShark.Json
  ( stringify
  , unsafeParse
  ) where

import JShark.Api
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | @JSON.stringify(x)@
stringify :: Expr f u -> Expr f 'String
stringify x = exprFfi "JSON.stringify" (x <: RecNil)

-- | @JSON.parse(x)@. Unsafe: the result type is asserted by the caller
-- and not checked.
unsafeParse :: Expr f 'String -> Expr f u
unsafeParse x = exprFfi "JSON.parse" (x <: RecNil)
