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

-- | @JSON.stringify(x)@. Assumed observationally pure (no mutation; the
-- result is determined by @x@). Implemented via 'unsafeExprFfi'.
stringify :: Expr f u -> Expr f 'String
stringify x = unsafeExprFfi "JSON.stringify" (x <: RecNil)

-- | @JSON.parse(x)@. Unsafe: the result type is asserted by the caller
-- and not checked.
unsafeParse :: Expr f 'String -> Expr f u
unsafeParse x = unsafeExprFfi "JSON.parse" (x <: RecNil)
