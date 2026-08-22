{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}

-- | Minimal @JSON@ wrapper.
module JShark.Json
  ( stringify
  , unsafeParse
  , tryParse
  ) where

import JShark.Api
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | @JSON.stringify(x)@. Observationally pure (no mutation; the result
-- is determined by @x@). Closed-name 'ExprUnary', not a general FFI.
stringify :: Expr f u -> Expr f 'String
stringify = ExprUnary StdStringify

-- | @JSON.parse(x)@. Throws on bad JSON, so this is an 'Effect'. The
-- result type is asserted by the caller and not checked.
unsafeParse :: Expr f 'String -> Effect f u
unsafeParse x = ffi "JSON.parse" (arg x <: RecNil)

-- | @JSON.parse@ that yields 'none' on throw.
tryParse :: Expr f 'String -> Effect f ('Option u)
tryParse s =
  catch_
    (Bind (unsafeParse s) (\x -> Lift (some (Var x))))
    (\_ -> expr none)
