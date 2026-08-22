{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}
-- | Wrappers over the JS @console@ object.
module JShark.Console
  ( log
  , warn
  , error_
  , info
  ) where

import Prelude hiding (log)
import JShark.Api
import JShark.Rec ((<:), Rec(..))
import JShark.Types

log :: Expr f u -> EffectSyntax f ()
log x = toSyntax_ (ffi "console.log" (arg x <: RecNil))

warn :: Expr f u -> EffectSyntax f ()
warn x = toSyntax_ (ffi "console.warn" (arg x <: RecNil))

error_ :: Expr f u -> EffectSyntax f ()
error_ x = toSyntax_ (ffi "console.error" (arg x <: RecNil))

info :: Expr f u -> EffectSyntax f ()
info x = toSyntax_ (ffi "console.info" (arg x <: RecNil))
