{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}
-- | Wrappers over the JS @console@ object.
-- Import qualified; names clash with 'Prelude'.
module JShark.Console
  ( log
  , warn
  , error
  , info
  ) where

import Prelude hiding (error, log)
import JShark.Api
import JShark.Rec ((<:), Rec(..))
import JShark.Types

log :: Expr f u -> EffectSyntax f ()
log x = toSyntax_ (ffi "console.log" (arg x <: RecNil))

warn :: Expr f u -> EffectSyntax f ()
warn x = toSyntax_ (ffi "console.warn" (arg x <: RecNil))

error :: Expr f u -> EffectSyntax f ()
error x = toSyntax_ (ffi "console.error" (arg x <: RecNil))

info :: Expr f u -> EffectSyntax f ()
info x = toSyntax_ (ffi "console.info" (arg x <: RecNil))
