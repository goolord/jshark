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

console_ :: String -> Expr f u -> EffectSyntax f ()
console_ name x = toSyntax_ (ffi ("console." ++ name) (arg x <: RecNil))

log :: Expr f u -> EffectSyntax f ()
log = console_ "log"

warn :: Expr f u -> EffectSyntax f ()
warn = console_ "warn"

error :: Expr f u -> EffectSyntax f ()
error = console_ "error"

info :: Expr f u -> EffectSyntax f ()
info = console_ "info"
