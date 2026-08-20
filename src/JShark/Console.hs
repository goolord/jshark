{-# language DataKinds #-}
{-# language OverloadedStrings #-}

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

-- | @console.log(x)@
log :: Expr f u -> EffectSyntax f ()
log = consoleLog

-- | @console.warn(x)@
warn :: Expr f u -> EffectSyntax f ()
warn x = toSyntax_ (ffi "console.warn" (x <: RecNil))

-- | @console.error(x)@
error_ :: Expr f u -> EffectSyntax f ()
error_ x = toSyntax_ (ffi "console.error" (x <: RecNil))

-- | @console.info(x)@
info :: Expr f u -> EffectSyntax f ()
info x = toSyntax_ (ffi "console.info" (x <: RecNil))
