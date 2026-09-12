{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Wrappers over the JS @console@ object.
-- Import qualified; names clash with 'Prelude'.
module JShark.Console
  ( log
  , warn
  , error
  , info
  )
where

import Data.Text (Text)
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import Prelude hiding (error, log)

console_ :: Text -> Expr f u -> EffectSyntax f ()
console_ name x = toSyntax_ (ffi ("console." <> name) (arg x <: RecNil))

-- | @console.log(x)@.
log :: Expr f u -> EffectSyntax f ()
log = console_ "log"

-- | @console.warn(x)@.
warn :: Expr f u -> EffectSyntax f ()
warn = console_ "warn"

-- | @console.error(x)@.
error :: Expr f u -> EffectSyntax f ()
error = console_ "error"

-- | @console.info(x)@.
info :: Expr f u -> EffectSyntax f ()
info = console_ "info"
