{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Minimal @Promise@ wrapper: @.then@/@.catch@ chaining only, not the full API.
module JShark.Promise
  ( Promise
  , promiseThen
  , promiseCatch
  )
where

import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types

data Promise (u :: Universe)

promiseMethod ::
  String
  -> Effect f ('MutableObject (Promise u))
  -> (f u -> Effect f v)
  -> EffectSyntax f (f v)
promiseMethod name p handler =
  toSyntax $ callMethod p name (ArgEffect (LambdaE handler) <: RecNil)

-- | @p.then(handler)@ — the handler receives the resolved value as
-- a PHOAS binder (@f u@, so it names itself via toSyntax/bindExpr).
promiseThen ::
  Effect f ('MutableObject (Promise u))
  -> (f u -> Effect f v)
  -> EffectSyntax f (f v)
promiseThen = promiseMethod "then"

-- | @p.catch(handler)@ — the handler receives the rejection reason.
promiseCatch ::
  Effect f ('MutableObject (Promise u))
  -> (f u -> Effect f v)
  -> EffectSyntax f (f v)
promiseCatch = promiseMethod "catch"
