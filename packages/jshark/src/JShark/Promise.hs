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

promiseThen ::
  Effect f ('MutableObject (Promise u))
  -> (f u -> Effect f v)
  -> EffectSyntax f (f v)
promiseThen = promiseMethod "then"

promiseCatch ::
  Effect f ('MutableObject (Promise u))
  -> (f u -> Effect f v)
  -> EffectSyntax f (f v)
promiseCatch = promiseMethod "catch"
