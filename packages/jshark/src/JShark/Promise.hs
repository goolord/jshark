{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Minimal @Promise@ wrapper: @.then@\/@.catch@ chaining only, not the full API.
--
-- Both return the Promise produced by the call (via 'hold'), so they can be
-- chained or awaited; the handler's return value is the resolution, and a
-- returned Promise is adopted by JS. The @catch@ handler receives the
-- rejection reason, typed by the caller.
module JShark.Promise
  ( Promise
  , promiseThen
  , promiseCatch
  )
where

import Data.Text (Text)
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types

-- | An opaque JS @Promise@ resolving a value in universe @u@.
data Promise (u :: Universe)

-- | @p.then(h)@ \/ @p.catch(h)@: binds the returned Promise as a reusable
-- handle. The handler argument universe is @a@ (resolved value for
-- @then@, rejection reason for @catch@); the result universe is the
-- handler's return type.
promiseMethod ::
  Text
  -> Effect f ('MutableObject (Promise u))
  -> (f a -> Effect f v)
  -> EffectSyntax f (Effect f ('MutableObject (Promise v)))
promiseMethod name p handler =
  hold (callMethod p name (ArgEffect (LambdaE handler) <: RecNil))

-- | @p.then(handler)@ — the handler receives the resolved value as
-- a PHOAS binder (@f u@). Returns the Promise @.then@ produced.
promiseThen ::
  Effect f ('MutableObject (Promise u))
  -> (f u -> Effect f v)
  -> EffectSyntax f (Effect f ('MutableObject (Promise v)))
promiseThen = promiseMethod "then"

-- | @p.catch(handler)@ — the handler receives the rejection reason. The
-- reason's universe is chosen by the caller (JS rejects with any value).
promiseCatch ::
  Effect f ('MutableObject (Promise u))
  -> (f r -> Effect f v)
  -> EffectSyntax f (Effect f ('MutableObject (Promise v)))
promiseCatch = promiseMethod "catch"
