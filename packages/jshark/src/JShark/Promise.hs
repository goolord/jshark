{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Minimal @Promise@ wrapper: @.then@\/@.catch@ chaining only, not the full API.
--
-- Both return the Promise produced by the call (via 'hold'), so they can be
-- chained or awaited. JS __adopts__ a returned thenable: if a @.then@
-- handler returns a 'Promise', the result resolves to that promise's
-- value, not to the promise itself. 'Resolved' encodes that so the types
-- stay truthful. A returned non-promise resolves directly.
--
-- @.catch@ only runs on rejection. An already-fulfilled input passes its
-- value through untouched, so the handler must recover to the original
-- resolution type 'u' and the result is a @Promise u@; use 'promiseThen'
-- to change the resolution type.
module JShark.Promise
  ( Promise
  , Resolved
  , promiseThen
  , promiseCatch
  )
where

import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types

-- | An opaque JS @Promise@ resolving a value in universe @u@.
data Promise (u :: Universe)

-- | The resolution type produced when JS adopts a handler's return
-- value: a returned @Promise v@ resolves to @v@; anything else to itself.
type family Resolved (u :: Universe) :: Universe where
  Resolved ('MutableObject (Promise v)) = v
  Resolved u = u

-- | @p.then(handler)@ — the handler receives the resolved value as a PHOAS
-- binder (@f u@). Returns the Promise @.then@ produced. A handler that
-- returns @Promise v@ makes the result a @Promise v@ ('Resolved').
promiseThen ::
  Effect f ('MutableObject (Promise u))
  -> (f u -> Effect f v)
  -> EffectSyntax f (Effect f ('MutableObject (Promise (Resolved v))))
promiseThen p handler =
  hold (callMethod p "then" (ArgEffect (LambdaE handler) <: RecNil))

-- | @p.catch(handler)@ — the handler receives the rejection reason and must
-- recover to the promise's original resolution type @u@, so the result is
-- a @Promise u@ on both the fulfilled and rejected paths. The reason's
-- universe is chosen by the caller (JS rejects with any value).
promiseCatch ::
  Effect f ('MutableObject (Promise u))
  -> (f r -> Effect f u)
  -> EffectSyntax f (Effect f ('MutableObject (Promise u)))
promiseCatch p handler =
  hold (callMethod p "catch" (ArgEffect (LambdaE handler) <: RecNil))
