{-# LANGUAGE
    DataKinds
  , KindSignatures
  , OverloadedStrings
#-}
-- | Minimal @Promise@ wrapper: @.then@/@.catch@ chaining only, not the full API.
module JShark.Promise
  ( Promise
  , promiseThen
  , promiseCatch
  ) where

import JShark.Api
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | An opaque phantom type representing a @Promise@ resolving to a value of
-- universe @u@.
data Promise (u :: Universe)

-- | @promise.then(function(x){...})@
promiseThen :: Effect f ('MutableObject (Promise u)) -> (f u -> Effect f v) -> EffectSyntax f (f v)
promiseThen p handler = toSyntax $ callMethod p "then" (ArgEffect (LambdaE handler) <: RecNil)

-- | @promise.catch(function(err){...})@
promiseCatch :: Effect f ('MutableObject (Promise u)) -> (f u -> Effect f v) -> EffectSyntax f (f v)
promiseCatch p handler = toSyntax $ callMethod p "catch" (ArgEffect (LambdaE handler) <: RecNil)
