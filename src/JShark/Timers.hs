{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}
-- | Wrappers over the JS timer functions.
module JShark.Timers
  ( setTimeout
  , setInterval
  , clearTimeout
  , clearInterval
  ) where

import JShark.Api
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | @setTimeout(function(){...}, ms)@. Returns the timer id.
setTimeout :: (f 'Unit -> Effect f a) -> Expr f 'Number -> EffectSyntax f (Expr f 'Number)
setTimeout handler ms = fmap Var $ toSyntax $ ffi "setTimeout" (ArgEffect (LambdaE handler) <: arg ms <: RecNil)

-- | @setInterval(function(){...}, ms)@. Returns the timer id.
setInterval :: (f 'Unit -> Effect f a) -> Expr f 'Number -> EffectSyntax f (Expr f 'Number)
setInterval handler ms = fmap Var $ toSyntax $ ffi "setInterval" (ArgEffect (LambdaE handler) <: arg ms <: RecNil)

-- | @clearTimeout(timerId)@
clearTimeout :: Expr f 'Number -> EffectSyntax f ()
clearTimeout timerId = toSyntax_ $ ffi "clearTimeout" (arg timerId <: RecNil)

-- | @clearInterval(timerId)@
clearInterval :: Expr f 'Number -> EffectSyntax f ()
clearInterval timerId = toSyntax_ $ ffi "clearInterval" (arg timerId <: RecNil)
