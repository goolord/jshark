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
  , requestAnimationFrame
  , foreverFrame
  ) where

import JShark.Api
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | @setTimeout(function(){...}, ms)@. Returns the timer id.
setTimeout :: (Expr f 'Unit -> Effect f a) -> Expr f 'Number -> EffectSyntax f (Expr f 'Number)
setTimeout handler ms =
  fmap Var $ toSyntax $
    ffi "setTimeout" (ArgEffect (LambdaE (\x -> handler (var x))) <: arg ms <: RecNil)

-- | @setInterval(function(){...}, ms)@. Returns the timer id.
setInterval :: (Expr f 'Unit -> Effect f a) -> Expr f 'Number -> EffectSyntax f (Expr f 'Number)
setInterval handler ms =
  fmap Var $ toSyntax $
    ffi "setInterval" (ArgEffect (LambdaE (\x -> handler (var x))) <: arg ms <: RecNil)

-- | @clearTimeout(timerId)@
clearTimeout :: Expr f 'Number -> EffectSyntax f ()
clearTimeout timerId = toSyntax_ $ ffi "clearTimeout" (arg timerId <: RecNil)

-- | @clearInterval(timerId)@
clearInterval :: Expr f 'Number -> EffectSyntax f ()
clearInterval timerId = toSyntax_ $ ffi "clearInterval" (arg timerId <: RecNil)

-- | @requestAnimationFrame(function(t){...})@. Returns the frame id.
requestAnimationFrame :: (Expr f 'Number -> Effect f a) -> EffectSyntax f (Expr f 'Number)
requestAnimationFrame handler =
  fmap Var $ toSyntax $
    ffi "requestAnimationFrame" (ArgEffect (LambdaE (\t -> handler (var t))) <: RecNil)

-- | Recurring rAF loop. The callback receives the frame timestamp.
foreverFrame :: (Expr f 'Number -> EffectSyntax f (f 'Unit)) -> EffectSyntax f (f 'Unit)
foreverFrame tick =
  toSyntax $
    bindRec
      (\frame ->
         LambdaE $ \t -> stmts $ do
           _ <- tick (var t)
           _ <- requestAnimationFrame $ \t1 -> stmts (toSyntax (ApplyE frame (Lift t1)))
           done)
      (\frame ->
         stmts $ do
           _ <- requestAnimationFrame $ \t0 -> stmts (toSyntax (ApplyE frame (Lift t0)))
           done)
