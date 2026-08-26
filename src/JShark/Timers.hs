{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Wrappers over the JS timer functions.
module JShark.Timers
  ( setTimeout
  , setInterval
  , clearTimeout
  , clearInterval
  , requestAnimationFrame
  , foreverFrame
  , foreverTick
  )
where

import Control.Monad (void)
import JShark.Api
import JShark.Rec (Rec (..), (<:))
import JShark.Types

callbackFFI ::
  String
  -> Rec (Arg f) us
  -> (Expr f u -> Effect f a)
  -> EffectSyntax f (Expr f v)
callbackFFI name extra handler =
  bindExpr $
    ffi name (ArgEffect (LambdaE (\x -> handler (var x))) <: extra)

timerCall ::
  String
  -> (Expr f 'Unit -> Effect f a)
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
timerCall name handler ms = callbackFFI name (arg ms <: RecNil) handler

-- | @setTimeout(function(){...}, ms)@. Returns the timer id.
setTimeout ::
  (Expr f 'Unit -> Effect f a)
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
setTimeout = timerCall "setTimeout"

-- | @setInterval(function(){...}, ms)@. Returns the timer id.
setInterval ::
  (Expr f 'Unit -> Effect f a)
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
setInterval = timerCall "setInterval"

-- | @clearTimeout(timerId)@
clearTimeout :: Expr f 'Number -> EffectSyntax f ()
clearTimeout timerId = toSyntax_ $ ffi "clearTimeout" (arg timerId <: RecNil)

-- | @clearInterval(timerId)@
clearInterval :: Expr f 'Number -> EffectSyntax f ()
clearInterval timerId = toSyntax_ $ ffi "clearInterval" (arg timerId <: RecNil)

-- | @requestAnimationFrame(function(t){...})@. Returns the frame id.
requestAnimationFrame ::
  (Expr f 'Number -> Effect f a) -> EffectSyntax f (Expr f 'Number)
requestAnimationFrame handler = callbackFFI "requestAnimationFrame" RecNil handler

-- | Recurring rAF loop. The callback receives the frame timestamp.
foreverFrame ::
  (Expr f 'Number -> EffectSyntax f (f 'Unit)) -> EffectSyntax f (f 'Unit)
foreverFrame tick =
  toSyntax $
    bindRec
      ( \frame ->
          LambdaE $ \t -> stmts $ do
            void (tick (var t))
            void (requestAnimationFrame $ \t1 -> stmts (toSyntax (ApplyE frame (Lift t1))))
            done
      )
      ( \frame ->
          stmts $ do
            void (requestAnimationFrame $ \t0 -> stmts (toSyntax (ApplyE frame (Lift t0))))
            done
      )

-- | Uncapped loop via @setTimeout(..., 0)@. Callback receives @performance.now()@.
foreverTick ::
  (Expr f 'Number -> EffectSyntax f (f 'Unit)) -> EffectSyntax f (f 'Unit)
foreverTick tick =
  toSyntax $
    bindRec
      ( \loop ->
          LambdaE $ \_ ->
            stmts $ do
              void $
                setTimeout
                  ( \u ->
                      stmts $ do
                        now <- bindExpr $ ffi "performance.now" RecNil
                        void (tick now)
                        void (toSyntax (ApplyE loop (Lift u)))
                        done
                  )
                  (number 0)
              done
      )
      ( \loop ->
          stmts $ do
            void (toSyntax (ApplyE loop noOp))
            done
      )
