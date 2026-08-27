{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Web Workers, SharedArrayBuffer, and Atomics helpers for parallel JS.
module JShark.Worker
  ( crossOriginIsolated
  , hasSharedArrayBuffer
  , performanceNow
  , newWorker
  , workerPostMessage
  , atomicsLoad
  , atomicsStore
  , atomicsAdd
  , atomicsNotify
  , atomicsWait
  )
where

import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))

crossOriginIsolated :: EffectSyntax f (Expr f 'Bool)
crossOriginIsolated =
  bindExpr $ ffi "(()=>globalThis.crossOriginIsolated===true)" RecNil

hasSharedArrayBuffer :: EffectSyntax f (Expr f 'Bool)
hasSharedArrayBuffer =
  bindExpr $ ffi "(()=>typeof SharedArrayBuffer!=='undefined')" RecNil

performanceNow :: EffectSyntax f (Expr f 'Number)
performanceNow = bindExpr $ ffi "(()=>performance.now())" RecNil

newWorker :: Expr f 'String -> EffectSyntax f (Effect f ('MutableObject ()))
newWorker url = pure (ffi "u=>new Worker(u)" (arg url <: RecNil))

workerPostMessage ::
  Effect f ('MutableObject ())
  -> Expr f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
workerPostMessage w msg = toSyntax $ callMethod w "postMessage" (arg msg <: RecNil)

atomicsLoad ::
  Expr f ('MutableObject ()) -> Expr f 'Number -> EffectSyntax f (Expr f 'Number)
atomicsLoad arr idx =
  bindExpr $ ffi "Atomics.load" (arg arr <: arg idx <: RecNil)

atomicsStore ::
  Expr f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
atomicsStore arr idx val =
  toSyntax $ ffi "Atomics.store" (arg arr <: arg idx <: arg val <: RecNil)

atomicsAdd ::
  Expr f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
atomicsAdd arr idx delta =
  bindExpr $ ffi "Atomics.add" (arg arr <: arg idx <: arg delta <: RecNil)

atomicsNotify ::
  Expr f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
atomicsNotify arr idx count =
  bindExpr $ ffi "Atomics.notify" (arg arr <: arg idx <: arg count <: RecNil)

atomicsWait ::
  Expr f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'String)
atomicsWait arr idx val timeout =
  bindExpr $
    ffi
      "Atomics.wait"
      (arg arr <: arg idx <: arg val <: arg timeout <: RecNil)
