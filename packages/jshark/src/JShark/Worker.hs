{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- | @globalThis.crossOriginIsolated === true@ — COOP/COEP isolation is active.
crossOriginIsolated :: EffectSyntax f (Expr f 'Bool)
crossOriginIsolated =
  bindExpr $ ffiExpr "globalThis.crossOriginIsolated===true" RecNil

-- | Whether the host defines @SharedArrayBuffer@.
hasSharedArrayBuffer :: EffectSyntax f (Expr f 'Bool)
hasSharedArrayBuffer =
  bindExpr $ ffiExpr "typeof SharedArrayBuffer!=='undefined'" RecNil

-- | @performance.now()@ — milliseconds since the time origin.
performanceNow :: EffectSyntax f (Expr f 'Number)
performanceNow = bindExpr $ ffi "performance.now" RecNil

-- | @new Worker(url)@. The worker is a raw 'MutableObject' handle.
newWorker :: Expr f 'String -> EffectSyntax f (Effect f ('MutableObject ()))
newWorker url = pure (ffi "new Worker" (arg url <: RecNil))

-- | @worker.postMessage(msg)@.
workerPostMessage ::
  Effect f ('MutableObject ())
  -> Expr f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
workerPostMessage w msg = toSyntax $ callMethod w "postMessage" (arg msg <: RecNil)

-- | @Atomics.load(arr, idx)@.
atomicsLoad ::
  Expr f ('MutableObject ()) -> Expr f 'Number -> EffectSyntax f (Expr f 'Number)
atomicsLoad arr idx =
  bindExpr $ ffi "Atomics.load" (arg arr <: arg idx <: RecNil)

-- | @Atomics.store(arr, idx, value)@.
atomicsStore ::
  Expr f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
atomicsStore arr idx val =
  toSyntax $ ffi "Atomics.store" (arg arr <: arg idx <: arg val <: RecNil)

-- | @Atomics.add(arr, idx, delta)@ — returns the previous value.
atomicsAdd ::
  Expr f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
atomicsAdd arr idx delta =
  bindExpr $ ffi "Atomics.add" (arg arr <: arg idx <: arg delta <: RecNil)

-- | @Atomics.notify(arr, idx, count)@ — returns the number woken.
atomicsNotify ::
  Expr f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
atomicsNotify arr idx count =
  bindExpr $ ffi "Atomics.notify" (arg arr <: arg idx <: arg count <: RecNil)

-- | @Atomics.wait(arr, idx, value, timeout)@ — returns @\"ok\"@,
-- @\"not-equal\"@, or @\"timed-out\"@.
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
