{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JS @Map@ with an impure-containers-style API (@new@, @insert@,
-- @lookup@, @delete@, @mapM_@, @foldM@).
--
-- Handles are effect-only (like DOM refs): there is no host @Value@ or
-- @evaluate@ support. After @toSyntax new@, pass @Lift (Var m)@ or use
-- 'withMap'.
module JShark.Map
  ( new
  , fromEntries
  , fromList
  , withMap
  , insert
  , lookup
  , delete
  , member
  , size
  , clear
  , mapM_
  , foldM
  )
where

import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types (Effect (..), Expr (Var))
import JShark.Object (unsafeObjectGet)
import Prelude hiding (lookup, mapM_)

-- | @new Map()@. Leading @(@ on the FFI string selects @FFICall@ so the
-- codegen appends @()@ and yields @(()=>new Map())()@, not a bare arrow.
new :: Effect f ('Map k v)
new = ffi "(()=>new Map())" RecNil

-- | @new Map(entries)@ where @entries@ is a JS array of @\[key, value\]@
-- pairs (e.g. @JSON.parse@ of catalog JSON). Phantom @k@/@v@ are caller
-- assertions — pair shape is not enforced at compile time.
fromEntries :: Expr f ('Array u) -> Effect f ('Map k v)
fromEntries xs = ffi "xs => new Map(xs)" (arg xs <: RecNil)

-- | Alias for 'fromEntries'.
fromList :: Expr f ('Array u) -> Effect f ('Map k v)
fromList = fromEntries

-- | Allocate a map and run @k@ on the handle (@Lift (Var m)@).
withMap ::
  (Effect f ('Map k v) -> EffectSyntax f a) -> EffectSyntax f a
withMap k = do
  m <- toSyntax new
  k (Lift (Var m))

-- | @map.set(k, v)@
insert ::
  Effect f ('Map k v)
  -> Expr f k
  -> Expr f v
  -> EffectSyntax f (f 'Unit)
insert m k v = toSyntax $ callMethod m "set" (arg k <: arg v <: RecNil)

-- | @map.get(k)@. Missing keys become 'None'.
--
-- JS @Map@ cannot distinguish a missing key from @map.set(k, undefined)@
-- (both yield @undefined@ from @.get@). JShark maps both to 'None'.
lookup ::
  Effect f ('Map k v)
  -> Expr f k
  -> EffectSyntax f (Expr f ('Option v))
lookup m k =
  fmap unsafeNullable
    $ bindExpr
    $ ffi
      "((m, k) => { const v = m.get(k); return v === undefined ? null : v; })"
      (ArgEffect m <: arg k <: RecNil)

-- | @map.delete(k)@
delete ::
  Effect f ('Map k v)
  -> Expr f k
  -> EffectSyntax f (f 'Unit)
delete m k = toSyntax $ callMethod m "delete" (arg k <: RecNil)

-- | @map.has(k)@
member ::
  Effect f ('Map k v)
  -> Expr f k
  -> EffectSyntax f (Expr f 'Bool)
member m k = bindExpr $ callMethod m "has" (arg k <: RecNil)

-- | @map.size@
size :: Effect f ('Map k v) -> EffectSyntax f (Expr f 'Number)
size m = bindExpr $ unsafeObjectGet m "size"

-- | @map.clear()@
clear :: Effect f ('Map k v) -> EffectSyntax f (f 'Unit)
clear m = toSyntax $ callMethod m "clear" RecNil

-- | @map.forEach((v, k) => …)@ with impure-containers argument order
-- @(k, v)@.
mapM_ ::
  (Expr f k -> Expr f v -> EffectSyntax f (f 'Unit))
  -> Effect f ('Map k v)
  -> EffectSyntax f (f 'Unit)
mapM_ body m =
  toSyntax $
    ffi
      "((m, f) => { m.forEach((v, k) => f(k)(v)); })"
      ( ArgEffect m
          <: ArgEffect
            ( LambdaE $ \k ->
                LambdaE $ \v ->
                  stmts (body (var k) (var v))
            )
          <: RecNil
      )

-- | Left fold over entries, impure-containers order @(acc, k, v)@.
foldM ::
  (Expr f acc -> Expr f k -> Expr f v -> Expr f acc)
  -> Expr f acc
  -> Effect f ('Map k v)
  -> Effect f acc
foldM body z m =
  ffi
    "((m, z, f) => { let a = z; m.forEach((v, k) => { a = f(a, k, v); }); return a; })"
    ( ArgEffect m
        <: arg z
        <: ArgExpr (toFn body)
        <: RecNil
    )
