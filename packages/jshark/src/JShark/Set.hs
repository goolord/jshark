{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JS @Set@ with an impure-containers-style API (@new@, @insert@,
-- @delete@, @member@, @mapM_@).
--
-- Handles are effect-only; use 'withSet' after binding.
module JShark.Set
  ( new
  , fromList
  , withSet
  , insert
  , delete
  , member
  , size
  , clear
  , mapM_
  )
where

import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types (Effect (..), Expr (Var))
import JShark.Object (unsafeObjectGet)
import Prelude hiding (mapM_)

-- | @new Set()@.
new :: Effect f ('Set a)
new = ffi "new Set" RecNil

-- | @new Set(values)@ from a JS array of @a@.
fromList :: Expr f ('Array a) -> Effect f ('Set a)
fromList xs = ffi "new Set" (arg xs <: RecNil)

-- | Allocate a set and run @k@ on the handle (@Lift (Var s)@).
withSet ::
  (Effect f ('Set a) -> EffectSyntax f b) -> EffectSyntax f b
withSet k = do
  s <- toSyntax new
  k (Lift (Var s))

insert ::
  Effect f ('Set a) -> Expr f a -> EffectSyntax f (f 'Unit)
insert s x = toSyntax $ callMethod s "add" (arg x <: RecNil)

delete ::
  Effect f ('Set a) -> Expr f a -> EffectSyntax f (f 'Unit)
delete s x = toSyntax $ callMethod s "delete" (arg x <: RecNil)

member ::
  Effect f ('Set a) -> Expr f a -> EffectSyntax f (Expr f 'Bool)
member s x = bindExpr $ callMethod s "has" (arg x <: RecNil)

size :: Effect f ('Set a) -> EffectSyntax f (Expr f 'Number)
size s = bindExpr $ unsafeObjectGet s "size"

clear :: Effect f ('Set a) -> EffectSyntax f (f 'Unit)
clear s = toSyntax $ callMethod s "clear" RecNil

mapM_ ::
  (Expr f a -> EffectSyntax f (f 'Unit))
  -> Effect f ('Set a)
  -> EffectSyntax f (f 'Unit)
mapM_ body s =
  toSyntax
    ( callMethod
        s
        "forEach"
        (ArgEffect (LambdaE (\x -> stmts (body (var x)))) <: RecNil)
    )
