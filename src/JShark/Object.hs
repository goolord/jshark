{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Orphans: HasField is GHC.Records; Effect/Expr live in Types (cannot import us).
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Typed object field access and runtime object helpers.
--
-- Field names are type-level 'Symbol's ('Field'). Mutable objects live on
-- 'Effect'; frozen object literals on 'Expr'. See 'get', 'set', 'frozen'.
module JShark.Object
  ( Field
  , get
  , set
  , newObject
  , obj
  , frozen
  , field
  , fieldEffect
  , create
  , delete
  , hasOwn
  , unsafeObject
  , unsafeObjectGet
  , unsafeObjectAssign
  , HasField (..)
  )
where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Records (HasField (..))
import GHC.TypeLits
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types

-- | @o.k@. With @OverloadedRecordDot@, mutable @n <- o.fullName@ is
-- 'getField' on 'Effect' or 'Expr' @'MutableObject@ (both yield
-- 'EffectSyntax'). Frozen @o.k@ on @'Expr' f ('Object r)@ is a pure
-- 'GetField'. PHOAS binders @f ('MutableObject r)@ need 'get' or
-- @(Var x).k@; frozen binders use @(Var x).k@.
get ::
  forall k r f.
  KnownSymbol k =>
  Effect f ('MutableObject r) -> EffectSyntax f (Expr f (Field r k))
get x = bindExpr $ UnsafeObjectGet x (T.pack (symbolVal (Proxy :: Proxy k)))

instance
  (KnownSymbol k, u ~ Field r k) =>
  HasField k (Effect f ('MutableObject r)) (EffectSyntax f (Expr f u))
  where
  getField = get @k

instance
  (KnownSymbol k, u ~ Field r k) =>
  HasField k (Expr f ('MutableObject r)) (EffectSyntax f (Expr f u))
  where
  getField o = get @k (Lift o)

instance
  (KnownSymbol k, u ~ Field r k) =>
  HasField k (Expr f ('Object r)) (Expr f u)
  where
  getField = GetField @k

-- | Assign a typed field (@o.k = v@).
set ::
  forall k r f.
  KnownSymbol k =>
  Effect f ('MutableObject r) -> Expr f (Field r k) -> EffectSyntax f (f 'Unit)
set o v =
  toSyntax $
    UnsafeObjectAssign
      (UnsafeObjectGet o (T.pack (symbolVal (Proxy :: Proxy k))))
      (Lift v)

-- | Empty object of a known record type.
newObject :: Effect f ('MutableObject r)
newObject = UnsafeObject "{}"

-- | One typed field of an 'obj' literal.
field :: forall k r f. KnownSymbol k => Expr f (Field r k) -> FieldLit f r
field = FieldLit @k

fieldEffect ::
  forall k r f. KnownSymbol k => Effect f (Field r k) -> FieldLit f r
fieldEffect = FieldLitEffect @k

-- | Typed mutable object literal @{k: v, …}@. Identity-sensitive; not cheap to inline.
obj :: [FieldLit f r] -> Effect f ('MutableObject r)
obj = ObjectLit

-- | Frozen object literal. Field reads are pure 'Expr' ('GetField').
frozen :: [FieldLit f r] -> Expr f ('Object r)
frozen = FrozenLit

-- | @Object.create(proto)@. Prototypal inheritance without @new@.
-- The child's row is independent of the prototype's.
create :: Effect f ('MutableObject proto) -> Effect f ('MutableObject child)
create proto = FFI (FFICall "Object.create") (ArgEffect proto <: RecNil)

-- | @delete o[k]@
delete :: Effect f ('MutableObject r) -> Expr f 'String -> Effect f 'Bool
delete = DeleteProp

-- | @Object.prototype.hasOwnProperty.call(o, k)@ — the book's enumeration guard.
hasOwn :: Effect f ('MutableObject r) -> Expr f 'String -> Effect f 'Bool
hasOwn o k =
  FFI
    (FFICall "Object.prototype.hasOwnProperty.call")
    (ArgEffect o <: ArgExpr k <: RecNil)

unsafeObject :: Text -> Effect f ('MutableObject a)
unsafeObject = UnsafeObject

unsafeObjectGet :: Effect f object -> String -> Effect f u
unsafeObjectGet o k = UnsafeObjectGet o (T.pack k)

unsafeObjectAssign :: Effect f object -> Effect f assignment -> Effect f u
unsafeObjectAssign = UnsafeObjectAssign
