{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}
-- Orphans: HasField is GHC.Records; Effect/Expr live in Types (cannot import us).
{-# OPTIONS_GHC -Wno-orphans #-}
module JShark.Object
  ( Field
  , get
  , set
  , newObject
  , obj
  , field
  , create
  , delete
  , hasOwn
  , unsafeObject
  , unsafeObjectGet
  , unsafeObjectAssign
  , HasField(..)
  ) where

import Data.Text (Text)
import Data.Proxy
import GHC.Records (HasField(..))
import GHC.TypeLits
import JShark.Rec (Rec(..), (<:))
import JShark.Types

-- | @o.k@. With @OverloadedRecordDot@, @n <- o.fullName@ is 'getField'
-- on 'Effect' or 'Expr' (both yield 'EffectSyntax'). PHOAS binders
-- @f ('Object r)@ need 'get' or @(Var x).k@.
get :: forall k r f. KnownSymbol k => Effect f ('Object r) -> EffectSyntax f (Expr f (Field r k))
get x = fmap Var $ toSyntax $ UnsafeObjectGet x (symbolVal (Proxy :: Proxy k))

instance (KnownSymbol k, u ~ Field r k) =>
  HasField k (Effect f ('Object r)) (EffectSyntax f (Expr f u)) where
  getField = get @k

instance (KnownSymbol k, u ~ Field r k) =>
  HasField k (Expr f ('Object r)) (EffectSyntax f (Expr f u)) where
  getField o = get @k (Lift o)

-- | Assign a typed field (@o.k = v@).
set :: forall k r f. KnownSymbol k => Effect f ('Object r) -> Expr f (Field r k) -> EffectSyntax f (f 'Unit)
set o v = toSyntax $ UnsafeObjectAssign (UnsafeObjectGet o (symbolVal (Proxy :: Proxy k))) (Lift v)

-- | Empty object of a known record type.
newObject :: Effect f ('Object r)
newObject = UnsafeObject "{}"

-- | One typed field of an 'obj' literal.
field :: forall k r f. KnownSymbol k => Expr f (Field r k) -> FieldLit f r
field = FieldLit @k

-- | Typed object literal @{k: v, …}@. Identity-sensitive; not cheap to inline.
obj :: [FieldLit f r] -> Effect f ('Object r)
obj = ObjectLit

-- | @Object.create(proto)@. Prototypal inheritance without @new@.
-- The child's row is independent of the prototype's.
create :: Effect f ('Object proto) -> Effect f ('Object child)
create proto = FFI "Object.create" (ArgEffect proto <: RecNil)

-- | @delete o[k]@
delete :: Effect f ('Object r) -> Expr f 'String -> Effect f 'Bool
delete = DeleteProp

-- | @Object.prototype.hasOwnProperty.call(o, k)@ — the book's enumeration guard.
hasOwn :: Effect f ('Object r) -> Expr f 'String -> Effect f 'Bool
hasOwn o k = FFI "Object.prototype.hasOwnProperty.call" (ArgEffect o <: ArgExpr k <: RecNil)

unsafeObject :: Text -> Effect f ('Object a)
unsafeObject = UnsafeObject

unsafeObjectGet :: Effect f object -> String -> Effect f u
unsafeObjectGet = UnsafeObjectGet

unsafeObjectAssign :: Effect f object -> Effect f assignment -> Effect f u
unsafeObjectAssign = UnsafeObjectAssign
