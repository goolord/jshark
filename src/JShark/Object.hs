{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
  , TypeFamilies
  , TypeOperators
#-}
module JShark.Object where

import Data.Kind
import Data.Text
import Data.Proxy
import GHC.TypeLits
import JShark.Types

type family Field (r :: Type) (k :: Symbol) :: Universe

get :: forall k r f. KnownSymbol k => Effect f ('Object r) -> EffectSyntax f (Expr f (Field r k))
get x = fmap Var $ toSyntax $ UnsafeObjectGet x (symbolVal (Proxy :: Proxy k))

-- | Assign a typed field (@o.k = v@).
set :: forall k r f. KnownSymbol k => Effect f ('Object r) -> Expr f (Field r k) -> EffectSyntax f (f 'Unit)
set o v = toSyntax $ UnsafeObjectAssign (UnsafeObjectGet o (symbolVal (Proxy :: Proxy k))) (Lift v)

-- | Empty object of a known record type.
newObject :: Effect f ('Object r)
newObject = UnsafeObject "{}"

unsafeObject :: Text -> Effect f ('Object a)
unsafeObject = UnsafeObject

unsafeObjectGet :: Effect f object -> String -> Effect f u
unsafeObjectGet = UnsafeObjectGet

unsafeObjectAssign :: Effect f object -> Effect f assignment -> Effect f u
unsafeObjectAssign = UnsafeObjectAssign

