{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , OverloadedStrings
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

unsafeObject :: Text -> Effect f ('Object a)
unsafeObject = UnsafeObject

unsafeObjectGet :: Effect f object -> String -> Effect f u
unsafeObjectGet = UnsafeObjectGet

unsafeObjectAssign :: Effect f object -> Effect f assignment -> Effect f u
unsafeObjectAssign = UnsafeObjectAssign

