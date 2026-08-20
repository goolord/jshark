{-# language DataKinds #-}
{-# language OverloadedStrings #-}

-- | Wrappers over the JS Web Storage API (@localStorage@/@sessionStorage@).
module JShark.Storage
  ( Storage
  , localStorage
  , sessionStorage
  , getItem
  , setItem
  , removeItem
  , clear
  ) where

import JShark.Api
import JShark.Object
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | An opaque phantom type representing a @Storage@ object.
data Storage

localStorage :: Effect f ('Object Storage)
localStorage = unsafeObject "localStorage"

sessionStorage :: Effect f ('Object Storage)
sessionStorage = unsafeObject "sessionStorage"

-- | @storage.getItem(key)@. Returns 'None' instead of a raw @null@.
getItem :: Effect f ('Object Storage) -> Expr f 'String -> EffectSyntax f (Expr f ('Option 'String))
getItem s key = fmap (unsafeNullable . Var) $ toSyntax $ objectFfi s (ffi "getItem" (key <: RecNil))

-- | @storage.setItem(key, value)@
setItem :: Effect f ('Object Storage) -> Expr f 'String -> Expr f 'String -> EffectSyntax f (f 'Unit)
setItem s key value = toSyntax $ objectFfi s (ffi "setItem" (key <: value <: RecNil))

-- | @storage.removeItem(key)@
removeItem :: Effect f ('Object Storage) -> Expr f 'String -> EffectSyntax f (f 'Unit)
removeItem s key = toSyntax $ objectFfi s (ffi "removeItem" (key <: RecNil))

-- | @storage.clear()@
clear :: Effect f ('Object Storage) -> EffectSyntax f (f 'Unit)
clear s = toSyntax $ objectFfi s (ffi "clear" RecNil)
