{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}

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
import JShark.Object hiding (get, set)
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | An opaque phantom type representing a @Storage@ object.
data Storage

localStorage :: Effect f ('MutableObject Storage)
localStorage = unsafeObject "localStorage"

sessionStorage :: Effect f ('MutableObject Storage)
sessionStorage = unsafeObject "sessionStorage"

-- | @storage.getItem(key)@. Returns 'None' instead of a raw @null@.
getItem :: Effect f ('MutableObject Storage) -> Expr f 'String -> EffectSyntax f (Expr f ('Option 'String))
getItem s key = fmap (unsafeNullable . Var) $ toSyntax $ callMethod s "getItem" (arg key <: RecNil)

-- | @storage.setItem(key, value)@
setItem :: Effect f ('MutableObject Storage) -> Expr f 'String -> Expr f 'String -> EffectSyntax f (f 'Unit)
setItem s key value = toSyntax $ callMethod s "setItem" (arg key <: arg value <: RecNil)

-- | @storage.removeItem(key)@
removeItem :: Effect f ('MutableObject Storage) -> Expr f 'String -> EffectSyntax f (f 'Unit)
removeItem s key = toSyntax $ callMethod s "removeItem" (arg key <: RecNil)

-- | @storage.clear()@
clear :: Effect f ('MutableObject Storage) -> EffectSyntax f (f 'Unit)
clear s = toSyntax $ callMethod s "clear" RecNil
