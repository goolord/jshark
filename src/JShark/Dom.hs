{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , ExistentialQuantification
  , GADTs
  , BangPatterns
  , TypeApplications
  , TypeFamilies
  , ScopedTypeVariables
#-}
module JShark.Dom
  ( DomElement
  , lookupId
  , lookupSelector
  , classAdd
  , classRemove
  , classToggle
  , createElement
  , setAttribute
  , getAttribute
  , appendChild
  , removeChild
  , innerHTML
  , setInnerHTML
  , innerText
  , setInnerText
  , getValue
  , setValue
  ) where

import Data.Text (Text)
import JShark
import JShark.Api
import JShark.Object
import JShark.Types
import JShark.Rec (Rec(..), (<:))

-- | An opaque phantom type representing a DOM element (what
-- @document.getElementById@ /etc. return in the browser). Modeled as an
-- 'Object' so it can share the same property-access machinery ('get',
-- 'getCall', 'Field') as other foreign objects.
data DomElement

type instance Field DomElement "innerHTML" = 'String
type instance Field DomElement "innerText" = 'String

-- | @document.getElementById(x)@. Bound via 'toSyntax' so reusing the
-- handle only references the variable, never re-runs the lookup.
lookupId :: Expr f 'String -> EffectSyntax f (Effect f ('Object DomElement))
lookupId x = fmap (expr . Var) $ toSyntax $ ffi "document.getElementById" (x <: RecNil)

lookupSelector :: Expr f 'String -> EffectSyntax f (Effect f ('Array ('Object DomElement)))
lookupSelector x = fmap (expr . Var) $ toSyntax $ ffi "document.querySelectorAll" (x <: RecNil)

classAdd, classRemove, classToggle :: Effect f ('Object DomElement) -> Expr f 'String -> EffectSyntax f (f 'Unit)
classAdd el x    = toSyntax $ objectFfi el (ffi "classList.add" (x <: RecNil))
classRemove el x = toSyntax $ objectFfi el (ffi "classList.remove" (x <: RecNil))
classToggle el x = toSyntax $ objectFfi el (ffi "classList.toggle" (x <: RecNil))

-- | @document.createElement(tag)@. Bound via 'toSyntax'; otherwise reusing
-- the handle would re-run @createElement@ and create a new element each time.
createElement :: Expr f 'String -> EffectSyntax f (Effect f ('Object DomElement))
createElement tag = fmap (expr . Var) $ toSyntax $ ffi "document.createElement" (tag <: RecNil)

-- | @el.setAttribute(name, value)@
setAttribute :: Effect f ('Object DomElement) -> Text -> Expr f 'String -> EffectSyntax f (f 'Unit)
setAttribute el name value = toSyntax $ objectFfi el (ffi "setAttribute" (string name <: value <: RecNil))

-- | @el.getAttribute(name)@
getAttribute :: Effect f ('Object DomElement) -> Text -> EffectSyntax f (Expr f 'String)
getAttribute el name = fmap Var $ toSyntax $ objectFfi el (ffi "getAttribute" (string name <: RecNil))

-- | @parent.appendChild(child)@
appendChild :: Effect f ('Object DomElement) -> Effect f ('Object DomElement) -> EffectSyntax f (f 'Unit)
appendChild parent child = toSyntax $ objectFfi parent (ffi "appendChild" (unsafeEffectExpr child <: RecNil))

-- | @parent.removeChild(child)@
removeChild :: Effect f ('Object DomElement) -> Effect f ('Object DomElement) -> EffectSyntax f (f 'Unit)
removeChild parent child = toSyntax $ objectFfi parent (ffi "removeChild" (unsafeEffectExpr child <: RecNil))

innerHTML :: Effect f ('Object DomElement) -> EffectSyntax f (Expr f 'String)
innerHTML = get @"innerHTML"

setInnerHTML :: Effect f ('Object DomElement) -> Expr f 'String -> EffectSyntax f (f 'Unit)
setInnerHTML el x = toSyntax $ unsafeObjectAssign (unsafeObjectGet el "innerHTML") (Lift x)

innerText :: Effect f ('Object DomElement) -> EffectSyntax f (Expr f 'String)
innerText = get @"innerText"

setInnerText :: Effect f ('Object DomElement) -> Expr f 'String -> EffectSyntax f (f 'Unit)
setInnerText el x = toSyntax $ unsafeObjectAssign (unsafeObjectGet el "innerText") (Lift x)

-- | @el.value@ (inputs).
getValue :: Effect f ('Object DomElement) -> EffectSyntax f (Expr f 'String)
getValue el = getProp el "value"

-- | @el.value = v@.
setValue :: Effect f ('Object DomElement) -> Expr f 'String -> EffectSyntax f (f 'Unit)
setValue el v = setProp el "value" v

