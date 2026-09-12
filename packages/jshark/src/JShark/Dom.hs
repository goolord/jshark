{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | DOM element helpers on 'Effect' ('lookupId', attributes, classes, …).
--
-- Elements are opaque 'DomElement' values; use 'JShark.Api.unsafeObject' at
-- integration boundaries when the host already has a node reference.
module JShark.Dom
  ( DomElement
  , byId
  , eventTarget
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
  , replaceChildren
  , replaceChildrenFrom
  , setTextContent
  , setStyleProperty
  , innerHTML
  , setInnerHTML
  , innerText
  , setInnerText
  , getValue
  , setValue
  )
where

import Data.Text (Text)
import JShark
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Object as Object

-- | Browser node. 'MutableObject' so 'get' / 'Field' apply.
data DomElement

type instance Field DomElement "innerHTML" = 'String

type instance Field DomElement "innerText" = 'String

-- | @document.getElementById(id)@ for a literal id.
byId :: Text -> EffectSyntax f (Effect f ('MutableObject DomElement))
byId = lookupId . string

-- | The element the handler fired on, as a usable 'DomElement' handle.
eventTarget ::
  forall f o.
  ToEffect f ('MutableObject Event) o =>
  o
  -> EffectSyntax f (Effect f ('MutableObject DomElement))
eventTarget o =
  hold
    (Object.unsafeObjectGet (toEffect o :: Effect f ('MutableObject Event)) "target")

-- | @document.getElementById(x)@. Bound via 'hold' so reusing the
-- handle only references the variable, never re-runs the lookup.
lookupId ::
  Expr f 'String -> EffectSyntax f (Effect f ('MutableObject DomElement))
lookupId x = hold $ ffi "document.getElementById" (arg x <: RecNil)

-- | @Array.from(document.querySelectorAll(selector))@ — all matching
-- elements as a real array. @querySelectorAll@ returns a @NodeList@, which
-- has @length@ and index access but none of the array methods, so convert
-- at the boundary and keep the 'Array' type truthful.
lookupSelector ::
  Expr f 'String -> EffectSyntax f (Effect f ('Array ('MutableObject DomElement)))
lookupSelector x =
  hold $
    ffi
      "((s) => Array.from(document.querySelectorAll(s)))"
      (arg x <: RecNil)

classOp ::
  Text
  -> Effect f ('MutableObject DomElement)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
classOp name el x = toSyntax $ callMethod el name (arg x <: RecNil)

-- | @el.classList.add@ / @remove@ / @toggle@ for one class name.
classAdd
  , classRemove
  , classToggle ::
    Effect f ('MutableObject DomElement)
    -> Expr f 'String
    -> EffectSyntax f (f 'Unit)
classAdd = classOp "classList.add"
classRemove = classOp "classList.remove"
classToggle = classOp "classList.toggle"

-- | @document.createElement(tag)@. Bound via 'hold'; otherwise reusing
-- the handle would re-run @createElement@ and create a new element each time.
createElement ::
  Expr f 'String -> EffectSyntax f (Effect f ('MutableObject DomElement))
createElement tag = hold $ ffi "document.createElement" (arg tag <: RecNil)

-- | @el.setAttribute(name, value)@.
setAttribute ::
  Effect f ('MutableObject DomElement)
  -> Text
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setAttribute el name value =
  toSyntax $
    callMethod el "setAttribute" (arg (string name) <: arg value <: RecNil)

-- | @el.getAttribute(name)@ — typed 'String'; a missing attribute is @null@.
getAttribute ::
  Effect f ('MutableObject DomElement) -> Text -> EffectSyntax f (Expr f 'String)
getAttribute el name =
  bindExpr $ callMethod el "getAttribute" (arg (string name) <: RecNil)

-- | @parent.appendChild(child)@.
appendChild ::
  Effect f ('MutableObject DomElement)
  -> Effect f ('MutableObject DomElement)
  -> EffectSyntax f (f 'Unit)
appendChild parent child = toSyntax $ callMethod parent "appendChild" (ArgEffect child <: RecNil)

-- | @parent.removeChild(child)@.
removeChild ::
  Effect f ('MutableObject DomElement)
  -> Effect f ('MutableObject DomElement)
  -> EffectSyntax f (f 'Unit)
removeChild parent child = toSyntax $ callMethod parent "removeChild" (ArgEffect child <: RecNil)

-- | @el.replaceChildren()@ — remove all children.
replaceChildren ::
  Effect f ('MutableObject DomElement) -> EffectSyntax f (f 'Unit)
replaceChildren el = toSyntax $ callMethod el "replaceChildren" RecNil

-- | @parent.replaceChildren(...source.childNodes)@ — one live-DOM update.
replaceChildrenFrom ::
  Effect f ('MutableObject DomElement)
  -> Effect f ('MutableObject DomElement)
  -> EffectSyntax f (f 'Unit)
replaceChildrenFrom parent source =
  toSyntax $
    ffi
      "((p, s) => { p.replaceChildren(...s.childNodes); })"
      (ArgEffect parent <: ArgEffect source <: RecNil)

-- | @el.textContent = x@.
setTextContent ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setTextContent el x = setProp el "textContent" x

-- | @el.style[prop] = value@.
setStyleProperty ::
  Effect f ('MutableObject DomElement)
  -> Text
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setStyleProperty el prop value =
  toSyntax
    $ discard
    $ ffi
      "((el, p, v) => { el.style[p] = v; })"
      (ArgEffect el <: arg (string prop) <: arg value <: RecNil)

-- | Read @el.innerHTML@ as an 'Expr' 'String'.
innerHTML ::
  Effect f ('MutableObject DomElement) -> EffectSyntax f (Expr f 'String)
innerHTML el = get @"innerHTML" el

-- | @el.innerHTML = x@.
setInnerHTML ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setInnerHTML el x = set @"innerHTML" el x

-- | Read @el.innerText@ as an 'Expr' 'String'.
innerText ::
  Effect f ('MutableObject DomElement) -> EffectSyntax f (Expr f 'String)
innerText el = get @"innerText" el

-- | @el.innerText = x@.
setInnerText ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setInnerText el x = set @"innerText" el x

-- | @el.value@ (inputs).
getValue ::
  Effect f ('MutableObject DomElement) -> EffectSyntax f (Expr f 'String)
getValue el = getProp el "value"

-- | @el.value = v@ (inputs).
setValue ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setValue el v = setProp el "value" v
