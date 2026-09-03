{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | DOM element helpers on 'Effect' ('lookupId', attributes, classes, …).
--
-- Elements are opaque 'DomElement' values; use 'JShark.Api.unsafeObject' at
-- integration boundaries when the host already has a node reference.
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

-- | Browser node. 'MutableObject' so 'get' / 'Field' apply.
data DomElement

type instance Field DomElement "innerHTML" = 'String

type instance Field DomElement "innerText" = 'String

-- | @document.getElementById(x)@. Bound via 'hold' so reusing the
-- handle only references the variable, never re-runs the lookup.
lookupId ::
  Expr f 'String -> EffectSyntax f (Effect f ('MutableObject DomElement))
lookupId x = hold $ ffi "document.getElementById" (arg x <: RecNil)

lookupSelector ::
  Expr f 'String -> EffectSyntax f (Effect f ('Array ('MutableObject DomElement)))
lookupSelector x = hold $ ffi "document.querySelectorAll" (arg x <: RecNil)

classOp ::
  String
  -> Effect f ('MutableObject DomElement)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
classOp name el x = toSyntax $ callMethod el name (arg x <: RecNil)

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

setAttribute ::
  Effect f ('MutableObject DomElement)
  -> Text
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setAttribute el name value =
  toSyntax $
    callMethod el "setAttribute" (arg (string name) <: arg value <: RecNil)

getAttribute ::
  Effect f ('MutableObject DomElement) -> Text -> EffectSyntax f (Expr f 'String)
getAttribute el name =
  bindExpr $ callMethod el "getAttribute" (arg (string name) <: RecNil)

appendChild ::
  Effect f ('MutableObject DomElement)
  -> Effect f ('MutableObject DomElement)
  -> EffectSyntax f (f 'Unit)
appendChild parent child = toSyntax $ callMethod parent "appendChild" (ArgEffect child <: RecNil)

removeChild ::
  Effect f ('MutableObject DomElement)
  -> Effect f ('MutableObject DomElement)
  -> EffectSyntax f (f 'Unit)
removeChild parent child = toSyntax $ callMethod parent "removeChild" (ArgEffect child <: RecNil)

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

setTextContent ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setTextContent el x = setProp el "textContent" x

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

innerHTML ::
  Effect f ('MutableObject DomElement) -> EffectSyntax f (Expr f 'String)
innerHTML el = get @"innerHTML" el

setInnerHTML ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setInnerHTML el x = set @"innerHTML" el x

innerText ::
  Effect f ('MutableObject DomElement) -> EffectSyntax f (Expr f 'String)
innerText el = get @"innerText" el

setInnerText ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setInnerText el x = set @"innerText" el x

-- | @el.value@ (inputs).
getValue ::
  Effect f ('MutableObject DomElement) -> EffectSyntax f (Expr f 'String)
getValue el = getProp el "value"

setValue ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setValue el v = setProp el "value" v
