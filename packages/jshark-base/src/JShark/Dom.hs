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
  , byIdOption
  , eventTarget
  , lookupId
  , lookupIdOption
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

    -- * Events / window
  , Event
  , window
  , host
  , locationHash
  , onClick
  , onClick_
  , addEventListener
  , addEventListener_
  , addEventListenerS
  , eventKey
  , eventCode
  , eventRepeat
  , eventPointerId
  , eventClientX
  , eventClientY
  , eventButton
  , eventShiftKey
  , eventOffsetX
  , eventOffsetY
  )
where

import Data.Text (Text)
import JShark
import JShark.Api
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
-- handle only references the variable, never re-runs the lookup. The
-- handle is JS @null@ when the id is absent; use 'lookupIdOption' when
-- that matters.
lookupId ::
  Expr f 'String -> EffectSyntax f (Effect f ('MutableObject DomElement))
lookupId x = hold $ ffi "document.getElementById" (arg x <: RecNil)

-- | Like 'lookupId', but a missing id is 'none'.
lookupIdOption ::
  Expr f 'String
  -> EffectSyntax f (Effect f ('Option ('MutableObject DomElement)))
lookupIdOption x =
  hold $
    Bind
      Nothing
      (ffi "document.getElementById" (arg x <: RecNil))
      (\v -> Lift (unsafeNullable (Var v)))

-- | @document.getElementById(id)@ for a literal id, as an 'Option'.
byIdOption ::
  Text -> EffectSyntax f (Effect f ('Option ('MutableObject DomElement)))
byIdOption = lookupIdOption . string

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

-- | @el.getAttribute(name)@ — 'none' when the attribute is absent (the
-- native @null@ result).
getAttribute ::
  Effect f ('MutableObject DomElement)
  -> Text
  -> EffectSyntax f (Expr f ('Option 'String))
getAttribute el name =
  fmap unsafeNullable
    $ bindExpr
    $ callMethod el "getAttribute" (arg (string name) <: RecNil)

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

data Window

type instance Field Window "location.host" = 'String

type instance Field Window "location.hash" = 'String

-- | The event object handed to 'addEventListener' callbacks. Read it
-- with the typed accessors ('eventKey', 'eventCode', …); @target@ is
-- typed in "JShark.Dom" ('JShark.Dom.eventTarget').
data Event

type instance Field Event "key" = 'String

type instance Field Event "code" = 'String

type instance Field Event "repeat" = 'Bool

type instance Field Event "pointerId" = 'Number

type instance Field Event "clientX" = 'Number

type instance Field Event "clientY" = 'Number

type instance Field Event "button" = 'Number

type instance Field Event "offsetX" = 'Number

type instance Field Event "offsetY" = 'Number

type instance Field Event "shiftKey" = 'Bool

-- | @event.key@ — the key value for keyboard events.
eventKey ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'String)
eventKey o = Object.get @"key" @Event (toEffect o)

-- | @event.code@ — the physical key code for keyboard events.
eventCode ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'String)
eventCode o = Object.get @"code" @Event (toEffect o)

-- | @event.repeat@ — whether a held key is auto-repeating.
eventRepeat ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Bool)
eventRepeat o = Object.get @"repeat" @Event (toEffect o)

-- | @event.pointerId@ — the unique id of the pointer that fired the event.
eventPointerId ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventPointerId o = Object.get @"pointerId" @Event (toEffect o)

-- | @event.clientX@ — pointer x in viewport coordinates.
eventClientX ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventClientX o = Object.get @"clientX" @Event (toEffect o)

-- | @event.clientY@ — pointer y in viewport coordinates.
eventClientY ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventClientY o = Object.get @"clientY" @Event (toEffect o)

-- | @event.button@ — the mouse button index.
eventButton ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventButton o = Object.get @"button" @Event (toEffect o)

-- | @event.shiftKey@ — whether Shift was held.
eventShiftKey ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Bool)
eventShiftKey o = Object.get @"shiftKey" @Event (toEffect o)

-- | @event.offsetX@ — pointer x relative to the target element.
eventOffsetX ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventOffsetX o = Object.get @"offsetX" @Event (toEffect o)

-- | @event.offsetY@ — pointer y relative to the target element.
eventOffsetY ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventOffsetY o = Object.get @"offsetY" @Event (toEffect o)

-- | The global JS @window@ object.
window :: Effect f ('MutableObject Window)
window = Object.unsafeObject "window"

-- | @window.location.host@.
host :: EffectSyntax f (Expr f 'String)
host = Object.get @"location.host" window

-- | @window.location.hash@.
locationHash :: EffectSyntax f (Expr f 'String)
locationHash = Object.get @"location.hash" window

-- | Set an element's @onclick@ property to a raw handler (not
-- @addEventListener@).
onClick ::
  Effect f ('MutableObject obj) -> (f 'Unit -> Effect f a) -> EffectSyntax f ()
onClick el f =
  toSyntax_ $ Object.unsafeObjectAssign (Object.unsafeObjectGet el "onclick") (LambdaE f)

-- | 'onClick' with the handler written in 'EffectSyntax'.
onClick_ ::
  Effect f ('MutableObject obj) -> EffectSyntax f (f 'Unit) -> EffectSyntax f ()
onClick_ el body = onClick el $ \_ -> stmts body

-- | @el.addEventListener(name, handler)@ with an 'Effect'-returning handler.
addEventListener ::
  Text
  -> Effect f ('MutableObject obj)
  -> (Expr f ('MutableObject Event) -> Effect f a)
  -> EffectSyntax f ()
addEventListener name el handler =
  toSyntax_ $
    callMethod
      el
      "addEventListener"
      (ArgExpr (string name) <: ArgEffect (LambdaE (\x -> handler (var x))) <: RecNil)

-- | 'addEventListener' with the handler written directly in
-- 'EffectSyntax' (no @stmts@ wrap needed).
addEventListenerS ::
  Text
  -> Effect f ('MutableObject obj)
  -> (Expr f ('MutableObject Event) -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f ()
addEventListenerS name el handler =
  addEventListener name el (stmts . handler)

-- | 'addEventListener' with an 'EffectSyntax' body that ignores the event.
addEventListener_ ::
  Text
  -> Effect f ('MutableObject obj)
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f ()
addEventListener_ name el body = addEventListener name el $ \_ -> stmts body
