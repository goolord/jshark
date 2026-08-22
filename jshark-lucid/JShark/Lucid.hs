{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Describe a DOM tree with Lucid's combinators, emit JavaScript that
builds it.

Lucid's 'Lucid.Html' is a function to a @Builder@, not a tree, so a
finished 'Lucid.Html' value cannot be walked. What /is/ reusable is
Lucid's __syntax__: 'Term' and 'With' are open classes and 'Attribute' is
an ordinary pair of 'Text', so container elements ('Lucid.div_',
'Lucid.li_', …) and every attribute ('Lucid.class_', 'Lucid.href_', …)
work unchanged at 'JsHtml'. Void elements ('Lucid.input_', 'Lucid.br_')
are fixed to Lucid's @HtmlT@ and cannot be reused; use 'void_'.

Anything dynamic is a child: 'dynText', 'dynAttr', 'classWhen', 'prop',
and 'on' sit in the child block and apply to the enclosing element. That
keeps Lucid's @[Attribute]@ list untouched (it only holds 'Text') while
letting a JShark 'Expr' or handler appear anywhere in the tree.

A template that uses none of those holes is polymorphic in 'Term', so the
same value renders as Lucid's @Html ()@ on the server and as 'JsHtml' in
the client:

@
shell :: (Term (h ()) (h ()), Term [Attribute] (h () -> h ()), IsString (h ())) => h ()
shell = li_ (div_ [class_ "view"] "hi")
@

The moment a hole appears that sharing stops: 'dynText' and friends have
no Lucid counterpart, so such a template is JavaScript-only. Keeping the
type free of a slot index is what buys the sharing, and the price is that
two mistakes — a modifier with no enclosing element, and a child inside a
void element — are caught when the JavaScript is generated rather than by
the type checker. Both are loud failures during the build that emits the
JS, never something that reaches a browser.

@
row :: Expr f 'String -> Expr f 'Bool -> JsHtml f ()
row title done = li_ $ do
  classWhen done "completed"
  div_ [class_ "view"] $ do
    void_ "input" [class_ "toggle", type_ "checkbox"]
    label_ (dynText title)
@

'renderInto' turns that into @createElement@ / @setAttribute@ /
@appendChild@ calls.
-}
module JShark.Lucid
  ( JsHtml

    -- * Rendering
  , renderInto

    -- * Text
  , text_
  , dynText

    -- * Elements Lucid cannot give us
  , void_
  , voidWith_

    -- * Dynamic parts
  , dynAttr
  , classWhen
  , prop
  , on
  )
where

import Control.Monad (void)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Api
import qualified JShark.Dom as Dom
import JShark.Rec (Rec (..), (<:))
import Lucid.Base (Attribute (..), Term (..), With (..))

{- | A DOM fragment. The @a@ parameter exists so @do@ blocks sequence
siblings, exactly as in Lucid; the payload is always discarded.
-}
newtype JsHtml f a = JsHtml ([Node f], a)

-- | One node of the fragment.
data Node f
  = Element Text [Attribute] (JsHtml f ())
  | -- | No children, so the block holds modifiers only.
    Void Text [Attribute] (JsHtml f ())
  | TextNode (Expr f 'String)
  | -- | Applies to the enclosing element rather than adding a node.
    Modifier (Modifier f)

data Modifier f
  = SetAttr Text (Expr f 'String)
  | ClassWhen (Expr f 'Bool) Text
  | SetProp Text (SomeExpr f)
  | Listen Text (EffectSyntax f (f 'Unit))

-- | A property value at a forgotten universe. @el.checked = true@ is
-- 'Bool'; @el.value = "x"@ is 'String'.
data SomeExpr f = forall u. SomeExpr (Expr f u)

single :: Node f -> JsHtml f ()
single n = JsHtml ([n], ())

instance Functor (JsHtml f) where
  fmap g (JsHtml (w, a)) = JsHtml (w, g a)

instance Applicative (JsHtml f) where
  pure a = JsHtml ([], a)
  JsHtml (w, g) <*> JsHtml (w', a) = JsHtml (w <> w', g a)

instance Monad (JsHtml f) where
  JsHtml (w, a) >>= k = case k a of
    JsHtml (w', b) -> JsHtml (w <> w', b)

instance a ~ () => Semigroup (JsHtml f a) where
  x <> y = x >> y

instance a ~ () => Monoid (JsHtml f a) where
  mempty = pure ()

-- | @OverloadedStrings@ in a child block is a text node.
instance a ~ () => IsString (JsHtml f a) where
  fromString = text_ . T.pack

{- | @div_ child@ and @div_ [class_ "x"] child@, from Lucid. The payload
types are equality constraints rather than a literal @()@ so that a
statement in a @do@ block, whose result is discarded and therefore
unconstrained, still picks this instance — the same trick Lucid uses.
-}
instance (a ~ (), b ~ ()) => Term (JsHtml f a) (JsHtml f b) where
  termWith name attrs child = single (Element name attrs child)

instance (child ~ JsHtml f (), a ~ ()) => Term [Attribute] (child -> JsHtml f a) where
  termWith name extra attrs child = single (Element name (extra <> attrs) child)

{- | Lucid's @with@: add the attributes to every element at the root of
the fragment (Lucid applies them to the one element it wraps; a JShark
fragment may have several roots). Text nodes and modifiers are untouched.
-}
instance a ~ () => With (JsHtml f a) where
  with (JsHtml (ns, a)) attrs = JsHtml (map add ns, a)
    where
      add = \case
        Element n as cs -> Element n (as <> attrs) cs
        Void n as cs -> Void n (as <> attrs) cs
        other -> other

-- | A literal text node.
text_ :: Text -> JsHtml f ()
text_ = single . TextNode . string

-- | A text node whose content is computed at run time.
dynText :: Expr f 'String -> JsHtml f ()
dynText = single . TextNode

{- | A void element (@input@, @br@, @img@, …). Lucid's own are fixed to
@HtmlT@, so they cannot be reused here.
-}
void_ :: Text -> [Attribute] -> JsHtml f ()
void_ name attrs = single (Void name attrs (pure ()))

-- | 'void_' with a block of modifiers ('prop', 'on', …).
voidWith_ :: Text -> [Attribute] -> JsHtml f () -> JsHtml f ()
voidWith_ name attrs mods = single (Void name attrs mods)

{- | @el.setAttribute(name, v)@ with a computed value.

Modifiers run after the element's Lucid attributes, so this wins over a
static @class_@ or @href_@ of the same name.
-}
dynAttr :: Text -> Expr f 'String -> JsHtml f ()
dynAttr n v = single (Modifier (SetAttr n v))

{- | Add a class when the test holds, remove it when it does not.

Compiles to @classList.toggle(c, test)@, so the element always carries a
@class@ attribute — an empty one when nothing matched, rather than no
attribute at all.
-}
classWhen :: Expr f 'Bool -> Text -> JsHtml f ()
classWhen c cls = single (Modifier (ClassWhen c cls))

{- | @el.name = v@. A property, not an attribute: a checkbox's state is
@checked@ the property, which is not the same as the @checked@ attribute.
-}
prop :: Text -> Expr f u -> JsHtml f ()
prop n v = single (Modifier (SetProp n (SomeExpr v)))

-- | @el.addEventListener(event, () => body)@.
on :: Text -> EffectSyntax f (f 'Unit) -> JsHtml f ()
on ev body = single (Modifier (Listen ev body))

{- | Emit the JavaScript that builds the fragment and appends its roots
to @parent@.

Fails while the JavaScript is being generated if the template puts a
modifier where there is no element to apply it to, or a child inside a
void element. See the module header for why those are build-time errors
rather than type errors.
-}
renderInto ::
  Effect f ('MutableObject Dom.DomElement) -> JsHtml f () -> EffectSyntax f (f 'Unit)
renderInto parent (JsHtml (ns, _)) = do
  mapM_ (renderNode parent) ns
  done

renderNode ::
  Effect f ('MutableObject Dom.DomElement) -> Node f -> EffectSyntax f ()
renderNode parent = \case
  Element name attrs (JsHtml (ns, _)) -> build parent name attrs ns
  Void name attrs (JsHtml (ns, _))
    | all isModifier ns -> build parent name attrs ns
    | otherwise ->
        error $
          "JShark.Lucid: <"
            ++ T.unpack name
            ++ "> is a void element and cannot have children"
  TextNode t -> do
    -- No JShark.Dom wrapper for text nodes; appendChild takes any Node.
    node <- hold (ffi "document.createTextNode" (arg t <: RecNil))
    void (Dom.appendChild parent node)
  Modifier _ ->
    error "JShark.Lucid: a modifier needs an enclosing element"

build ::
  Effect f ('MutableObject Dom.DomElement)
  -> Text
  -> [Attribute]
  -> [Node f]
  -> EffectSyntax f ()
build parent name attrs ns = do
  el <- Dom.createElement (string name)
  mapM_ (applyAttribute el) attrs
  -- Attributes first, then modifiers: a dynAttr overrides a static one.
  mapM_ (applyModifier el) [m | Modifier m <- ns]
  mapM_ (renderNode el) (Prelude.filter (not . isModifier) ns)
  void (Dom.appendChild parent el)

isModifier :: Node f -> Bool
isModifier = \case
  Modifier _ -> True
  _ -> False

applyAttribute ::
  Effect f ('MutableObject Dom.DomElement) -> Attribute -> EffectSyntax f ()
applyAttribute el (Attribute n v) = void (Dom.setAttribute el n (string v))

applyModifier ::
  Effect f ('MutableObject Dom.DomElement) -> Modifier f -> EffectSyntax f ()
applyModifier el = \case
  SetAttr n v -> void (Dom.setAttribute el n v)
  -- @toggle(cls, force)@, not @if (c) add(cls)@: one call instead of a
  -- statement, and it clears the class when the test stops holding.
  ClassWhen c cls ->
    void . toSyntax $
      callMethod el "classList.toggle" (arg (string cls) <: arg c <: RecNil)
  SetProp n (SomeExpr v) -> void (setProp el (T.unpack n) v)
  Listen ev body -> addEventListener_ ev el body
