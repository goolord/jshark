{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Describe a DOM tree with Lucid's combinators, emit JavaScript that
-- builds it.
--
-- Lucid's 'Lucid.Html' is a function to a @Builder@, not a tree, so it
-- cannot be walked, but its __syntax__ is reusable: 'Term' and 'With' are
-- open classes and 'Attribute' is a pair of 'Text', so container elements
-- ('Lucid.div_', 'Lucid.li_', …) and every attribute ('Lucid.class_', …)
-- work unchanged at 'JsHtml'. Lucid's void elements ('Lucid.input_',
-- 'Lucid.br_') are fixed to @HtmlT@; use 'void_'.
--
-- Anything dynamic is a child: 'dynText', 'dynAttr', 'classWhen', 'prop',
-- and 'on' sit in the child block, and modifiers apply to the enclosing
-- element wherever they appear (rendering hoists them ahead of its
-- children). A template with no such holes is polymorphic in 'Term', so
-- the same value renders as Lucid's @Html ()@ on the server and as
-- 'JsHtml' in the client:
--
-- @
-- shell :: (Term (h ()) (h ()), Term [Attribute] (h () -> h ()), IsString (h ())) => h ()
-- shell = li_ (div_ [class_ "view"] "hi")
-- @
--
-- The price of keeping the type free of a slot index is that a modifier
-- with no enclosing element, or a child inside a void element, is caught
-- when the JavaScript is generated rather than by the type checker:
-- 'templateErrors' reports them with the element path, and 'renderInto' /
-- 'renderFragment' throw the first one, so a bad template fails the build.
--
-- @
-- row :: Expr f 'String -> Expr f 'Bool -> JsHtml f ()
-- row title done = li_ $ do
--   classWhen done "completed"
--   div_ [class_ "view"] $ do
--     void_ "input" [class_ "toggle", type_ "checkbox"]
--     label_ (dynText title)
-- @
module JShark.Lucid
  ( JsHtml

    -- * Rendering
  , renderInto
  , renderFragment

    -- * Validation
  , TemplateError (..)
  , templateErrors

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

import Control.Exception (Exception (..), throw)
import Control.Monad (void)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Api
import qualified JShark.Dom as Dom
import Lucid.Base (Attribute (..), Term (..), With (..))

-- | A DOM fragment. The @a@ parameter exists so @do@ blocks sequence
-- siblings, exactly as in Lucid; the payload is always discarded.
newtype JsHtml f a = JsHtml ([Node f], a)
  deriving (Functor, Applicative, Monad) via ((,) [Node f])

-- | One node of the fragment.
data Node f
  = Element Text [Attribute] (JsHtml f ())
  | -- | No children, so the block holds modifiers only.
    Void Text [Attribute] (JsHtml f ())
  | TextNode (Expr f 'String)
  | -- | Applies to the enclosing element rather than adding a node.
    Modifier (El f -> EffectSyntax f ())

type El f = Effect f ('MutableObject Dom.DomElement)

single :: Node f -> JsHtml f ()
single n = JsHtml ([n], ())

modifier :: (El f -> EffectSyntax f ()) -> JsHtml f ()
modifier = single . Modifier

instance a ~ () => Semigroup (JsHtml f a) where
  x <> y = x >> y

instance a ~ () => Monoid (JsHtml f a) where
  mempty = pure ()

-- | @OverloadedStrings@ in a child block is a text node.
instance a ~ () => IsString (JsHtml f a) where
  fromString = text_ . T.pack

-- | @div_ child@ and @div_ [class_ "x"] child@, from Lucid. The payload
-- types are equality constraints rather than a literal @()@ so that a
-- statement in a @do@ block, whose result is discarded and therefore
-- unconstrained, still picks this instance — the same trick Lucid uses.
instance (a ~ (), b ~ ()) => Term (JsHtml f a) (JsHtml f b) where
  termWith name attrs child = single (Element name attrs child)

instance (child ~ JsHtml f (), a ~ ()) => Term [Attribute] (child -> JsHtml f a) where
  termWith name extra attrs child = single (Element name (extra <> attrs) child)

-- | Lucid's @with@: add the attributes to every element at the root of
-- the fragment (Lucid applies them to the one element it wraps; a JShark
-- fragment may have several roots). Text nodes and modifiers are untouched.
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

-- | A void element (@input@, @br@, @img@, …). Lucid's own are fixed to
-- @HtmlT@, so they cannot be reused here.
void_ :: Text -> [Attribute] -> JsHtml f ()
void_ name attrs = single (Void name attrs (pure ()))

-- | 'void_' with a block of modifiers ('prop', 'on', …).
voidWith_ :: Text -> [Attribute] -> JsHtml f () -> JsHtml f ()
voidWith_ name attrs mods = single (Void name attrs mods)

-- | @el.setAttribute(name, v)@ with a computed value. Modifiers run after
-- the element's Lucid attributes, so this wins over a static @class_@ or
-- @href_@ of the same name.
dynAttr :: Text -> Expr f 'String -> JsHtml f ()
dynAttr n v = modifier $ \el -> void (Dom.setAttribute el n v)

-- | Add a class when the test holds, remove it when it does not.
--
-- Compiles to @classList.toggle(c, test)@ (one call rather than an @if@),
-- so the element always carries a @class@ attribute — an empty one when
-- nothing matched, rather than no attribute at all.
classWhen :: Expr f 'Bool -> Text -> JsHtml f ()
classWhen c cls = modifier $ \el ->
  void . toSyntax $
    callMethod el "classList.toggle" (arg (string cls) <: arg c <: RecNil)

-- | @el.name = v@. A property, not an attribute: a checkbox's state is
-- @checked@ the property, which is not the same as the @checked@ attribute.
prop :: Text -> Expr f u -> JsHtml f ()
prop n v = modifier $ \el -> void (setProp el (T.unpack n) v)

-- | @el.addEventListener(event, () => body)@.
on :: Text -> EffectSyntax f (f 'Unit) -> JsHtml f ()
on ev body = modifier $ \el -> Dom.addEventListener_ ev el body

-- | A structural problem with a template. 'tePath' is the chain of element
-- names from the root to the offending node (empty for a root modifier).
data TemplateError = TemplateError
  { tePath :: [Text]
  , teMessage :: Text
  }
  deriving (Show, Eq)

instance Exception TemplateError where
  displayException (TemplateError path msg) =
    "JShark.Lucid: "
      ++ (if null path then "(root)" else T.unpack (T.intercalate " > " path))
      ++ ": "
      ++ T.unpack msg

-- | All structural problems in a fragment: a modifier with no enclosing
-- element, or a child inside a void element. The renderers check these
-- first and throw the first one, so a bad template fails the build rather
-- than reaching a browser.
templateErrors :: JsHtml f () -> [TemplateError]
templateErrors (JsHtml (ns, _)) = go False [] ns
 where
  go inElement path = concatMap $ \case
    Modifier _ ->
      [ TemplateError path "a modifier needs an enclosing element"
      | not inElement
      ]
    TextNode _ -> []
    Element name _ (JsHtml (cs, _)) -> go True (path <> [name]) cs
    Void name _ (JsHtml (cs, _)) ->
      [ TemplateError
          (path <> [name])
          ("<" <> name <> "> is a void element and cannot have children")
      | not (all isModifier cs)
      ]

-- | Emit the JavaScript that builds the fragment and appends its roots
-- to @parent@. Throws the first of 'templateErrors', if any, while the
-- JavaScript is being generated.
renderInto ::
  Effect f ('MutableObject Dom.DomElement)
  -> JsHtml f ()
  -> EffectSyntax f (f 'Unit)
renderInto parent h = renderChildren parent h >> done

-- | Build the fragment offline, then append it once for a single live-DOM
-- insertion (see @DocumentFragment@ in the DOM performance guides).
renderFragment ::
  JsHtml f () -> EffectSyntax f (Effect f ('MutableObject Dom.DomElement))
renderFragment h = do
  frag <- hold $ ffi "document.createDocumentFragment" RecNil
  frag <$ renderChildren frag h

-- | Check the template, then render its nodes into @parent@.
renderChildren :: El f -> JsHtml f () -> EffectSyntax f ()
renderChildren parent h@(JsHtml (ns, _)) = do
  mapM_ throw (take 1 (templateErrors h))
  mapM_ (renderNode parent) ns

-- | 'templateErrors' has already rejected orphan modifiers and void
-- children, so both element kinds render alike and 'build' has applied
-- any modifiers.
renderNode :: El f -> Node f -> EffectSyntax f ()
renderNode parent = \case
  Element name attrs (JsHtml (ns, _)) -> build parent name attrs ns
  Void name attrs (JsHtml (ns, _)) -> build parent name attrs ns
  TextNode t -> do
    -- No JShark.Dom wrapper for text nodes; appendChild takes any Node.
    node <- hold (ffi "document.createTextNode" (arg t <: RecNil))
    void (Dom.appendChild parent node)
  Modifier _ -> pure ()

build :: El f -> Text -> [Attribute] -> [Node f] -> EffectSyntax f ()
build parent name attrs ns = do
  el <- Dom.createElement (string name)
  -- Attributes first, then modifiers (so a dynAttr overrides a static
  -- attribute, and properties are set before any child is appended).
  mapM_ (\(Attribute n v) -> void (Dom.setAttribute el n (string v))) attrs
  mapM_ ($ el) [m | Modifier m <- ns]
  mapM_ (renderNode el) ns
  void (Dom.appendChild parent el)

isModifier :: Node f -> Bool
isModifier = \case
  Modifier _ -> True
  _ -> False
