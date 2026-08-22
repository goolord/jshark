{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
#-}

-- | 'JShark.Lucid' templates, executed in a real DOM (happy-dom via bun).
module LucidTests (lucidDomTests) where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import JShark.Api
import JShark.Bun
  ( BunConfig (..)
  , BunEnv (..)
  , HappyDomOptions (..)
  , defaultHappyDomOptions
  , domBunConfig
  , evaluateEffectJSONWith
  )
import qualified JShark.Dom as Dom
import JShark.Lucid
import JShark.Rec (Rec (..), (<:))
import JShark.Types
import Lucid (button_, class_, div_, label_, li_, renderText, type_)
import Lucid.Base (Attribute, Html, Term)
import Test.Tasty
import Test.Tasty.HUnit

lucidDomTests :: TestTree
lucidDomTests =
  after AllSucceed "happy-dom is available" $
    testGroup "JShark.Lucid"
      [ domCase "static structure survives the round trip"
          (markupOf (li_ (div_ [class_ "view"] "hi")))
          "\"<li><div class=\\\"view\\\">hi</div></li>\""
      , testCase "a hole-free template renders the same as Lucid" $ do
          -- The point of reusing Lucid's classes: one value, two backends.
          let expected = TL.unpack (renderText (shared :: Html ()))
          got <- runDom (markupOf shared)
          assertEqual "same markup" (show expected) got
      , domCase "text_ is a text node" (markupOf (li_ (text_ "plain"))) "\"<li>plain</li>\""
      , domCase "void_ has no closing tag"
          (markupOf (void_ "input" [class_ "toggle", type_ "checkbox"]))
          "\"<input class=\\\"toggle\\\" type=\\\"checkbox\\\">\""
      , domCase "dynText is a text node"
          (textOf (string "li label") (todoRow (string "write tests") true_))
          "\"write tests\""
      , domCase "classWhen adds the class when the test holds"
          (attrOf (string "li") (string "class") (todoRow (string "x") true_))
          "\"completed\""
      , domCase "classWhen leaves the class off when the test fails"
          (hasClass (string "li") (string "completed") (todoRow (string "x") false_))
          "false"
      , domCase "prop sets the property, not the attribute"
          (checkedOf (todoRow (string "x") true_))
          "true"
      , domCase "dynAttr sets a computed attribute"
          ( attrOf
              (string "li")
              (string "data-id")
              (li_ (dynAttr "data-id" (Concat (string "id-") (string "7"))))
          )
          "\"id-7\""
      , domCase "dynAttr overrides a static attribute of the same name"
          ( attrOf
              (string "li")
              (string "class")
              (li_ [class_ "static"] (dynAttr "class" (string "dynamic")))
          )
          "\"dynamic\""
      , domCase "on wires an event listener" clickTemplate "\"yes\""
      ]

{- | A template with no holes, so it is polymorphic over 'Term' and can be
rendered by Lucid as well as by 'renderInto'.
-}
shared ::
  forall h.
  (Term (h ()) (h ()), Term [Attribute] (h () -> h ()), IsString (h ())) => h ()
-- The literal needs the annotation: a given @Term@ constraint's fundep
-- does not drive inference the way an instance's would.
shared = li_ (div_ [class_ "view"] ("shared" :: h ()))

-- | Render into @#root@, then hand the block an element to inspect.
withRoot ::
  JsHtml f ()
  -> (Effect f ('MutableObject Dom.DomElement) -> EffectSyntax f (Expr f u))
  -> Effect f u
withRoot html k = fromSyntax $ do
  root <- Dom.lookupId (string "root")
  _ <- renderInto root html
  k root >>= yield

-- | The markup the template produced.
markupOf :: JsHtml f () -> Effect f 'String
markupOf html = withRoot html Dom.innerHTML

-- | Text of the first element matching a selector.
textOf :: Expr f 'String -> JsHtml f () -> Effect f 'String
textOf selector html = withRoot html $ \_ -> querySelector selector >>= Dom.innerText

-- | An attribute of the first element matching a selector.
attrOf ::
  Expr f 'String -> Expr f 'String -> JsHtml f () -> Effect f ('Option 'String)
attrOf selector name html = withRoot html $ \_ -> do
  el <- querySelector selector
  v <- toSyntax (callMethod el "getAttribute" (arg name <: RecNil))
  pure (unsafeNullable (Var v))

-- | Whether an element carries a class.
hasClass :: Expr f 'String -> Expr f 'String -> JsHtml f () -> Effect f 'Bool
hasClass selector cls html = withRoot html $ \_ -> do
  el <- querySelector selector
  b <- toSyntax (callMethod el "classList.contains" (arg cls <: RecNil))
  pure (Var b)

-- | The checkbox's @checked@ property.
checkedOf :: JsHtml f () -> Effect f 'Bool
checkedOf html = withRoot html $ \_ -> do
  el <- querySelector (string "input.toggle")
  getProp el "checked"

querySelector ::
  Expr f 'String -> EffectSyntax f (Effect f ('MutableObject Dom.DomElement))
querySelector selector = hold (ffi "document.querySelector" (arg selector <: RecNil))

{- | A click handler that marks the root. Proves the listener is attached
to the element the template describes.
-}
clickTemplate :: forall f. Effect f 'String
clickTemplate = fromSyntax $ do
  root <- Dom.lookupId (string "root")
  _ <-
    renderInto root $
      button_ [class_ "hit"] $
        on "click" (Dom.setAttribute root "data-hit" (string "yes"))
  btn <- querySelector (string "button.hit")
  _ <- toSyntax (callMethod btn "click" RecNil :: Effect f 'Unit)
  v <- Dom.getAttribute root "data-hit"
  yield v

-- | The TodoMVC row, the template this library was built for.
todoRow :: Expr f 'String -> Expr f 'Bool -> JsHtml f ()
todoRow title isDone = li_ $ do
  classWhen isDone "completed"
  div_ [class_ "view"] $ do
    voidWith_ "input" [class_ "toggle", type_ "checkbox"] (prop "checked" isDone)
    label_ (dynText title)
    button_ [class_ "destroy"] mempty

domCase :: String -> (forall f. Effect f u) -> String -> TestTree
domCase name e expected = testCase name $ do
  got <- runDom e
  assertEqual name expected got

runDom :: (forall f. Effect f u) -> IO String
runDom e = T.unpack <$> evaluateEffectJSONWith cfg e
  where
    cfg =
      domBunConfig
        {bunEnv = HappyDom defaultHappyDomOptions {happyDomBody = rootDiv}}

rootDiv :: Text
rootDiv = "<div id=\"root\"></div>"
