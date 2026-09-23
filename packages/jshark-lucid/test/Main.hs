{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 'JShark.Lucid' templates, executed in a real DOM (happy-dom via bun).
module Main (main) where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import JShark.Api
import JShark.Api.Types
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
import Lucid (button_, class_, div_, label_, li_, renderText, type_)
import Lucid.Base (Attribute, Html, Term)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "JShark.Lucid" [validationTests, lucidDomTests]

-- | Structural checks do not need a DOM.
validationTests :: TestTree
validationTests =
  testGroup
    "validation"
    [ testCase "a valid template has no errors" $
        templateErrors (todoRow (string "x") true_) @?= []
    , testCase "an orphan modifier is reported" $ do
        let
          errs = templateErrors (dynAttr "class" (string "x"))
        assertBool
          ("orphan message: " <> show errs)
          (any (T.isInfixOf "enclosing" . teMessage) errs)
    , testCase "a void child is reported with a path" $
        case templateErrors (voidWith_ "input" [] (text_ "nope")) of
          e : _ -> do
            tePath e @?= ["input"]
            assertBool "void message" (T.isInfixOf "void" (teMessage e))
          [] -> assertFailure "expected a void-child error"
    , testCase "modifiers inside an element are valid" $
        templateErrors (li_ (classWhen true_ "x")) @?= []
    ]

lucidDomTests :: TestTree
lucidDomTests =
  testGroup
    "happy-dom"
    [ domCase
        "static structure survives the round trip"
        (markupOf (li_ (div_ [class_ "view"] "hi")))
        "\"<li><div class=\\\"view\\\">hi</div></li>\""
    , testCase "a hole-free template renders the same as Lucid" $ do
        -- The point of reusing Lucid's classes: one value, two backends.
        got <- runDom (markupOf shared)
        got @?= show (TL.unpack (renderText (shared :: Html ())))
    , domCase
        "text_ is a text node"
        (markupOf (li_ (text_ "plain")))
        "\"<li>plain</li>\""
    , domCase
        "void_ has no closing tag"
        (markupOf (void_ "input" [class_ "toggle", type_ "checkbox"]))
        "\"<input class=\\\"toggle\\\" type=\\\"checkbox\\\">\""
    , domCase
        "dynText is a text node"
        (inspect "li label" Dom.innerText (todoRow (string "write tests") true_))
        "\"write tests\""
    , domCase
        "classWhen adds the class when the test holds"
        (attrOf "li" "class" (todoRow (string "x") true_))
        "\"completed\""
    , domCase
        "classWhen leaves the class off when the test fails"
        (hasClass "li" "completed" (todoRow (string "x") false_))
        "false"
    , domCase
        "prop sets the property, not the attribute"
        ( inspect "input.toggle" (`getProp` "checked") (todoRow (string "x") true_) ::
            Effect f 'Bool
        )
        "true"
    , domCase
        "dynAttr sets a computed attribute"
        ( attrOf
            "li"
            "data-id"
            (li_ (dynAttr "data-id" (string "id-" <> string "7")))
        )
        "\"id-7\""
    , domCase
        "dynAttr overrides a static attribute of the same name"
        (attrOf "li" "class" (li_ [class_ "static"] (dynAttr "class" (string "dynamic"))))
        "\"dynamic\""
    , domCase "on wires an event listener" clickTemplate "\"yes\""
    , domCase
        "renderFragment inserts in one replaceChildrenFrom"
        ( fromSyntax $ do
            frag <- renderFragment (li_ (text_ "batched"))
            root <- Dom.lookupId (string "root")
            _ <- Dom.replaceChildrenFrom root frag
            Dom.innerHTML root >>= yield
        )
        "\"<li>batched</li>\""
    ]

-- | A template with no holes, so it is polymorphic over 'Term' and can be
-- rendered by Lucid as well as by 'renderInto'.
shared ::
  forall h.
  (Term (h ()) (h ()), Term [Attribute] (h () -> h ()), IsString (h ())) => h ()
-- The literal needs the annotation: a given @Term@ constraint's fundep
-- does not drive inference the way an instance's would.
shared = li_ (div_ [class_ "view"] ("shared" :: h ()))

-- | The TodoMVC row, the template this library was built for.
todoRow :: Expr f 'String -> Expr f 'Bool -> JsHtml f ()
todoRow title isDone = li_ $ do
  classWhen isDone "completed"
  div_ [class_ "view"] $ do
    voidWith_ "input" [class_ "toggle", type_ "checkbox"] (prop "checked" isDone)
    label_ (dynText title)
    button_ [class_ "destroy"] mempty

type El f = Effect f ('MutableObject Dom.DomElement)

-- | Render into @#root@, then inspect the first element matching a
-- selector.
inspect ::
  Text -> (El f -> EffectSyntax f (Expr f u)) -> JsHtml f () -> Effect f u
inspect selector k html = fromSyntax $ do
  root <- Dom.lookupId (string "root")
  _ <- renderInto root html
  select selector >>= k >>= yield

select :: Text -> EffectSyntax f (El f)
select s = hold (ffi "document.querySelector" (arg (string s) <: RecNil))

-- | The markup the template produced.
markupOf :: JsHtml f () -> Effect f 'String
markupOf = inspect "#root" Dom.innerHTML

-- | An attribute of the selected element, or @\"\"@.
attrOf :: Text -> Text -> JsHtml f () -> Effect f 'String
attrOf selector name = inspect selector $ \el -> do
  v <- toSyntax (callMethod el "getAttribute" (arg (string name) <: RecNil))
  pure (orElse (unsafeNullable (Var v)) (string ""))

-- | Whether the selected element carries a class.
hasClass :: Text -> Text -> JsHtml f () -> Effect f 'Bool
hasClass selector cls = inspect selector $ \el ->
  Var
    <$> toSyntax (callMethod el "classList.contains" (arg (string cls) <: RecNil))

-- | A click handler that marks the root. Proves the listener is attached
-- to the element the template describes.
clickTemplate :: forall f. Effect f 'String
clickTemplate = fromSyntax $ do
  root <- Dom.lookupId (string "root")
  _ <-
    renderInto root . button_ [class_ "hit"] $
      on "click" (Dom.setAttribute root "data-hit" (string "yes"))
  btn <- select "button.hit"
  _ <- toSyntax (callMethod btn "click" RecNil :: Effect f 'Unit)
  v <- Dom.getAttribute root "data-hit"
  yield (orElse v (string ""))

domCase :: String -> (forall f. Effect f u) -> String -> TestTree
domCase name e expected = testCase name (runDom e >>= (@?= expected))

runDom :: (forall f. Effect f u) -> IO String
runDom e = T.unpack <$> evaluateEffectJSONWith cfg e
 where
  body = "<div id=\"root\"></div>"
  cfg = domBunConfig {bunEnv = HappyDom defaultHappyDomOptions {happyDomBody = body}}
