{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | The Haskell snippets in @docs\/tutorial.md@, compiled as real
-- definitions so the prose cannot drift from the API. Nothing here is
-- executed beyond the pure evaluation smoke test; the point is that the
-- module typechecks.
module TutorialSnippets (tutorialSnippets) where

import Data.Text (Text)
import GHC.Generics (Generic)
import JShark
import JShark.Api
import qualified JShark.Api.Generic as G
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Console as Console
import qualified JShark.Dom as Dom
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

greet :: Expr f 'String -> Effect f 'Unit
greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done

addFn :: Expr f 'Number -> Expr f 'Number -> Expr f ('Function 'Number 'Number)
addFn a b = lambda (\x -> x + a + b)

wire :: Effect f ('MutableObject Dom.DomElement) -> EffectSyntax f (f 'Unit)
wire el = do
  board <- Dom.byId "board"
  addEventListenerS "keydown" board $ \e -> do
    k <- eventKey e
    toSyntax_ (callMethod el "flash" (arg k <: RecNil))
    done
  done

logMax :: Effect f 'Unit
logMax = fromSyntax $ do
  toSyntax_ $ ffi "console.log" (arg "max" <: arg 2 <: arg 9 <: RecNil)
  done

data Person = Person
  { fullName :: Text
  , years :: Double
  }
  deriving (Generic)

-- | @G.toObject (Person "Ada" 36)@ is already an 'Effect'; compile it
-- directly (the tutorial previously wrapped it in @fromSyntax@, which does
-- not typecheck).
personObject :: Effect f ('MutableObject (G.As Person))
personObject = G.toObject (Person "Ada" 36)

tutorialSnippets :: TestTree
tutorialSnippets =
  testGroup
    "tutorial snippets"
    [ testCase "compiled and evaluated: curried add via lambda" $
        evaluateNumber (apply (addFn (number 1) (number 2)) (number 0)) @?= 3
    ]
