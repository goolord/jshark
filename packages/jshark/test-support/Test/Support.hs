{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Shared fixtures and golden-case helpers for the core test suite and the
--   example suites. Not a user-facing API.
--
--   Case helpers take the test name, then the expectation, then the
--   program last so a multi-line program can follow a trailing @$@.
module Test.Support
  ( LitRow
  , Person (..)
  , Packet (..)
  , Tagged (..)
  , Group (..)
  , Team (..)
  , Color (..)
  , Shape (..)
  , Badge (..)
  , fooE
  , barE
  , condE
  , yieldString
  , with1
  , with2
  , readableBindSample
  , readableLetSample
  , callerHintProbe
  , prettyIfLambda
  , numArray
  , mulDiv
  , jsText
  , effectText
  , pureText
  , jsIs
  , effectJS
  , pureJS
  , syntaxJS
  , jsHas
  , effectHas
  , pureHas
  , syntaxHas
  , Needle
  , lacks
  , evalCase
  , assertEval
  , assertJS
  , assertJSContains
  , assertJSOmits
  , assertThrows
  , captureStderr
  , requireBiome
  )
where

import CaptureStderr (captureStderr)
import qualified Control.Exception as E
import Control.Monad (unless)
import Data.Array.Byte (ByteArray)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import JShark (JS, evaluate, renderJS)
import JShark.Api
import JShark.Api.Caller (callerBinderHint)
import qualified JShark.Api.Generic as G
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types
import JShark.Compiler (biomeAvailable)
import JShark.Internal (effectfulAST, pureAST)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

data LitRow

type instance Field LitRow "x" = 'Number

type instance Field LitRow "y" = 'Number

type instance Field LitRow "s" = 'String

data Person = Person {fullName :: Text, years :: Double} deriving Generic

data Packet = Packet {octets :: ByteArray} deriving Generic

data Tagged = Tagged {label :: Text, tags :: [Text], nickname :: Maybe Text}
  deriving Generic

data Group = Group {members :: [Person]} deriving Generic

data Team = Team {lead :: Maybe Person} deriving Generic

data Color = Red | Green | Blue deriving Generic

data Shape = Circle Double | Rect Double Double deriving Generic

data Badge = Badge {hue :: Color} deriving Generic

fooE, barE :: Effect f u
fooE = ffi "foo" RecNil
barE = ffi "bar" RecNil

condE :: Effect f 'Bool
condE = ffi "cond" RecNil

yieldString :: Expr f 'String -> EffectSyntax f (f 'String)
yieldString = yield

with1 :: Effect f a -> (Expr f a -> Expr f b) -> Effect f b
with1 e k = fromSyntax $ do
  x <- toSyntax e
  toSyntax (expr (k (Var x)))

with2 ::
  Effect f a -> Effect f b -> (Expr f a -> Expr f b -> Expr f c) -> Effect f c
with2 e1 e2 k = fromSyntax $ do
  x <- toSyntax e1
  y <- toSyntax e2
  toSyntax (expr (k (Var x) (Var y)))

-- | Pure let sample for readable 'HasCallStack' binder names.
{-# NOINLINE readableLetSample #-}
readableLetSample :: HasCallStack => Expr f 'Number
readableLetSample = let_ (sin (number 1)) (\x -> x + x)

-- | Effect bind sample for readable 'HasCallStack' binder names.
{-# NOINLINE readableBindSample #-}
readableBindSample :: HasCallStack => EffectSyntax f (f 'Number)
readableBindSample = do
  x <- toSyntax fooE
  toSyntax (expr (Var x + Var x))

{-# NOINLINE callerHintProbe #-}
callerHintProbe :: HasCallStack => () -> Maybe Text
callerHintProbe _ = callerBinderHint

prettyIfLambda :: forall f. Effect f 'Number
prettyIfLambda = fromSyntax $ do
  r <-
    toSyntax $
      ApplyE
        ( lambdaE
            ( \x ->
                Bind Nothing (expr (number 0)) $ \z ->
                  ifE
                    (ffi "Boolean" (arg (number 1) <: RecNil))
                    x
                    (Lift (Var z))
            )
        )
        (expr (number 6))
  yield (Var r)

numArray :: forall f. Expr f ('Array 'Number)
numArray = Literal (ValueArray [ValueNumber 1, ValueNumber 2])

mulDiv :: forall f. Expr f 'Number
mulDiv = number 6 * number 7 / number 2

-- | Emitted JS as 'Text'.
jsText :: JS -> Text
jsText = TE.decodeUtf8 . renderJS

effectText :: ClosedEffect u -> Text
effectText e = jsText (effectfulAST e)

pureText :: ClosedExpr u -> Text
pureText e = jsText (pureAST e)

-- | Golden case: the rendered JS is exactly @golden@.
jsIs :: String -> Text -> JS -> TestTree
jsIs name golden js = testCase name (renderJS js @?= TE.encodeUtf8 golden)

effectJS :: String -> Text -> ClosedEffect u -> TestTree
effectJS name golden e = jsIs name golden (effectfulAST e)

pureJS :: String -> Text -> ClosedExpr u -> TestTree
pureJS name golden e = jsIs name golden (pureAST e)

-- | 'effectJS' of @'fromSyntax' body@.
syntaxJS :: String -> Text -> (forall f. EffectSyntax f (f u)) -> TestTree
syntaxJS name golden body = effectJS name golden (fromSyntax body)

-- | Smoke-check needle: a string literal must occur in the JS; 'lacks'
-- must not.
data Needle = Needle Bool Text

instance IsString Needle where
  fromString = Needle True . T.pack

lacks :: Text -> Needle
lacks = Needle False

-- | Smoke case: the rendered JS satisfies every needle.
jsHas :: String -> [Needle] -> JS -> TestTree
jsHas name needles js = testCase name (assertJS needles (jsText js))

effectHas :: String -> [Needle] -> ClosedEffect u -> TestTree
effectHas name needles e = jsHas name needles (effectfulAST e)

pureHas :: String -> [Needle] -> ClosedExpr u -> TestTree
pureHas name needles e = jsHas name needles (pureAST e)

syntaxHas :: String -> [Needle] -> (forall f. EffectSyntax f (f u)) -> TestTree
syntaxHas name needles body = effectHas name needles (fromSyntax body)

-- | Host evaluation of @e@ yields @expected@ (use @\@Double@ etc. when the
-- literal's type is ambiguous).
evalCase ::
  (G.ToValue a, Eq a, Show a) =>
  String -> a -> ClosedExpr (G.UniverseOf a) -> TestTree
evalCase name expected e = testCase name (assertEval expected e)

assertEval ::
  (G.ToValue a, Eq a, Show a) => a -> ClosedExpr (G.UniverseOf a) -> Assertion
assertEval expected e = G.fromValue (evaluate e) @?= expected

-- | Layout-independent smoke check of @js@ against every needle.
assertJS :: [Needle] -> Text -> Assertion
assertJS needles js = mapM_ (\(Needle want n) -> assertInfix want n js) needles

-- | Assert emitted JS contains @needle@.
assertJSContains :: Text -> Text -> Assertion
assertJSContains = assertInfix True

-- | Assert emitted JS does /not/ contain @needle@.
assertJSOmits :: Text -> Text -> Assertion
assertJSOmits = assertInfix False

assertInfix :: Bool -> Text -> Text -> Assertion
assertInfix want needle haystack =
  unless (T.isInfixOf needle haystack == want)
    $ assertFailure
    $ (if want then "missing " else "unexpected ")
      <> T.unpack needle
      <> " in:\n"
      <> T.unpack haystack

-- | Force @x@ to WHNF and assert it throws an 'EvalFailure' or 'ErrorCall'
-- whose rendered message contains @needle@.
assertThrows :: Show a => String -> a -> IO ()
assertThrows needle x = do
  r <- E.try (E.evaluate x)
  case r of
    Left (e :: E.SomeException)
      | T.pack needle `T.isInfixOf` T.pack (E.displayException e) -> pure ()
      | otherwise ->
          assertFailure ("unexpected exception: " <> E.displayException e)
    Right v -> assertFailure ("expected throw, got " <> show v)

requireBiome :: IO ()
requireBiome = do
  avail <- biomeAvailable
  unless avail $
    assertFailure "biome not on PATH (install biome, bunx, or use nix develop)"
