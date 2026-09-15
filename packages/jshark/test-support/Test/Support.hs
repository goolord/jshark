{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Shared fixtures and golden-case helpers for the core test suite and the
--   compiler benchmarks. Not a user-facing API.
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
  , effectCodeCase
  , effectCodeCaseWith
  , pureCodeCase
  , effectContains
  , effectContainsWith
  , pureContains
  , evalBoolCase
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
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import JShark (JS, evaluate, renderJS)
import JShark.Api
import JShark.Api.Caller (callerBinderHint)
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types
import JShark.Compiler (biomeAvailable)
import JShark.Internal (EmitStyle, effectfulAST, effectfulASTWith, pureAST)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

data LitRow

type instance Field LitRow "x" = 'Number

type instance Field LitRow "y" = 'Number

type instance Field LitRow "s" = 'String

data Person = Person
  { fullName :: Text
  , years :: Double
  }
  deriving Generic

data Packet = Packet
  { octets :: ByteArray
  }
  deriving Generic

data Tagged = Tagged
  { label :: Text
  , tags :: [Text]
  , nickname :: Maybe Text
  }
  deriving Generic

data Group = Group
  { members :: [Person]
  }
  deriving Generic

data Team = Team
  { lead :: Maybe Person
  }
  deriving Generic

data Color = Red | Green | Blue
  deriving Generic

data Shape
  = Circle Double
  | Rect Double Double
  deriving Generic

data Badge = Badge
  { hue :: Color
  }
  deriving Generic

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

-- | Golden case: rendered JS must equal @golden@ exactly.
codeCase :: String -> JS -> Text -> TestTree
codeCase name js golden =
  testCase name (renderJS js @?= TE.encodeUtf8 golden)

-- | Smoke case: rendered JS must contain every needle.
containsCase :: String -> JS -> [Text] -> TestTree
containsCase name js needles =
  testCase name $ mapM_ (assertJSContains' (renderJS js)) needles
 where
  assertJSContains' hay n =
    assertBool (T.unpack n <> " missing") (TE.encodeUtf8 n `BS.isInfixOf` hay)

-- | Golden case for a closed effectful program.
effectCodeCase :: String -> ClosedEffect u -> Text -> TestTree
effectCodeCase name eff = codeCase name (effectfulAST eff)

-- | Golden case for a closed effectful program under an explicit emit style.
effectCodeCaseWith :: EmitStyle -> String -> ClosedEffect u -> Text -> TestTree
effectCodeCaseWith style name eff = codeCase name (effectfulASTWith style eff)

-- | Golden case for a closed pure expression.
pureCodeCase :: String -> ClosedExpr u -> Text -> TestTree
pureCodeCase name e = codeCase name (pureAST e)

-- | Smoke case: rendered effect must contain every needle.
effectContains :: String -> ClosedEffect u -> [Text] -> TestTree
effectContains name eff = containsCase name (effectfulAST eff)

-- | 'effectContains' under an explicit emit style.
effectContainsWith ::
  EmitStyle -> String -> ClosedEffect u -> [Text] -> TestTree
effectContainsWith style name eff = containsCase name (effectfulASTWith style eff)

-- | Smoke case: rendered pure expression must contain every needle.
pureContains :: String -> ClosedExpr u -> [Text] -> TestTree
pureContains name e = containsCase name (pureAST e)

-- | Evaluate a closed pure Bool expression.
evalBoolCase :: String -> ClosedExpr 'Bool -> Bool -> TestTree
evalBoolCase name e expected =
  testCase name $ case evaluate e of
    ValueBool b -> b @?= expected

-- | Assert emitted JS contains @needle@ (layout-independent smoke check).
assertJSContains :: Text -> Text -> IO ()
assertJSContains = assertInfix True

-- | Assert emitted JS does /not/ contain @needle@.
assertJSOmits :: Text -> Text -> IO ()
assertJSOmits = assertInfix False

assertInfix :: Bool -> Text -> Text -> IO ()
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
