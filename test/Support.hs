{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Support
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
  , prettyIfLambda
  , numArray
  , mulDiv
  , assertJSContains
  , captureStderr
  , requireBiome
  )
where

import Control.Exception (evaluate, finally)
import Control.Monad (unless)
import Data.Array.Byte (ByteArray)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types
import JShark.Compiler (biomeAvailable)
import System.IO
  ( BufferMode (..)
  , hClose
  , hFlush
  , hGetContents
  , hSetBuffering
  , stderr
  )
import System.Posix.IO (closeFd, createPipe, dup, dupTo, fdToHandle, stdError)
import Test.Tasty.HUnit (assertFailure)

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

prettyIfLambda :: forall f. Effect f 'Number
prettyIfLambda = fromSyntax $ do
  r <-
    toSyntax $
      ApplyE
        ( lambdaE
            ( \x ->
                Bind (expr (number 0)) $ \z ->
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

-- | Assert emitted JS contains @needle@ (layout-independent smoke check).
assertJSContains :: Text -> Text -> IO ()
assertJSContains needle haystack =
  unless (T.isInfixOf needle haystack)
    $ assertFailure
    $ "missing "
      <> T.unpack needle
      <> " in:\n"
      <> T.unpack haystack

-- | Run @io@ with stderr redirected to a string (restores stderr afterward).
captureStderr :: IO a -> IO (a, String)
captureStderr io = do
  (readFd, writeFd) <- createPipe
  backup <- dup stdError
  _ <- dupTo writeFd stdError
  closeFd writeFd
  result <-
    io `finally` do
      hFlush stderr
      _ <- dupTo backup stdError
      closeFd backup
      pure ()
  readH <- fdToHandle readFd
  hSetBuffering readH NoBuffering
  msg <- hGetContents readH
  _ <- evaluate (length msg)
  hClose readH
  pure (result, msg)

requireBiome :: IO ()
requireBiome = do
  avail <- biomeAvailable
  unless avail $
    assertFailure "biome not on PATH (install biome, bunx, or use nix develop)"
