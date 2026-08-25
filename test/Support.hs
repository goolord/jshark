{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

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
  , bytes
  , byteElems
  , assertJSContains
  )
where

import Data.Array.Byte (ByteArray (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts
  ( Int (..)
  , indexWord8Array#
  , newByteArray#
  , sizeofByteArray#
  , unsafeFreezeByteArray#
  , writeWord8Array#
  , (+#)
  )
import GHC.Generics (Generic)
import GHC.ST (ST (..), runST)
import GHC.Word (Word8 (..))
import JShark.Api
import JShark.Rec (Rec (..), (<:))
import JShark.Types
import Test.Tasty.HUnit ((@?=))

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
                ifE
                  (ffi "Boolean" (arg (number 1) <: RecNil))
                  x
                  (expr (number 0))
            )
        )
        (expr (number 6))
  yield (Var r)

numArray :: forall f. Expr f ('Array 'Number)
numArray = Literal (ValueArray [ValueNumber 1, ValueNumber 2])

mulDiv :: forall f. Expr f 'Number
mulDiv = number 6 * number 7 / number 2

bytes :: [Word8] -> ByteArray
bytes xs = runST go
 where
  !(I# n#) = length xs
  go :: ST s ByteArray
  go = ST $ \s0 ->
    case newByteArray# n# s0 of
      (# s1, mba #) ->
        case write 0# xs mba s1 of
          s2 -> case unsafeFreezeByteArray# mba s2 of
            (# s3, ba #) -> (# s3, ByteArray ba #)
  write _ [] _ s = s
  write i# (W8# w : rest) mba s =
    write (i# +# 1#) rest mba (writeWord8Array# mba i# w s)

byteElems :: ByteArray -> [Word8]
byteElems (ByteArray ba#) =
  [W8# (indexWord8Array# ba# i#) | I# i# <- [0 .. I# (sizeofByteArray# ba#) - 1]]

-- | Assert emitted JS contains @needle@ (layout-independent smoke check).
assertJSContains :: Text -> Text -> IO ()
assertJSContains needle haystack =
  T.isInfixOf needle haystack @?= True
