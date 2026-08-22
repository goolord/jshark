{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
#-}

module Support
  ( LitRow
  , Person(..)
  , Tagged(..)
  , Group(..)
  , Color(..)
  , Shape(..)
  , Badge(..)
  , fooE
  , barE
  , condE
  , yieldString
  , with1
  , with2
  , prettyIfLambda
  , numArray
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import JShark.Api
import JShark.Rec (Rec(..), (<:))
import JShark.Types

data LitRow
type instance Field LitRow "x" = 'Number
type instance Field LitRow "y" = 'Number
type instance Field LitRow "s" = 'String

data Person = Person
  { fullName :: Text
  , years :: Double
  }
  deriving (Generic)

data Tagged = Tagged
  { label :: Text
  , tags :: [Text]
  , nickname :: Maybe Text
  }
  deriving (Generic)

data Group = Group
  { members :: [Person]
  }
  deriving (Generic)

data Color = Red | Green | Blue
  deriving (Generic)

data Shape
  = Circle Double
  | Rect Double Double
  deriving (Generic)

data Badge = Badge
  { hue :: Color
  }
  deriving (Generic)

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

with2 :: Effect f a -> Effect f b -> (Expr f a -> Expr f b -> Expr f c) -> Effect f c
with2 e1 e2 k = fromSyntax $ do
  x <- toSyntax e1
  y <- toSyntax e2
  toSyntax (expr (k (Var x) (Var y)))

prettyIfLambda :: forall f. Effect f 'Number
prettyIfLambda = fromSyntax $ do
  r <- toSyntax $ ApplyE
    (lambdaE (\x ->
        ifE (ffi "Boolean" (arg (number 1) <: RecNil))
          x
          (expr (number 0))))
    (expr (number 6))
  yield (Var r)

numArray :: forall f. Expr f ('Array 'Number)
numArray = Literal (ValueArray [ValueNumber 1, ValueNumber 2])
