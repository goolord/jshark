{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared IR for TypeScript / JavaScript → JShark bindings.
module JShark.Bindgen.Ir
  ( Ty (..)
  , Param (..)
  , Fun (..)
  , Prop (..)
  , ClassDecl (..)
  , ConstDecl (..)
  , EnumDecl (..)
  , EnumMember (..)
  , Skipped (..)
  , ModuleIr (..)
  , emptyModule
  , tyAndChildren
  , tyUsesUnknown
  , tyUsesPromise
  , moduleUsesUnknown
  , moduleUsesPromise
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- | TypeScript type after erasure onto JShark 'Universe' constructors.
data Ty
  = TyNumber
  | TyBigInt
  | TyString
  | TyBool
  | TyUnit
  | TyUint8Array
  | TyArray Ty
  | TyOption Ty
  | TyMap Ty Ty
  | TySet Ty
  | TyPromise Ty
  | TyFun [Ty] Ty
  | -- | Class / interface / enum phantom.
    TyNamed Text
  | -- | Could not map; emit 'JsUnknown'.
    TyUnknown Text
  deriving (Eq, Show)

data Param = Param
  { pName :: Text
  , pTy :: Ty
  , pOptional :: Bool
  }
  deriving (Eq, Show)

data Fun = Fun
  { fnName :: Text
  , fnFfi :: Text
  , fnParams :: [Param]
  , fnRet :: Ty
  , fnIsCtor :: Bool
  , fnStatic :: Bool
  }
  deriving (Eq, Show)

data Prop = Prop
  { prName :: Text
  , prTy :: Ty
  , prReadonly :: Bool
  }
  deriving (Eq, Show)

data ClassDecl = ClassDecl
  { clName :: Text
  , clFfi :: Text
  , clCtors :: [Fun]
  , clProps :: [Prop]
  , clMethods :: [Fun]
  }
  deriving (Eq, Show)

data ConstDecl = ConstDecl
  { cnName :: Text
  , cnFfi :: Text
  , cnTy :: Ty
  }
  deriving (Eq, Show)

data EnumMember = EnumMember
  { emName :: Text
  , emValue :: Maybe Text
  , emNumeric :: Bool
  }
  deriving (Eq, Show)

data EnumDecl = EnumDecl
  { enName :: Text
  , enMembers :: [EnumMember]
  }
  deriving (Eq, Show)

data Skipped = Skipped
  { skName :: Text
  , skReason :: Text
  }
  deriving (Eq, Show)

data ModuleIr = ModuleIr
  { irModule :: Text
  , irPrefix :: Text
  , irSource :: Text
  , irClasses :: [ClassDecl]
  , irFuns :: [Fun]
  , irConsts :: [ConstDecl]
  , irEnums :: [EnumDecl]
  , irSkipped :: [Skipped]
  }
  deriving (Eq, Show)

emptyModule :: Text -> Text -> ModuleIr
emptyModule name source =
  ModuleIr
    { irModule = name
    , irPrefix = T.empty
    , irSource = source
    , irClasses = []
    , irFuns = []
    , irConsts = []
    , irEnums = []
    , irSkipped = []
    }

-- | @t@ and all its descendants, pre-order. Single traversal shared by
-- the 'Ty' predicates ('tyUsesUnknown', 'tyUsesPromise', and Emit's
-- @tyNamed@ / @isHandle@).
tyAndChildren :: Ty -> [Ty]
tyAndChildren t = t : children t
 where
  children = \case
    TyArray a -> tyAndChildren a
    TyOption a -> tyAndChildren a
    TySet a -> tyAndChildren a
    TyPromise a -> tyAndChildren a
    TyMap k v -> tyAndChildren k <> tyAndChildren v
    TyFun as r -> concatMap tyAndChildren as <> tyAndChildren r
    _ -> []

tyUsesUnknown :: Ty -> Bool
tyUsesUnknown = any isUnknown . tyAndChildren
 where
  isUnknown = \case
    TyUnknown _ -> True
    _ -> False

tyUsesPromise :: Ty -> Bool
tyUsesPromise = any isPromise . tyAndChildren
 where
  isPromise = \case
    TyPromise _ -> True
    _ -> False

walkFuns :: (Fun -> Bool) -> ModuleIr -> Bool
walkFuns p ir =
  any p (irFuns ir)
    || any
      ( \c ->
          any p (clCtors c) || any p (clMethods c)
      )
      (irClasses ir)

moduleUsesUnknown :: ModuleIr -> Bool
moduleUsesUnknown ir =
  walkFuns funUnk ir
    || any (any (tyUsesUnknown . prTy) . clProps) (irClasses ir)
    || any (tyUsesUnknown . cnTy) (irConsts ir)
 where
  funUnk f =
    tyUsesUnknown (fnRet f) || any (tyUsesUnknown . pTy) (fnParams f)

moduleUsesPromise :: ModuleIr -> Bool
moduleUsesPromise ir =
  walkFuns funP ir
    || any (any (tyUsesPromise . prTy) . clProps) (irClasses ir)
    || any (tyUsesPromise . cnTy) (irConsts ir)
 where
  funP f =
    tyUsesPromise (fnRet f) || any (tyUsesPromise . pTy) (fnParams f)
