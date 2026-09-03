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
  , tyUsesUnknown
  , tyUsesPromise
  , moduleUsesUnknown
  , moduleUsesPromise
  , mergeModules
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

tyUsesUnknown :: Ty -> Bool
tyUsesUnknown = \case
  TyUnknown _ -> True
  TyArray t -> tyUsesUnknown t
  TyOption t -> tyUsesUnknown t
  TySet t -> tyUsesUnknown t
  TyPromise t -> tyUsesUnknown t
  TyMap k v -> tyUsesUnknown k || tyUsesUnknown v
  TyFun as r -> any tyUsesUnknown as || tyUsesUnknown r
  _ -> False

tyUsesPromise :: Ty -> Bool
tyUsesPromise = \case
  TyPromise _ -> True
  TyArray t -> tyUsesPromise t
  TyOption t -> tyUsesPromise t
  TySet t -> tyUsesPromise t
  TyMap k v -> tyUsesPromise k || tyUsesPromise v
  TyFun as r -> any tyUsesPromise as || tyUsesPromise r
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

-- | Concatenate declarations; later module/prefix/source win when non-empty.
mergeModules :: ModuleIr -> ModuleIr -> ModuleIr
mergeModules a b =
  ModuleIr
    { irModule = pick (irModule b) (irModule a)
    , irPrefix = pick (irPrefix b) (irPrefix a)
    , irSource = pick (irSource b) (irSource a)
    , irClasses = irClasses a <> irClasses b
    , irFuns = irFuns a <> irFuns b
    , irConsts = irConsts a <> irConsts b
    , irEnums = irEnums a <> irEnums b
    , irSkipped = irSkipped a <> irSkipped b
    }
 where
  pick x y = if T.null x then y else x
