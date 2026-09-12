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
  , Diagnostic (..)
  , bindgenSchemaVersion
  , ModuleIr (..)
  , emptyModule
  , tyAndChildren
  , tyUsesUnknown
  , tyUsesPromise
  , moduleUsesUnknown
  , moduleUsesPromise
  , tyHasNestedOption
  , validateModule
  )
where

import Data.List (nub)
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

-- | A function parameter: name, erased type, and optionality.
data Param = Param
  { pName :: Text
  , pTy :: Ty
  , pOptional :: Bool
  }
  deriving (Eq, Show)

-- | An extracted function, constructor, or method overload.
data Fun = Fun
  { fnName :: Text
  , fnFfi :: Text
  , fnParams :: [Param]
  , fnRet :: Ty
  , fnIsCtor :: Bool
  , fnStatic :: Bool
  , -- | Position among same-named overloads (stable declaration identity).
    fnOverload :: Int
  }
  deriving (Eq, Show)

-- | A class or interface property.
data Prop = Prop
  { prName :: Text
  , prTy :: Ty
  , prReadonly :: Bool
  }
  deriving (Eq, Show)

-- | An extracted class or interface declaration.
data ClassDecl = ClassDecl
  { clName :: Text
  , clFfi :: Text
  , clCtors :: [Fun]
  , clProps :: [Prop]
  , clMethods :: [Fun]
  }
  deriving (Eq, Show)

-- | A top-level exported constant.
data ConstDecl = ConstDecl
  { cnName :: Text
  , cnFfi :: Text
  , cnTy :: Ty
  }
  deriving (Eq, Show)

-- | A single member of a TypeScript enum.
data EnumMember = EnumMember
  { emName :: Text
  , emValue :: Maybe Text
  , emNumeric :: Bool
  }
  deriving (Eq, Show)

-- | An exported TypeScript enum.
data EnumDecl = EnumDecl
  { enName :: Text
  , enMembers :: [EnumMember]
  }
  deriving (Eq, Show)

-- | An input declaration the extractor could not bind, with a reason.
data Skipped = Skipped
  { skName :: Text
  , skReason :: Text
  }
  deriving (Eq, Show)

-- | A TypeScript or extractor diagnostic surfaced alongside the module.
data Diagnostic = Diagnostic
  { dgCategory :: Text
  , dgMessage :: Text
  }
  deriving (Eq, Show)

-- | JSON schema version emitted by @extract.mjs@ and understood here.
bindgenSchemaVersion :: Int
bindgenSchemaVersion = 1

-- | Everything extracted from one source file.
data ModuleIr = ModuleIr
  { irModule :: Text
  , irPrefix :: Text
  , irSource :: Text
  , irClasses :: [ClassDecl]
  , irFuns :: [Fun]
  , irConsts :: [ConstDecl]
  , irEnums :: [EnumDecl]
  , irSkipped :: [Skipped]
  , irDiagnostics :: [Diagnostic]
  }
  deriving (Eq, Show)

-- | An empty 'ModuleIr' with the given module name and source.
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
    , irDiagnostics = []
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

-- | @True@ when an 'TyOption' appears below the top level (inside an
-- array, map, set, promise, or callback). The emitter adapts a top-level
-- optional argument with 'unsafeOptionToNative'; nested nullables are not
-- yet converted, so they are surfaced as diagnostics rather than silently
-- passed as tagged objects.
tyHasNestedOption :: Ty -> Bool
tyHasNestedOption = go True
 where
  go top = \case
    TyOption a -> not top || go False a
    TyArray a -> go False a
    TySet a -> go False a
    TyPromise a -> go False a
    TyMap k v -> go False k || go False v
    TyFun as r -> any (go False) as || go False r
    _ -> False

-- | Diagnostics for declarations the emitter cannot bind correctly:
-- unknown types, nested nullables, and duplicate non-overload exports.
validateModule :: ModuleIr -> [Diagnostic]
validateModule ir =
  concatMap validateFun (irFuns ir)
    <> concatMap validateClass (irClasses ir)
    <> concatMap (\c -> checkConst c) (irConsts ir)
    <> duplicateDiags
 where
  validateFun f =
    checkUnknown (fnName f) (fnRet f : map pTy (fnParams f))
      <> checkNested (fnName f) (fnRet f : map pTy (fnParams f))
  validateClass c =
    checkUnknown (clName c) (concatMap propAndFun (clProps c) <> concatMap funTys (clCtors c <> clMethods c))
      <> checkNested (clName c) (concatMap (\p -> [prTy p]) (clProps c) <> concatMap funTys (clCtors c <> clMethods c))
  propAndFun p = [prTy p]
  funTys f = fnRet f : map pTy (fnParams f)
  checkConst c = checkUnknown (cnName c) [cnTy c] <> checkNested (cnName c) [cnTy c]
  checkUnknown who tys
    | any tyUsesUnknown tys =
        [Diagnostic "unsupported-type" (who <> ": contains a type bindgen could not map")]
    | otherwise = []
  checkNested who tys
    | any tyHasNestedOption tys =
        [ Diagnostic
            "unsupported-nullable"
            (who <> ": a nullable inside a container or callback is not converted at the boundary")
        ]
    | otherwise = []
  duplicateDiags =
    [ Diagnostic "duplicate-declaration" (n <> ": declared more than once")
    | n <- duplicates (fmap cnName (irConsts ir) <> fmap enName (irEnums ir) <> fmap clName (irClasses ir))
    ]
  duplicates xs = [x | x <- nub xs, length (filter (== x) xs) > 1]

-- | True when the type or any nested type is a 'TyUnknown'.
tyUsesUnknown :: Ty -> Bool
tyUsesUnknown = any isUnknown . tyAndChildren
 where
  isUnknown = \case
    TyUnknown _ -> True
    _ -> False

-- | True when the type or any nested type is a 'TyPromise'.
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

-- | True when any function, property, or constant uses an unknown type.
moduleUsesUnknown :: ModuleIr -> Bool
moduleUsesUnknown ir =
  walkFuns funUnk ir
    || any (any (tyUsesUnknown . prTy) . clProps) (irClasses ir)
    || any (tyUsesUnknown . cnTy) (irConsts ir)
 where
  funUnk f =
    tyUsesUnknown (fnRet f) || any (tyUsesUnknown . pTy) (fnParams f)

-- | True when any function, property, or constant uses a promise.
moduleUsesPromise :: ModuleIr -> Bool
moduleUsesPromise ir =
  walkFuns funP ir
    || any (any (tyUsesPromise . prTy) . clProps) (irClasses ir)
    || any (tyUsesPromise . cnTy) (irConsts ir)
 where
  funP f =
    tyUsesPromise (fnRet f) || any (tyUsesPromise . pTy) (fnParams f)
