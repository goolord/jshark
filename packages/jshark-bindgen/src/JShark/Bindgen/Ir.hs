{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared IR for TypeScript / JavaScript → JShark bindings, and its decoder
-- for the JSON emitted by @extract.mjs@.
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
  , ModuleIr (..)
  , bindgenSchemaVersion
  , decodeModule
  , emptyModule
  , tyAndChildren
  , tyUsesUnknown
  , tyUsesPromise
  , tyHasNestedOption
  , funTypes
  , declTypes
  , moduleUsesUnknown
  , moduleUsesPromise
  , validateModule
  )
where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List (group, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

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
data Param = Param {pName :: Text, pTy :: Ty, pOptional :: Bool}
  deriving (Eq, Show)

-- | An extracted function, constructor, or method overload.
data Fun = Fun
  { fnName :: Text
  , fnFfi :: Text
  , fnParams :: [Param]
  , fnRet :: Ty
  , fnIsCtor :: Bool
  , fnStatic :: Bool
  , fnOverload :: Int
  -- ^ Position among same-named overloads (stable declaration identity).
  }
  deriving (Eq, Show)

-- | A class or interface property.
data Prop = Prop {prName :: Text, prTy :: Ty, prReadonly :: Bool}
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
data ConstDecl = ConstDecl {cnName :: Text, cnFfi :: Text, cnTy :: Ty}
  deriving (Eq, Show)

-- | A single member of a TypeScript enum.
data EnumMember = EnumMember
  {emName :: Text, emValue :: Maybe Text, emNumeric :: Bool}
  deriving (Eq, Show)

-- | An exported TypeScript enum.
data EnumDecl = EnumDecl {enName :: Text, enMembers :: [EnumMember]}
  deriving (Eq, Show)

-- | An input declaration the extractor could not bind, with a reason.
data Skipped = Skipped {skName :: Text, skReason :: Text}
  deriving (Eq, Show)

-- | A TypeScript or extractor diagnostic surfaced alongside the module.
data Diagnostic = Diagnostic {dgCategory :: Text, dgMessage :: Text}
  deriving (Eq, Show)

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

-- | JSON schema version emitted by @extract.mjs@ and understood here.
bindgenSchemaVersion :: Int
bindgenSchemaVersion = 1

-- | Decode the extractor's JSON IR for one module, or fail with a message.
decodeModule :: Text -> Either String ModuleIr
decodeModule = eitherDecodeStrict' . encodeUtf8

instance FromJSON ModuleIr where
  parseJSON = withObject "ModuleIr" $ \o -> do
    v <- o .: "v"
    unless (v == bindgenSchemaVersion) . fail $
      "unsupported extractor JSON schema version "
        <> show v
        <> " (expected "
        <> show bindgenSchemaVersion
        <> "); update jshark-bindgen"
    ModuleIr
      <$> o .: "module"
      <*> o .: "prefix"
      <*> o .: "source"
      <*> o .: "classes"
      <*> o .: "funs"
      <*> o .: "consts"
      <*> o .: "enums"
      <*> o .: "skipped"
      <*> o .:? "diagnostics" .!= []

instance FromJSON ClassDecl where
  parseJSON = withObject "ClassDecl" $ \o ->
    ClassDecl
      <$> o .: "name"
      <*> o .: "ffi"
      <*> o .: "ctors"
      <*> o .: "props"
      <*> o .: "methods"

instance FromJSON Fun where
  parseJSON = withObject "Fun" $ \o ->
    Fun
      <$> o .: "name"
      <*> o .: "ffi"
      <*> o .: "params"
      <*> o .: "ret"
      <*> o .: "ctor"
      <*> o .:? "static" .!= False
      <*> o .:? "overload" .!= 0

instance FromJSON Param where
  parseJSON = withObject "Param" $ \o ->
    Param <$> o .: "name" <*> o .: "ty" <*> o .: "optional"

instance FromJSON Prop where
  parseJSON = withObject "Prop" $ \o ->
    Prop <$> o .: "name" <*> o .: "ty" <*> o .: "readonly"

instance FromJSON ConstDecl where
  parseJSON = withObject "ConstDecl" $ \o ->
    ConstDecl <$> o .: "name" <*> o .: "ffi" <*> o .: "ty"

instance FromJSON EnumDecl where
  parseJSON = withObject "EnumDecl" $ \o ->
    EnumDecl <$> o .: "name" <*> o .: "members"

instance FromJSON EnumMember where
  parseJSON = withObject "EnumMember" $ \o ->
    EnumMember <$> o .: "name" <*> o .:? "value" <*> o .: "numeric"

instance FromJSON Skipped where
  parseJSON = withObject "Skipped" $ \o ->
    Skipped <$> o .: "name" <*> o .: "reason"

instance FromJSON Diagnostic where
  parseJSON = withObject "Diagnostic" $ \o ->
    Diagnostic <$> o .: "category" <*> o .: "message"

instance FromJSON Ty where
  parseJSON = withObject "Ty" $ \o -> do
    -- Nested types parse outside the key's error-path context.
    let
      sub k = (o .: k :: Parser Value) >>= parseJSON
    o .: "k" >>= \case
      "num" -> pure TyNumber
      "bigint" -> pure TyBigInt
      "str" -> pure TyString
      "bool" -> pure TyBool
      "unit" -> pure TyUnit
      "u8" -> pure TyUint8Array
      "arr" -> TyArray <$> sub "el"
      "opt" -> TyOption <$> sub "el"
      "set" -> TySet <$> sub "el"
      "promise" -> TyPromise <$> sub "el"
      "map" -> TyMap <$> sub "key" <*> sub "val"
      "fn" -> TyFun <$> o .: "args" <*> sub "ret"
      "named" -> TyNamed <$> o .: "n"
      "unk" -> TyUnknown <$> o .: "note"
      k -> fail ("unknown type tag: " <> show (k :: Text))

-- | An empty 'ModuleIr' with the given module name and source.
emptyModule :: Text -> Text -> ModuleIr
emptyModule name source = ModuleIr name T.empty source [] [] [] [] [] []

-- | @t@ and all its descendants, pre-order.
tyAndChildren :: Ty -> [Ty]
tyAndChildren t = t : concatMap tyAndChildren (tyChildren t)

tyChildren :: Ty -> [Ty]
tyChildren = \case
  TyArray a -> [a]
  TyOption a -> [a]
  TySet a -> [a]
  TyPromise a -> [a]
  TyMap k v -> [k, v]
  TyFun as r -> as <> [r]
  _ -> []

-- | True when the type or any nested type is a 'TyUnknown'.
tyUsesUnknown :: Ty -> Bool
tyUsesUnknown t = or [True | TyUnknown _ <- tyAndChildren t]

-- | True when the type or any nested type is a 'TyPromise'.
tyUsesPromise :: Ty -> Bool
tyUsesPromise t = or [True | TyPromise _ <- tyAndChildren t]

-- | @True@ when a 'TyOption' appears below the top level (inside an array,
-- map, set, promise, or callback). The emitter adapts a top-level optional
-- argument with 'unsafeOptionToNative'; nested nullables are not yet
-- converted, so they are surfaced as diagnostics rather than silently passed
-- as tagged objects.
tyHasNestedOption :: Ty -> Bool
tyHasNestedOption =
  any (\c -> or [True | TyOption _ <- tyAndChildren c]) . tyChildren

-- | Every type in a function's signature: the result, then the parameters.
funTypes :: Fun -> [Ty]
funTypes f = fnRet f : map pTy (fnParams f)

-- | Each function, class, and constant's name with the types it mentions.
declTypes :: ModuleIr -> [(Text, [Ty])]
declTypes ir =
  [(fnName f, funTypes f) | f <- irFuns ir]
    <> [(clName c, classTypes c) | c <- irClasses ir]
    <> [(cnName c, [cnTy c]) | c <- irConsts ir]
 where
  classTypes c =
    map prTy (clProps c) <> concatMap funTypes (clCtors c <> clMethods c)

-- | True when any function, property, or constant uses an unknown type.
moduleUsesUnknown :: ModuleIr -> Bool
moduleUsesUnknown = any (any tyUsesUnknown . snd) . declTypes

-- | True when any function, property, or constant uses a promise.
moduleUsesPromise :: ModuleIr -> Bool
moduleUsesPromise = any (any tyUsesPromise . snd) . declTypes

-- | Diagnostics for declarations the emitter cannot bind correctly:
-- unknown types, nested nullables, and duplicate non-overload exports.
validateModule :: ModuleIr -> [Diagnostic]
validateModule ir =
  concatMap check (declTypes ir)
    <> [ Diagnostic "duplicate-declaration" (n <> ": declared more than once")
       | n : _ : _ <- group (sort names)
       ]
 where
  names =
    map cnName (irConsts ir) <> map enName (irEnums ir) <> map clName (irClasses ir)
  check (who, tys) =
    [ Diagnostic
        "unsupported-type"
        (who <> ": contains a type bindgen could not map")
    | any tyUsesUnknown tys
    ]
      <> [ Diagnostic
             "unsupported-nullable"
             ( who
                 <> ": a nullable inside a container or callback is not"
                 <> " converted at the boundary"
             )
         | any tyHasNestedOption tys
         ]
