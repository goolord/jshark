{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Decoder for the @extract.mjs@ JSON IR emitted by the TypeScript
-- extractor, using aeson. The schema is this package's own (aeson instances
-- for 'JShark.Bindgen.Ir'), independent of any other package's JSON.
module JShark.Bindgen.Json
  ( decodeModule
  )
where

import Data.Aeson
  ( FromJSON (..)
  , eitherDecodeStrict'
  , withObject
  , (.!=)
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import JShark.Bindgen.Ir

-- | Decode the extractor's JSON IR for one module, or fail with a message.
decodeModule :: Text -> Either String ModuleIr
decodeModule = eitherDecodeStrict' . encodeUtf8

instance FromJSON ModuleIr where
  parseJSON = withObject "ModuleIr" $ \o -> do
    v <- o .: "v" :: Parser Int
    if v == bindgenSchemaVersion
      then
        ModuleIr
          <$> o .: "module"
          <*> o .: "prefix"
          <*> o .: "source"
          <*> o .: "classes"
          <*> o .: "funs"
          <*> o .: "consts"
          <*> o .: "enums"
          <*> o .: "skipped"
          <*> (o .:? "diagnostics" .!= [])
      else
        fail
          ( "unsupported extractor JSON schema version "
              <> show v
              <> " (expected "
              <> show bindgenSchemaVersion
              <> "); update jshark-bindgen"
          )

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
      <*> (fromMaybe False <$> o .:? "static")
      <*> (fromMaybe 0 <$> o .:? "overload")

instance FromJSON Param where
  parseJSON = withObject "Param" $ \o ->
    Param
      <$> o .: "name"
      <*> o .: "ty"
      <*> o .: "optional"

instance FromJSON Prop where
  parseJSON = withObject "Prop" $ \o ->
    Prop
      <$> o .: "name"
      <*> o .: "ty"
      <*> o .: "readonly"

instance FromJSON ConstDecl where
  parseJSON = withObject "ConstDecl" $ \o ->
    ConstDecl
      <$> o .: "name"
      <*> o .: "ffi"
      <*> o .: "ty"

instance FromJSON EnumDecl where
  parseJSON = withObject "EnumDecl" $ \o ->
    EnumDecl
      <$> o .: "name"
      <*> o .: "members"

instance FromJSON EnumMember where
  parseJSON = withObject "EnumMember" $ \o ->
    EnumMember
      <$> o .: "name"
      <*> (o .:? "value" .!= Nothing)
      <*> o .: "numeric"

instance FromJSON Skipped where
  parseJSON = withObject "Skipped" $ \o ->
    Skipped <$> o .: "name" <*> o .: "reason"

instance FromJSON Diagnostic where
  parseJSON = withObject "Diagnostic" $ \o ->
    Diagnostic <$> o .: "category" <*> o .: "message"

instance FromJSON Ty where
  parseJSON = withObject "Ty" $ \o -> do
    k <- o .: "k" :: Parser Text
    case k of
      "num" -> pure TyNumber
      "bigint" -> pure TyBigInt
      "str" -> pure TyString
      "bool" -> pure TyBool
      "unit" -> pure TyUnit
      "u8" -> pure TyUint8Array
      "arr" -> TyArray <$> (o .: "el" >>= parseJSON)
      "opt" -> TyOption <$> (o .: "el" >>= parseJSON)
      "set" -> TySet <$> (o .: "el" >>= parseJSON)
      "promise" -> TyPromise <$> (o .: "el" >>= parseJSON)
      "map" ->
        TyMap
          <$> (o .: "key" >>= parseJSON)
          <*> (o .: "val" >>= parseJSON)
      "fn" ->
        TyFun
          <$> o .: "args"
          <*> (o .: "ret" >>= parseJSON)
      "named" -> TyNamed <$> o .: "n"
      "unk" -> TyUnknown <$> o .: "note"
      _ -> fail ("unknown type tag: " <> show k)
