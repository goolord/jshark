{-# LANGUAGE OverloadedStrings #-}

-- | TypeScript / JavaScript → JShark FFI bindings.
--
-- The user-facing entry is the @jshark-bindgen@ executable:
--
-- @
-- cabal run jshark-bindgen -- widget.d.ts --module JShark.Widget
-- @
module JShark.Bindgen
  ( BindgenOpts (..)
  , defaultBindgenOpts
  , generateFromFile
  , generateFromSource
  , generateFromIr
  , generateFromJson
  , parseIrFromFile
  , parseSource
  , applyOpts
  )
where

import Data.Char (isAlpha, toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO (readFile)
import JShark.Bindgen.Emit (emitModule)
import JShark.Bindgen.Extract
  ( extractWithTs
  , findExtractScript
  )
import JShark.Bindgen.Ir
import JShark.Bindgen.Json (decodeModule)
import JShark.Bindgen.ParseDts (parseDts)
import JShark.Bindgen.ParseJs (parseJs)
import System.FilePath (takeBaseName, takeExtension)

data BindgenOpts = BindgenOpts
  { optModuleName :: Maybe Text
  , optPrefix :: Maybe Text
  , optNoTs :: Bool
  }

defaultBindgenOpts :: BindgenOpts
defaultBindgenOpts =
  BindgenOpts
    { optModuleName = Nothing
    , optPrefix = Nothing
    , optNoTs = False
    }

generateFromFile :: BindgenOpts -> FilePath -> IO (Either String Text)
generateFromFile opts path =
  fmap (fmap generateFromIr) (parseIrFromFile opts path)

parseIrFromFile :: BindgenOpts -> FilePath -> IO (Either String ModuleIr)
parseIrFromFile opts path = do
  src <- TIO.readFile path
  parseIrFromFileSrc opts path src

parseIrFromFileSrc ::
  BindgenOpts -> FilePath -> Text -> IO (Either String ModuleIr)
parseIrFromFileSrc opts path src
  | optNoTs opts = pure (parseSource opts path src)
  | otherwise = do
      script <- findExtractScript
      case script of
        Nothing -> pure (parseSource opts path src)
        Just s -> do
          ts <-
            extractWithTs
              s
              (optModuleName opts)
              (optPrefix opts)
              path
          case ts of
            Right json -> do
              pure $ do
                ir <- decodeModule json
                Right (applyOpts opts path ir)
            Left err -> pure (Left err)

generateFromSource ::
  BindgenOpts -> FilePath -> Text -> Either String Text
generateFromSource opts path src = do
  ir <- parseSource opts path src
  Right (generateFromIr ir)

parseSource :: BindgenOpts -> FilePath -> Text -> Either String ModuleIr
parseSource opts path src =
  fmap (applyOpts opts path) (parseByExt path src)

parseByExt :: FilePath -> Text -> Either String ModuleIr
parseByExt path src =
  let
    name = moduleFromPath path
    srcName = T.pack path
   in
    case takeExtension path of
      ".js" -> parseJs name srcName src
      ".mjs" -> parseJs name srcName src
      ".cjs" -> parseJs name srcName src
      _ -> parseDts name srcName src

generateFromIr :: ModuleIr -> Text
generateFromIr = emitModule

generateFromJson :: BindgenOpts -> FilePath -> Text -> Either String Text
generateFromJson opts path json = do
  ir <- decodeModule json
  Right (generateFromIr (applyOpts opts path ir))

applyOpts :: BindgenOpts -> FilePath -> ModuleIr -> ModuleIr
applyOpts opts path ir =
  let
    m = maybe (irModule ir) id (optModuleName opts)
    m' = if T.null m then moduleFromPath path else m
    p = maybe (irPrefix ir) id (optPrefix opts)
   in
    qualifyPrefix $
      ir
        { irModule = m'
        , irPrefix = p
        , irSource =
            if T.null (irSource ir) then T.pack path else irSource ir
        }

qualifyPrefix :: ModuleIr -> ModuleIr
qualifyPrefix ir
  | T.null (irPrefix ir) = ir
  | otherwise =
      ir
        { irFuns = fmap (qualFun (irPrefix ir)) (irFuns ir)
        , irConsts = fmap (qualConst (irPrefix ir)) (irConsts ir)
        , irClasses = fmap (qualClass (irPrefix ir)) (irClasses ir)
        }

qualFun :: Text -> Fun -> Fun
qualFun p f
  | fnFfi f == p = f
  | already (fnFfi f) = f
  | otherwise = f {fnFfi = p <> "." <> fnFfi f}
 where
  already n = (p <> ".") `T.isPrefixOf` n

qualConst :: Text -> ConstDecl -> ConstDecl
qualConst p c
  | cnFfi c == p = c
  | (p <> ".") `T.isPrefixOf` cnFfi c = c
  | otherwise = c {cnFfi = p <> "." <> cnFfi c}

qualClass :: Text -> ClassDecl -> ClassDecl
qualClass p c
  | clFfi c == p || (p <> ".") `T.isPrefixOf` clFfi c =
      c {clCtors = fmap (qualFun p) (clCtors c)}
  | otherwise =
      c
        { clFfi = p <> "." <> clFfi c
        , clCtors = fmap (qualFun p) (clCtors c)
        }

moduleFromPath :: FilePath -> Text
moduleFromPath path =
  let
    raw = takeBaseName path
    base
      | ".d" `isSuffixOf` raw = takeBaseName raw
      | otherwise = raw
    titled = case base of
      [] -> "Bindings"
      c : cs | isAlpha c -> toUpper c : cs
      _ -> 'B' : base
   in
    "JShark." <> T.pack titled
 where
  isSuffixOf s t = s `T.isSuffixOf` T.pack t
