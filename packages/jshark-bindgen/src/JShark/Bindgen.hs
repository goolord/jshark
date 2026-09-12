{-# LANGUAGE OverloadedStrings #-}

-- | TypeScript / JavaScript → JShark FFI bindings.
--
-- The user-facing entry is the @jshark-bindgen@ executable:
--
-- @
-- cabal run jshark-bindgen -- widget.d.ts --module JShark.Widget
-- @
--
-- Extraction runs the bundled @extract.mjs@ through @bun@ (with
-- @typescript@ installed), which handles the full TS grammar; the Haskell
-- side decodes its JSON IR and emits bindings.
module JShark.Bindgen
  ( BindgenOpts (..)
  , defaultBindgenOpts
  , generateFromFile
  , parseIrFromFile
  , generateFromIr
  , applyOpts
  )
where

import Data.Char (isAlpha, toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Bindgen.Emit (emitModule)
import JShark.Bindgen.Extract
  ( extractWithTs
  , findExtractScript
  )
import JShark.Bindgen.Ir
import JShark.Bindgen.Json (decodeModule)
import System.FilePath (takeBaseName)

-- | Options controlling the generated module name and FFI prefix.
data BindgenOpts = BindgenOpts
  { optModuleName :: Maybe Text
  , optPrefix :: Maybe Text
  }

-- | 'BindgenOpts' with no module-name or prefix override.
defaultBindgenOpts :: BindgenOpts
defaultBindgenOpts =
  BindgenOpts
    { optModuleName = Nothing
    , optPrefix = Nothing
    }

-- | Extract the file at the given path and render a Haskell module.
generateFromFile :: BindgenOpts -> FilePath -> IO (Either String Text)
generateFromFile opts path =
  fmap (fmap generateFromIr) (parseIrFromFile opts path)

-- | Extract and decode the input file to a 'ModuleIr', applying 'BindgenOpts'.
parseIrFromFile :: BindgenOpts -> FilePath -> IO (Either String ModuleIr)
parseIrFromFile opts path = do
  script <- findExtractScript
  case script of
    Nothing ->
      pure
        ( Left
            "bun and jshark-bindgen/extract.mjs are required (with typescript installed)"
        )
    Just s -> do
      ts <- extractWithTs s (optModuleName opts) (optPrefix opts) path
      case ts of
        Left err -> pure (Left err)
        Right json -> pure $ do
          ir <- decodeModule json
          let
            applied = applyOpts opts path ir
          Right
            applied
              { irDiagnostics =
                  irDiagnostics applied <> validateModule applied
              }

-- | Render a 'ModuleIr' to Haskell source.
generateFromIr :: ModuleIr -> Text
generateFromIr = emitModule

-- | Override an IR's module name, prefix, and source from 'BindgenOpts'.
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
