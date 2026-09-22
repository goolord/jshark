{-# LANGUAGE LambdaCase #-}
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
  , findExtractScript
  , tsExtractorAvailable
  , extractWithTs
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Data.Char (isAlpha, toUpper)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe, isJust, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Bindgen.Emit (emitModule)
import JShark.Bindgen.Ir
import Paths_jshark_bindgen (getDataFileName)
import System.Directory (doesFileExist, findExecutable, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (splitDirectories, takeBaseName, (</>))
import System.Process (readProcessWithExitCode)

-- | Options controlling the generated module name and FFI prefix.
data BindgenOpts = BindgenOpts
  { optModuleName :: Maybe Text
  , optPrefix :: Maybe Text
  }

-- | 'BindgenOpts' with no module-name or prefix override.
defaultBindgenOpts :: BindgenOpts
defaultBindgenOpts = BindgenOpts Nothing Nothing

-- | Extract the file at the given path and render a Haskell module.
generateFromFile :: BindgenOpts -> FilePath -> IO (Either String Text)
generateFromFile opts path = fmap generateFromIr <$> parseIrFromFile opts path

-- | Extract and decode the input file to a 'ModuleIr', applying 'BindgenOpts'.
--
-- Declarations whose signature nests a nullable inside a container or
-- callback are dropped: the emitter does not convert those at the foreign
-- boundary, so they would pass tagged objects to native code. The matching
-- 'Diagnostic' is still reported.
parseIrFromFile :: BindgenOpts -> FilePath -> IO (Either String ModuleIr)
parseIrFromFile opts path =
  findExtractScript >>= \case
    Nothing -> pure (Left missing)
    Just s -> do
      json <- extractWithTs s (optModuleName opts) (optPrefix opts) path
      pure $ do
        ir <- applyOpts opts path <$> (decodeModule =<< json)
        pure (prune ir {irDiagnostics = irDiagnostics ir <> validateModule ir})
 where
  missing =
    "bun and jshark-bindgen/extract.mjs are required (with typescript installed)"
  prune ir =
    ir
      { irFuns = keep funTypes (irFuns ir)
      , irClasses = map pruneClass (irClasses ir)
      , irConsts = keep (pure . cnTy) (irConsts ir)
      }
  pruneClass c =
    c
      { clCtors = keep funTypes (clCtors c)
      , clMethods = keep funTypes (clMethods c)
      , clProps = keep (pure . prTy) (clProps c)
      }
  keep :: (a -> [Ty]) -> [a] -> [a]
  keep tys = filter (not . any tyHasNestedOption . tys)

-- | Render a 'ModuleIr' to Haskell source.
generateFromIr :: ModuleIr -> Text
generateFromIr = emitModule

-- | Override an IR's module name, prefix, and source from 'BindgenOpts', and
-- qualify its foreign names with the prefix.
applyOpts :: BindgenOpts -> FilePath -> ModuleIr -> ModuleIr
applyOpts opts path ir =
  ir
    { irModule = if T.null m then moduleFromPath path else m
    , irPrefix = p
    , irSource = if T.null (irSource ir) then T.pack path else irSource ir
    , irFuns = map qualFun (irFuns ir)
    , irConsts = [c {cnFfi = qualify (cnFfi c)} | c <- irConsts ir]
    , irClasses =
        [ c {clFfi = qualify (clFfi c), clCtors = map qualFun (clCtors c)}
        | c <- irClasses ir
        ]
    }
 where
  m = fromMaybe (irModule ir) (optModuleName opts)
  p = fromMaybe (irPrefix ir) (optPrefix opts)
  qualFun f = f {fnFfi = qualify (fnFfi f)}
  -- Leave names that already are, or sit under, the prefix.
  qualify n
    | T.null p || n == p || (p <> ".") `T.isPrefixOf` n = n
    | otherwise = p <> "." <> n

moduleFromPath :: FilePath -> Text
moduleFromPath path = "JShark." <> T.pack (titled base)
 where
  raw = takeBaseName path
  base = if ".d" `isSuffixOf` raw then takeBaseName raw else raw
  titled = \case
    [] -> "Bindings"
    c : cs | isAlpha c -> toUpper c : cs
    cs -> 'B' : cs

-- | Locate @extract.mjs@: @$JSHARK_BINDGEN_EXTRACT@, the cabal data file,
-- then the working directory or (under @packages/jshark-bindgen@) a parent.
findExtractScript :: IO (Maybe FilePath)
findExtractScript = do
  env <- lookupEnv "JSHARK_BINDGEN_EXTRACT"
  installed <-
    try (getDataFileName "extract.mjs") :: IO (Either SomeException FilePath)
  cwd <- getCurrentDirectory
  let
    -- The working directory, then each of its ancestors up to the root.
    dirs = reverse (scanl1 (</>) (splitDirectories cwd))
  fmap listToMaybe . filterM doesFileExist $
    maybeToList env
      <> either (const []) pure installed
      <> [cwd </> "extract.mjs"]
      <> [d </> "packages/jshark-bindgen" </> "extract.mjs" | d <- dirs]

-- | True when both @bun@ and @extract.mjs@ are available.
tsExtractorAvailable :: IO Bool
tsExtractorAvailable =
  (&&) <$> (isJust <$> findExecutable "bun") <*> (isJust <$> findExtractScript)

-- | @bun extract.mjs [--module M] [--prefix P] FILE@ → JSON IR on stdout.
extractWithTs ::
  FilePath -> Maybe Text -> Maybe Text -> FilePath -> IO (Either String Text)
extractWithTs script moduleName prefix input =
  findExecutable "bun" >>= \case
    Nothing -> pure (Left "bun not on PATH (needed for the TypeScript extractor)")
    Just bun -> do
      r <- try (readProcessWithExitCode bun args "")
      pure $ case r of
        Left e -> Left (show (e :: SomeException))
        Right (ExitSuccess, out, _) -> Right (T.pack out)
        Right (ExitFailure c, out, err) ->
          Left (unlines ["TypeScript extractor exited " <> show c, err, out])
 where
  args = [script] <> flag "--module" moduleName <> flag "--prefix" prefix <> [input]
  flag name = maybe [] (\v -> [name, T.unpack v])
