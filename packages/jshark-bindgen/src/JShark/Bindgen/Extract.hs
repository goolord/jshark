{-# LANGUAGE OverloadedStrings #-}

-- | Run the TypeScript compiler API extractor (@extract.mjs@ via bun).
module JShark.Bindgen.Extract
  ( extractWithTs
  , findExtractScript
  , tsExtractorAvailable
  )
where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import Paths_jshark_bindgen (getDataFileName)
import System.Directory
  ( doesFileExist
  , findExecutable
  , getCurrentDirectory
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcessWithExitCode)

-- | Locate @extract.mjs@: env, cabal data file, CWD, or parents.
findExtractScript :: IO (Maybe FilePath)
findExtractScript = do
  env <- lookupEnv "JSHARK_BINDGEN_EXTRACT"
  case env of
    Just p -> do
      ok <- doesFileExist p
      if ok then pure (Just p) else search
    Nothing -> search
 where
  search = do
    installed <-
      try (getDataFileName "extract.mjs") ::
        IO (Either SomeException FilePath)
    case installed of
      Right p -> do
        ok <- doesFileExist p
        if ok then pure (Just p) else walkCwd
      Left _ -> walkCwd

  walkCwd = do
    cwd <- getCurrentDirectory
    firstExist
      ( [ cwd </> "extract.mjs"
        , cwd </> "packages/jshark-bindgen" </> "extract.mjs"
        ]
          <> [ walk </> "packages/jshark-bindgen" </> "extract.mjs"
             | walk <- parents cwd
             ]
      )

  parents p =
    let
      up = takeDirectory p
     in
      if up == p then [] else up : parents up

firstExist :: [FilePath] -> IO (Maybe FilePath)
firstExist [] = pure Nothing
firstExist (p : ps) = do
  ok <- doesFileExist p
  if ok then pure (Just p) else firstExist ps

tsExtractorAvailable :: IO Bool
tsExtractorAvailable = do
  bun <- findExecutable "bun"
  script <- findExtractScript
  pure (maybe False (const True) bun && maybe False (const True) script)

-- | @bun extract.mjs [--module M] [--prefix P] FILE@ → JSON IR on stdout.
extractWithTs ::
  FilePath
  -> Maybe Text
  -> Maybe Text
  -> FilePath
  -> IO (Either String Text)
extractWithTs script moduleName prefix input = do
  bun <- findExecutable "bun"
  case bun of
    Nothing ->
      pure (Left "bun not on PATH (needed for the TypeScript extractor)")
    Just bunPath -> do
      let
        args =
          [script]
            <> maybe [] (\m -> ["--module", T.unpack m]) moduleName
            <> maybe [] (\p -> ["--prefix", T.unpack p]) prefix
            <> [input]
      r <-
        try
          (readProcessWithExitCode bunPath args "") ::
          IO (Either SomeException (ExitCode, String, String))
      case r of
        Left e -> pure (Left (show e))
        Right (ExitSuccess, out, _) ->
          pure (Right (T.pack out))
        Right (ExitFailure c, out, err) ->
          pure
            ( Left
                ( unlines
                    [ "TypeScript extractor exited " <> show c
                    , err
                    , out
                    ]
                )
            )
