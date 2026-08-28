{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JavaScript pretty printing via Biome (when on PATH). Replaces the
-- old brace-scanner in 'JShark.Compiler'.
--
-- Readable compiles need @biome@ on PATH (or @bunx @biomejs/biome@2.5.11@ /
-- @bun x @biomejs/biome@2.5.11@). The nix devShell and CI install Biome via
-- bun.
module JShark.Compiler.JsFormat
  ( prettyJS
  , tryPrettyJSIO
  , biomeAvailable
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import JShark.Compiler.Process (executeProcessStdin)
import System.Directory (findExecutable)

biomePackage :: String
biomePackage = "@biomejs/biome@2.5.11"

-- | Pretty-print compact JS with Biome when available; otherwise return the
-- input unchanged (no stderr). Compile paths should use 'finishReadableIO' in
-- 'JShark.Compiler' so fallback notices reach stderr.
prettyJS :: Text -> IO Text
prettyJS src =
  tryPrettyJSIO src >>= \case
    Right out -> pure out
    Left _ -> pure (T.strip src)

-- | Biome format attempt. Returns stripped output or an error message.
tryPrettyJSIO :: Text -> IO (Either String Text)
tryPrettyJSIO src = do
  resolved <- resolveBiome
  case resolved of
    Left err -> pure (Left err)
    Right (exe, wrap) ->
      fmap (fmap T.strip) (runBiome exe wrap (T.strip src))

biomeAvailable :: IO Bool
biomeAvailable = do
  resolved <- resolveBiome
  case resolved of
    Left _ -> pure False
    Right (exe, wrap) ->
      isRight <$> executeProcessStdin exe (wrap ["--version"]) ""
 where
  isRight (Right _) = True
  isRight _ = False

resolveBiome :: IO (Either String (FilePath, [String] -> [String]))
resolveBiome = do
  mDirect <- findExecutable "biome"
  mBunx <- findExecutable "bunx"
  mBun <- findExecutable "bun"
  case (mDirect, mBunx, mBun) of
    (Just exe, _, _) -> pure (Right (exe, id))
    (Nothing, Just bunxExe, _) ->
      pure (Right (bunxExe, (biomePackage :)))
    (Nothing, Nothing, Just bunExe) ->
      pure (Right (bunExe, (["x", biomePackage] ++)))
    _ ->
      pure
        ( Left
            "biome executable not found on PATH (install biome or use bun)"
        )

runBiome ::
  FilePath -> ([String] -> [String]) -> Text -> IO (Either String Text)
runBiome exe wrap source =
  executeProcessStdin
    exe
    ( wrap
        [ "format"
        , "--stdin-file-path=jshark.js"
        , "--indent-style=space"
        , "--indent-width=2"
        ]
    )
    source
