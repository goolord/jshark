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

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import JShark.Compiler.Process (executeProcessStdin)
import System.Directory (findExecutable)

biomePackage :: String
biomePackage = "@biomejs/biome@2.5.11"

-- | Pretty-print compact JS with Biome when available; otherwise return the
-- input unchanged (no stderr).
prettyJS :: ByteString -> IO ByteString
prettyJS src =
  tryPrettyJSIO src >>= \case
    Right out -> pure out
    Left _ -> pure (BC.strip src)

-- | Biome format attempt. Returns stripped output or an error message.
tryPrettyJSIO :: ByteString -> IO (Either String ByteString)
tryPrettyJSIO src = do
  resolved <- resolveBiome
  case resolved of
    Left err -> pure (Left err)
    Right (exe, wrap) ->
      fmap (fmap BC.strip) (runBiome exe wrap (BC.strip src))

biomeAvailable :: IO Bool
biomeAvailable = do
  resolved <- resolveBiome
  case resolved of
    Left _ -> pure False
    Right (exe, wrap) ->
      isRight <$> executeProcessStdin exe (wrap ["--version"]) BS.empty
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
  FilePath -> ([String] -> [String]) -> ByteString -> IO (Either String ByteString)
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
