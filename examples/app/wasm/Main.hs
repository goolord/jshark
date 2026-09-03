{-# LANGUAGE OverloadedStrings #-}

-- | Build @examples/static/hvm2-demo.wasm@ from 'Kernels.hvm2Entries'.
--
-- Pipeline: JShark → Bend → @bend gen-c@ → HVM2 C → Zig WASM.
--
-- Requires @bend@ and @zig@ on PATH:
--
-- @
-- cabal run build-hvm2-demo-wasm
-- @
module Main (main) where

import JShark.Example.Hvm2Demo.Kernels (hvm2Entries, maxIter)
import JShark.Hvm2 (bendModule, compileHvm2Wasm, defaultHvm2Config)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.Exit (ExitCode (..), die)
import System.FilePath (takeDirectory)
import System.Process (readProcessWithExitCode)

outDir :: FilePath
outDir = "wasm/hvm2/generated-demo"

staticWasm :: FilePath
staticWasm = "examples/static/hvm2-demo.wasm"

main :: IO ()
main = do
  case bendModule hvm2Entries of
    Left err -> die ("bend emit: " <> show err)
    Right bendSrc -> do
      createDirectoryIfMissing True outDir
      createDirectoryIfMissing True (takeDirectory staticWasm)
      res <- compileHvm2Wasm defaultHvm2Config outDir maxIter bendSrc
      case res of
        Left err -> die ("hvm2 wasm: " <> show err)
        Right wasmPath -> do
          absWasm <- makeAbsolute wasmPath
          absStatic <- makeAbsolute staticWasm
          (code, _, err) <-
            readProcessWithExitCode "cp" [absWasm, absStatic] ""
          case code of
            ExitSuccess -> putStrLn ("wrote " <> staticWasm)
            _ -> die ("cp failed: " <> err)
