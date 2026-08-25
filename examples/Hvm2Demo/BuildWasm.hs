{-# LANGUAGE OverloadedStrings #-}

-- | Build @examples/static/hvm2-demo.wasm@ from 'Kernels.hvm2Entries'.
--
-- Writes @kernel.bend@ (JShark → Bend) and links @shims.c@ (matching native
-- exports) until Bend @gen-c@ output is freestanding-WASM ready.
--
-- Requires @bend@ and @zig@ on PATH:
--
-- @
-- cabal run build-hvm2-demo-wasm
-- @
module Main (main) where

import JShark.Hvm2
  ( Hvm2Config (..)
  , bendDefExports
  , bendModule
  , defaultHvm2Config
  , emitKernelExportsC
  )
import Kernels (hvm2Entries)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.Exit (die, ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcessWithExitCode)
import qualified Data.Text.IO as T

outDir :: FilePath
outDir = "wasm/hvm2/generated-demo"

staticWasm :: FilePath
staticWasm = "examples/static/hvm2-demo.wasm"

shimsC :: FilePath
shimsC = "examples/Hvm2Demo/shims.c"

main :: IO ()
main = do
  case bendModule hvm2Entries of
    Left err -> die ("bend emit: " <> show err)
    Right bendSrc -> do
      createDirectoryIfMissing True outDir
      createDirectoryIfMissing True (takeDirectory staticWasm)
      let
        bendPath = outDir </> "kernel.bend"
        exportsPath = outDir </> "kernel_exports.c"
      T.writeFile bendPath bendSrc
      T.writeFile exportsPath (emitKernelExportsC (bendDefExports bendSrc))
      putStrLn ("wrote " <> bendPath)
      buildWasm exportsPath

buildWasm :: FilePath -> IO ()
buildWasm exportsPath = do
  let
    cfg = defaultHvm2Config
    wasmPath = outDir </> "bin" </> "jshark-hvm2.wasm"
    buildDir = hvm2WasmBuildDir cfg
    buildFile = buildDir </> "build.zig"
  createDirectoryIfMissing True (takeDirectory wasmPath)
  absShims <- makeAbsolute shimsC
  absExports <- makeAbsolute exportsPath
  absBuildFile <- makeAbsolute buildFile
  zigRes <-
    readProcessWithExitCode
      (hvm2ZigExe cfg)
      [ "build"
      , "--build-file"
      , absBuildFile
      , "-Doptimize=ReleaseFast"
      , "-Dkernel-c=" ++ absShims
      , "-Dexports-c=" ++ absExports
      , "--prefix"
      , outDir
      ]
      ""
  case zigRes of
    (ExitSuccess, _, _) -> do
      (code, _, err) <-
        readProcessWithExitCode "cp" [wasmPath, staticWasm] ""
      case code of
        ExitSuccess -> putStrLn ("wrote " <> staticWasm)
        _ -> die ("cp failed: " <> err)
    (ExitFailure code, out, err) ->
      die $
        "zig build failed (exit "
          <> show code
          <> "): "
          <> out
          ++ err
