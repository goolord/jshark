{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Compile pure 'Expr' kernels to Bend for the HVM2 pipeline.
--
-- Pipeline: JShark 'ClosedExpr' → Bend (.bend) → HVM2 ('bend gen-c') → C →
-- WASM (Zig @wasm/hvm2/build.zig@, same role as Life's Zig wasm build).
-- HVM2 is the interaction-combinator runtime; Bend is its frontend.
module JShark.Hvm2
  ( Hvm2Config (..)
  , Hvm2Error (..)
  , bendKernel
  , bendModule
  , bendModuleFromTree
  , compileHvm2GenC
  , compileHvm2Wasm
  , defaultHvm2Config
  , defaultWasmBuildDir
  , bendDefNames
  , bendDefExports
  , emitKernelExportsC
  , emitKernelWasmBridge
  , sanitizeKernelCForWasm
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import JShark (collectHvm2Kernels, irExprFromClosed)
import JShark.Api.Types
  ( ClosedExpr
  , Expr
  , Hvm2KernelEntry (..)
  )
import JShark.Compiler.EmitBend
  ( Hvm2Error (..)
  , bendDefExports
  , bendDefNames
  , emitBendKernel
  , emitBendModuleFromDefs
  , emitKernelExportsC
  , emitKernelWasmBridge
  , sanitizeKernelCForWasm
  )
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcessWithExitCode)

-- | Tool paths for the HVM2 build pipeline.
data Hvm2Config = Hvm2Config
  { hvm2BendExe :: !FilePath
  , hvm2ZigExe :: !FilePath
  , hvm2WasmBuildDir :: !FilePath
  }
  deriving (Eq, Show)

defaultWasmBuildDir :: FilePath
defaultWasmBuildDir = "wasm/hvm2"

defaultHvm2Config :: Hvm2Config
defaultHvm2Config =
  Hvm2Config
    { hvm2BendExe = "bend"
    , hvm2ZigExe = "zig"
    , hvm2WasmBuildDir = defaultWasmBuildDir
    }

bendKernel :: Text -> ClosedExpr u -> Either Hvm2Error Text
bendKernel name closed = emitBendKernel name (irExprFromClosed closed)

bendModule :: [Hvm2KernelEntry] -> Either Hvm2Error Text
bendModule entries = do
  defs <- traverse emitEntry entries
  emitBendModuleFromDefs defs
 where
  emitEntry (Hvm2KernelEntry name k) =
    emitBendKernel name (irExprFromClosed k)

bendModuleFromTree :: Expr f u -> Either Hvm2Error Text
bendModuleFromTree e = bendModule (collectHvm2Kernels e)

-- | Bend → HVM2 C. Writes @kernel.bend@, @kernel.c@, and @kernel_exports.c@
-- under @outDir@.
compileHvm2GenC ::
  Hvm2Config
  -> FilePath
  -> Int
  -> Text
  -> IO (Either Hvm2Error FilePath)
compileHvm2GenC cfg outDir maxIter bendSrc = do
  createDirectoryIfMissing True outDir
  let
    bendPath = outDir </> "kernel.bend"
    cPath = outDir </> "kernel.c"
    exportsPath = outDir </> "kernel_exports.c"
  T.writeFile bendPath bendSrc
  T.writeFile exportsPath (emitKernelExportsC (bendDefExports bendSrc))
  -- All bend optimization passes that preserve callable-by-name defs.
  -- @prune@ is deliberately absent: it deletes defs unreachable from main,
  -- and the WASM bridge invokes @jshark_grid@ by name at runtime.
  let
    bendOpts =
      concatMap
        (\o -> ["-O", o])
        ["eta", "merge", "inline", "linearize-matches", "float-combinators"]
  genRes <-
    readProcessWithExitCode
      (hvm2BendExe cfg)
      (["gen-c"] <> bendOpts <> [bendPath])
      ""
  case genRes of
    (ExitSuccess, out, _) -> do
      let
        exports = bendDefExports bendSrc
        kernelC =
          sanitizeKernelCForWasm (T.pack out)
            <> "\n"
            <> emitKernelWasmBridge maxIter exports
      T.writeFile cPath kernelC
      pure (Right cPath)
    (ExitFailure code, out, err) ->
      pure $
        Left
          ( Hvm2Unsupported
              ( "bend gen-c failed (exit "
                  <> T.pack (show code)
                  <> "): "
                  <> T.pack (out ++ err)
              )
          )

-- | Full pipeline: Bend → C → WASM via Zig.
compileHvm2Wasm ::
  Hvm2Config
  -> FilePath
  -> Int
  -> Text
  -> IO (Either Hvm2Error FilePath)
compileHvm2Wasm cfg outDir maxIter bendSrc = do
  cRes <- compileHvm2GenC cfg outDir maxIter bendSrc
  case cRes of
    Left err -> pure (Left err)
    Right cPath -> do
      let
        wasmPath = outDir </> "bin" </> "jshark-hvm2.wasm"
        buildDir = hvm2WasmBuildDir cfg
        buildFile = buildDir </> "build.zig"
        exportsPath = outDir </> "kernel_exports.c"
      createDirectoryIfMissing True (takeDirectory wasmPath)
      absCPath <- makeAbsolute cPath
      absExportsPath <- makeAbsolute exportsPath
      absBuildFile <- makeAbsolute buildFile
      zigRes <-
        readProcessWithExitCode
          (hvm2ZigExe cfg)
          [ "build"
          , "--build-file"
          , absBuildFile
          , "-Doptimize=ReleaseFast"
          , "-Dtpc-l2=0"
          , "-Dkernel-c=" ++ absCPath
          , "-Dexports-c=" ++ absExportsPath
          , "--prefix"
          , outDir
          ]
          ""
      case zigRes of
        (ExitSuccess, _, _) -> pure (Right wasmPath)
        (ExitFailure code, out, err) ->
          pure $
            Left
              ( Hvm2Unsupported
                  ( "zig build failed (exit "
                      <> T.pack (show code)
                      <> "): "
                      <> T.pack (out ++ err)
                  )
              )
