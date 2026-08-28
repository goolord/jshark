{-# LANGUAGE OverloadedStrings #-}

module Hvm2Tests (hvm2Tests) where

import qualified Data.Text as T
import JShark (effectfulAST, pureAST, renderJS)
import JShark.Api
import JShark.Api.Types (Hvm2KernelEntry (..))
import JShark.Compiler
  ( applyCompilerArgs
  , configWarnHvm2Candidates
  , readableConfig
  )
import JShark.Compiler.Hvm2Lint
  ( Hvm2Candidate (..)
  , defaultHvm2MinCandidateSize
  , hvm2CandidatesFromExpr
  )
import JShark.Hvm2
import Kernels (hvm2Entries, mandelJsSource, maxIter)
import Test.Tasty
import Test.Tasty.HUnit

hvm2Tests :: TestTree
hvm2Tests =
  testGroup
    "hvm2"
    [ testCase "bendKernel emits def for lambda" $
        case bendKernel "double" (lambda (\x -> x + number 1)) of
          Left bendErr -> assertFailure (show bendErr)
          Right bend -> do
            T.isInfixOf "def double" bend @?= True
            T.isInfixOf "return" bend @?= True
    , testCase "bendModule includes main" $
        case bendModule [Hvm2KernelEntry "inc" (lambda (\x -> x + number 1))] of
          Left bendErr -> assertFailure (show bendErr)
          Right bend -> T.isInfixOf "def main():" bend @?= True
    , testCase "bendDefNames skips main" $
        bendDefNames "def inc(x): return x\ndef main(): return 0\n"
          @?= ["inc"]
    , testCase "emitKernelWasmBridge names WASM exports" $
        let
          bridge = emitKernelWasmBridge maxIter [("mandel", 2)]
         in
          do
            T.isInfixOf "export_name(\"mandel\")" bridge @?= True
            T.isInfixOf "export_name(\"mandel_f64\")" bridge @?= True
            T.isInfixOf "export_name(\"mandel_grid\")" bridge @?= True
            -- true HVM2 execution: net reduction of the Bend-compiled book
            T.isInfixOf "export_name(\"mandel_hvm2_grid\")" bridge @?= True
            T.isInfixOf "jshark_hvm2_def_id(\"jshark_grid\")" bridge @?= True
            T.isInfixOf "export_name(\"jshark_worker_eval\")" bridge @?= True
            T.isInfixOf "export_name(\"jshark_tpc\")" bridge @?= True
            T.isInfixOf "import_name(\"spawn_eval\")" bridge @?= True
            T.isInfixOf "import_name(\"eval_done\")" bridge @?= True
            T.isInfixOf "import_name(\"reset_evals\")" bridge @?= True
            T.isInfixOf "import_name(\"live_threads\")" bridge @?= True
            T.isInfixOf "JSHARK_HVM2_MAX_CELLS" bridge @?= True
            T.isInfixOf "JSHARK_HVM2_TILE" bridge @?= False
            T.isInfixOf "jshark_hvm2_blit_tile" bridge @?= False
            T.isInfixOf "jshark_steal_eval" bridge @?= True
            T.isInfixOf "export_name(\"jshark_cancel_eval\")" bridge @?= True
            T.isInfixOf "node_buf + (u64)ti * part" bridge @?= True
            T.isInfixOf "evaluator(net, tm[0], book)" bridge @?= False
    , testCase "bend demo kernels pass bend check (no or/and)" $
        case bendModule hvm2Entries of
          Left bendErr -> assertFailure (show bendErr)
          Right bend -> do
            T.isInfixOf " or " bend @?= False
            T.isInfixOf " and " bend @?= False
            T.isInfixOf " + " bend @?= True
    , testCase "pureAST emits callable HVM2 export ref" $
        let
          js =
            renderJS
              ( pureAST
                  ( apply
                      (hvm2Kernel "double" (lambda (\x -> x + x)))
                      (number 3)
                  )
              )
         in
          do
            T.isInfixOf "__jsharkHvm2" js @?= True
            T.isInfixOf "Float64Array" js @?= True
            T.isInfixOf "(3.0)" js @?= True
    , testCase "loadHvm2Wasm emits fetch/instantiate loader" $
        let
          js =
            renderJS
              ( effectfulAST
                  (fromSyntax (loadHvm2Wasm (string "static/jshark-hvm2.wasm") *> toSyntax noOp))
              )
         in
          do
            T.isInfixOf "fetch(" js @?= True
            T.isInfixOf "WebAssembly.instantiate" js @?= True
            T.isInfixOf "__jsharkHvm2" js @?= True
            T.isInfixOf "static/jshark-hvm2.wasm" js @?= True
    , testCase "hvm2Candidates finds heavy closed lambda" $
        let
          heavy =
            lambda
              ( \x ->
                  let_ (x + x) (\a -> let_ (a + a) (\b -> let_ (b + b) (\c -> c + c)))
              )
          cands = hvm2CandidatesFromExpr heavy
         in
          do
            not (null cands) @?= True
            all ((>= defaultHvm2MinCandidateSize) . hvm2CandidateSize) cands @?= True
    , testCase "hvm2Candidates skips string kernels" $
        null (hvm2CandidatesFromExpr (lambda (\x -> toString x)))
          @?= True
    , testCase "hvm2 demo kernels emit bend module" $
        case bendModule hvm2Entries of
          Left bendErr -> assertFailure (show bendErr)
          Right bend -> do
            T.isInfixOf "def mandel" bend @?= True
            T.isInfixOf "def main():" bend @?= True
            T.isInfixOf "rec6(0.0, 0.0, 0.0)" bend @?= True
            T.isInfixOf "rec6(0.0)(0.0)(0.0)" bend @?= False
            -- JS numbers are floats; kernels must be f24 end to end.
            T.isInfixOf "def mandel(a2: f24, a4: f24) -> f24:" bend @?= True
    , testCase "bend module main is a parallel bend + fold sweep" $
        case bendModule hvm2Entries of
          Left bendErr -> assertFailure (show bendErr)
          Right bend -> do
            T.isInfixOf "def main():" bend @?= True
            T.isInfixOf "type ParTree:" bend @?= True
            T.isInfixOf "fold t:" bend @?= True
            T.isInfixOf "bend lo = 0, hi = 4096:" bend @?= True
            T.isInfixOf
              "ParTree/Leaf(f24/to_u24(mandel((u24/to_f24(lo % 64) / 32.0) - 2.0, (u24/to_f24(lo / 64) / 32.0) - 1.0)))"
              bend
              @?= True
            -- The whole-frame HVM2 driver for the WASM bridge is emitted…
            T.isInfixOf "def jshark_grid(cRe, cIm, scale, w, h, blk, bxN, byN):" bend
              @?= True
            -- …but bridged by hand, so it is not a scalar export; the tree
            -- defs are local to main, so no stray WASM exports either.
            bendDefNames bend @?= ["mandel"]
    , testCase "mandelJsSource matches maxIter" $
        T.pack (show maxIter) `T.isInfixOf` T.pack mandelJsSource @?= True
    , testCase "mandel bend module uses maxIter" $
        case bendModule hvm2Entries of
          Left bendErr -> assertFailure (show bendErr)
          Right bend ->
            T.pack (show maxIter) `T.isInfixOf` bend @?= True
    , testCase "sanitizeKernelCForWasm sizes one-net buffers" $
        let
          out =
            sanitizeKernelCForWasm $
              "#define G_NODE_LEN (1ul << 29)\n"
                <> "#define G_VARS_LEN (1ul << 29)\n"
                <> "#define RLEN (1ul << 24)\n"
                <> "#define TPC_L2 8\n"
         in
          do
            T.isInfixOf "#define G_NODE_LEN (1ul << 23)" out @?= True
            T.isInfixOf "#define G_VARS_LEN (1ul << 23)" out @?= True
            T.isInfixOf "#define RLEN (1ul << 18)" out @?= True
            T.isInfixOf "#ifndef TPC_L2" out @?= True
    , testCase "applyCompilerArgs enables hvm2 warnings" $
        configWarnHvm2Candidates
          (applyCompilerArgs ["--warn-hvm2-candidates"] readableConfig)
          @?= True
    ]
