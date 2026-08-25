{-# LANGUAGE OverloadedStrings #-}

module Hvm2Tests (hvm2Tests) where

import qualified Data.Text as T
import JShark (effectfulAST, pureAST, renderJS)
import JShark.Api
import JShark.Compiler
  ( applyCompilerArgs
  , configWarnHvm2Candidates
  , readableConfig
  )
import JShark.Hvm2
import JShark.Hvm2Lint
  ( Hvm2Candidate (..)
  , defaultHvm2MinCandidateSize
  , hvm2CandidatesFromExpr
  )
import JShark.Types (Hvm2KernelEntry (..))
import Kernels (hvm2Entries)
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
    , testCase "emitKernelExportsC names WASM exports" $
        let shim = emitKernelExportsC [("double", 1)]
         in T.isInfixOf "export_name(\"double\")" shim @?= True
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
         in do
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
         in do
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
         in do
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
    , testCase "applyCompilerArgs enables hvm2 warnings" $
        configWarnHvm2Candidates (applyCompilerArgs ["--warn-hvm2-candidates"] readableConfig)
          @?= True
    ]
