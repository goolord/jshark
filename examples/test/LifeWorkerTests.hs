{-# LANGUAGE OverloadedStrings #-}

module LifeWorkerTests (lifeWorkerTests) where

import BunGate (bunGated)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import JShark.Bun (evaluateEffectJSON)
import JShark.Example.Life (engineWorkerJs, lifeLutWorkerBootJs)
import JShark.Example.Life.LifeTestSupport (blinkerLutStepJson)
import Paths_jshark_examples (getDataFileName)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

lifeWorkerTests :: TestTree
lifeWorkerTests =
  bunGated $ \getBun ->
    testGroup
      "life worker bundle"
      [ testCase "EngineWorker.js matches Haskell engineWorkerJs" $ do
          onDisk <-
            TIO.readFile =<< getDataFileName "src/JShark/Example/Life/js/EngineWorker.js"
          onDisk @?= engineWorkerJs
      , testCase "worker LifeLUT.stepRegionLUT matches JShark LUT" $ do
          mBun <- getBun
          case mBun of
            Nothing -> assertFailure "bun not found on PATH"
            Just bun -> do
              want <- evaluateEffectJSON blinkerLutStepJson
              let
                script =
                  T.unpack lifeLutWorkerBootJs
                    <> "const w=8,h=8,n=64,a=new Uint8Array(n);"
                    <> "a[18]=1;a[19]=1;a[20]=1;"
                    <> "const b=new Uint8Array(n);"
                    <> "const LUT=LifeLUT.createLifeLUT();"
                    <> "LifeLUT.stepRegionLUT(LUT,a,b,w,h,0,h);"
                    <> "console.log(JSON.stringify(Array.from(b)));"
                gridJson txt =
                  let
                    s = T.unpack (T.strip txt)
                    body = case s of
                      ('"' : rest)
                        | not (null rest)
                        , last rest == '"' ->
                            take (length rest - 1) rest
                      _ -> s
                   in
                    read body :: [Int]
              (code, out, err) <- readProcessWithExitCode bun ["-e", script] ""
              assertEqual
                ("bun exit " ++ show code ++ " stderr: " ++ err)
                ExitSuccess
                code
              assertEqual
                "worker stepRegionLUT grid"
                (gridJson want)
                (gridJson (T.pack out))
      ]
