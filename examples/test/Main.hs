{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BunTests (bunEvalTests)
import CatalogTests (catalogTests)
import ExampleTests (exampleTests, watchMappingTests)
import LifeTests (lifeTests)
import LifeWorkerTests (lifeWorkerTests)
import PerfTests (perfTests)
import StaticCssTests (staticCssTests)
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "jshark-examples"
    [ bunEvalTests
    , lifeTests
    , catalogTests
    , lifeWorkerTests
    , staticCssTests
    , watchMappingTests
    , exampleTests
    , perfTests
    ]
