{-# LANGUAGE OverloadedStrings #-}

module CatalogTests (catalogTests) where

import Life (catalogJs)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text.IO as T
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

catalogTests :: TestTree
catalogTests =
  testGroup
    "life catalog sidecar"
    [ testCase "catalog.js matches Haskell catalogJs" $ do
        root <- getCurrentDirectory
        onDisk <- T.readFile (root </> "examples/Life/js/catalog.js")
        onDisk @?= catalogJs
    ]
