{-# LANGUAGE OverloadedStrings #-}

module CatalogTests (catalogTests) where

import qualified Data.Text.IO as T
import Life (catalogJs)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

catalogTests :: TestTree
catalogTests =
  testGroup
    "life catalog sidecar"
    [ testCase "catalog.js matches Haskell catalogJs" $ do
        root <- getCurrentDirectory
        onDisk <- T.readFile (root </> "examples/Life/js/catalog.js")
        onDisk @?= catalogJs
    ]
