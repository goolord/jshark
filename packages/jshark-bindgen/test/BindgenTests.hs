{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BindgenTests (bindgenTests) where

import BindgenToy
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import JShark.Api
import JShark.Bindgen
import JShark.Bindgen.Cli
  ( Cli (..)
  , parseCliArgs
  )
import JShark.Bindgen.Extract (tsExtractorAvailable)
import JShark.Compiler (compileEffect, readableConfig)
import Paths_jshark_bindgen (getDataFileName)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- | Relative package path used by the pure CLI-parse cases (equality only).
fixture :: FilePath -> FilePath
fixture name = "test/fixtures/jshark-bindgen/" <> name

-- | Fixture path from the package's data files, so it resolves both in the
-- checkout and from an unpacked source distribution.
fixtureAbs :: FilePath -> IO FilePath
fixtureAbs name =
  getDataFileName ("test" </> "fixtures" </> "jshark-bindgen" </> name)

-- | A file shipped as package data.
pkgFileAbs :: FilePath -> IO FilePath
pkgFileAbs rel = getDataFileName rel

-- | The generated header records the resolved absolute fixture path; rewrite
-- it to the checkout-relative form the golden uses. 'getDataFileName' can
-- insert an extra @.@ segment, so collapse it first.
relativizeSource :: FilePath -> FilePath -> Text -> Text
relativizeSource fx name =
  T.replace normalizedFx (T.pack ("test/fixtures/jshark-bindgen/" <> name))
 where
  normalizedFx =
    T.replace "\\.\\" "\\" (T.replace "/./" "/" (T.pack fx))

mustGenWith :: BindgenOpts -> FilePath -> IO Text
mustGenWith opts name = do
  fx <- fixtureAbs name
  r <- generateFromFile opts fx
  either fail (pure . relativizeSource fx name) r

mustGen :: FilePath -> IO Text
mustGen = mustGenWith defaultBindgenOpts

bindgenTests :: TestTree
bindgenTests =
  testGroup
    "bindgen"
    [ testCase "bun and jshark-bindgen/extract.mjs are available" $ do
        ready <- tsExtractorAvailable
        unless ready $
          fail "bun and jshark-bindgen/extract.mjs required (with typescript installed)"
    , testCase "toy.d.ts emits greet / Widget / util.clamp" $ do
        hs <- mustGen "toy.d.ts"
        assertBool "module" ("module JShark.Toy" `T.isInfixOf` hs)
        assertBool "greet ffi" ("ffi \"toy.greet\"" `T.isInfixOf` hs)
        assertBool "add" ("ffi \"toy.add\"" `T.isInfixOf` hs)
        assertBool "log_" ("log_" `T.isInfixOf` hs)
        assertBool "data Widget" ("data Widget" `T.isInfixOf` hs)
        assertBool "new Widget" ("ffi \"new toy.Widget\"" `T.isInfixOf` hs)
        assertBool "newWidget hold" ("newWidget" `T.isInfixOf` hs)
        assertBool "resize" ("callMethod self \"resize\"" `T.isInfixOf` hs)
        assertBool "Field width" ("Field Widget \"width\"" `T.isInfixOf` hs)
        assertBool "util.clamp" ("ffi \"toy.util.clamp\"" `T.isInfixOf` hs)
        assertBool "VERSION" ("ffiExpr \"toy.VERSION\"" `T.isInfixOf` hs)
        assertBool "version name" ("version ::" `T.isInfixOf` hs)
    , testCase "plain.d.ts --prefix acme qualifies globals" $ do
        hs <-
          mustGenWith
            defaultBindgenOpts {optPrefix = Just "acme"}
            "plain.d.ts"
        assertBool
          "prefixed greet"
          ("ffi \"acme.greet\"" `T.isInfixOf` hs)
        assertBool
          "prefixed add"
          ("ffi \"acme.add\"" `T.isInfixOf` hs)
        assertBool
          "no double prefix"
          (not ("acme.acme.greet" `T.isInfixOf` hs))
    , testCase "ms.d.ts (real lib shape) emits ms / ms2" $ do
        hs <- mustGen "ms.d.ts"
        assertBool "module" ("module JShark.Ms" `T.isInfixOf` hs)
        assertBool "ms ffi" ("ffi \"ms\"" `T.isInfixOf` hs)
        assertBool "overload" $
          "ms2 ::" `T.isInfixOf` hs || "ms ::" `T.isInfixOf` hs
        assertBool "string or number" $
          "Expr f ('String)" `T.isInfixOf` hs
            && "Expr f ('Number)" `T.isInfixOf` hs
    , testCase "toy.js JSDoc emits greet / add / log" $ do
        hs <- mustGen "toy.js"
        assertBool "greet" ("ffi \"greet\"" `T.isInfixOf` hs)
        assertBool "add" ("ffi \"add\"" `T.isInfixOf` hs)
        assertBool "log_" ("log_" `T.isInfixOf` hs)
        assertBool "JSDoc string" ("Expr f ('String)" `T.isInfixOf` hs)
        assertBool "JSDoc number" ("Expr f ('Number)" `T.isInfixOf` hs)
    , testCase "CLI-shaped opts set module name" $ do
        hs <-
          mustGenWith
            defaultBindgenOpts {optModuleName = Just "JShark.Demo.Toy"}
            "toy.d.ts"
        assertBool
          "module name"
          ("module JShark.Demo.Toy" `T.isInfixOf` hs)
    , testCase "generated BindgenToy.hs is a golden of toy.d.ts" $ do
        hs <-
          mustGenWith
            defaultBindgenOpts {optModuleName = Just "BindgenToy"}
            "toy.d.ts"
        golden <- TIO.readFile =<< pkgFileAbs ("test" </> "BindgenToy.hs")
        assertEqual
          "golden"
          (T.strip golden)
          (T.strip hs)
    , testCase "CLI parses module / prefix flags" $ do
        case parseCliArgs
          [ "-m"
          , "JShark.Demo.Toy"
          , "-p"
          , "acme"
          , fixture "toy.d.ts"
          ] of
          Left e -> fail e
          Right cli -> do
            assertEqual "file" (fixture "toy.d.ts") (cliFile cli)
            assertEqual
              "module"
              (Just "JShark.Demo.Toy")
              (optModuleName (cliOpts cli))
            assertEqual
              "prefix"
              (Just "acme")
              (optPrefix (cliOpts cli))
    , testCase "CLI rejects unknown flag with usage" $ do
        case parseCliArgs ["--nope", fixture "toy.d.ts"] of
          Left msg ->
            assertBool "usage" ("Usage:" `T.isInfixOf` T.pack msg)
          Right _ -> fail "expected unknown-flag parse error"
    , testCase "CLI rejects extra positional argument" $ do
        case parseCliArgs
          [ fixture "toy.d.ts"
          , "extra.d.ts"
          ] of
          Left msg ->
            assertBool
              "invalid arg"
              ( "Invalid argument" `T.isInfixOf` T.pack msg
                  || "extra.d.ts" `T.isInfixOf` T.pack msg
              )
          Right _ -> fail "expected extra-arg parse error"
    , testCase "generated wrappers compileEffect to JS" $ do
        js <- compileEffect readableConfig toyDemo
        assertBool "greet" ("toy.greet" `T.isInfixOf` js)
        assertBool "add" ("toy.add" `T.isInfixOf` js)
        assertBool "new Widget" ("new toy.Widget" `T.isInfixOf` js)
        assertBool "resize" ("resize" `T.isInfixOf` js)
    ]

toyDemo :: Effect f 'Unit
toyDemo = fromSyntax $ do
  msg <- greet (string "x")
  log_ msg
  n <- add (number 2) (number 3)
  w <- newWidget (string "a")
  resize w n n
  done
