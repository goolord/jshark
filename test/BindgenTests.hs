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
  , Mode (..)
  , parseCliArgs
  )
import JShark.Bindgen.Extract (tsExtractorAvailable)
import JShark.Bindgen.Ir
import JShark.Bindgen.Json (decodeModule, encodeModule)
import JShark.Compiler (compileEffect, readableConfig)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

fixture :: FilePath -> FilePath
fixture name = "test/fixtures/jshark-bindgen/" <> name

noTs :: BindgenOpts
noTs = defaultBindgenOpts {optNoTs = True}

mustGen :: FilePath -> IO Text
mustGen path = do
  src <- TIO.readFile path
  case generateFromSource noTs path src of
    Left e -> fail e
    Right t -> pure t

mustParse :: FilePath -> IO ModuleIr
mustParse path = do
  src <- TIO.readFile path
  case parseSource noTs path src of
    Left e -> fail e
    Right ir -> pure ir

bindgenTests :: TestTree
bindgenTests =
  testGroup
    "bindgen"
    [ testCase "toy.d.ts emits greet / Widget / util.clamp" $ do
        hs <- mustGen (fixture "toy.d.ts")
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
        src <- TIO.readFile (fixture "plain.d.ts")
        let
          opts = noTs {optPrefix = Just "acme"}
        case generateFromSource opts (fixture "plain.d.ts") src of
          Left e -> fail e
          Right hs -> do
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
        hs <- mustGen (fixture "ms.d.ts")
        assertBool "module" ("module JShark.Ms" `T.isInfixOf` hs)
        assertBool "ms ffi" ("ffi \"ms\"" `T.isInfixOf` hs)
        assertBool "overload" $
          "ms2 ::" `T.isInfixOf` hs || "ms ::" `T.isInfixOf` hs
        assertBool "string or number" $
          "Expr f ('String)" `T.isInfixOf` hs
            && "Expr f ('Number)" `T.isInfixOf` hs
    , testCase "toy.js JSDoc emits greet / add / log" $ do
        hs <- mustGen (fixture "toy.js")
        assertBool "greet" ("ffi \"greet\"" `T.isInfixOf` hs)
        assertBool "add" ("ffi \"add\"" `T.isInfixOf` hs)
        assertBool "log_" ("log_" `T.isInfixOf` hs)
        assertBool "JSDoc string" ("Expr f ('String)" `T.isInfixOf` hs)
        assertBool "JSDoc number" ("Expr f ('Number)" `T.isInfixOf` hs)
    , testCase "JSON IR round-trips" $ do
        ir <- mustParse (fixture "toy.d.ts")
        case decodeModule (encodeModule ir) of
          Left e -> fail e
          Right ir' ->
            assertEqual "roundtrip" (irFuns ir) (irFuns ir')
    , testCase "CLI-shaped opts set module name" $ do
        src <- TIO.readFile (fixture "toy.d.ts")
        let
          opts = noTs {optModuleName = Just "JShark.Demo.Toy"}
        case generateFromSource opts (fixture "toy.d.ts") src of
          Left e -> fail e
          Right hs ->
            assertBool
              "module name"
              ("module JShark.Demo.Toy" `T.isInfixOf` hs)
    , testCase "generated BindgenToy.hs is a golden of toy.d.ts" $ do
        src <- TIO.readFile (fixture "toy.d.ts")
        let
          opts = noTs {optModuleName = Just "BindgenToy"}
        case generateFromSource opts (fixture "toy.d.ts") src of
          Left e -> fail e
          Right hs -> do
            golden <- TIO.readFile "test/BindgenToy.hs"
            assertEqual
              "golden"
              (T.strip golden)
              (T.strip hs)
    , testCase "TypeScript extractor requires bun + extract.mjs" $ do
        ready <- tsExtractorAvailable
        unless ready $
          fail "bun and jshark-bindgen/extract.mjs required for TS extractor test"
        hs <- generateFromFile defaultBindgenOpts (fixture "toy.d.ts")
        case hs of
          Left e -> fail e
          Right t -> do
            assertBool "greet" ("ffi \"toy.greet\"" `T.isInfixOf` t)
            assertBool "Widget ctor" ("newWidget" `T.isInfixOf` t)
            assertBool
              "resize"
              ("callMethod self \"resize\"" `T.isInfixOf` t)
    , testCase "CLI parses module / prefix / no-ts flags" $ do
        case parseCliArgs
          [ "--no-ts"
          , "-m"
          , "JShark.Demo.Toy"
          , "-p"
          , "acme"
          , fixture "toy.d.ts"
          ] of
          Left e -> fail e
          Right cli -> do
            assertEqual "file" (fixture "toy.d.ts") (cliFile cli)
            assertEqual "mode" Haskell (cliMode cli)
            assertEqual
              "module"
              (Just "JShark.Demo.Toy")
              (optModuleName (cliOpts cli))
            assertEqual
              "prefix"
              (Just "acme")
              (optPrefix (cliOpts cli))
            assertBool "no-ts" (optNoTs (cliOpts cli))
    , testCase "CLI rejects unknown flag with usage" $ do
        case parseCliArgs ["--nope", fixture "toy.d.ts"] of
          Left msg ->
            assertBool "usage" ("Usage:" `T.isInfixOf` T.pack msg)
          Right _ -> fail "expected unknown-flag parse error"
    , testCase "CLI rejects extra positional argument" $ do
        case parseCliArgs
          [ "--no-ts"
          , fixture "toy.d.ts"
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
