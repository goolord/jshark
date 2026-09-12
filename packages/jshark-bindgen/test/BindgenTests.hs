{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BindgenTests (bindgenTests) where

import BindgenToy
import qualified BindgenJs
import qualified BindgenMs
import qualified BindgenPlain
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import JShark (effectfulProgram, renderJS)
import JShark.Api
import JShark.Bindgen
import JShark.Bindgen.Cli
  ( Cli (..)
  , parseCliArgs
  )
import JShark.Bindgen.Extract (tsExtractorAvailable)
import JShark.Bindgen.Ir (Diagnostic (..), irDiagnostics)
import JShark.Bun.Internal (JSProgram (..), bunTimeoutMicroseconds, runProgram)
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
        assertBool "greet" ("toy.greet" `BS.isInfixOf` js)
        assertBool "add" ("toy.add" `BS.isInfixOf` js)
        assertBool "new Widget" ("new toy.Widget" `BS.isInfixOf` js)
        assertBool "resize" ("resize" `BS.isInfixOf` js)
    , testCase "nullable arguments are unwrapped to native null/value" $ do
        hs <- mustGen "toy.d.ts"
        assertBool
          "primitive option arg"
          ("arg (unsafeOptionToNative w)" `T.isInfixOf` hs)
        assertBool
          "handle option arg"
          ( "ArgEffect (unsafeOptionToNativeEffect fallback)"
              `T.isInfixOf` hs
          )
        js <- compileEffect readableConfig toyNullableArgs
        assertBool
          "native null/value sentinel"
          ("o.some ? o.value : null" `BS.isInfixOf` js)
    , testCase "overloads get distinct Haskell names" $ do
        hs <- mustGen "ms.d.ts"
        assertBool "first overload" ("ms ::" `T.isInfixOf` hs)
        assertBool "second overload" ("ms2 ::" `T.isInfixOf` hs)
    , testCase "generated BindgenMs.hs is a golden of ms.d.ts" $
        assertGolden "BindgenMs"
          (defaultBindgenOpts {optModuleName = Just "BindgenMs"})
          "ms.d.ts"
    , testCase "generated BindgenPlain.hs is a golden of plain.d.ts" $
        assertGolden "BindgenPlain"
          (defaultBindgenOpts {optModuleName = Just "BindgenPlain"})
          "plain.d.ts"
    , testCase "generated BindgenJs.hs is a golden of toy.js" $
        assertGolden "BindgenJs"
          (defaultBindgenOpts {optModuleName = Just "BindgenJs"})
          "toy.js"
    , testCase "every fixture wrapper module compiles to JS" $ do
        js <- compileEffect readableConfig allFixtureWrappers
        assertBool "toy.greet" ("toy.greet" `BS.isInfixOf` js)
        assertBool "toy.add" ("toy.add" `BS.isInfixOf` js)
        -- plain.d.ts and toy.js both export unqualified `greet`.
        assertBool "unqualified greet" ("greet(" `BS.isInfixOf` js)
        assertBool "ms overloads" ("ms(" `BS.isInfixOf` js)
    , testCase "representative wrapper executes against a JS fixture" $ do
        let
          js = renderJS (effectfulProgram msRun)
          prog =
            JSProgram
              { jsFlags = []
              , jsPrelude =
                  "globalThis.ms = (v) => typeof v === \"string\" ? v.length : String(v);"
              , jsExpression = BC.unpack js
              , jsEpilogue = ""
              }
        got <- runProgram bunTimeoutMicroseconds prog
        assertEqual "ms(\"abcd\") === 4" "4" got
    , testCase "nested nullable declarations surface a diagnostic" $ do
        fx <- fixtureAbs "nested.d.ts"
        r <- parseIrFromFile defaultBindgenOpts fx
        case r of
          Left e -> fail e
          Right ir -> do
            let cats = map dgCategory (irDiagnostics ir)
            assertBool
              ("unsupported-nullable diagnostic, got " <> show cats)
              ("unsupported-nullable" `elem` cats)
    ]

-- | Generate @name@ from @fixture@ and compare against the committed golden.
assertGolden :: FilePath -> BindgenOpts -> FilePath -> IO ()
assertGolden moduleNameSym opts fixtureName = do
  hs <- mustGenWith opts fixtureName
  golden <- TIO.readFile =<< pkgFileAbs ("test" </> moduleNameSym <> ".hs")
  assertEqual "golden" (T.strip golden) (T.strip hs)

toyDemo :: Effect f 'Unit
toyDemo = fromSyntax $ do
  msg <- greet (string "x")
  log_ msg
  n <- add (number 2) (number 3)
  w <- newWidget (string "a")
  resize w n n
  done

-- | Exercise a nullable primitive and nullable handle argument so the
-- generated conversion shows up in compiled JS.
toyNullableArgs :: Effect f 'Unit
toyNullableArgs = fromSyntax $ do
  setWidth (string "a") none
  setWidth (string "a") (some (number 4))
  _ <- pickWidget (string "a") (expr none)
  done

-- | One program touching every fixture module, so all generated wrappers
-- are compiled (and type-checked) by the test suite.
allFixtureWrappers :: Effect f 'Unit
allFixtureWrappers = fromSyntax $ do
  t <- greet (string "toy")
  log_ t
  n <- add (number 1) (number 2)
  _ <- BindgenPlain.greet (string "plain")
  p <- BindgenPlain.add (number 1) (number 2)
  _ <- BindgenJs.greet (string "js")
  j <- BindgenJs.add (number 2) (number 3)
  m1 <- BindgenMs.ms (string "abcd")
  m2 <- BindgenMs.ms2 (number 7)
  _ <- add (n + p + j + m1) (number 0)
  log_ m2
  done

-- | A wrapper whose foreign fixture is supplied by the test prelude.
msRun :: Effect f 'Number
msRun = fromSyntax $ do
  n <- BindgenMs.ms (string "abcd")
  yield n
