{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified BindgenJs
import qualified BindgenMs
import qualified BindgenPlain
import BindgenToy
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import JShark (effectfulProgram, renderJS)
import JShark.Api
import JShark.Bindgen
import JShark.Bindgen.Cli (Cli (..), parseCliArgs)
import JShark.Bindgen.Ir (Diagnostic (..), irDiagnostics, irFuns)
import JShark.Bun.Internal (JSProgram (..), bunTimeoutMicroseconds, runProgram)
import JShark.Compiler (compileEffect, readableConfig)
import Paths_jshark_bindgen (getDataFileName)
import System.FilePath ((</>))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

main :: IO ()
main =
  defaultMain . testGroup "bindgen" $
    [ testCase "bun and jshark-bindgen/extract.mjs are available" $ do
        ready <- tsExtractorAvailable
        unless ready $
          fail "bun and jshark-bindgen/extract.mjs required (with typescript installed)"
    , testCase "plain.d.ts --prefix acme qualifies globals" $ do
        hs <- gen Nothing (Just "acme") "plain.d.ts"
        hasAll T.isInfixOf T.unpack hs ["ffi \"acme.greet\"", "ffi \"acme.add\""]
        assertBool "no double prefix" (not ("acme.acme.greet" `T.isInfixOf` hs))
    , testCase "CLI parses module / prefix flags" $
        case parseCliArgs ["-m", "JShark.Demo.Toy", "-p", "acme", toyRel] of
          Left e -> fail e
          Right cli -> do
            assertEqual "file" toyRel (cliFile cli)
            assertEqual "module" (Just "JShark.Demo.Toy") (optModuleName (cliOpts cli))
            assertEqual "prefix" (Just "acme") (optPrefix (cliOpts cli))
    , testCase "generated wrappers compileEffect to JS" $ do
        js <- compileEffect readableConfig allFixtureWrappers
        hasAll BS.isInfixOf BC.unpack js $
          ["toy.greet", "toy.add", "new toy.Widget", "resize"]
            -- nullable arguments are unwrapped to native null/value
            <> ["o.some ? o.value : null"]
            -- plain.d.ts and toy.js both export unqualified `greet`.
            <> ["greet(", "ms("]
    , testCase "representative wrapper executes against a JS fixture" $ do
        got <-
          runProgram bunTimeoutMicroseconds $
            JSProgram
              { jsFlags = []
              , jsPrelude =
                  "globalThis.ms = (v) => typeof v === \"string\" ? v.length : String(v);"
              , jsExpression = BC.unpack (renderJS (effectfulProgram msRun))
              , jsEpilogue = ""
              }
        assertEqual "ms(\"abcd\") === 4" "4" got
    , testCase "nested nullable declarations surface a diagnostic" $ do
        r <- parseIrFromFile defaultBindgenOpts =<< fixtureAbs "nested.d.ts"
        ir <- either fail pure r
        let
          cats = map dgCategory (irDiagnostics ir)
        assertBool
          ("unsupported-nullable diagnostic, got " <> show cats)
          ("unsupported-nullable" `elem` cats)
        assertEqual "nested-nullable declarations are pruned" [] (irFuns ir)
    ]
      <> [ testCase (fx <> " emits " <> what) $
             gen m Nothing fx >>= \hs -> hasAll T.isInfixOf T.unpack hs needles
         | (fx, m, what, needles) <- emitCases
         ]
      <> [ testCase ("generated " <> m <> ".hs is a golden of " <> fx) $ do
             hs <- gen (Just (T.pack m)) Nothing fx
             golden <- TIO.readFile =<< getDataFileName ("test" </> m <> ".hs")
             assertEqual "golden" (T.strip golden) (T.strip hs)
         | (m, fx) <-
             [ ("BindgenToy", "toy.d.ts")
             , ("BindgenMs", "ms.d.ts")
             , ("BindgenPlain", "plain.d.ts")
             , ("BindgenJs", "toy.js")
             ]
         ]
      <> [ testCase ("CLI rejects " <> what) $ case parseCliArgs args of
             Left msg -> assertBool msg (any (`isInfixOf` msg) expected)
             Right _ -> fail ("expected a parse error: " <> what)
         | (what, args, expected) <-
             [ ("unknown flag with usage", ["--nope", toyRel], ["Usage:"])
             ,
               ( "extra positional argument"
               , [toyRel, "extra.d.ts"]
               , ["Invalid argument", "extra.d.ts"]
               )
             ]
         ]

-- | Fixture, module-name override, description, and expected substrings.
emitCases :: [(FilePath, Maybe Text, String, [Text])]
emitCases =
  [
    ( "toy.d.ts"
    , Nothing
    , "greet / Widget / util.clamp / nullable args"
    ,
      [ "module JShark.Toy"
      , "ffi \"toy.greet\""
      , "ffi \"toy.add\""
      , "log_"
      , "data Widget"
      , "ffi \"new toy.Widget\""
      , "newWidget"
      , "callMethod self \"resize\""
      , "Field Widget \"width\""
      , "ffi \"toy.util.clamp\""
      , "ffiExpr \"toy.VERSION\""
      , "version ::"
      , "arg (unsafeOptionToNative w)"
      , "ArgEffect (unsafeOptionToNativeEffect fallback)"
      ]
    )
  ,
    ( "toy.d.ts"
    , Just "JShark.Demo.Toy"
    , "the --module name"
    , ["module JShark.Demo.Toy"]
    )
  ,
    ( "ms.d.ts"
    , Nothing
    , "distinct overloads ms / ms2"
    ,
      [ "module JShark.Ms"
      , "ffi \"ms\""
      , "ms ::"
      , "ms2 ::"
      , "Expr f ('String)"
      , "Expr f ('Number)"
      ]
    )
  ,
    ( "toy.js"
    , Nothing
    , "JSDoc greet / add / log"
    , ["ffi \"greet\"", "ffi \"add\"", "log_", "Expr f ('String)", "Expr f ('Number)"]
    )
  ]

hasAll :: (a -> a -> Bool) -> (a -> String) -> a -> [a] -> Assertion
hasAll isIn label hay = mapM_ (\n -> assertBool (label n) (n `isIn` hay))

-- | Package-relative path used by the pure CLI-parse cases (equality only).
toyRel :: FilePath
toyRel = "test/fixtures/jshark-bindgen/toy.d.ts"

-- | Fixture path from the package's data files, so it resolves both in the
-- checkout and from an unpacked source distribution.
fixtureAbs :: FilePath -> IO FilePath
fixtureAbs name =
  getDataFileName ("test" </> "fixtures" </> "jshark-bindgen" </> name)

-- | Generate a fixture's module. The header records the resolved absolute
-- fixture path; rewrite it to the checkout-relative form the goldens use
-- ('getDataFileName' can insert an extra @.@ segment, so collapse it first).
gen :: Maybe Text -> Maybe Text -> FilePath -> IO Text
gen m p name = do
  fx <- fixtureAbs name
  r <- generateFromFile (BindgenOpts m p) fx
  let
    normalized = T.replace "\\.\\" "\\" (T.replace "/./" "/" (T.pack fx))
  either
    fail
    (pure . T.replace normalized (T.pack ("test/fixtures/jshark-bindgen/" <> name)))
    r

-- | One program touching every generated wrapper module, so all of them are
-- compiled (and type-checked) by the test suite, including nullable
-- primitive and handle arguments.
allFixtureWrappers :: Effect f 'Unit
allFixtureWrappers = fromSyntax $ do
  t <- greet (string "toy")
  log_ t
  n <- add (number 1) (number 2)
  w <- newWidget (string "a")
  resize w n n
  setWidth (string "a") none
  setWidth (string "a") (some (number 4))
  _ <- pickWidget (string "a") (expr none)
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
