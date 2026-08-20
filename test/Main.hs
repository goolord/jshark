{-# language DataKinds #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import JShark
import JShark.Api
import JShark.Compiler
import JShark.Types
import System.Directory
  ( createDirectoryIfMissing
  , findExecutable
  , getTemporaryDirectory
  , listDirectory
  , removePathForcibly
  )
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import qualified JShark.Array as Array
import qualified JShark.Console as Console
import qualified JShark.Dom as Dom
import qualified JShark.Json as Json
import qualified JShark.Math as Math
import qualified JShark.Storage as Storage
import qualified JShark.String as Str

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "jshark"
  [ evaluatorTests
  , codegenTests
  , controlFlowTests
  , stdlibTests
  , compilerTests
  ]

evaluatorTests :: TestTree
evaluatorTests = testGroup "evaluate"
  [ testCase "addition" $
      evaluateNumber (plus (number 1) (number 2)) @?= 3
  , testCase "subtraction" $
      evaluateNumber ((number 5 :: Expr f 'Number) - number 2) @?= 3
  , testCase "multiplication and division" $
      evaluateNumber ((number 6 :: Expr f 'Number) * number 7 / number 2) @?= 21
  , testCase "abs and negate" $ do
      evaluateNumber (abs (negate (number 5) :: Expr f 'Number)) @?= 5
  , testCase "let bindings" $
      evaluateNumber (let_ (number 21) (\x -> x + x)) @?= 42
  , testCase "lambda application" $
      evaluateNumber (apply (lambda (\x -> x * 2)) (number 21)) @?= 42
  , testCase "evaluateCached agrees with evaluate on a shared heap node" $ do
      let x = plus (number 21) (number 21)
          e = x + x
      cached <- evaluateCached e
      case cached of
        ValueNumber n -> do
          n @?= evaluateNumber e
          n @?= 84
  ]

codegenTests :: TestTree
codegenTests = testGroup "codegen"
  [ testCase "pure arithmetic" $
      renderJS (pureAST (plus (number 1) (number 2))) @?= "1.0 + 2.0"
  , testCase "nested single-use lets are both inlined" $
      renderJS (pureAST (let_ (number 1) (\x -> let_ (number 2) (\y -> y + x))))
        @?= "2.0 + 1.0"
  , testCase "let used more than once renders as a const binding" $
      renderJS (pureAST (let_ (number 5) (\x -> x + x)))
        @?= "const n0 = 5.0;\nn0 + n0"
  , testCase "let used once under a lambda is not inlined" $
      renderJS (pureAST (let_ (number 5) (\x -> lambda (\_ -> x + number 1))))
        @?= "const n0 = 5.0;\nfunction (n1) {return (n0 + 1.0)}"
  , testCase "let used once in an if_ branch is not inlined" $
      renderJS (pureAST (let_ (number 5) (\x -> if_ (bool True) x (number 0))))
        @?= "const n0 = 5.0;\n(true ? n0 : 0.0)"
  , testCase "let used once on the && RHS is not inlined" $
      renderJS (pureAST (let_ (bool True) (\x -> And (bool False) x)))
        @?= "const n0 = true;\nfalse && n0"
  , testCase "let used once on the && LHS is inlined" $
      renderJS (pureAST (let_ (bool True) (\x -> And x (bool False))))
        @?= "true && false"
  , testCase "lambda application renders as a direct call" $
      renderJS (pureAST (apply (lambda (\x -> x * 2)) (number 21)))
        @?= "(function (n0) {return (n0 * 2.0)})(21.0)"
  , testCase "pureProgram wraps decls and the result in a JS IIFE" $
      renderJS (pureProgram (let_ (number 5) (\x -> x + x)))
        @?= "(() => {\n  const n0 = 5.0;\n  return n0 + n0;\n})()"
  , testCase "effectful console.log FFI call" $
      renderJS (effectfulAST (fromSyntax (consoleLog (string "hi" :: Expr f 'String) *> toSyntax noOp)))
        @?= "console.log(\"hi\");"
  ]

controlFlowTests :: TestTree
controlFlowTests = testGroup "control flow"
  [ testCase "if_ picks the true branch" $
      evaluateNumber (if_ (bool True) (number 1) (number 2)) @?= 1
  , testCase "if_ picks the false branch" $
      evaluateNumber (if_ (bool False) (number 1) (number 2)) @?= 2
  , testCase "if_ renders as a ternary" $
      renderJS (pureAST (if_ (bool True) (number 1) (number 2)))
        @?= "(true ? 1.0 : 2.0)"
  , testCase "optionCase on Some" $
      evaluateNumber (optionCase (JShark.Api.some (number 5) :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1)) @?= 6
  , testCase "optionCase on None" $
      evaluateNumber (optionCase (none :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1)) @?= 0
  , testCase "resultCase on Ok" $
      evaluateNumber (resultCase (ok (number 5) :: Expr f ('Result 'Number 'String)) (\x -> x + 1) (\_ -> number (-1))) @?= 6
  , testCase "resultCase on Err" $
      evaluateNumber (resultCase (err (string "bad") :: Expr f ('Result 'Number 'String)) (\x -> x + 1) (\_ -> number (-1))) @?= (-1)
  , testCase "ifE renders an if/else statement with a shared result variable" $
      renderJS (effectfulAST (fromSyntax (toSyntax (ifE (bool True) (expr (number 1)) (expr (number 2))) *> toSyntax noOp)))
        @?= "let n0;\nif (true) {n0 = 1.0;}\nelse {n0 = 2.0;}"
  , testCase "while_ renders a while loop" $
      renderJS (effectfulAST (fromSyntax (toSyntax_ (while_ (bool False) (Bind (expr (number 1)) (\_ -> Lift (Literal ValueUnit)))) *> toSyntax noOp)))
        @?= "while (false) {1.0;}"
  ]

numArray :: forall f. Expr f ('Array 'Number)
numArray = Literal (ValueArray [ValueNumber 1, ValueNumber 2])

stdlibTests :: TestTree
stdlibTests = testGroup "stdlib"
  [ testCase "Array.index evaluates" $
      evaluateNumber (Array.index numArray (number 1)) @?= 2
  , testCase "Array.index renders as bracket indexing" $
      renderJS (pureAST (Array.index numArray (number 0)))
        @?= "[1.0, 2.0][0.0]"
  , testCase "Array.length_ renders as .length" $
      renderJS (pureAST (Array.length_ numArray)) @?= "[1.0, 2.0].length"
  , testCase "Array.map_ renders as .map with a callback" $
      renderJS (pureAST (Array.map_ numArray (\x -> x + number 1)))
        @?= "[1.0, 2.0].map(function (n0) {return n0 + 1.0})"
  , testCase "Array.map_ callback with an internal let is inlined when used once" $
      renderJS (pureAST (Array.map_ numArray (\x -> let_ (x + number 1) (\y -> y * 2))))
        @?= "[1.0, 2.0].map(function (n0) {return (n0 + 1.0) * 2.0})"
  , testCase "Array.join renders as .join" $
      renderJS (pureAST (Array.join numArray (string ",")))
        @?= "[1.0, 2.0].join(\",\")"
  , testCase "Array.push renders as a mutating .push call" $
      renderJS (effectfulAST (fromSyntax (toSyntax (Array.push numArray (number 3)) *> toSyntax noOp)))
        @?= "[1.0, 2.0].push(3.0);"
  , testCase "String.toUpper renders as .toUpperCase()" $
      renderJS (pureAST (Str.toUpper (string "hi"))) @?= "\"hi\".toUpperCase()"
  , testCase "Math.sin renders as Math.sin(x)" $
      renderJS (pureAST (Math.sin (number 0))) @?= "Math.sin(0.0)"
  , testCase "Math.sqrt evaluates" $
      evaluateNumber (Math.sqrt (number 9)) @?= 3
  , testCase "Math.round matches JS half-toward-+Infinity semantics" $ do
      evaluateNumber (Math.round (number 2.5)) @?= 3
      evaluateNumber (Math.round (number (-2.5))) @?= (-2)
  , testCase "Math.pow evaluates" $
      evaluateNumber (Math.pow (number 2) (number 10)) @?= 1024
  , testCase "Json.stringify renders as JSON.stringify(x)" $
      renderJS (pureAST (Json.stringify (number 1))) @?= "JSON.stringify(1.0)"
  , testCase "Console.log renders as console.log(x)" $
      renderJS (effectfulAST (fromSyntax (Console.log (string "hi" :: Expr f 'String) *> toSyntax noOp)))
        @?= "console.log(\"hi\");"
  , testCase "Dom appendChild inlines single-use handles" $
      renderJS (effectfulAST (fromSyntax (do
        p <- Dom.lookupId (string "p")
        c <- Dom.createElement (string "div")
        _ <- Dom.appendChild p c
        toSyntax noOp)))
        @?= "document.getElementById(\"p\").appendChild(document.createElement(\"div\"));"
  , testCase "Dom appendChild keeps a handle that is used more than once" $
      renderJS (effectfulAST (fromSyntax (do
        p <- Dom.lookupId (string "p")
        c1 <- Dom.createElement (string "div")
        c2 <- Dom.createElement (string "span")
        _ <- Dom.appendChild p c1
        _ <- Dom.appendChild p c2
        toSyntax noOp)))
        @?= "const n0 = document.getElementById(\"p\");\nn0.appendChild(document.createElement(\"div\"));\nn0.appendChild(document.createElement(\"span\"));"
  , testCase "Storage.getItem is typed as an Option and dispatches via optionCase" $
      renderJS (effectfulAST (fromSyntax (do
        v <- Storage.getItem Storage.localStorage (string "k")
        toSyntax (expr (optionCase v (string "missing") (\x -> x))))))
        @?= "const n0 = localStorage.getItem(\"k\");\n(n0 === null ? \"missing\" : n0)"
  ]

compilerTests :: TestTree
compilerTests = testGroup "compiler"
  [ testCase "passthrough is identity" $ do
      clearCompilerCache
      let src = "const x = 1 + 2;" :: Text
      out <- compileWith passthroughConfig src
      out @?= src
  , testCase "memory cache returns the same payload" $ do
      clearCompilerCache
      let cfg = CompilerConfig Passthrough MemoryCache False Minified
          src = "const x = 1 + 2;" :: Text
      a <- compileWith cfg src
      b <- compileWith cfg src
      a @?= b
      a @?= src
      clearCompilerCache
  , testCase "compilePure passthrough emits an IIFE" $ do
      clearCompilerCache
      out <- compilePure passthroughConfig (plus (number 1) (number 2))
      out @?= T.pack (renderJS (pureProgram (plus (number 1) (number 2))))
      assertBool "IIFE wrapper present" ("(() => {" `T.isInfixOf` out)
      assertBool "result is returned so minifiers cannot DCE it" ("return" `T.isInfixOf` out)
  , testCase "disk cache roundtrips passthrough output" $ do
      clearCompilerCache
      tmp <- getTemporaryDirectory
      let dir = tmp </> "jshark-compiler-disk-test"
      removePathForcibly dir
      createDirectoryIfMissing True dir
      let cfg = CompilerConfig Passthrough (DiskCache dir) False Minified
          src = "const x = 1 + 2;" :: Text
      a <- compileWith cfg src
      b <- compileWith cfg src
      a @?= src
      b @?= src
      files <- listDirectory dir
      assertBool "wrote a cache file" (not (null files))
      removePathForcibly dir
  , testCase "disk cache ignores a file whose stored key does not match" $ do
      clearCompilerCache
      tmp <- getTemporaryDirectory
      let dir = tmp </> "jshark-compiler-disk-mismatch"
      removePathForcibly dir
      createDirectoryIfMissing True dir
      let cfg = CompilerConfig Passthrough (DiskCache dir) False Minified
      _ <- compileWith cfg "const a = 1;"
      files <- listDirectory dir
      mapM_ (\f -> writeFile (dir </> f) "not-a-cache-file") files
      out <- compileWith cfg "const b = 2;"
      out @?= "const b = 2;"
      removePathForcibly dir
  , testCase "esbuild minifies an IIFE when on PATH" $ do
      clearCompilerCache
      m <- findExecutable "esbuild"
      case m of
        Nothing -> pure ()
        Just _ -> do
          let snippet = plus (number 1) (number 2)
              raw = T.pack (renderJS (pureProgram snippet))
              cfg = CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Minified
          out <- compilePure cfg snippet
          assertBool "non-empty" (not (T.null out))
          assertBool "minifier changed the IIFE" (out /= raw)
  , testCase "tryCompileWith reports missing esbuild" $ do
      clearCompilerCache
      mExe <- findExecutable "esbuild"
      mNpx <- findExecutable "npx"
      case (mExe, mNpx) of
        (Nothing, Nothing) -> do
          res <- tryCompileWith (CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Minified) "1+2;"
          case res of
            Left _ -> pure ()
            Right _ -> assertFailure "expected Left when esbuild is missing"
        _ -> pure ()
  , testCase "configFallback False surfaces minifier errors" $ do
      clearCompilerCache
      m <- findExecutable "esbuild"
      case m of
        Nothing -> pure ()
        Just _ -> do
          let cfg = CompilerConfig
                (Esbuild defaultEsbuildConfig { esbuildExtraArgs = ["--definitely-not-a-flag"] })
                NoCache
                False
                Minified
          res <- tryCompileWith cfg "(() => { return 1; })();"
          case res of
            Left _ -> pure ()
            Right out -> assertFailure ("expected Left, got " <> T.unpack out)
  , testCase "configFallback True returns the original source" $ do
      clearCompilerCache
      m <- findExecutable "esbuild"
      case m of
        Nothing -> pure ()
        Just _ -> do
          let src = "(() => { return 1; })();" :: Text
              cfg = CompilerConfig
                (Esbuild defaultEsbuildConfig { esbuildExtraArgs = ["--definitely-not-a-flag"] })
                NoCache
                True
                Minified
          out <- compileWith cfg src
          out @?= src
  , testCase "readableConfig compileEffect is a snippet, not an IIFE" $ do
      clearCompilerCache
      out <- compileEffect readableConfig
        (fromSyntax (consoleLog (string "hi" :: Expr f 'String) *> toSyntax noOp))
      out @?= "console.log(\"hi\");"
  , testCase "readableConfig compilePure has no IIFE and inlines single-use lets" $ do
      clearCompilerCache
      out <- compilePure readableConfig (let_ (number 5) (\x -> x + number 1))
      out @?= "5.0 + 1.0"
  , testCase "readableConfig keeps multi-use lets as const" $ do
      clearCompilerCache
      out <- compilePure readableConfig (let_ (number 5) (\x -> x + x))
      out @?= "const n0 = 5.0;\nn0 + n0"
  , testCase "Readable style skips the minifier even when a backend is set" $ do
      clearCompilerCache
      out <- compilePure
        (CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Readable)
        (plus (number 1) (number 2))
      out @?= "1.0 + 2.0"
  , testCase "compileWith Readable skips the minifier even when a backend is set" $ do
      clearCompilerCache
      let src = "const x = 1 + 2;" :: Text
          cfg = CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Readable
      out <- compileWith cfg src
      out @?= src
  ]
