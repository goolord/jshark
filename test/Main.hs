{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}

module Main (main) where

import Control.Exception (IOException, bracket, catch)
import Data.Char (isSpace)
import Data.Functor.Const (Const(..))
import Data.List (dropWhileEnd, intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import JShark
import JShark.Api
import JShark.Compiler
import qualified JShark.ExprF as ExprF
import JShark.Rec (Rec(..))
import JShark.Types
import System.Directory
  ( createDirectoryIfMissing
  , findExecutable
  , getTemporaryDirectory
  , listDirectory
  , removeFile
  , removePathForcibly
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
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
  , optimizeTests
  , exprFTests
  , compilerTests
  , bunEvalTests
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

-- Non-foldable holes so snapshot tests can pin JS shape rather than
-- constant-folded results. Cheap literals are propagated even under
-- lambdas; FFI/methods are not.
fooN :: Expr f 'Number
fooN = exprFfi "foo" RecNil

barN :: Expr f 'Number
barN = exprFfi "bar" RecNil

condB :: Expr f 'Bool
condB = exprFfi "cond" RecNil

codegenTests :: TestTree
codegenTests = testGroup "codegen"
  [ testCase "nested single-use lets are both inlined" $
      renderJS (pureAST (let_ fooN (\x -> let_ barN (\y -> y + x))))
        @?= "bar() + foo()"
  , testCase "let used more than once renders as a const binding" $
      renderJS (pureAST (let_ fooN (\x -> x + x)))
        @?= "const n0 = foo();\nn0 + n0"
  , testCase "let used once under a lambda is not inlined" $
      renderJS (pureAST (let_ fooN (\x -> lambda (\_ -> x + number 1))))
        @?= "const n0 = foo();\nfunction (n1) {return (n0 + 1.0)}"
  , testCase "let used once in an if_ branch is not inlined" $
      renderJS (pureAST (let_ fooN (\x -> if_ condB x (number 0))))
        @?= "const n0 = foo();\n(cond() ? n0 : 0.0)"
  , testCase "let used once on the && RHS is not inlined" $
      renderJS (pureAST (let_ condB (\x -> And (exprFfi "bar" RecNil) x)))
        @?= "const n0 = cond();\nbar() && n0"
  , testCase "let used once on the && LHS is inlined" $
      renderJS (pureAST (let_ condB (\x -> And x (exprFfi "bar" RecNil))))
        @?= "cond() && bar()"
  , testCase "unknown function application renders as a direct call" $
      renderJS (pureAST (apply (exprFfi "f" RecNil) fooN))
        @?= "(f())(foo())"
  , testCase "pureProgram wraps decls and the result in a JS IIFE" $
      renderJS (pureProgram (let_ fooN (\x -> x + x)))
        @?= "(() => {\n  const n0 = foo();\n  return n0 + n0;\n})()"
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
      renderJS (pureAST (if_ condB (number 1) (number 2)))
        @?= "(cond() ? 1.0 : 2.0)"
  , testCase "optionCase on Some" $
      evaluateNumber (optionCase (JShark.Api.some (number 5) :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1)) @?= 6
  , testCase "optionCase on None" $
      evaluateNumber (optionCase (none :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1)) @?= 0
  , testCase "resultCase on Ok" $
      evaluateNumber (resultCase (ok (number 5) :: Expr f ('Result 'Number 'String)) (\x -> x + 1) (\_ -> number (-1))) @?= 6
  , testCase "resultCase on Err" $
      evaluateNumber (resultCase (err (string "bad") :: Expr f ('Result 'Number 'String)) (\x -> x + 1) (\_ -> number (-1))) @?= (-1)
  , testCase "ifE renders an if/else statement with a shared result variable" $
      renderJS (effectfulAST (fromSyntax (toSyntax (ifE condB (expr (number 1)) (expr (number 2))) *> toSyntax noOp)))
        @?= "let n0;\nif (cond()) {n0 = 1.0;}\nelse {n0 = 2.0;}"
  , testCase "while_ renders a while loop" $
      renderJS (effectfulAST (fromSyntax (toSyntax_ (while_ condB (ffi "foo" RecNil)) *> toSyntax noOp)))
        @?= "while (cond()) {foo();}"
  ]

numArray :: forall f. Expr f ('Array 'Number)
numArray = Literal (ValueArray [ValueNumber 1, ValueNumber 2])

stdlibTests :: TestTree
stdlibTests = testGroup "stdlib"
  [ testCase "Array.index evaluates" $
      evaluateNumber (Array.index numArray (number 1)) @?= 2
  , testCase "Array.index renders as bracket indexing" $
      renderJS (pureAST (Array.index (exprFfi "xs" RecNil :: Expr f ('Array 'Number)) (exprFfi "i" RecNil)))
        @?= "xs()[i()]"
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
      renderJS (pureAST (Math.sin fooN)) @?= "Math.sin(foo())"
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

optimizeTests :: TestTree
optimizeTests = testGroup "optimize"
  [ testCase "literal arithmetic folds" $
      renderJS (pureAST (plus (number 1) (number 2))) @?= "3.0"
  , testCase "nested single-use lets fold" $
      renderJS (pureAST (let_ (number 1) (\x -> let_ (number 2) (\y -> y + x))))
        @?= "3.0"
  , testCase "cheap multi-use let is propagated and folded" $
      renderJS (pureAST (let_ (number 5) (\x -> x + x))) @?= "10.0"
  , testCase "dead pure let is dropped" $
      renderJS (pureAST (let_ (number 1) (\_ -> number 2))) @?= "2.0"
  , testCase "unused FFI let is kept as a statement" $
      renderJS (pureAST (let_ fooN (\_ -> number 1))) @?= "foo();\n1.0"
  , testCase "lambda application of a literal folds" $
      renderJS (pureAST (apply (lambda (\x -> x * 2)) (number 21))) @?= "42.0"
  , testCase "if_ of True takes the true branch" $
      renderJS (pureAST (if_ (bool True) (number 1) (number 2))) @?= "1.0"
  , testCase "literal is propagated under a lambda" $
      renderJS (pureAST (let_ (number 5) (\x -> lambda (\_ -> x + number 1))))
        @?= "function (n0) {return (6.0)}"
  , testCase "array index of a literal folds" $
      renderJS (pureAST (Array.index numArray (number 0))) @?= "1.0"
  , testCase "Math.sin of 0 folds" $
      renderJS (pureAST (Math.sin (number 0))) @?= "0.0"
  , testCase "Math.sin of a non-zero literal is left to JS" $
      renderJS (pureAST (Math.sin (number 1))) @?= "Math.sin(1.0)"
  , testCase "unknown MathUnary is not folded" $
      renderJS (pureAST (MathUnary "nope" (number 1))) @?= "Math.nope(1.0)"
  , testCase "GTh of array literals is not folded" $
      renderJS (pureAST (GTh numArray numArray))
        @?= "[1.0, 2.0] > [1.0, 2.0]"
  , testCase "optionCase of a Literal ValueOption folds" $
      renderJS (pureAST (optionCase (Literal (ValueOption (Just (ValueNumber 5)))) (number 0) (\x -> x + 1)))
        @?= "6.0"
  , testCase "if_ True does not fold the dead branch" $
      renderJS (pureAST (if_ (bool True) (number 1) (MathUnary "nope" (number 1))))
        @?= "1.0"
  , testCase "false && does not fold the RHS" $
      renderJS (pureAST (And (bool False) (Eq (MathUnary "nope" (number 1)) (number 0))))
        @?= "false"
  , testCase "while false becomes a no-op" $
      renderJS (effectfulAST (while_ (bool False) (ffi "foo" RecNil)))
        @?= ""
  , testCase "ifE of True takes the true branch" $
      renderJS (effectfulAST (ifE (bool True) (ffi "foo" RecNil) (ffi "bar" RecNil)))
        @?= "foo()"
  ]

exprFTests :: TestTree
exprFTests = testGroup "ExprF removeUnusedBindings"
  [ testCase "drops an unused let" $ do
      let e = ExprF.LetF (ExprF.LiteralF (ValueNumber 1)) (\_ -> ExprF.LiteralF (ValueNumber 2))
          e' = ExprF.removeUnusedBindings e
      case e' of
        ExprF.LiteralF (ValueNumber 2) -> pure ()
        _ -> assertFailure "expected LiteralF 2 after DCE"
  , testCase "keeps a let that is used twice" $ do
      let e = ExprF.LetF (ExprF.LiteralF (ValueNumber 5)) (\x -> ExprF.PlusF (ExprF.VarF x) (ExprF.VarF x))
          e' = ExprF.removeUnusedBindings e
      case e' of
        ExprF.LetF (ExprF.LiteralF (ValueNumber 5)) body ->
          case body (Const (0 :: Int)) of
            ExprF.PlusF (ExprF.VarF (Const 0)) (ExprF.VarF (Const 0)) -> pure ()
            _ -> assertFailure "expected body x + x"
        _ -> assertFailure "expected LetF 5 in x + x"
  , testCase "bottom-up: dropping an inner let frees the outer binder" $ do
      let e =
            ExprF.LetF (ExprF.LiteralF (ValueNumber 1)) $ \x ->
              ExprF.LetF (ExprF.VarF x) $ \_y ->
                ExprF.LiteralF (ValueNumber 2)
          e' = ExprF.removeUnusedBindings e
      case e' of
        ExprF.LiteralF (ValueNumber 2) -> pure ()
        _ -> assertFailure "expected both lets to disappear"
  , testCase "keeps a lambda even when the parameter is unused" $ do
      let e = ExprF.LambdaF (\_ -> ExprF.LiteralF (ValueNumber 1))
          e' = ExprF.removeUnusedBindings e
      case e' of
        ExprF.LambdaF{} -> pure ()
        _ -> assertFailure "expected LambdaF to remain"
  , testCase "removeUnusedBindingsExpr round-trips a fragment Expr" $ do
      let e = let_ (number 1) (\_ -> number 2)
      case ExprF.removeUnusedBindingsExpr e of
        Just (Literal (ValueNumber 2)) -> pure ()
        Just _ -> assertFailure "expected Literal 2 after DCE"
        Nothing -> assertFailure "expected fragment conversion to succeed"
  , testCase "removeUnusedBindingsExpr rejects non-fragment Expr" $
      case ExprF.removeUnusedBindingsExpr (if_ (bool True) (number 1) (number 2)) of
        Nothing -> pure ()
        Just _ -> assertFailure "expected Nothing for if_"
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
      out <- compilePure readableConfig (let_ fooN (\x -> x + number 1))
      out @?= "foo() + 1.0"
  , testCase "readableConfig keeps multi-use lets as const" $ do
      clearCompilerCache
      out <- compilePure readableConfig (let_ fooN (\x -> x + x))
      out @?= "const n0 = foo();\nn0 + n0"
  , testCase "Readable style skips the minifier even when a backend is set" $ do
      clearCompilerCache
      out <- compilePure
        (CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Readable)
        fooN
      out @?= "foo()"
  , testCase "compileWith Readable skips the minifier even when a backend is set" $ do
      clearCompilerCache
      let src = "const x = 1 + 2;" :: Text
          cfg = CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Readable
      out <- compileWith cfg src
      out @?= src
  ]

-- Run generated JS with bun and compare JSON.stringify of the result to
-- `evaluate`. If bun is missing, the PATH check fails and the eval cases
-- are skipped (not reported as passing).
bunEvalTests :: TestTree
bunEvalTests =
  withResource (findExecutable "bun") (const (pure ())) $ \getBun ->
    testGroup "bun agrees with evaluate"
      [ testCase "bun is on PATH" $ do
          m <- getBun
          case m of
            Just _ -> pure ()
            Nothing ->
              assertFailure "bun not found on PATH; install https://bun.sh"
      , after AllSucceed "bun is on PATH" $
          testGroup "eval"
            [ bunCase getBun "addition" (plus (number 1) (number 2))
            , bunCase getBun "subtraction" ((number 5 :: Expr f 'Number) - number 2)
            , bunCase getBun "multiplication and division"
                ((number 6 :: Expr f 'Number) * number 7 / number 2)
            , bunCase getBun "abs and negate" (abs (negate (number 5) :: Expr f 'Number))
            , bunCase getBun "let used twice" (let_ (number 21) (\x -> x + x))
            , bunCase getBun "nested single-use lets"
                (let_ (number 1) (\x -> let_ (number 2) (\y -> y + x)))
            , bunCase getBun "lambda application"
                (apply (lambda (\x -> x * 2)) (number 21))
            , bunCase getBun "if_ true" (if_ (bool True) (number 1) (number 2))
            , bunCase getBun "if_ false" (if_ (bool False) (number 1) (number 2))
            , bunCase getBun "&& short-circuit false"
                (And (bool False) (bool True))
            , bunCase getBun "|| short-circuit true"
                (Or (bool True) (bool False))
            , bunCase getBun "let on && LHS" (let_ (bool True) (\x -> And x (bool False)))
            , bunCase getBun "let on && RHS" (let_ (bool True) (\x -> And (bool False) x))
            , bunCase getBun "let in if_ branch"
                (let_ (number 5) (\x -> if_ (bool True) x (number 0)))
            , bunCase getBun "optionCase Some"
                (optionCase (JShark.Api.some (number 5) :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1))
            , bunCase getBun "optionCase None"
                (optionCase (none :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1))
            , bunCase getBun "some is the wrapped value"
                (JShark.Api.some (number 5) :: Expr f ('Option 'Number))
            , bunCase getBun "none is null" (none :: Expr f ('Option 'Number))
            , bunCase getBun "resultCase Ok"
                (resultCase (ok (number 5) :: Expr f ('Result 'Number 'String)) (\x -> x + 1) (\_ -> number (-1)))
            , bunCase getBun "resultCase Err"
                (resultCase (err (string "bad") :: Expr f ('Result 'Number 'String)) (\x -> x + 1) (\_ -> number (-1)))
            , bunCase getBun "ok is a tagged pair"
                (ok (number 5) :: Expr f ('Result 'Number 'String))
            , bunCase getBun "err is a tagged pair"
                (err (string "bad") :: Expr f ('Result 'Number 'String))
            , bunCase getBun "string concat" (Concat (string "a") (string "b"))
            , bunCase getBun "Show number" (Show (number 3))
            , bunCase getBun "Eq numbers" (Eq (number 1) (number 1))
            , bunCase getBun "NEq numbers" (NEq (number 1) (number 2))
            , bunCase getBun "array index" (Array.index numArray (number 1))
            , bunCase getBun "Math.sqrt" (Math.sqrt (number 9))
            , bunCase getBun "Math.round half toward +Infinity" (Math.round (number 2.5))
            , bunCase getBun "Math.round negative half" (Math.round (number (-2.5)))
            , bunCase getBun "Math.pow" (Math.pow (number 2) (number 10))
            , bunCase getBun "Math.sin 0" (Math.sin (number 0))
            ]
      ]

bunCase :: IO (Maybe FilePath) -> String -> (forall f. Expr f u) -> TestTree
bunCase getBun name e = testCase name $ do
  m <- getBun
  case m of
    Nothing -> assertFailure "bun not found on PATH"
    Just bun -> assertBunAgrees bun e

assertBunAgrees :: FilePath -> (forall f. Expr f u) -> IO ()
assertBunAgrees bun e = do
  let expected = encodeJSValue (evaluate e)
      program = renderJS (pureProgram e)
  got <- bunJSONStringify bun program
  assertEqual
    ("evaluate JSON: " <> expected <> "\nbun JSON: " <> got <> "\njs:\n" <> program)
    expected
    got

-- Evaluate a JS expression (typically a `pureProgram` IIFE) with bun and
-- return JSON.stringify of its result, without a trailing newline.
-- `JSON.stringify(undefined)` is `undefined`; we print the word `undefined`
-- so unit values have a stable encoding.
bunJSONStringify :: FilePath -> String -> IO String
bunJSONStringify bun js = do
  tmp <- getTemporaryDirectory
  let script =
        unlines
          [ "const $jshark = (" ++ js ++ ");"
          , "const $json = JSON.stringify($jshark);"
          , "console.log($json === undefined ? \"undefined\" : $json);"
          ]
  bracket
    (openTempFile tmp "jshark-bun.js")
    (\(path, h) -> do
        hClose h `catch` ignoreIO
        removeFile path `catch` ignoreIO)
    $ \(path, h) -> do
      hPutStr h script
      hClose h
      (ex, out, errOut) <- readProcessWithExitCode bun [path] ""
      case ex of
        ExitSuccess -> pure (dropWhileEnd isSpace out)
        ExitFailure n ->
          assertFailure $
            "bun exited " <> show n
              <> "\nstderr:\n" <> errOut
              <> "\njs:\n" <> js

-- Encoding of the JS runtime representation `evaluate` uses, matching
-- `JSON.stringify` (non-finite numbers become null; Option Some is the
-- payload; Result is a [okFlag, payload] pair).
encodeJSValue :: Value u -> String
encodeJSValue = \case
  ValueNumber d -> encodeJSNumber d
  ValueBool True -> "true"
  ValueBool False -> "false"
  ValueString s -> encodeJSString (T.unpack s)
  ValueUnit -> "undefined"
  ValueArray xs -> "[" ++ intercalate "," (map encodeJSValue xs) ++ "]"
  ValueOption Nothing -> "null"
  ValueOption (Just x) -> encodeJSValue x
  ValueResult (Left x) -> "[true," ++ encodeJSValue x ++ "]"
  ValueResult (Right x) -> "[false," ++ encodeJSValue x ++ "]"
  ValueFunction _ -> error "encodeJSValue: functions are not JSON"

-- JSON.stringify of a finite number; NaN/Infinity stringify to null.
encodeJSNumber :: Double -> String
encodeJSNumber d
  | isNaN d || isInfinite d = "null"
  | isInt = show (truncate d :: Integer)
  | otherwise = show d
  where
    isInt = d == fromInteger (truncate d)

encodeJSString :: String -> String
encodeJSString s = '"' : concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c = [c]

ignoreIO :: IOException -> IO ()
ignoreIO _ = pure ()
