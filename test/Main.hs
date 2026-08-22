{-# LANGUAGE
    DataKinds
  , GADTs
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , TypeApplications
#-}
module Main (main) where

import Control.Exception (IOException, bracket, catch)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import JShark
import JShark.Api
import JShark.Compiler
import JShark.Rec (Rec(..), (<:))
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
import qualified JShark.Ajax as Ajax
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
  , compilerTests
  , bunEvalTests
  ]

evaluatorTests :: TestTree
evaluatorTests = testGroup "evaluate"
  [ testCase "addition" $
      evaluateNumber (number 1 + number 2) @?= 3
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
      let x = number 21 + number 21
          e = x + x
      cached <- evaluateCached e
      case cached of
        ValueNumber n -> do
          n @?= evaluateNumber e
          n @?= 84
  ]

-- Non-foldable holes so snapshot tests can pin JS shape rather than
-- constant-folded results. Foreign calls live on 'Effect'; bind then
-- yield an 'Expr' via 'Var'.
fooE, barE :: Effect f u
fooE = ffi "foo" RecNil
barE = ffi "bar" RecNil

condE :: Effect f 'Bool
condE = ffi "cond" RecNil

-- Bind one FFI result and yield a pure expression.
with1 :: Effect f a -> (Expr f a -> Expr f b) -> Effect f b
with1 e k = fromSyntax $ do
  x <- toSyntax e
  toSyntax (expr (k (Var x)))

-- Bind two FFI results and yield a pure expression.
with2 :: Effect f a -> Effect f b -> (Expr f a -> Expr f b -> Expr f c) -> Effect f c
with2 e1 e2 k = fromSyntax $ do
  x <- toSyntax e1
  y <- toSyntax e2
  toSyntax (expr (k (Var x) (Var y)))

-- Self-contained: Boolean(1) is not folded, so prettyJS must keep a real if
-- inside a LambdaE and still be valid JS.
prettyIfLambda :: forall f. Effect f 'Number
prettyIfLambda = fromSyntax $ do
  r <- toSyntax $ ApplyE
    (lambdaE (\x ->
        ifE (ffi "Boolean" (arg (number 1) <: RecNil))
          x
          (expr (number 0))))
    (expr (number 6))
  yield (Var r)

codegenTests :: TestTree
codegenTests = testGroup "codegen"
  [ testCase "nested single-use lets are both inlined" $
      renderJS (effectfulAST (with2 fooE barE (\x y -> y + x)))
        @?= "bar() + foo()"
  , testCase "let used more than once renders as a const binding" $
      renderJS (effectfulAST (with1 fooE (\x -> x + x)))
        @?= "const n0 = foo();\nn0 + n0"
  , testCase "let used once under a lambda is not inlined" $
      renderJS (effectfulAST (with1 fooE (\x -> lambda (\_ -> x + number 1))))
        @?= "const n0 = foo();\nfunction (n1) {return (n0 + 1.0)}"
  , testCase "let used once in an if_ branch is not inlined" $
      renderJS (effectfulAST (with2 fooE condE (\x c -> if_ c x (number 0))))
        @?= "const n0 = foo();\n(cond() ? n0 : 0.0)"
  , testCase "let used once on the && RHS is not inlined" $
      renderJS (effectfulAST (with2 condE barE (\x y -> And y x)))
        @?= "const n0 = cond();\nbar() && n0"
  , testCase "let used once on the && LHS is inlined" $
      renderJS (effectfulAST (with2 condE barE (\x y -> And x y)))
        @?= "const n0 = bar();\ncond() && n0"
  , testCase "unknown function application renders as a direct call" $
      renderJS (effectfulAST (ApplyE (ffi "f" RecNil) fooE))
        @?= "(f())(foo())"
  , testCase "effectfulProgram wraps decls and the result in a JS IIFE" $
      renderJS (effectfulProgram (with1 fooE (\x -> x + x)))
        @?= "(() => {\n  const n0 = foo();\n  return n0 + n0;\n})()"
  , testCase "effectful console.log FFI call" $
      renderJS (effectfulAST (fromSyntax (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp)))
        @?= "console.log(\"hi\");"
  , testCase "OverloadedStrings Expr literal" $
      renderJS (pureAST ("hi" :: Expr f 'String)) @?= "\"hi\""
  , testCase "OverloadedStrings Value via Literal" $
      renderJS (pureAST (Literal ("hi" :: Value 'String))) @?= "\"hi\""
  , testCase "Num Value literal via Literal" $
      renderJS (pureAST (Literal (3 :: Value 'Number))) @?= "3.0"
  , testCase "Num Expr literal" $
      renderJS (pureAST (3 :: Expr f 'Number)) @?= "3.0"
  , testCase "Num Value host arithmetic" $
      case ((1 + 2 * 3) :: Value 'Number) of
        ValueNumber n -> n @?= 7
  , testCase "Fractional Value via Literal" $
      renderJS (pureAST (Literal ((1 / 2) :: Value 'Number))) @?= "0.5"
  ]

controlFlowTests :: TestTree
controlFlowTests = testGroup "control flow"
  [ testCase "if_ picks the true branch" $
      evaluateNumber (if_ (bool True) (number 1) (number 2)) @?= 1
  , testCase "if_ picks the false branch" $
      evaluateNumber (if_ (bool False) (number 1) (number 2)) @?= 2
  , testCase "if_ renders as a ternary" $
      renderJS (effectfulAST (with1 condE (\c -> if_ c (number 1) (number 2))))
        @?= "(cond() ? 1.0 : 2.0)"
  , testCase "optionCase on Some" $
      evaluateNumber (optionCase (JShark.Api.some (number 5) :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1)) @?= 6
  , testCase "optionCase on None" $
      evaluateNumber (optionCase (none :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1)) @?= 0
  , testCase "ifE renders an if/else statement with a shared result variable" $
      renderJS (effectfulAST (fromSyntax (toSyntax (ifE condE (expr (number 1)) (expr (number 2))) *> toSyntax noOp)))
        @?= "let n0;\nif (cond()) {n0 = 1.0;}\nelse {n0 = 2.0;}"
  , testCase "whileE re-emits an FFI condition" $
      renderJS (effectfulAST (fromSyntax (toSyntax_ (while_ condE (ffi "foo" RecNil)) *> toSyntax noOp)))
        @?= "while (cond()) {foo();}"
  , testCase "when_ of Unit skips the result bind" $
      renderJS (effectfulAST (when_ condE (ffi "foo" RecNil)))
        @?= "if (cond()) {foo();}"
  , testCase "ifS of two CallMethods skips the result bind" $
      renderJS (effectfulAST (IfS condE
        (callMethod (UnsafeObject "el") "setAttribute" (arg (string "k") <: arg (string "a") <: RecNil))
        (callMethod (UnsafeObject "el") "setAttribute" (arg (string "k") <: arg (string "b") <: RecNil))))
        @?= "if (cond()) {el.setAttribute(\"k\", \"a\");}\nelse {el.setAttribute(\"k\", \"b\");}"
  , testCase "ifE of two getAttributes keeps the result bind" $
      renderJS (effectfulAST (ifE condE
        (callMethod (UnsafeObject "el") "getAttribute" (arg (string "a") <: RecNil))
        (callMethod (UnsafeObject "el") "getAttribute" (arg (string "b") <: RecNil))))
        @?= "let n0;\nif (cond()) {n0 = el.getAttribute(\"a\");}\nelse {n0 = el.getAttribute(\"b\");}\nn0"
  , testCase "ifE of assign vs number keeps the result bind" $
      renderJS (effectfulAST (ifE condE
        (UnsafeObjectAssign (UnsafeObject "x") (expr (number 1)))
        (expr (number 2))))
        @?= "let n0;\nif (cond()) {n0 = x = 1.0;}\nelse {n0 = 2.0;}\nn0"
  , testCase "try_ of Unit skips the result bind" $
      renderJS (effectfulAST (try_ (ffi "foo" RecNil) noOp))
        @?= "try {foo();}\ncatch (n0) {}"
  ]

numArray :: forall f. Expr f ('Array 'Number)
numArray = Literal (ValueArray [ValueNumber 1, ValueNumber 2])

stdlibTests :: TestTree
stdlibTests = testGroup "stdlib"
  [ testCase "Array.index evaluates" $
      evaluateNumber (Array.index numArray (number 1)) @?= 2
  , testCase "Array.index renders as bracket indexing" $
      renderJS (effectfulAST (with2 (ffi "xs" RecNil) (ffi "i" RecNil) Array.index))
        @?= "xs()[i()]"
  , testCase "Array.length_ renders as .length" $
      renderJS (pureAST (Array.length_ numArray)) @?= "[1.0, 2.0].length"
  , testCase "Array.map_ renders as .map with a callback" $
      renderJS (pureAST (Array.map_ numArray (\x -> x + number 1)))
        @?= "[1.0, 2.0].map(function (n0) {return n0 + 1.0})"
  , testCase "Array.filterE renders an effectful callback" $
      renderJS (effectfulAST (fromSyntax (do
        toSyntax_ $ Array.filterE numArray (\x -> ffi "pred" (arg x <: RecNil))
        toSyntax noOp)))
        @?= "[1.0, 2.0].filter(function (n0) {return pred(n0)});"
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
      renderJS (effectfulAST (with1 fooE Math.sin)) @?= "Math.sin(foo())"
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
      renderJS (effectfulAST (fromSyntax (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp)))
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
  , testCase "multi-use UnsafeObject stays one const (identity)" $
      renderJS (effectfulAST (fromSyntax (do
        o <- fmap (expr . Var) $ toSyntax $ UnsafeObject "{}"
        toSyntax_ $ UnsafeObjectAssign (UnsafeObjectGet o "a") (Lift (number 1))
        toSyntax_ $ UnsafeObjectAssign (UnsafeObjectGet o "b") (Lift (number 2))
        toSyntax noOp)))
        @?= "const n0 = {};\nn0.a = 1.0;\nn0.b = 2.0;"
  , testCase "forEach param name matches body uses" $
      renderJS (effectfulAST (fromSyntax (do
        toSyntax_ $ forEach numArray (\x -> ffi "foo" (arg x <: RecNil))
        toSyntax noOp)))
        @?= "[1.0, 2.0].forEach(function (n0) {return foo(n0)});"
  , testCase "onClick assigns the DOM onclick property" $
      T.isInfixOf ".onclick ="
        (T.pack $ renderJS (effectfulAST (fromSyntax (do
          el <- Dom.lookupId (string "b")
          onClick el $ \_ -> noOp
          toSyntax noOp))))
        @?= True
  , testCase ".== compiles to strict === (never ==)" $
      renderJS (effectfulAST (with2 fooE barE (.==)))
        @?= "foo() === bar()"
  , testCase ".!= compiles to strict !==" $
      renderJS (effectfulAST (with2 fooE barE (.!=)))
        @?= "foo() !== bar()"
  , testCase "ffi takes an effectful function via ArgEffect, not UnsafeEffectExpr" $
      renderJS (effectfulAST (ffi "setTimeout" (ArgEffect (LambdaE (\_ -> ffi "tick" RecNil)) <: arg (number 0) <: RecNil)))
        @?= "setTimeout(function (n0) {return tick()}, 0.0)"
  , testCase "send emits xhr.send()" $
      renderJS (effectfulAST (fromSyntax (Ajax.send (UnsafeObject "xhr") *> toSyntax noOp)))
        @?= "xhr.send();"
  , testCase "sendPost emits xhr.send(body)" $
      renderJS (effectfulAST (fromSyntax (Ajax.sendPost (UnsafeObject "xhr") (string "hi") *> toSyntax noOp)))
        @?= "xhr.send(\"hi\");"
  ]

optimizeTests :: TestTree
optimizeTests = testGroup "optimize"
  [ testCase "literal arithmetic folds" $
      renderJS (pureAST (number 1 + number 2)) @?= "3.0"
  , testCase "nested single-use lets fold" $
      renderJS (pureAST (let_ (number 1) (\x -> let_ (number 2) (\y -> y + x))))
        @?= "3.0"
  , testCase "cheap multi-use let is propagated and folded" $
      renderJS (pureAST (let_ (number 5) (\x -> x + x))) @?= "10.0"
  , testCase "dead pure let is dropped" $
      renderJS (pureAST (let_ (number 1) (\_ -> number 2))) @?= "2.0"
  , testCase "unused FFI let is kept as a statement" $
      renderJS (effectfulAST (Bind fooE (\_ -> Lift (number 1)))) @?= "foo();\n1.0"
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
  , testCase "unused closed-name stdlib is dropped" $
      renderJS (pureAST (let_ (Str.toUpper (string "hi")) (\_ -> number 1)))
        @?= "1.0"
  , testCase "unused stringify is kept (can throw)" $
      renderJS (pureAST (let_ (Json.stringify (number 1)) (\_ -> number 2)))
        @?= "JSON.stringify(1.0);\n2.0"
  , testCase "optionCase of a Literal ValueOption folds" $
      renderJS (pureAST (optionCase (Literal (ValueOption (Just (ValueNumber 5)))) (number 0) (\x -> x + 1)))
        @?= "6.0"
  , testCase "optionCase of some of a folded literal peels" $
      renderJS (pureAST (optionCase (some (number 1 + number 2)) (number 0) (\x -> x + 1)))
        @?= "4.0"
  , testCase "if_ True takes the true branch" $
      renderJS (pureAST (if_ (bool True) (number 1) (number 99)))
        @?= "1.0"
  , testCase "false && folds the RHS" $
      renderJS (pureAST (And (bool False) (Eq (number 1) (number 0))))
        @?= "false"
  , testCase "while false becomes a no-op" $
      renderJS (effectfulAST (while_ (expr (bool False)) (ffi "foo" RecNil)))
        @?= ""
  , testCase "ifE of True takes the true branch" $
      renderJS (effectfulAST (ifE (expr (bool True)) (ffi "foo" RecNil) (ffi "bar" RecNil)))
        @?= "foo()"
  , testCase "typeof of a literal folds" $
      renderJS (pureAST (typeOf (number 1))) @?= "\"number\""
  , testCase "string Semigroup is Concat" $
      renderJS (pureAST (("a" :: Expr f 'String) <> "b")) @?= "\"ab\""
  , testCase "try_ renders try/catch" $
      renderJS (effectfulAST (try_ (ffi "foo" RecNil) (expr (number 0))))
        @?= "let n0;\ntry {n0 = foo();}\ncatch (n1) {n0 = 0.0;}\nn0"
  , testCase "optionCaseE of none takes the none branch" $
      renderJS (effectfulAST (optionCaseE (none :: Expr f ('Option 'Number)) (ffi "missing" RecNil) (\x -> expr x)))
        @?= "missing()"
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
      out <- compilePure passthroughConfig (number 1 + number 2)
      out @?= T.pack (renderJS (pureProgram (number 1 + number 2)))
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
          -- Constant-folded to a pure literal IIFE; esbuild DCE's that
          -- unless Compiler re-anchors via export default and strips it.
          let snippet = number 1 + number 2
              raw = T.pack (renderJS (pureProgram snippet))
              cfg = CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Minified
          out <- compilePure cfg snippet
          assertBool "non-empty" (not (T.null out))
          assertBool "minifier changed the IIFE" (out /= raw)
          assertBool "stripped ESM export anchor" (not ("export" `T.isInfixOf` out))
          assertBool "result still an expression (no var binding left)" (not ("var " `T.isPrefixOf` out))
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
        (fromSyntax (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp))
      out @?= "console.log(\"hi\");"
  , testCase "readableConfig compilePure has no IIFE and inlines single-use lets" $ do
      clearCompilerCache
      out <- compileEffect readableConfig (with1 fooE (\x -> x + number 1))
      out @?= "foo() + 1.0"
  , testCase "readableConfig keeps multi-use lets as const" $ do
      clearCompilerCache
      out <- compileEffect readableConfig (with1 fooE (\x -> x + x))
      out @?= "const n0 = foo();\nn0 + n0"
  , testCase "Readable style skips the minifier even when a backend is set" $ do
      clearCompilerCache
      out <- compileEffect
        (CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Readable)
        fooE
      out @?= "foo()"
  , testCase "compileWith Readable skips the minifier even when a backend is set" $ do
      clearCompilerCache
      let src = "const x = 1 + 2;" :: Text
          cfg = CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Readable
      out <- compileWith cfg src
      out @?= src
  , testCase "prettyJS breaks if/else and function bodies onto their own lines" $
      prettyJS "if (cond()) {foo();} else {bar();}"
        @?= "if (cond()) {\n  foo();\n} else {\n  bar();\n}"
  , testCase "prettyJS does not split braces that live inside a string" $
      prettyJS "foo(\"{;}\");"
        @?= "foo(\"{;}\");"
  , testCase "prettyJS keeps else and catch on the closing brace line" $
      prettyJS "try {foo();} catch (n0) {bar();}"
        @?= "try {\n  foo();\n} catch (n0) {\n  bar();\n}"
  , testCase "prettyJS joins empty if/else and keeps `}(` on one line" $
      prettyJS "if (c) {\n}\nelse {\n  foo();\n}\n(bar)()"
        @?= "if (c) {} else {\n  foo();\n}(bar)()"
  , testCase "prettyJS keeps IIFE call on the closing brace" $
      prettyJS "function () {return 1;}()"
        @?= "function () {\n  return 1;\n}()"
  , testCase "prettyJS does not treat elsewhere as else" $
      prettyJS "if (c) {}elsewhere"
        @?= "if (c) {}\nelsewhere"
  , testCase "readableConfig pretty-prints ifE" $ do
      clearCompilerCache
      out <- compileEffect readableConfig
        (fromSyntax (toSyntax (ifE condE (expr (number 1)) (expr (number 2))) *> toSyntax noOp))
      out @?= "let n0;\nif (cond()) {\n  n0 = 1.0;\n} else {\n  n0 = 2.0;\n}"
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
            [ bunCase getBun "addition" (number 1 + number 2)
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
            , testCase "prettyJS compileEffect ifE+LambdaE" $ do
                bun <- getBun >>= \case
                  Just b -> pure b
                  Nothing -> assertFailure "bun not found on PATH"
                clearCompilerCache
                out <- compileEffect readableConfig prettyIfLambda
                assertBool "indented if body" ("{\n" `T.isInfixOf` out)
                got <- bunJSONStringify bun (wrapReadableExpr out)
                assertEqual
                  ("expected 6\nbun JSON: " <> got <> "\njs:\n" <> T.unpack out)
                  "6"
                  got
            ]
      ]

bunCase :: IO (Maybe FilePath) -> String -> (forall f. Expr f u) -> TestTree
bunCase getBun name e = testCase name $ do
  m <- getBun
  case m of
    Nothing -> assertFailure "bun not found on PATH"
    Just bun -> assertBunAgrees bun e

-- Readable effect snippets are statements plus a trailing result
-- expression. Wrap so bunJSONStringify can treat them as a value.
wrapReadableExpr :: Text -> String
wrapReadableExpr src =
  let ls = filter (not . T.null) (T.lines (T.strip src))
   in case reverse ls of
        [] -> "undefined"
        result : revStmts ->
          T.unpack $ T.unlines $
            "(() => {" : reverse revStmts ++ ["return " <> result, "})()"]

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
-- payload).
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
