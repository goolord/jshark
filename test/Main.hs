{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import BunTests (bunEvalTests)
import CatalogTests (catalogTests)
import qualified Control.Exception as Ex
import Data.Array.Byte (ByteArray)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
-- import ExampleTests (exampleTests)

import JShark
import qualified JShark.Ajax as Ajax
import JShark.Api
import JShark.FlatTest
import qualified JShark.Array as Array
import qualified JShark.Canvas as Canvas
import qualified JShark.Classes as C
import JShark.Compiler
import qualified JShark.Console as Console
import qualified JShark.Dom as Dom
import qualified JShark.Generic as G
import qualified JShark.Json as Json
import qualified JShark.Map as Map
import qualified JShark.Math as Math
import qualified JShark.Object as Object
import JShark.Params (Param)
import JShark.Rec (Rec (..), (<:))
import qualified JShark.Regex as Regex
import qualified JShark.Set as Set
import qualified JShark.Storage as Storage
import qualified JShark.String as Str
import qualified JShark.Timers as Timers
import JShark.Types (jsHelperValueEq)
import LifeTests (lifeTests)
import LucidTests (lucidDomTests)
import Support
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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "jshark"
    [ evaluatorTests
    , bigIntTests
    , codegenTests
    , controlFlowTests
    , stdlibTests
    , goodPartsTests
    , genericTests
    , optimizeTests
    , irParityTests
    , flatSoATests
    , compilerTests
    , bunEvalTests
    , lucidDomTests
    -- Example codegen (Breakout/Life/…) is slow; re-enable when tuning IR.
    -- , exampleTests
    , lifeTests
    , catalogTests
    ]

bigIntTests :: TestTree
bigIntTests =
  testGroup
    "bigint"
    [ testCase "bigInt 10 + bigInt 3 evaluates to 13" $
        evaluateBigInt (bigInt 10 + bigInt 3) @?= 13
    , testCase "2^80+1 stays exact" $
        evaluateBigInt (bigInt (2 ^ (80 :: Int) + 1)) @?= 2 ^ (80 :: Int) + 1
    , testCase "codegen emits 42n" $
        renderJS (pureAST (bigInt 42)) @?= "42n"
    , testCase "negative literal is parenthesized" $
        renderJS (pureAST (bigInt (-42))) @?= "(-42n)"
    , testCase "Number inference still defaults" $
        evaluateNumber (let_ (number 1) (\seqN -> seqN + 1)) @?= 2
    , testCase "quot_ truncates toward 0" $
        evaluateBigInt (quot_ (bigInt (-7)) (bigInt 3)) @?= -2
    , testCase "rem_ is remainder after truncating division" $
        evaluateBigInt (rem_ (bigInt (-7)) (bigInt 3)) @?= -1
    , testCase "bitwise and shifts evaluate" $ do
        evaluateBigInt (bitAnd (bigInt 7) (bigInt 3)) @?= 3
        evaluateBigInt (bitOr (bigInt 4) (bigInt 1)) @?= 5
        evaluateBigInt (bitXor (bigInt 7) (bigInt 3)) @?= 4
        evaluateBigInt (shl (bigInt 1) (bigInt 8)) @?= 256
        evaluateBigInt (shr (bigInt 256) (bigInt 3)) @?= 32
    , testCase "negative shift throws" $ do
        r <- Ex.try (Ex.evaluate (evaluateBigInt (shl (bigInt 1) (bigInt (-1)))))
        case r of
          Left (Ex.ErrorCall msg)
            | "negative" `T.isInfixOf` T.pack msg -> pure ()
            | otherwise -> assertFailure ("unexpected ErrorCall: " <> msg)
          Right n -> assertFailure ("expected throw, got " <> show n)
    , testCase "toBigInt of an integer Number" $
        evaluateBigInt (toBigInt (number 10)) @?= 10
    , testCase "toBigInt of a non-integer Number throws" $ do
        r <- Ex.try (Ex.evaluate (evaluateBigInt (toBigInt (number 1.5))))
        case r of
          Left (Ex.ErrorCall msg)
            | "not an integer" `T.isInfixOf` T.pack msg -> pure ()
            | otherwise -> assertFailure ("unexpected ErrorCall: " <> msg)
          Right n -> assertFailure ("expected throw, got " <> show n)
    , testCase "fromBigInt of a small value" $
        evaluateNumber (fromBigInt (bigInt 9)) @?= 9
    , testCase "parseBigInt_ sign and prefixes" $ do
        evaluateBigInt (parseBigInt_ (string "-10")) @?= -10
        evaluateBigInt (parseBigInt_ (string "0x10")) @?= 16
        evaluateBigInt (parseBigInt_ (string "0b101")) @?= 5
        evaluateBigInt (parseBigInt_ (string "0o17")) @?= 15
        evaluateBigInt (parseBigInt_ (string "+0Xff")) @?= 255
    , testCase "comparisons and toString" $ do
        case evaluate (bigInt 3 .> bigInt 2) of
          ValueBool b -> b @?= True
        case evaluate (toString (bigInt 10 + bigInt 3)) of
          ValueString s -> s @?= "13"
    , testCase "typeof bigint" $
        case evaluate (typeOf (bigInt 1)) of
          ValueString s -> s @?= "bigint"
    , testCase "Generic Integer is BigInt" $
        G.fromValue (evaluate (G.toJS (13 :: Integer))) @?= (13 :: Integer)
    , testCase "evaluateCached matches evaluate" $ do
        let
          e = bigInt 10 + bigInt 3
        cached <- evaluateCached e
        case cached of
          ValueBigInt n -> n @?= evaluateBigInt e
    ]

evaluatorTests :: TestTree
evaluatorTests =
  testGroup
    "evaluate"
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
    , testCase "frozen records use $deepEqual in codegen" $ do
        let
          o1 =
            Object.frozen [Object.field @"x" (number 1), Object.field @"y" (number 2)] ::
              Expr f ('Object LitRow)
          o2 =
            Object.frozen [Object.field @"x" (number 1), Object.field @"y" (number 2)] ::
              Expr f ('Object LitRow)
        case evaluate (structuralEq o1 o2) of
          ValueBool b -> b @?= True
        T.isInfixOf
          "$deepEqual"
          ( ( renderJS
                (pureAST (toLambda (\(a :: Expr f u) (b :: Expr f u) -> structuralEq a b)))
            )
          )
          @?= True
    , testCase "GetField of FrozenLit evaluates" $
        evaluateNumber
          ((Object.frozen [Object.field @"x" (number 21)] :: Expr f ('Object LitRow)).x)
          @?= 21
    , testCase "let-bound frozen field evaluates" $
        evaluateNumber
          ( let_
              (Object.frozen [Object.field @"x" (number 21)] :: Expr f ('Object LitRow))
              (\o -> o.x)
          )
          @?= 21
    , testCase "if_ of frozen fields evaluates" $
        evaluateNumber
          ( ( if_
                (bool True)
                (Object.frozen [Object.field @"x" (number 21)] :: Expr f ('Object LitRow))
                (Object.frozen [Object.field @"x" (number 0)])
            ).x
          )
          @?= 21
    , testCase "duplicate frozen keys last-wins" $
        evaluateNumber
          ( ( Object.frozen [Object.field @"x" (number 1), Object.field @"x" (number 2)] ::
                Expr f ('Object LitRow)
            ).x
          )
          @?= 2
    , testCase "frozen records compare by last-wins fields" $
        case evaluate
          ( Eq
              (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
              (Object.frozen [Object.field @"x" (number 1)])
          ) of
          ValueBool b -> b @?= True
    , testCase "Show of Result is JS String(object)" $
        case evaluate (Show (ok (number 5) :: Expr f ('Result 'String 'Number))) of
          ValueString s -> s @?= "[object Object]"
    , testCase "Uint8Array literals compare by contents" $ do
        case evaluate
          (structuralEq (uint8Array (bytes [1, 2, 3])) (uint8Array (bytes [1, 2, 3]))) of
          ValueBool b -> b @?= True
        case evaluate
          (structuralEq (uint8Array (bytes [1, 2])) (uint8Array (bytes [1, 2, 3]))) of
          ValueBool b -> b @?= False
    , testCase "Show of Uint8Array is comma-joined bytes" $
        case evaluate (Show (uint8Array sampleArray)) of
          ValueString s -> s @?= "1,2,3"
    , testCase "typeof of Uint8Array is object" $
        case evaluate (typeOf (uint8Array sampleArray)) of
          ValueString s -> s @?= "object"
    , testCase "evaluateCached agrees with evaluate on a shared heap node" $ do
        let
          x = number 21 + number 21
          e = x + x
        cached <- evaluateCached e
        case cached of
          ValueNumber n -> do
            n @?= evaluateNumber e
            n @?= 84
    , testCase "evaluateCached parseInt_ matches evaluate" $ do
        let
          e = parseInt_ (string "10") (number 16)
        cached <- evaluateCached e
        case cached of
          ValueNumber n -> n @?= evaluateNumber e
    ]

codegenTests :: TestTree
codegenTests =
  testGroup
    "codegen"
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
        renderJS
          ( effectfulAST
              (fromSyntax (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp))
          )
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
    , testCase "emptyArray renders as []" $
        renderJS (pureAST (emptyArray :: Expr f ('Array 'Number))) @?= "[]"
    , testCase "toString renders String(x)" $
        renderJS (effectfulAST (with1 fooE toString)) @?= "String(foo())"
    , testCase "assign is Object.assign" $
        renderJS
          (effectfulAST (fromSyntax (assign (UnsafeObject "dst") (UnsafeObject "src"))))
          @?= "Object.assign(dst, src);"
    , testCase "whenSomeE binds then option-cases" $ do
        let
          js =
            renderJS
              ( effectfulAST
                  ( fromSyntax
                      ( whenSomeE (ffi "opt" RecNil :: Effect f ('Option 'String)) $ \x ->
                          Console.log x *> done
                      )
                  )
              )
        T.isInfixOf "opt()" js @?= True
        T.isInfixOf "=== null" js @?= True
    , testCase "loop0 is a recursive zero-arg function" $ do
        let
          js =
            renderJS
              ( effectfulAST
                  ( fromSyntax
                      ( loop0
                          (\_ -> Console.log ("p" :: Expr f 'String) *> done)
                          (\_ -> Console.log ("w" :: Expr f 'String) *> done)
                      )
                  )
              )
        T.isInfixOf "function" js @?= True
        T.isInfixOf "console.log(\"p\")" js @?= True
        T.isInfixOf "console.log(\"w\")" js @?= True
    , testCase "foreverFrame reschedules requestAnimationFrame" $
        T.count
          "requestAnimationFrame"
          (renderJS (effectfulAST (fromSyntax (Timers.foreverFrame (\_ -> done)))))
          @?= 2
    ]

controlFlowTests :: TestTree
controlFlowTests =
  testGroup
    "control flow"
    [ testCase "if_ picks the true branch" $
        evaluateNumber (if_ (bool True) (number 1) (number 2)) @?= 1
    , testCase "if_ picks the false branch" $
        evaluateNumber (if_ (bool False) (number 1) (number 2)) @?= 2
    , testCase "if_ renders as a ternary" $
        renderJS (effectfulAST (with1 condE (\c -> if_ c (number 1) (number 2))))
          @?= "(cond() ? 1.0 : 2.0)"
    , testCase "optionCase on Some" $
        evaluateNumber
          ( optionCase
              (JShark.Api.some (number 5) :: Expr f ('Option 'Number))
              (number 0)
              (\x -> x + 1)
          )
          @?= 6
    , testCase "optionCase on None" $
        evaluateNumber
          (optionCase (none :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1))
          @?= 0
    , testCase "ifE renders an if/else statement with a shared result variable" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  (toSyntax (ifE condE (expr (number 1)) (expr (number 2))) *> toSyntax noOp)
              )
          )
          @?= "let n0;\nif (cond()) {n0 = 1.0;}\nelse {n0 = 2.0;}"
    , testCase "whileE re-emits an FFI condition" $ do
        let js =
              renderJS
                ( effectfulAST
                    (fromSyntax (toSyntax_ (while_ condE (ffi "foo" RecNil)) *> toSyntax noOp))
                )
        assertJSContains "while (cond())" js
        assertJSContains "foo();" js
    , testCase "forRange_ emits a C-style for loop" $ do
        let js =
              renderJS
                ( effectfulAST
                    ( fromSyntax
                        ( toSyntax_
                            ( forRange (number 0) (number 3) $ \i ->
                                discard (u8Set (uint8Array (bytes [0])) i (number 1))
                            )
                            *> toSyntax noOp
                        )
                    )
                )
        assertJSContains "for (let n0 = 0.0 ; n0 < 3.0 ; n0 ++)" js
        assertJSContains "new Uint8Array(1)[n0] = 1.0;" js
    , testCase "u8Index renders direct Uint8Array indexing" $
        renderJS (pureAST (u8Index (uint8Array (bytes [7, 8, 9])) (number 1)))
          @?= "new Uint8Array([7, 8, 9])[1.0]"
    , testCase "when_ of Unit skips the result bind" $
        renderJS (effectfulAST (when_ condE (ffi "foo" RecNil)))
          @?= "if (cond()) {foo();}"
    , testCase "discarded do keeps the last assignment" $ do
        let
          js =
            renderJS
              ( effectfulAST
                  ( when_
                      condE
                      ( fromSyntax $ do
                          _ <- toSyntax $ UnsafeObjectAssign (UnsafeObject "o") (expr (number 1))
                          toSyntax $ UnsafeObjectAssign (UnsafeObject "p") (expr (number 2))
                      )
                  )
              )
        T.isInfixOf "o = 1.0" js @?= True
        T.isInfixOf "p = 2.0" js @?= True
    , testCase "ifS of two CallMethods skips the result bind" $
        renderJS
          ( effectfulAST
              ( ifE
                  condE
                  ( discard
                      ( callMethod
                          (UnsafeObject "el")
                          "setAttribute"
                          (arg (string "k") <: arg (string "a") <: RecNil)
                      )
                  )
                  ( discard
                      ( callMethod
                          (UnsafeObject "el")
                          "setAttribute"
                          (arg (string "k") <: arg (string "b") <: RecNil)
                      )
                  )
              )
          )
          @?= "if (cond()) {el.setAttribute(\"k\", \"a\");}\nelse {el.setAttribute(\"k\", \"b\");}"
    , testCase "ifE keeps impure prelude when condition folds" $
        let
          js =
            renderJS
              ( effectfulAST
                  ( fromSyntax $ do
                      dst <- bindExpr (newByteArray (number 4))
                      src <- bindExpr (newByteArray (number 4))
                      toSyntax_ $
                        ifE
                          (expr (number 1 .== number 1))
                          (u8Copy dst src)
                          noOp
                      toSyntax noOp
                  )
              )
         in T.isInfixOf ".set(" js @?= True
    , testCase "ifE of two getAttributes keeps the result bind" $
        renderJS
          ( effectfulAST
              ( ifE
                  condE
                  (callMethod (UnsafeObject "el") "getAttribute" (arg (string "a") <: RecNil))
                  (callMethod (UnsafeObject "el") "getAttribute" (arg (string "b") <: RecNil))
              )
          )
          @?= "let n0;\nif (cond()) {n0 = el.getAttribute(\"a\");}\nelse {n0 = el.getAttribute(\"b\");}\nn0"
    , testCase "ifE of assign vs number keeps the result bind" $
        renderJS
          ( effectfulAST
              ( ifE
                  condE
                  (UnsafeObjectAssign (UnsafeObject "x") (expr (number 1)))
                  (expr (number 2))
              )
          )
          @?= "let n0;\nif (cond()) {n0 = x = 1.0;}\nelse {n0 = 2.0;}\nn0"
    , testCase "try_ of two Unit arms skips the result bind" $
        renderJS (effectfulAST (try_ noOp noOp))
          @?= "try {}\ncatch (n0) {}"
    , testCase "try_ of FFI vs Unit keeps the result bind" $
        renderJS (effectfulAST (try_ (ffi "foo" RecNil) noOp))
          @?= "let n0;\ntry {n0 = foo();}\ncatch (n1) {}\nn0"
    , testCase "stringCaseE of Unit arms is a switch statement" $ do
        let
          js =
            renderJS
              ( effectfulAST
                  ( fromSyntax $ do
                      k <- toSyntax (ffi "key" RecNil)
                      toSyntax
                        ( stringCaseE
                            (var k)
                            [ ("a", discard (ffi "foo" RecNil))
                            , ("b", discard (ffi "bar" RecNil))
                            ]
                            (discard (ffi "baz" RecNil))
                        )
                  )
              )
        T.isInfixOf "switch (" js @?= True
        T.isInfixOf "case \"a\":" js @?= True
        T.isInfixOf "case \"b\":" js @?= True
        T.isInfixOf "default:" js @?= True
        T.isInfixOf "break;" js @?= True
        T.isInfixOf "foo()" js @?= True
        T.isInfixOf "=;" js @?= False
    , testCase "stringCaseE of values keeps the result bind" $ do
        let
          js =
            renderJS
              ( effectfulAST
                  ( fromSyntax $ do
                      k <- toSyntax (ffi "key" RecNil)
                      toSyntax
                        ( stringCaseE
                            (var k)
                            [("a", expr (number 1))]
                            (expr (number 0))
                        )
                  )
              )
        T.isInfixOf "let n" js @?= True
        T.isInfixOf "switch (" js @?= True
        T.isInfixOf "case \"a\":" js @?= True
        T.isInfixOf " = 1.0" js @?= True
        T.isInfixOf " = 0.0" js @?= True
        T.isInfixOf "break;" js @?= True
        T.isInfixOf "=;" js @?= False
    , testCase "stringCaseE switches on the scrutinee ref" $ do
        let
          js =
            renderJS
              ( effectfulAST
                  ( fromSyntax $ do
                      x <- toSyntax (ffi "val" RecNil)
                      toSyntax
                        ( stringCaseE
                            (typeOf (var x))
                            [("number", discard (ffi "foo" RecNil))]
                            (discard (ffi "bar" RecNil))
                        )
                  )
              )
        T.isInfixOf "switch (typeof " js @?= True
        T.isInfixOf " = typeof" js @?= False
        T.isInfixOf "case \"number\":" js @?= True
    ]

stdlibTests :: TestTree
stdlibTests =
  testGroup
    "stdlib"
    [ testCase "Array.index evaluates" $
        evaluateNumber (Array.index numArray (number 1)) @?= 2
    , testCase "Array.index 1.9 is the integer slot" $
        evaluateNumber (Array.index numArray (number 1.9)) @?= 2
    , testCase "Array.index out of bounds throws" $ do
        r <- Ex.try (Ex.evaluate (evaluateNumber (Array.index numArray (number 9))))
        case r of
          Left (Ex.ErrorCall msg)
            | "evaluate: array index" `T.isPrefixOf` T.pack msg -> pure ()
            | otherwise -> assertFailure ("unexpected ErrorCall: " <> msg)
          Right n -> assertFailure ("expected throw, got " <> show n)
    , testCase "Array.index NaN is out of bounds" $ do
        r <-
          Ex.try (Ex.evaluate (evaluateNumber (Array.index numArray (number (0 / 0)))))
        case r of
          Left (Ex.ErrorCall msg)
            | "evaluate: array index" `T.isPrefixOf` T.pack msg -> pure ()
            | otherwise -> assertFailure ("unexpected ErrorCall: " <> msg)
          Right n -> assertFailure ("expected throw, got " <> show n)
    , testCase "Array.index truncates and throws out of bounds" $ do
        let
          js =
            renderJS (effectfulAST (with2 (ffi "xs" RecNil) (ffi "i" RecNil) Array.index))
        T.isInfixOf "Math.trunc" js @?= True
        T.isInfixOf "throw" js @?= True
    , testCase "Array.map evaluates" $
        case evaluate
          ( Eq
              (Array.map numArray (\x -> x + number 1))
              (Literal (ValueArray [ValueNumber 2, ValueNumber 3]))
          ) of
          ValueBool b -> b @?= True
    , testCase "Array.filter evaluates" $
        case evaluate
          ( Eq
              (Array.filter numArray (\x -> x .> number 1))
              (Literal (ValueArray [ValueNumber 2]))
          ) of
          ValueBool b -> b @?= True
    , testCase "Array.groupBy is first-seen [{key, items}]" $ do
        let
          xs = Literal (ValueArray [ValueNumber 1, ValueNumber 2, ValueNumber 1])
          grouped =
            Array.groupBy xs (\n -> if_ (n .== number 1) (string "one") (string "two"))
          keys = Array.map grouped (\g -> GetField @"key" g)
          firstItems = GetField @"items" (Array.index grouped (number 0))
        case evaluate (Eq keys (Literal (ValueArray [ValueString "one", ValueString "two"]))) of
          ValueBool b -> b @?= True
        evaluateNumber (Array.length firstItems) @?= 2
    , testCase "Array.groupBy is map/filter/reduce, not a helper" $ do
        let
          js = renderJS (pureAST (Array.groupBy numArray (\_ -> string "k")))
        T.isInfixOf "$groupBy" js @?= False
        T.isInfixOf ".reduce" js @?= True
        T.isInfixOf "\"key\"" js @?= True
    , testCase "Array.zipWith is Array.from, not a helper" $ do
        let
          js = renderJS (pureAST (Array.zipWith (+) numArray numArray))
        T.isInfixOf "$zipWith" js @?= False
        T.isInfixOf "Array.from" js @?= True
    , testCase "Classes.fmap Array" $
        case evaluate
          ( Eq
              (C.fmap (\x -> x + number 1) numArray)
              (Literal (ValueArray [ValueNumber 2, ValueNumber 3]))
          ) of
          ValueBool b -> b @?= True
    , testCase "Classes.liftA2 Option" $ do
        case evaluate (C.liftA2 (+) (some (number 2)) (some (number 3))) of
          ValueOption (Just (ValueNumber n)) -> n @?= 5
          _ -> assertFailure "expected Some 5"
        case evaluate (C.liftA2 (+) (none :: Expr f ('Option 'Number)) (some (number 3))) of
          ValueOption Nothing -> pure ()
          _ -> assertFailure "expected None"
    , testCase "Classes.traverse Array Option" $ do
        let
          pos x = if_ (x .> number 0) (some x) none
        case evaluate
          ( Eq
              (C.traverse pos numArray)
              (some (Literal (ValueArray [ValueNumber 1, ValueNumber 2])))
          ) of
          ValueBool b -> b @?= True
        case evaluate
          (C.traverse pos (Literal (ValueArray [ValueNumber 1, ValueNumber (-1)]))) of
          ValueOption Nothing -> pure ()
          _ -> assertFailure "expected None"
    , testCase "Classes.join Array" $
        case evaluate
          ( Eq
              ( C.join
                  ( Literal
                      ( ValueArray
                          [ ValueArray [ValueNumber 1]
                          , ValueArray [ValueNumber 2, ValueNumber 3]
                          ]
                      )
                  )
              )
              (Literal (ValueArray [ValueNumber 1, ValueNumber 2, ValueNumber 3]))
          ) of
          ValueBool b -> b @?= True
    , testCase "Classes.fmap Function" $
        evaluateNumber (apply (C.fmap (\y -> y + 1) (lambda (\x -> x * 2))) (number 3))
          @?= 7
    , testCase "Classes.bimap Result" $
        case evaluate
          (C.bimap id (\x -> x + 1) (ok (number 2) :: Expr f ('Result 'String 'Number))) of
          ValueResult (Right (ValueNumber n)) -> n @?= 3
          _ -> assertFailure "expected Ok 3"
    , testCase "Classes Semigroup Array" $
        case evaluate
          ( Eq
              (numArray C.<> Literal (ValueArray [ValueNumber 3]))
              (Literal (ValueArray [ValueNumber 1, ValueNumber 2, ValueNumber 3]))
          ) of
          ValueBool b -> b @?= True
    , testCase "Classes Category Function" $
        evaluateNumber
          ( apply
              (C.fmap (\y -> y + 1) (lambda (\x -> x * 2)) C.. lambda (\x -> x + 1))
              (number 3)
          )
          @?= 9
    , testCase "Classes.mzipWith Array" $
        case evaluate
          ( Eq
              ( C.mzipWith
                  (+)
                  numArray
                  (Literal (ValueArray [ValueNumber 10, ValueNumber 20, ValueNumber 30]))
              )
              (Literal (ValueArray [ValueNumber 11, ValueNumber 22]))
          ) of
          ValueBool b -> b @?= True
    , testCase "Classes.foldMap Array String" $
        case evaluate
          ( Eq
              (C.foldMap (\n -> if_ (n .== number 1) (string "a") (string "b")) numArray)
              (string "ab")
          ) of
          ValueBool b -> b @?= True
    , testCase "Classes.foldr is reduceRight" $ do
        evaluateNumber (C.foldr (-) (number 0) numArray) @?= -1
        evaluateNumber (C.foldl (-) (number 0) numArray) @?= -3
        T.isInfixOf
          ".reduceRight"
          (renderJS (pureAST (C.foldr (+) (number 0) numArray)))
          @?= True
    , testCase "LetRec value rhs evaluates" $
        evaluateNumber (letRec (\_ -> number 1 + number 2) (\n -> n)) @?= 3
    , testCase "Classes.mfix Function" $
        evaluateNumber
          ( apply
              (C.mfix (\a -> lambda (\r -> if_ (r .== number 0) (number 1) a)))
              (number 0)
          )
          @?= 1
    , testCase "Classes Semigroup Option is Maybe" $ do
        case evaluate (some (string "a") C.<> some (string "b")) of
          ValueOption (Just (ValueString s)) -> s @?= "ab"
          _ -> assertFailure "expected Some \"ab\""
        case evaluate ((none :: Expr f ('Option 'String)) C.<> some (string "x")) of
          ValueOption (Just (ValueString s)) -> s @?= "x"
          _ -> assertFailure "expected Some \"x\""
    , testCase "Classes.elem Array uses $valueEq" $
        case evaluate (C.elem (number 2) numArray) of
          ValueBool b -> b @?= True
    , testCase "Array.singleton is a one-element array" $ do
        evaluateNumber (Array.length (Array.singleton (number 7))) @?= 1
        T.isInfixOf "[]" (renderJS (pureAST (Array.singleton (number 7))))
          @?= False
    , testCase "unit array literal keeps its slots" $
        renderJS (pureAST (Literal (ValueArray [ValueUnit, ValueUnit])))
          @?= "[undefined, undefined]"
    , testCase "Array.join renders null as the empty string" $ do
        let
          opts =
            Literal
              ( ValueArray
                  [ValueOption Nothing, ValueOption (Just (ValueNumber 1))]
              )
        case evaluate (Array.join opts (string "-")) of
          ValueString s -> s @?= "-1"
        case evaluate (Show opts) of
          ValueString s -> s @?= ",1"
    , testCase "$valueEq helpers are defined once for two comparisons" $ do
        let
          js =
            ( renderJS
                ( pureProgram
                    ( toLambda
                        (\(a :: Expr f u) (b :: Expr f u) -> (structuralEq a b) .|| (structuralEq b a))
                    )
                )
            )
        T.count "const $valueEq" js @?= 1
        T.count "const $arrayEq" js @?= 1
        T.count "const $deepEqual" js @?= 1
        T.count "const $uint8ArrayEq" js @?= 1
        T.count "$valueEq(n" js @?= 2
    , testCase "Array.length of a literal folds" $
        renderJS (pureAST (Array.length numArray)) @?= "2.0"
    , testCase "Array.length of a binder renders as .length" $
        renderJS (pureAST (lambda (\xs -> Array.length xs)))
          @?= "function (n0) {return (n0.length)}"
    , testCase "Array.map renders as .map with a callback" $
        renderJS (pureAST (Array.map numArray (\x -> x + number 1)))
          @?= "[1.0, 2.0].map(function (n0) {return n0 + 1.0})"
    , testCase "Array.filterE renders an effectful callback" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      toSyntax_ $ Array.filterE numArray (\x -> ffi "pred" (arg x <: RecNil))
                      toSyntax noOp
                  )
              )
          )
          @?= "[1.0, 2.0].filter(function (n0) {return (pred(n0))});"
    , testCase "Array.map callback with an internal let is inlined when used once" $
        renderJS
          (pureAST (Array.map numArray (\x -> let_ (x + number 1) (\y -> y * 2))))
          @?= "[1.0, 2.0].map(function (n0) {return (n0 + 1.0) * 2.0})"
    , testCase "Array.join renders as .join" $
        renderJS (pureAST (Array.join numArray (string ",")))
          @?= "[1.0, 2.0].join(\",\")"
    , testCase "Array.push renders as a mutating .push call" $
        renderJS
          ( effectfulAST
              (fromSyntax (toSyntax (Array.push numArray (number 3)) *> toSyntax noOp))
          )
          @?= "[1.0, 2.0].push(3.0);"
    , testCase "Array.clear renders as length = 0" $
        renderJS
          ( effectfulAST
              (fromSyntax (toSyntax (Array.clear numArray) *> toSyntax noOp))
          )
          @?= "(a=>{a.length=0})([1.0, 2.0]);"
    , testCase "Array.pushMany renders one call with every argument" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  (toSyntax (Array.pushMany numArray [number 3, number 4]) *> toSyntax noOp)
              )
          )
          @?= "[1.0, 2.0].push(3.0, 4.0);"
    , testCase "Array.fromEffects renders an array literal" $
        renderJS (effectfulAST (Array.fromEffects [expr (number 1), expr (number 2)]))
          @?= "[1.0, 2.0]"
    , testCase "String.toUpper renders as .toUpperCase()" $
        renderJS (pureAST (Str.toUpper (string "hi"))) @?= "\"hi\".toUpperCase()"
    , testCase "Floating sin renders as Math.sin(x)" $
        renderJS (effectfulAST (with1 fooE sin)) @?= "Math.sin(foo())"
    , testCase "Floating sqrt evaluates" $
        evaluateNumber (sqrt (number 9)) @?= 3
    , testCase "Math.round matches JS half-toward-+Infinity semantics" $ do
        evaluateNumber (Math.round (number 2.5)) @?= 3
        evaluateNumber (Math.round (number (-2.5))) @?= (-2)
    , testCase "Floating (**) evaluates as Math.pow" $
        evaluateNumber (number 2 ** number 10) @?= 1024
    , testCase "Json.stringify renders as JSON.stringify(x)" $
        renderJS (pureAST (Json.stringify (number 1))) @?= "JSON.stringify(1.0)"
    , testCase "Console.log renders as console.log(x)" $
        renderJS
          ( effectfulAST
              (fromSyntax (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp))
          )
          @?= "console.log(\"hi\");"
    , testCase "Dom appendChild inlines single-use handles" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      p <- Dom.lookupId (string "p")
                      c <- Dom.createElement (string "div")
                      _ <- Dom.appendChild p c
                      toSyntax noOp
                  )
              )
          )
          @?= "document.getElementById(\"p\").appendChild(document.createElement(\"div\"));"
    , testCase "Dom appendChild keeps a handle that is used more than once" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      p <- Dom.lookupId (string "p")
                      c1 <- Dom.createElement (string "div")
                      c2 <- Dom.createElement (string "span")
                      _ <- Dom.appendChild p c1
                      _ <- Dom.appendChild p c2
                      toSyntax noOp
                  )
              )
          )
          @?= "const n0 = document.getElementById(\"p\");\nn0.appendChild(document.createElement(\"div\"));\nn0.appendChild(document.createElement(\"span\"));"
    , testCase "Canvas.getContext2d is typed as an Option" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      c <- Dom.lookupId (string "c")
                      ctx <- Canvas.getContext2d c
                      toSyntax
                        (Bind ctx (\o -> Lift (optionCase (var o) (string "no") (\_ -> string "ok"))))
                  )
              )
          )
          @?= "const n0 = document.getElementById(\"c\").getContext(\"2d\");\n(n0 === null ? \"no\" : \"ok\")"
    , testCase
        "optionCaseE of getContext plus a large object array tests that context"
        $ do
          let
            people = [Person ("p" <> T.pack (show i)) (fromIntegral i) | i <- [1 .. 15 :: Int]]
            js =
              renderJS
                ( effectfulAST
                    ( fromSyntax $ do
                        c <- Dom.lookupId (string "c")
                        ctx <- Canvas.getContext2d c
                        toSyntax $
                          Bind ctx $ \o ->
                            optionCaseE (var o) noOp $ \_ ->
                              stmts $ do
                                _ <- toSyntax (G.toObject (Group people))
                                done
                    )
                )
            jsIdent = T.takeWhile (\c -> c == 'n' || isDigit c)
            ctxIds =
              [ i
              | chunk <- T.splitOn "const " js
              , T.isInfixOf "getContext(\"2d\")" (T.takeWhile (/= ';') chunk)
              , let
                  i = jsIdent chunk
              , not (T.null i)
              ]
            nullId =
              let
                pre = fst (T.breakOn " === null" js)
                stem = T.dropWhileEnd (\c -> c == 'n' || isDigit c) pre
               in
                T.drop (T.length stem) pre
            -- `const a = b;` aliases can chain, so follow them rather than
            -- assuming the null test names the context binding directly.
            aliasOf i =
              [ rhs
              | chunk <- T.splitOn "const " js
              , let
                  (lhs, rest) = T.breakOn " = " chunk
              , lhs == i
              , let
                  rhs = T.takeWhile (/= ';') (T.drop 3 rest)
              , rhs == jsIdent rhs
              , not (T.null rhs)
              ]
            resolvesToCtx fuel i
              | i `elem` ctxIds = True
              | fuel <= (0 :: Int) = False
              | otherwise = any (resolvesToCtx (fuel - 1)) (aliasOf i)
          T.isInfixOf "=;" js @?= False
          (not (null ctxIds) && resolvesToCtx 8 nullId) @?= True
    , testCase "Canvas.fillRect renders a 2D call" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      _ <-
                        Canvas.fillRect
                          (UnsafeObject "ctx")
                          (number 0)
                          (number 0)
                          (number 10)
                          (number 20)
                      toSyntax noOp
                  )
              )
          )
          @?= "ctx.fillRect(0.0, 0.0, 10.0, 20.0);"
    , testCase "Canvas.rect renders a 2D call" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      _ <-
                        Canvas.rect
                          (UnsafeObject "ctx")
                          (number 1)
                          (number 2)
                          (number 3)
                          (number 4)
                      toSyntax noOp
                  )
              )
          )
          @?= "ctx.rect(1.0, 2.0, 3.0, 4.0);"
    , testCase "Canvas fillStyle is a Field" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      _ <-
                        Object.set @"fillStyle"
                          (UnsafeObject "ctx" :: Effect f ('MutableObject Canvas.Context2D))
                          (string "#f00")
                      toSyntax noOp
                  )
              )
          )
          @?= "ctx.fillStyle = \"#f00\";"
    , testCase "Storage.getItem is typed as an Option and dispatches via optionCase" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      v <- Storage.getItem Storage.localStorage (string "k")
                      toSyntax (expr (optionCase v (string "missing") (\x -> x)))
                  )
              )
          )
          @?= "const n0 = localStorage.getItem(\"k\");\n(n0 === null ? \"missing\" : n0)"
    , testCase "Map.lookup treats undefined as None" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( Map.withMap $ \m ->
                      do
                        v <- Map.lookup m (string "k")
                        toSyntax (expr (optionCase v (string "missing") (\x -> x)))
                  )
              )
          )
          @?= "const n0 = ((m, k) => { const v = m.get(k); return v === undefined ? null : v; })((()=>new Map())(), \"k\");\n(n0 === null ? \"missing\" : n0)"
    , testCase "Map.insert emits set" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( Map.withMap $ \m ->
                      do
                        _ <- Map.insert m (string "a") (number 1)
                        toSyntax noOp
                  )
              )
          )
          @?= "(()=>new Map())().set(\"a\", 1.0);"
    , testCase "Set.insert emits add" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( Set.withSet $ \s ->
                      do
                        _ <- Set.insert s (string "x")
                        toSyntax noOp
                  )
              )
          )
          @?= "(()=>new Set())().add(\"x\");"
    , testCase "Map.mapM_ emits forEach with (k,v) callback order" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( Map.withMap $ \m ->
                      Map.mapM_ (\_ _ -> toSyntax noOp) m
                  )
              )
          )
          @?= "((m, f) => { m.forEach((v, k) => f(k)(v)); })((()=>new Map())(), function (n0) {return (function (n1) {return})})"
    , testCase "multi-use Map.new stays one allocation (identity)" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( Map.withMap $ \m ->
                      do
                        _ <- Map.insert m (string "a") (number 1)
                        _ <- Map.insert m (string "b") (number 2)
                        toSyntax noOp
                  )
              )
          )
          @?= "const n0 = (()=>new Map())();\nn0.set(\"a\", 1.0);\nn0.set(\"b\", 2.0);"
    , testCase "multi-use Set.new stays one allocation (identity)" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( Set.withSet $ \s ->
                      do
                        _ <- Set.insert s (string "a")
                        _ <- Set.insert s (string "b")
                        toSyntax noOp
                  )
              )
          )
          @?= "const n0 = (()=>new Set())();\nn0.add(\"a\");\nn0.add(\"b\");"
    , testCase "multi-use UnsafeObject stays one const (identity)" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      o <- fmap (expr . Var) $ toSyntax $ UnsafeObject "{}"
                      toSyntax_ $ UnsafeObjectAssign (UnsafeObjectGet o "a") (Lift (number 1))
                      toSyntax_ $ UnsafeObjectAssign (UnsafeObjectGet o "b") (Lift (number 2))
                      toSyntax noOp
                  )
              )
          )
          @?= "const n0 = {};\nn0.a = 1.0;\nn0.b = 2.0;"
    , -- A Uint8Array is mutable, so propagating the literal to each use
      -- would hand out separate arrays: whoever fills one would not be
      -- seen by whoever reads the other. Guarded by `isCheapValue`.
      testCase "multi-use Uint8Array literal stays one array (identity)" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      b <- yield (uint8Array (bytes [0, 0]))
                      toSyntax_ (ffi "fill" (arg (var b) <: RecNil))
                      toSyntax_ (ffi "read" (arg (var b) <: RecNil))
                      toSyntax noOp
                  )
              )
          )
          @?= "const n0 = new Uint8Array(2);\nfill(n0);\nread(n0);"
    , testCase "locationHash is window.location.hash, not a bracket key" $ do
        let
          js = renderJS (effectfulAST (fromSyntax (locationHash *> toSyntax noOp)))
        T.isInfixOf "window.location.hash" js @?= True
        T.isInfixOf "[\"location.hash\"]" js @?= False
    , testCase "forEach param name matches body uses" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      toSyntax_ $ forEach numArray (\x -> ffi "foo" (arg x <: RecNil))
                      toSyntax noOp
                  )
              )
          )
          @?= "[1.0, 2.0].forEach(function (n0) {return (foo(n0))});"
    , testCase "LambdaE of Unit does not emit return ()" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      toSyntax_ $ forEach numArray (\_ -> noOp)
                      toSyntax noOp
                  )
              )
          )
          @?= "[1.0, 2.0].forEach(function (n0) {return});"
    , testCase "onClick assigns the DOM onclick property" $
        T.isInfixOf
          ".onclick ="
          ( renderJS
              ( effectfulAST
                  ( fromSyntax
                      ( do
                          el <- Dom.lookupId (string "b")
                          onClick el $ \_ -> noOp
                          toSyntax noOp
                      )
                  )
              )
          )
          @?= True
    , testCase "NaN .== NaN is false" $ do
        case evaluate (number (0 / 0) .== number (0 / 0)) of
          ValueBool b -> b @?= False
        renderJS (pureAST (number (0 / 0) .== number (0 / 0))) @?= "false"
    , testCase ".== on number exprs uses === without eq helpers" $ do
        let
          js =
            renderJS
              ( pureAST
                  (toLambda (\(a :: Expr f 'Number) (b :: Expr f 'Number) -> (a + b) .== (a + b)))
              )
        T.count "const $valueEq" js @?= 0
        T.isInfixOf "===" js @?= True
    , testCase "bound Number .== uses === (not $valueEq)" $ do
        let
          js =
            renderJS
              ( pureAST
                  (toLambda (\(a :: Expr f 'Number) (_ :: Expr f 'Number) -> a .== number 1))
              )
        T.isInfixOf "$valueEq" js @?= False
        T.isInfixOf "===" js @?= True
    , testCase "$valueEq helper includes null/object fast-path" $ do
        let
          (_, body) = jsHelperValueEq
        T.isInfixOf "typeof" body @?= True
        T.isInfixOf "null" body @?= True
    , testCase "frozen Number literals fold to === in .==" $ do
        let
          js =
            renderJS
              (pureAST (number 1 .== number 1))
        T.isInfixOf "true" js @?= True
        T.isInfixOf "$valueEq" js @?= False
    , testCase ".== hoists $valueEq (=== then structural; never ==)" $ do
        let
          js = renderJS (effectfulAST (with2 fooE barE structuralEq))
        T.isInfixOf "$valueEq" js @?= True
        T.isInfixOf " == " js @?= False
    , testCase ".!= is !$valueEq" $ do
        let
          js = renderJS (effectfulAST (with2 fooE barE structuralNEq))
        T.isInfixOf "$valueEq" js @?= True
        T.isInfixOf "!($valueEq(" js @?= True
    , testCase "ffi takes an effectful function via ArgEffect" $
        renderJS
          ( effectfulAST
              ( ffi
                  "setTimeout"
                  (ArgEffect (LambdaE (\_ -> ffi "tick" RecNil)) <: arg (number 0) <: RecNil)
              )
          )
          @?= "setTimeout(function (n0) {return (tick())}, 0.0)"
    , testCase "requestAnimationFrame takes ArgEffect" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  (Timers.requestAnimationFrame (\_ -> ffi "tick" RecNil) *> toSyntax noOp)
              )
          )
          @?= "requestAnimationFrame(function (n0) {return (tick())});"
    , testCase "send emits xhr.send()" $
        renderJS
          (effectfulAST (fromSyntax (Ajax.send (UnsafeObject "xhr") *> toSyntax noOp)))
          @?= "xhr.send();"
    , testCase "sendPost emits xhr.send(body)" $
        renderJS
          ( effectfulAST
              (fromSyntax (Ajax.sendPost (UnsafeObject "xhr") (string "hi") *> toSyntax noOp))
          )
          @?= "xhr.send(\"hi\");"
    ]

goodPartsTests :: TestTree
goodPartsTests =
  testGroup
    "good parts"
    [ testCase "rem and bitwise evaluate" $ do
        evaluateNumber (rem_ (number 7) (number 3)) @?= 1
        evaluateNumber (bitAnd (number 7) (number 3)) @?= 3
        evaluateNumber (ushr (number (-1)) (number 0)) @?= 4294967295
    , testCase "parseInt_ requires a radix and evaluates" $
        evaluateNumber (parseInt_ (string "10") (number 16)) @?= 16
    , testCase "parseInt_ keeps an optional sign" $
        evaluateNumber (parseInt_ (string "-10") (number 10)) @?= -10
    , testCase "resultCase on ok" $
        evaluateNumber
          ( resultCase
              (ok (number 5) :: Expr f ('Result 'String 'Number))
              (\_ -> number 0)
              (\x -> x + 1)
          )
          @?= 6
    , testCase "ok of Unit emits undefined, not an empty property" $
        renderJS (pureAST (ok (Literal ValueUnit) :: Expr f ('Result 'String 'Unit)))
          @?= "{ok: true, value: undefined}"
    , testCase "resultCase picks .ok and unwraps .value" $
        let
          getR :: Effect f ('Result 'String 'Number)
          getR = ffi "r" RecNil
          js =
            renderJS
              ( effectfulAST
                  (Bind getR (\r -> Lift (resultCase (var r) (\_ -> number 0) id)))
              )
         in
          do
            T.isInfixOf ".ok" js @?= True
            T.isInfixOf ".value" js @?= True
    , testCase "orElse on none" $
        evaluateNumber (orElse (none :: Expr f ('Option 'Number)) (number 3)) @?= 3
    , testCase "reduce evaluates" $
        evaluateNumber (Array.reduce numArray (number 0) (\a x -> a + x)) @?= 3
    , testCase "arraySlice evaluates" $
        evaluateNumber
          (Array.index (Array.arraySlice numArray (number 1) (number 2)) (number 0))
          @?= 2
    , testCase "arraySlice negatives count from the end" $
        evaluateNumber
          (Array.index (Array.arraySlice numArray (number (-1)) (number 2)) (number 0))
          @?= 2
    , testCase "apply2 is curried Apply" $
        evaluateNumber
          ( apply2
              (toLambda (\(x :: Expr f 'Number) (y :: Expr f 'Number) -> x + y))
              (number 1)
              (number 2)
          )
          @?= 3
    , testCase "try_ of two Unit arms still skips the result bind" $
        renderJS (effectfulAST (try_ noOp noOp))
          @?= "try {}\ncatch (n0) {}"
    , testCase "throw_ renders throw" $
        renderJS (effectfulAST (throw_ (string "boom") :: Effect f 'Unit))
          @?= "throw \"boom\";"
    , testCase "regex is new RegExp, not a literal" $
        renderJS (pureAST (Regex.test (Regex.regex "ab") (string "xab")))
          @?= "new RegExp(\"ab\").test(\"xab\")"
    , testCase "regex source escapes quotes" $
        renderJS (pureAST (Regex.test (Regex.regex "a\"b") (string "x")))
          @?= "new RegExp(\"a\\\"b\").test(\"x\")"
    , testCase "uint8Array is new Uint8Array, not a JS Array" $
        renderJS (pureAST (uint8Array sampleArray))
          @?= "new Uint8Array([1, 2, 3])"
    , testCase "empty uint8Array is new Uint8Array(0)" $
        renderJS (pureAST (uint8Array emptyArray8))
          @?= "new Uint8Array(0)"
    , testCase "zero-filled uint8Array uses length, not a literal" $
        renderJS (pureAST (uint8Array (bytes [0, 0, 0])))
          @?= "new Uint8Array(3)"
    , testCase "newByteArray takes the size, not the bytes" $
        renderJS (effectfulAST (newByteArray (number 4)))
          @?= "(n => new Uint8Array(n))(4.0)"
    , testCase "seedLiveCells stamps sparse pairs into zeroed buffers" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      a <- fmap var (toSyntax (newByteArray (number 3)))
                      s <- fmap var (toSyntax (newByteArray (number 3)))
                      toSyntax_ (seedLiveCells a s [(0, 1), (2, 5)])
                      toSyntax noOp
                  )
              )
          )
          @?= "((a,s,p)=>{for(let k=0;k<p.length;k++){const t=p[k];a[t[0]]=1;s[t[0]]=t[1];}})((n => new Uint8Array(n))(3.0), (n => new Uint8Array(n))(3.0), [[0.0, 1.0], [2.0, 5.0]]);"
    , -- Allocation has identity: folding two occurrences together would
      -- hand the writer and the reader different arrays.
      testCase "multi-use newByteArray stays one allocation (identity)" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      b <- fmap var (toSyntax (newByteArray (number 2)))
                      toSyntax_ (ffi "fill" (arg b <: RecNil))
                      toSyntax_ (ffi "read" (arg b <: RecNil))
                      toSyntax noOp
                  )
              )
          )
          @?= "const n0 = (n => new Uint8Array(n))(2.0);\nfill(n0);\nread(n0);"
    , testCase "hasOwn uses Object.prototype.hasOwnProperty.call" $
        T.isInfixOf
          "Object.prototype.hasOwnProperty.call"
          (renderJS (effectfulAST (Object.hasOwn (UnsafeObject "o") (string "k"))))
          @?= True
    , testCase "create is Object.create" $
        renderJS
          (effectfulAST (Object.create (UnsafeObject "p") :: Effect f ('MutableObject ())))
          @?= "Object.create(p)"
    , testCase "obj literal quotes keys" $
        renderJS
          ( effectfulAST
              (Object.obj [Object.field @"x" (number 1)] :: Effect f ('MutableObject LitRow))
          )
          @?= "{\"x\": 1.0}"
    , testCase "frozen literal quotes keys" $
        renderJS
          ( pureAST
              (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
          )
          @?= "{\"x\": 1.0}"
    , testCase "typeof of FrozenLit parenthesizes the literal" $
        renderJS
          ( pureAST
              (typeOf (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow)))
          )
          @?= "typeof ({\"x\": 1.0})"
    , testCase "sort emits a binary compare callback" $
        renderJS (effectfulAST (Array.sort numArray (\a b -> a - b)))
          @?= "[1.0, 2.0].sort(function (n0, n1) {return n0 - n1})"
    , testCase "toSorted emits a binary compare callback" $
        renderJS (pureAST (Array.toSorted numArray (\a b -> a - b)))
          @?= "[1.0, 2.0].toSorted(function (n0, n1) {return n0 - n1})"
    , testCase "toSorted evaluates" $
        evaluateNumber
          (Array.index (Array.toSorted numArray (\a b -> a - b)) (number 1))
          @?= 2
    , testCase "toFn emits a binary function value" $
        renderJS
          ( effectfulAST
              ( ffi
                  "f"
                  (arg (toFn (\(a :: Expr f 'Number) (b :: Expr f 'Number) -> a + b)) <: RecNil)
              )
          )
          @?= "f(function (n0, n1) {return n0 + n1})"
    , testCase "toFn emits a ternary function value" $
        renderJS
          ( effectfulAST
              ( ffi
                  "f"
                  ( arg
                      ( toFn
                          (\(a :: Expr f 'Number) (b :: Expr f 'Number) (c :: Expr f 'Number) -> a + b + c)
                      )
                      <: RecNil
                  )
              )
          )
          @?= "f(function (n0, n1, n2) {return (n0 + n1) + n2})"
    , testCase "lambdaRow emits a nested unary function value" $
        renderJS
          ( pureAST
              ( lambdaRow @('[Param "x" 'Number, Param "y" 'Number]) $
                  \p -> p.x + p.y
              )
          )
          @?= "function (n0) {return (function (n1) {return (n0 + n1)})}"
    , testCase "ifE of throw vs number keeps the result bind" $
        renderJS (effectfulAST (ifE condE (throw_ "boom") (expr (number 1))))
          @?= "let n0;\nif (cond()) {throw \"boom\";}\nelse {n0 = 1.0;}\nn0"
    ]

genericTests :: TestTree
genericTests =
  testGroup
    "generic"
    [ testCase "toJS primitives evaluate" $ do
        G.fromValue (evaluate (G.toJS (3.5 :: Double))) @?= (3.5 :: Double)
        G.fromValue (evaluate (G.toJS True)) @?= True
        G.fromValue (evaluate (G.toJS ("hi" :: Text))) @?= ("hi" :: Text)
        G.fromValue (evaluate (G.toJS [1, 2 :: Double])) @?= [1, 2 :: Double]
        G.fromValue (evaluate (G.toJS (Just (1 :: Int)))) @?= Just (1 :: Int)
        G.fromValue (evaluate (G.toJS (Left "e" :: Either Text Double)))
          @?= (Left "e" :: Either Text Double)
        G.fromValue (evaluate (G.toJS sampleArray)) @?= sampleArray
    , testCase "toObject renders record fields" $
        renderJS (effectfulAST (G.toObject (Person "Ada" 36)))
          @?= "{\"fullName\": \"Ada\", \"years\": 36.0}"
    , testCase "toObject renders a ByteArray field as Uint8Array" $
        renderJS (effectfulAST (G.toObject (Packet sampleArray)))
          @?= "{\"octets\": new Uint8Array([1, 2, 3])}"
    , testCase "toObject renders list and Maybe fields" $
        renderJS (effectfulAST (G.toObject (Tagged "x" ["a", "b"] Nothing)))
          @?= "{\"label\": \"x\", \"tags\": [\"a\", \"b\"], \"nickname\": null}"
    , testCase "toObject Maybe record field is nullable object not Some wrapper" $
        renderJS (effectfulAST (G.toObject (Team (Just (Person "Ada" 36)))))
          @?= "{\"lead\": {\"fullName\": \"Ada\", \"years\": 36.0}}"
    , testCase "get on a Generic object uses derived Field" $
        renderJS
          ( effectfulAST
              ( fromSyntax $ do
                  o <- hold (G.toObject (Person "Ada" 36))
                  n <- Object.get @"fullName" o
                  yieldString n
              )
          )
          @?= "{\"fullName\": \"Ada\", \"years\": 36.0}.fullName"
    , testCase "record dot getField matches get" $
        renderJS
          ( effectfulAST
              ( fromSyntax $ do
                  o <- hold (G.toObject (Person "Ada" 36))
                  n <- o.fullName
                  yieldString n
              )
          )
          @?= "{\"fullName\": \"Ada\", \"years\": 36.0}.fullName"
    , testCase "record dot getField on Expr" $
        renderJS
          ( effectfulAST
              ( fromSyntax $ do
                  o <- toSyntax (G.toObject (Person "Ada" 36))
                  n <- (Var o).fullName
                  yieldString n
              )
          )
          @?= "{\"fullName\": \"Ada\", \"years\": 36.0}.fullName"
    , testCase "frozen record dot is a pure Expr" $
        renderJS
          ( pureAST
              ((Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow)).x)
          )
          @?= "1.0"
    , testCase "newRecord is an empty object of the Generic row" $
        renderJS (effectfulAST (G.newRecord @Person))
          @?= "{}"
    , testCase "toObjectArray is an array of records" $
        renderJS (effectfulAST (G.toObjectArray [Person "Ada" 36, Person "Bob" 40]))
          @?= "[{\"fullName\": \"Ada\", \"years\": 36.0}, {\"fullName\": \"Bob\", \"years\": 40.0}]"
    , testCase "toObjectArray of [] is a literal empty array" $
        renderJS (effectfulAST (G.toObjectArray ([] :: [Person])))
          @?= "[]"
    , testCase "record field of [Person] uses toObjectArray" $
        renderJS (effectfulAST (G.toObject (Group [Person "Ada" 36])))
          @?= "{\"members\": [{\"fullName\": \"Ada\", \"years\": 36.0}]}"
    , testCase "toSum nullary is a tagged object" $
        renderJS (effectfulAST (G.toSum Red))
          @?= "{\"tag\": \"Red\"}"
    , testCase "toSum unary payload is the value" $
        renderJS (effectfulAST (G.toSum (Circle 1.5)))
          @?= "{\"tag\": \"Circle\", \"payload\": 1.5}"
    , testCase "toSum n-ary payload is a quoted object" $
        renderJS (effectfulAST (G.toSum (Rect 2 3)))
          @?= "{\"tag\": \"Rect\", \"payload\": {\"0\": 2.0, \"1\": 3.0}}"
    , testCase "toSumArray is an array of sums" $
        renderJS (effectfulAST (G.toSumArray [Red, Blue]))
          @?= "[{\"tag\": \"Red\"}, {\"tag\": \"Blue\"}]"
    , testCase "record field of a sum uses toSum" $
        T.isInfixOf
          "\"Red\""
          (renderJS (effectfulAST (G.toObject (Badge Red))))
          @?= True
    , testCase "whenTag on a nullary ctor compares .tag" $ do
        let
          js =
            renderJS
              ( effectfulAST
                  (G.whenTag @"Red" (G.toSum Red) (\_ -> expr (string "yes")) (expr (string "no")))
              )
        T.isInfixOf ".tag" js @?= True
        T.isInfixOf "\"Red\"" js @?= True
        T.isInfixOf "===" js @?= True
        T.isInfixOf "$valueEq" js @?= False
    , testCase "whenTag unary payload is the value" $
        T.isInfixOf
          ".payload"
          ( renderJS
              ( effectfulAST
                  (G.whenTag @"Circle" (G.toSum (Circle 1.5)) (\r -> expr r) (expr (number 0)))
              )
          )
          @?= True
    , testCase "whenTag n-ary payload fields are gettable" $
        T.isInfixOf
          "[\"0\"]"
          ( renderJS
              ( effectfulAST
                  ( fromSyntax $ do
                      s <- hold (G.toSum (Rect 2 3))
                      toSyntax $
                        G.whenTag @"Rect"
                          s
                          ( \p -> fromSyntax $ do
                              w <- Object.get @"0" (Lift p)
                              yield w
                          )
                          (expr (number 0))
                  )
              )
          )
          @?= True
    , testCase "caseSum nullary checks every named tag" $ do
        let
          js =
            renderJS
              ( effectfulAST
                  ( G.caseSum @Color (ffi "color" RecNil)
                      $ G.on @"Red" (\_ -> expr (string "r"))
                      $ G.on @"Green" (\_ -> expr (string "g"))
                      $ G.on @"Blue" (\_ -> expr (string "b"))
                      $ G.CaseEnd
                  )
              )
        T.isInfixOf ".tag" js @?= True
        T.isInfixOf "===" js @?= True
        T.isInfixOf "$valueEq" js @?= False
        T.isInfixOf "\"Red\"" js @?= True
        T.isInfixOf "\"Green\"" js @?= True
        T.isInfixOf "\"Blue\"" js @?= True
        T.isInfixOf "throw" js @?= True
    , testCase "caseSum Case_ is a suffix wildcard" $ do
        let
          js =
            renderJS
              ( effectfulAST
                  ( G.caseSum @Color (ffi "color" RecNil)
                      $ G.on @"Red" (\_ -> expr (string "r"))
                      $ G.Case_ (\_ -> expr (string "other"))
                  )
              )
        T.isInfixOf "\"Red\"" js @?= True
        T.isInfixOf "\"Green\"" js @?= False
        T.isInfixOf "\"Blue\"" js @?= False
    , testCase "caseSum unary payload is the value" $
        T.isInfixOf
          ".payload"
          ( renderJS
              ( effectfulAST
                  ( G.caseSum @Shape (ffi "shape" RecNil)
                      $ G.on @"Circle" (\r -> expr r)
                      $ G.on @"Rect" (\_ -> expr (number 0))
                      $ G.CaseEnd
                  )
              )
          )
          @?= True
    , testCase "caseSum n-ary payload fields are gettable" $
        T.isInfixOf
          "[\"0\"]"
          ( renderJS
              ( effectfulAST
                  ( fromSyntax $ do
                      s <- hold (ffi "shape" RecNil)
                      toSyntax
                        $ G.caseSum @Shape s
                        $ G.on @"Circle" (\_ -> expr (number 0))
                        $ G.on @"Rect"
                          ( \p -> fromSyntax $ do
                              w <- Object.get @"0" (Lift p)
                              yield w
                          )
                        $ G.CaseEnd
                  )
              )
          )
          @?= True
    ]

optimizeTests :: TestTree
optimizeTests =
  testGroup
    "optimize"
    [ testCase "literal arithmetic folds" $
        renderJS (pureAST (number 1 + number 2)) @?= "3.0"
    , testCase "nested single-use lets fold" $
        renderJS (pureAST (let_ (number 1) (\x -> let_ (number 2) (\y -> y + x))))
          @?= "3.0"
    , testCase "cheap multi-use let is propagated and folded" $
        renderJS (pureAST (let_ (number 5) (\x -> x + x))) @?= "10.0"
    , testCase "multi-use outer keeps inner folded let" $
        renderJS
          (effectfulAST (with1 fooE (\x -> let_ (number 1 + number 1) (\y -> x + x + y))))
          @?= "const n0 = foo();\n(n0 + n0) + 2.0"
    , testCase "letRec rhs folds" $
        renderJS (pureAST (letRec (\_ -> number 1 + number 2) (\n -> n)))
          @?= "let n0;\nn0 = 3.0;\nn0"
    , testCase "dead pure let is dropped" $
        renderJS (pureAST (let_ (number 1) (\_ -> number 2))) @?= "2.0"
    , testCase "unused FFI let is kept as a statement" $
        renderJS (effectfulAST (Bind fooE (\_ -> Lift (number 1)))) @?= "foo();\n1.0"
    , testCase "top-level do-notation bind chain compiles" $ do
        let
          chain =
            foldr
              (\_ k -> toSyntax (ffi "step" RecNil) *> k)
              (toSyntax noOp)
              [1 .. 40 :: Int]
        out <- compileEffect readableConfig (fromSyntax chain)
        assertBool "emitted js" (T.length out > 20)
    , testCase "lambda application of a literal folds" $
        renderJS (pureAST (apply (lambda (\x -> x * 2)) (number 21))) @?= "42.0"
    , testCase "if_ of True takes the true branch" $
        renderJS (pureAST (if_ (bool True) (number 1) (number 2))) @?= "1.0"
    , testCase "literal is propagated under a lambda" $
        renderJS (pureAST (let_ (number 5) (\x -> lambda (\_ -> x + number 1))))
          @?= "function (n0) {return (6.0)}"
    , testCase "let inside a lambda folds" $
        renderJS (pureAST (lambda (\x -> let_ (number 1) (\y -> y + x))))
          @?= "function (n0) {return (1.0 + n0)}"
    , testCase "multi-use let inside a lambda stays inside the function" $
        renderJS (pureAST (lambda (\x -> let_ (x + x) (\y -> y + y))))
          @?= "function (n0) {const n1 = n0 + n0;\nreturn (n1 + n1)}"
    , testCase "array index of a literal folds" $
        renderJS (pureAST (Array.index numArray (number 0))) @?= "1.0"
    , testCase "let-bound frozen field is cheap and folds" $
        renderJS
          ( pureAST
              ( let_
                  (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
                  (\o -> o.x)
              )
          )
          @?= "1.0"
    , testCase "GetField does not DCE an impure sibling field" $
        renderJS
          ( pureAST
              ( ( Object.frozen
                    [ Object.field @"s" (Json.stringify (number 1))
                    , Object.field @"y" (number 2)
                    ] ::
                    Expr f ('Object LitRow)
                ).y
              )
          )
          @?= "{\"s\": JSON.stringify(1.0), \"y\": 2.0}.y"
    , testCase "duplicate frozen keys fold last-wins" $
        renderJS
          ( pureAST
              ( ( Object.frozen [Object.field @"x" (number 1), Object.field @"x" (number 2)] ::
                    Expr f ('Object LitRow)
                ).x
              )
          )
          @?= "2.0"
    , testCase "sin of 0 folds" $
        renderJS (pureAST (sin (number 0))) @?= "0.0"
    , testCase "sin of a non-zero literal is left to JS" $
        renderJS (pureAST (sin (number 1))) @?= "Math.sin(1.0)"
    , testCase "sinh of 0 folds" $
        renderJS (pureAST (sinh (number 0))) @?= "0.0"
    , testCase "sinh of a non-zero literal is Math.sinh" $
        renderJS (pureAST (sinh (number 1))) @?= "Math.sinh(1.0)"
    , testCase "unused closed-name stdlib is dropped" $
        renderJS (pureAST (let_ (Str.toUpper (string "hi")) (\_ -> number 1)))
          @?= "1.0"
    , testCase "unused stringify is kept (can throw)" $
        renderJS (pureAST (let_ (Json.stringify (number 1)) (\_ -> number 2)))
          @?= "JSON.stringify(1.0);\n2.0"
    , testCase "optionCase of a Literal ValueOption folds" $
        renderJS
          ( pureAST
              ( optionCase
                  (Literal (ValueOption (Just (ValueNumber 5))))
                  (number 0)
                  (\x -> x + 1)
              )
          )
          @?= "6.0"
    , testCase "optionCase of some of a folded literal peels" $
        renderJS
          (pureAST (optionCase (some (number 1 + number 2)) (number 0) (\x -> x + 1)))
          @?= "4.0"
    , testCase "if_ True takes the true branch" $
        renderJS (pureAST (if_ (bool True) (number 1) (number 99)))
          @?= "1.0"
    , testCase "false && folds the RHS" $
        renderJS (pureAST (And (bool False) (number 1 .== number 0)))
          @?= "false"
    , testCase "while false becomes a no-op" $
        renderJS (effectfulAST (while_ (expr (bool False)) (ffi "foo" RecNil)))
          @?= ""
    , testCase "ifE of True takes the true branch" $
        renderJS
          (effectfulAST (ifE (expr (bool True)) (ffi "foo" RecNil) (ffi "bar" RecNil)))
          @?= "foo()"
    , testCase "typeof of a literal folds" $
        renderJS (pureAST (typeOf (number 1))) @?= "\"number\""
    , testCase "typeof of Uint8Array folds to object" $
        renderJS (pureAST (typeOf (uint8Array sampleArray))) @?= "\"object\""
    , testCase "string Semigroup is Concat" $
        renderJS (pureAST (("a" :: Expr f 'String) <> "b")) @?= "\"ab\""
    , testCase "try_ renders try/catch" $
        renderJS (effectfulAST (try_ (ffi "foo" RecNil) (expr (number 0))))
          @?= "let n0;\ntry {n0 = foo();}\ncatch (n1) {n0 = 0.0;}\nn0"
    , testCase "optionCaseE of none takes the none branch" $
        renderJS
          ( effectfulAST
              ( optionCaseE
                  (none :: Expr f ('Option 'Number))
                  (ffi "missing" RecNil)
                  (\x -> expr x)
              )
          )
          @?= "missing()"
    , testCase "stringCaseE of a literal takes the matching arm" $
        renderJS
          ( effectfulAST
              ( stringCaseE
                  (string "a")
                  [("a", ffi "foo" RecNil), ("b", ffi "bar" RecNil)]
                  (ffi "baz" RecNil)
              )
          )
          @?= "foo()"
    , testCase "stringCaseE of a literal miss takes default" $
        renderJS
          ( effectfulAST
              ( stringCaseE
                  (string "z")
                  [("a", ffi "foo" RecNil)]
                  (ffi "baz" RecNil)
              )
          )
          @?= "baz()"
    , testCase "forRange array index uses the loop variable" $ do
        let
          eff =
            fromSyntax $ do
              coords <-
                bindExpr $
                  Array.fromEffects
                    [ Array.fromEffects [expr (number 1), expr (number 1)]
                    , Array.fromEffects [expr (number 2), expr (number 1)]
                    ]
              forRange_ (number 0) (Array.length coords) $ \k -> do
                let
                  cell = Array.index coords k
                  x = Array.index cell 0
                toSyntax_ $ ffi "sink" (arg x <: RecNil)
                done
          js = renderJS (effectfulAST eff)
        -- Row index must depend on the loop counter (not constant-folded to
        -- the first coordinate); column index 0 is expected to stay literal.
        T.isInfixOf "sink(" js @?= True
        T.isInfixOf "sink(1.0)" js @?= False
        T.isInfixOf "Math.trunc(n1)" js @?= True
    ]

-- | The IR optimizer runs instead of the PHOAS one above
-- 'optIrLargeThreshold', so the two must agree. These cases cover the
-- positions whose IR metadata used to report no free variables at all
-- (FFI arguments, method receivers, kernel operands, lambda bodies),
-- which silently deleted still-referenced bindings.
irParityTests :: TestTree
irParityTests =
  testGroup
    "ir parity"
    [ parity "multi-use bind" (with1 fooE (\x -> x + x))
    , parity "chained binds" (with2 fooE barE (\x y -> y + x))
    , parity "use under a lambda" $
        with1 fooE (\x -> lambda (\_ -> x + number 1))
    , parity "use in an if_ branch" $
        with2 fooE condE (\x c -> if_ c x (number 0))
    , parity "use on the && RHS" (with2 condE barE (\x y -> And y x))
    , parity "use inside an ffi argument" ffiArgUse
    , parity "use inside a method receiver" methodUse
    , parity "use in both a kernel and a lambda" kernelAndLambdaUse
    , parity "while body use" whileUse
    , parity "unused pure bind is dropped" unusedPureBind
    , parity "try_ result binding" tryBinding
    ]
 where
  parity :: String -> (forall f. Effect f u) -> TestTree
  parity name p =
    testCase name (renderJS (effectfulASTIr p) @?= renderJS (effectfulAST p))
  ffiArgUse :: Effect f 'Unit
  ffiArgUse =
    bindSyntax (fooE :: Effect f 'Number) (\x -> ffi "sink" (arg x <: RecNil))
  methodUse :: Effect f ('Array 'Number)
  methodUse =
    bindSyntax (ffi "list" RecNil :: Effect f ('Array 'Number)) $ \x ->
      expr (Array.map x (\y -> y + number 1))
  kernelAndLambdaUse :: Effect f 'Number
  kernelAndLambdaUse =
    bindSyntax (fooE :: Effect f 'Number) $ \x ->
      expr (x + Apply (lambda (\_ -> x * number 2)) (number 1))
  whileUse :: Effect f 'Unit
  whileUse = bindSyntax condE (\c -> while_ (expr c) (ffi "tick" RecNil))
  unusedPureBind :: Effect f 'Unit
  unusedPureBind =
    bindSyntax (expr (number 1)) (\_ -> ffi "foo" RecNil)
  tryBinding :: Effect f 'Number
  tryBinding = try_ (ffi "foo" RecNil) (expr (number 0))

flatSoATests :: TestTree
flatSoATests =
  testGroup
    "flat soa"
    [ testCase "column round-trip" $
        flatSoaColumnsRoundTrip kernelAndLambdaUse @?= True
    , testCase "program round-trip" $
        flatProgramRoundTrip kernelAndLambdaUse @?= True
    , testCase "optimize attaches fpPure" $
        flatSoaPureNodeCount (expr (number 1 + number 2)) > (0 :: Int) @?= True
    , testCase "constant fold chains" $
        renderJS (effectfulASTIr (expr ((number 1 + number 2) + number 3)))
          @?= renderJS (effectfulASTIr (expr (number 6)))
    , testCase "flat matches phoas on kernel" $
        renderJS (effectfulASTIr kernelAndLambdaUse)
          @?= renderJS (effectfulAST kernelAndLambdaUse)
    ]
 where
  kernelAndLambdaUse :: Effect f 'Number
  kernelAndLambdaUse =
    bindSyntax (fooE :: Effect f 'Number) $ \x ->
      expr (x + Apply (lambda (\_ -> x * number 2)) (number 1))

-- | Bind an effect and use its result in another effect.
bindSyntax :: Effect f a -> (Expr f a -> Effect f b) -> Effect f b
bindSyntax e k = fromSyntax $ do
  x <- toSyntax e
  toSyntax (k (Var x))

compilerTests :: TestTree
compilerTests =
  testGroup
    "compiler"
    [ testCase "passthrough is identity" $ do
        clearCompilerCache
        let
          src = "const x = 1 + 2;" :: Text
        out <- compileWith passthroughConfig src
        out @?= src
    , testCase "memory cache returns the same payload" $ do
        clearCompilerCache
        let
          cfg = CompilerConfig Passthrough MemoryCache False Minified
          src = "const x = 1 + 2;" :: Text
        a <- compileWith cfg src
        b <- compileWith cfg src
        a @?= b
        a @?= src
        clearCompilerCache
    , testCase "compilePure passthrough emits an IIFE" $ do
        clearCompilerCache
        out <- compilePure passthroughConfig (number 1 + number 2)
        out @?= renderJSCompact (pureProgram (number 1 + number 2))
        assertBool "IIFE wrapper present" ("(() => {" `T.isInfixOf` out)
        assertBool
          "result is returned so minifiers cannot DCE it"
          ("return" `T.isInfixOf` out)
    , testCase "disk cache roundtrips passthrough output" $ do
        clearCompilerCache
        tmp <- getTemporaryDirectory
        let
          dir = tmp </> "jshark-compiler-disk-test"
        removePathForcibly dir
        createDirectoryIfMissing True dir
        let
          cfg = CompilerConfig Passthrough (DiskCache dir) False Minified
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
        let
          dir = tmp </> "jshark-compiler-disk-mismatch"
        removePathForcibly dir
        createDirectoryIfMissing True dir
        let
          cfg = CompilerConfig Passthrough (DiskCache dir) False Minified
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
            let
              snippet = number 1 + number 2
              raw = renderJS (pureProgram snippet)
              cfg = CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Minified
            out <- compilePure cfg snippet
            assertBool "non-empty" (not (T.null out))
            assertBool "minifier changed the IIFE" (out /= raw)
            assertBool "stripped ESM export anchor" (not ("export" `T.isInfixOf` out))
            assertBool
              "result still an expression (no var binding left)"
              (not ("var " `T.isPrefixOf` out))
    , testCase "tryCompileWith reports missing esbuild" $ do
        clearCompilerCache
        mExe <- findExecutable "esbuild"
        mNpx <- findExecutable "npx"
        case (mExe, mNpx) of
          (Nothing, Nothing) -> do
            res <-
              tryCompileWith
                (CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Minified)
                "1+2;"
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
            let
              cfg =
                CompilerConfig
                  (Esbuild defaultEsbuildConfig {esbuildExtraArgs = ["--definitely-not-a-flag"]})
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
            let
              src = "(() => { return 1; })();" :: Text
              cfg =
                CompilerConfig
                  (Esbuild defaultEsbuildConfig {esbuildExtraArgs = ["--definitely-not-a-flag"]})
                  NoCache
                  True
                  Minified
            out <- compileWith cfg src
            out @?= src
    , testCase "readableConfig compileEffect is a snippet, not an IIFE" $ do
        clearCompilerCache
        out <-
          compileEffect
            readableConfig
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
        out <-
          compileEffect
            (CompilerConfig (Esbuild defaultEsbuildConfig) NoCache False Readable)
            fooE
        out @?= "foo()"
    , testCase "compileWith Readable skips the minifier even when a backend is set" $ do
        clearCompilerCache
        let
          src = "const x = 1 + 2;" :: Text
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
        out <-
          compileEffect
            readableConfig
            ( fromSyntax
                (toSyntax (ifE condE (expr (number 1)) (expr (number 2))) *> toSyntax noOp)
            )
        out @?= "let n0;\nif (cond()) {\n  n0 = 1.0;\n} else {\n  n0 = 2.0;\n}"
    ]

emptyArray8 :: ByteArray
emptyArray8 = bytes []

sampleArray :: ByteArray
sampleArray = bytes [1, 2, 3]
