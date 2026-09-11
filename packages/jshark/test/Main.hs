{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Array.Byte (ByteArray)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import FlatTest
  ( flatDirectPackDeterministic
  , flatDirectPackForRangeOk
  , flatDirectPackOptimizeStable
  , flatOpcodeRoundTripOk
  , flatSoaPureNodeCount
  , optIrEffectForRangeImpure
  )
import JShark
import qualified JShark.Ajax as Ajax
import JShark.Api
import qualified JShark.Api.Classes as C
import qualified JShark.Api.Generic as G
import JShark.Api.Params (Param)
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Array as Array
import qualified JShark.Canvas as Canvas
import JShark.Compiler
import JShark.Compiler.Codegen.Core (minifiedStyle)
import qualified JShark.Console as Console
import qualified JShark.Dom as Dom
import qualified JShark.Json as Json
import qualified JShark.Map as Map
import qualified JShark.Math as Math
import qualified JShark.Object as Object
import qualified JShark.Regex as Regex
import qualified JShark.Set as Set
import qualified JShark.Storage as Storage
import qualified JShark.String as Str
import qualified JShark.Timers as Timers
import qualified JShark.Worker as Worker
import Support
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "jshark"
    [ evaluatorTests
    , rewriteRuleTests
    , bigIntTests
    , codegenTests
    , controlFlowTests
    , stdlibTests
    , goodPartsTests
    , genericTests
    , optimizeTests
    , flatSoATests
    , compilerTests
    , ergonomicsTests
    ]

bigIntTests :: TestTree
bigIntTests =
  testGroup
    "bigint"
    [ testCase "bigInt 10 + bigInt 3 evaluates to 13" $
        evaluateBigInt (bigInt 10 + bigInt 3) @?= 13
    , testCase "2^80+1 stays exact" $
        evaluateBigInt (bigInt (2 ^ (80 :: Int) + 1)) @?= 2 ^ (80 :: Int) + 1
    , pureCodeCase "codegen emits 42n" (bigInt 42) "42n"
    , pureCodeCase "negative literal is parenthesized" (bigInt (-42)) "(-42n)"
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
    , testCase "negative shift throws" $
        assertThrows "negative" (evaluateBigInt (shl (bigInt 1) (bigInt (-1))))
    , testCase "toBigInt of an integer Number" $
        evaluateBigInt (toBigInt (number 10)) @?= 10
    , testCase "toBigInt of a non-integer Number throws" $
        assertThrows "not an integer" (evaluateBigInt (toBigInt (number 1.5)))
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
    ]

-- | GHC RULES fold literal-literal EDSL ops at compile time of the
-- client. These inspect the unoptimized tree; if a rule fails to fire
-- the case falls through to an AST node and the test fails.
rewriteRuleTests :: TestTree
rewriteRuleTests =
  testGroup
    "rewrite rules"
    [ testCase "plus folds literals" $
        case number 1 + number 2 of
          Literal (ValueNumber n) -> n @?= 3
          _ -> assertFailure "jshark/plus/lit"
    , testCase "times and minus fold literals" $ do
        case number 3 * number 4 of
          Literal (ValueNumber n) -> n @?= 12
          _ -> assertFailure "jshark/times/lit"
        case number 10 - number 3 of
          Literal (ValueNumber n) -> n @?= 7
          _ -> assertFailure "jshark/minus/lit"
    , testCase "div and negate fold literals" $ do
        case number 8 / number 2 of
          Literal (ValueNumber n) -> n @?= 4
          _ -> assertFailure "jshark/div/lit"
        case negate (number 5) of
          Literal (ValueNumber n) -> n @?= -5
          _ -> assertFailure "jshark/negate/lit"
    , testCase "concat folds string literals" $
        case string "ab" <> string "cd" of
          Literal (ValueString t) -> t @?= "abcd"
          _ -> assertFailure "jshark/concat/lit"
    , testCase "and/or fold boolean literals" $ do
        case bool True .&& bool False of
          Literal (ValueBool b) -> b @?= False
          _ -> assertFailure "jshark/and"
        case bool False .|| bool True of
          Literal (ValueBool b) -> b @?= True
          _ -> assertFailure "jshark/or"
    , testCase "and/or keep an impure left" $ do
        case (Json.stringify (number 1) .== string "1") .&& false_ of
          And _ (Literal (ValueBool False)) -> pure ()
          _ -> assertFailure "andE dropped impure left"
        case (Json.stringify (number 1) .== string "1") .|| true_ of
          Or _ (Literal (ValueBool True)) -> pure ()
          _ -> assertFailure "orE dropped impure left"
    , testCase "eq/ord fold number literals" $ do
        case number 1 .== number 1 of
          Literal (ValueBool b) -> b @?= True
          _ -> assertFailure "jshark/eq/num"
        case number 2 .< number 1 of
          Literal (ValueBool b) -> b @?= False
          _ -> assertFailure "jshark/lt/num"
    , testCase "if_ of a literal bool picks a branch" $
        case if_ (bool True) (number 1) (number 2) of
          Literal (ValueNumber n) -> n @?= 1
          _ -> assertFailure "jshark/if/true"
    , testCase "rem/bitAnd/shl fold literals" $ do
        case rem_ (number 10) (number 3) of
          Literal (ValueNumber n) -> n @?= 1
          _ -> assertFailure "jshark/rem/lit"
        case bitAnd (number 7) (number 3) of
          Literal (ValueNumber n) -> n @?= 3
          _ -> assertFailure "jshark/bitand/lit"
        case shl (number 23) (number 8) of
          Literal (ValueNumber n) -> n @?= 5888
          _ -> assertFailure "jshark/shl/lit"
        case rem_ (number (-10)) (number 3) of
          Literal (ValueNumber n) -> n @?= -1
          _ -> assertFailure "jshark/rem/neg"
        case ushr (number (-1)) (number 0) of
          Literal (ValueNumber n) -> n @?= 4294967295
          _ -> assertFailure "jshark/ushr/lit"
    , testCase "let_ of a literal betas" $
        case let_ (number 1) (\x -> x + x) of
          Literal (ValueNumber n) -> n @?= 2
          _ -> assertFailure "jshark/let/lit"
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
    , evalBoolCase
        "frozen records compare by last-wins fields"
        ( Eq
            (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
            (Object.frozen [Object.field @"x" (number 1)])
        )
        True
    , testCase "Show of Result is JS String(object)" $
        case evaluate (Show (ok (number 5) :: Expr f ('Result 'String 'Number))) of
          ValueString s -> s @?= "[object Object]"
    , testCase "Uint8Array literals compare by contents" $ do
        case evaluate
          ( structuralEq
              (uint8Array (packUint8 [1, 2, 3]))
              (uint8Array (packUint8 [1, 2, 3]))
          ) of
          ValueBool b -> b @?= True
        case evaluate
          (structuralEq (uint8Array (packUint8 [1, 2])) (uint8Array (packUint8 [1, 2, 3]))) of
          ValueBool b -> b @?= False
    , testCase "Show of Uint8Array is comma-joined bytes" $
        case evaluate (Show (uint8Array sampleArray)) of
          ValueString s -> s @?= "1,2,3"
    , testCase "typeof of Uint8Array is object" $
        case evaluate (typeOf (uint8Array sampleArray)) of
          ValueString s -> s @?= "object"
    ]

codegenTests :: TestTree
codegenTests =
  testGroup
    "codegen"
    [ effectCodeCase
        "nested single-use lets are both inlined"
        (with2 fooE barE (\x y -> y + x))
        "const n0 = foo();\nconst n1 = bar();\nn1 + n0"
    , effectCodeCase
        "let used more than once renders as a const binding"
        (with1 fooE (\x -> x + x))
        "const n0 = foo();\nn0 + n0"
    , effectCodeCase
        "let used once under a lambda is not inlined"
        (with1 fooE (\x -> lambda (\_ -> x + number 1)))
        "const n0 = foo();\nn1 => n0 + 1"
    , effectCodeCase
        "let used once in an if_ branch is not inlined"
        (with2 fooE condE (\x c -> if_ c x (number 0)))
        "const n0 = foo();\nconst n1 = cond();\n(n1 ? n0 : 0)"
    , effectCodeCase
        "let used once on the && RHS is not inlined"
        (with2 condE barE (\x y -> And y x))
        "const n0 = cond();\nconst n1 = bar();\nn1 && n0"
    , effectCodeCase
        "let used once on the && LHS is inlined"
        (with2 condE barE (\x y -> And x y))
        "const n0 = cond();\nconst n1 = bar();\nn0 && n1"
    , effectCodeCase
        "unknown function application renders as a direct call"
        (ApplyE (ffi "f" RecNil) fooE)
        "(f())(foo())"
    , effectCodeCase
        "ffiExpr with no args omits trailing call parens"
        (ffiExpr "globalThis.crossOriginIsolated===true" RecNil)
        "globalThis.crossOriginIsolated===true"
    , effectCodeCase
        "call FFI with no args appends trailing call parens"
        (ffi "performance.now" RecNil)
        "performance.now()"
    , effectCodeCase
        "ffiExpr typeof omits trailing call parens"
        (ffiExpr "typeof PIXI !== 'undefined'" RecNil)
        "typeof PIXI !== 'undefined'"
    , effectCodeCase
        "parenthesized IIFE FFI still invokes"
        (ffi "(function(){return 1})" RecNil)
        "(function(){return 1})()"
    , testCase "effectfulProgram wraps decls and the result in a JS IIFE" $
        renderJS (effectfulProgram (with1 fooE (\x -> x + x)))
          @?= "(() => {\n  const n0 = foo();\n  return n0 + n0;\n})()"
    , effectCodeCase
        "effectful console.log FFI call"
        (fromSyntax (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp))
        "console.log(\"hi\");"
    , pureCodeCase "OverloadedStrings Expr literal" ("hi" :: Expr f 'String) "\"hi\""
    , pureCodeCase
        "OverloadedStrings Value via Literal"
        (Literal ("hi" :: Value 'String))
        "\"hi\""
    , pureCodeCase "Num Value literal via Literal" (Literal (3 :: Value 'Number)) "3"
    , pureCodeCase "Num Expr literal" (3 :: Expr f 'Number) "3"
    , testCase "Num Value host arithmetic" $
        case ((1 + 2 * 3) :: Value 'Number) of
          ValueNumber n -> n @?= 7
    , pureCodeCase
        "Fractional Value via Literal"
        (Literal ((1 / 2) :: Value 'Number))
        "0.5"
    , pureCodeCase
        "emptyArray renders as []"
        (emptyArray :: Expr f ('Array 'Number))
        "[]"
    , effectCodeCase
        "toString renders String(x)"
        (with1 fooE toString)
        "const n0 = foo();\nString(n0)"
    , effectCodeCase
        "assign is Object.assign"
        (fromSyntax (assign (UnsafeObject "dst") (UnsafeObject "src")))
        "Object.assign(dst, src);"
    , effectContains
        "whenSomeE binds then option-cases"
        ( fromSyntax
            ( whenSomeE (ffi "opt" RecNil :: Effect f ('Option 'String)) $ \x ->
                Console.log x *> done
            )
        )
        ["opt()", "=== null"]
    , effectContains
        "loop0 is a recursive zero-arg function"
        ( fromSyntax
            ( loop0
                (\_ -> Console.log ("p" :: Expr f 'String) *> done)
                (\_ -> Console.log ("w" :: Expr f 'String) *> done)
            )
        )
        ["=>", "console.log(\"p\")", "console.log(\"w\")"]
    , testCase "foreverFrame reschedules requestAnimationFrame" $
        T.count
          "requestAnimationFrame"
          (renderJS (effectfulAST (fromSyntax (Timers.foreverFrame (\_ -> done)))))
          @?= 2
    , effectContains
        "foreverTick reschedules setTimeout"
        (fromSyntax (Timers.foreverTick (\_ -> done)))
        ["setTimeout", "performance.now"]
    ]

controlFlowTests :: TestTree
controlFlowTests =
  testGroup
    "control flow"
    [ testCase "if_ picks the true branch" $
        evaluateNumber (if_ (bool True) (number 1) (number 2)) @?= 1
    , testCase "if_ picks the false branch" $
        evaluateNumber (if_ (bool False) (number 1) (number 2)) @?= 2
    , effectCodeCase
        "if_ renders as a ternary"
        (with1 condE (\c -> if_ c (number 1) (number 2)))
        "const n0 = cond();\n(n0 ? 1 : 2)"
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
    , effectCodeCase
        "ifE renders an if/else statement with a shared result variable"
        ( fromSyntax
            (toSyntax (ifE condE (expr (number 1)) (expr (number 2))) *> toSyntax noOp)
        )
        "(cond() ? 1 : 2);"
    , effectContains
        "whileE re-emits an FFI condition"
        (fromSyntax (toSyntax_ (while_ condE (ffi "foo" RecNil)) *> toSyntax noOp))
        ["while (cond())", "foo();"]
    , effectContains
        "forRange_ emits a C-style for loop"
        ( fromSyntax
            ( toSyntax_
                ( forRange (number 0) (number 3) $ \i ->
                    discard (u8Set (uint8Array (packUint8 [0])) i (number 1))
                )
                *> toSyntax noOp
            )
        )
        ["for (let n0 = 0; n0 < 3; n0++)", "new Uint8Array(1)[n0] = 1;"]
    , effectContainsWith
        minifiedStyle
        "flat forRange_ emits u8Set in loop body"
        ( fromSyntax
            ( toSyntax_
                ( forRange (number 0) (number 3) $ \i ->
                    discard (u8Set (uint8Array (packUint8 [0])) i (number 1))
                )
                *> toSyntax noOp
            )
        )
        ["for (let", "[n"]
    , effectContainsWith
        minifiedStyle
        "flat bindExpr forRange u8Set keeps loop"
        ( fromSyntax $ do
            buf <- bindExpr (newByteArray (number 4))
            _ <-
              forRange_ (number 0) (number 4) $ \i -> do
                toSyntax_ (u8Set buf i (number 255))
                done
            toSyntax noOp
        )
        ["for (let", "[n", "= 255;"]
    , effectContainsWith
        minifiedStyle
        "flat initPaletteRgba pattern keeps fill loop"
        ( fromSyntax $ do
            pal <- bindExpr (newByteArray (number 12))
            rgba <- bindExpr (newByteArray (number 16))
            _ <-
              forRange_ (number 0) (number 4) $ \s -> do
                toSyntax_ (u8Set rgba (s * number 4) (u8Index pal (s * number 3)))
                done
            toSyntax noOp
        )
        ["for (let", "[n"]
    , testCase "flat nested forRange u8Set keeps both loops" $ do
        let
          w = number 3
          h = number 3
          js =
            renderJS
              ( effectfulASTWith
                  minifiedStyle
                  ( fromSyntax $ do
                      buf <- bindExpr (newByteArray (w * h))
                      _ <-
                        forRange_ (number 0) h $ \y ->
                          forRange_ (number 0) w $ \x -> do
                            toSyntax_ (u8Set buf (y * w + x) (number 1))
                            done
                      toSyntax noOp
                  )
              )
        assertJSContains "for (let" js
        T.count "for (let" js @?= 2
    , effectContainsWith
        minifiedStyle
        "flat whenS u8Set keeps assignment"
        ( fromSyntax $ do
            buf <- bindExpr (newByteArray (number 1))
            _ <-
              whenS (number 1 .== number 1) $ do
                toSyntax_ (u8Set buf (number 0) (number 42))
                done
            toSyntax noOp
        )
        ["= 42;"]
    , effectCodeCase
        "multi-arg arrow FFI wraps IIFE"
        (ffi ("(a,b)=>a+b") (arg (number 1) <: arg (number 2) <: RecNil))
        "((a,b)=>a+b)(1, 2)"
    , effectCodeCaseWith
        minifiedStyle
        "flat multi-arg arrow FFI wraps IIFE"
        (ffi ("(a,b)=>a+b") (arg (number 1) <: arg (number 2) <: RecNil))
        "((a,b)=>a+b)(1, 2)"
    , pureCodeCase
        "u8Index renders direct Uint8Array indexing"
        (u8Index (uint8Array (packUint8 [7, 8, 9])) (number 1))
        "new Uint8Array([7, 8, 9])[1]"
    , effectCodeCase
        "when_ of Unit skips the result bind"
        (when_ condE (ffi "foo" RecNil))
        "if (cond()) {foo();}"
    , effectContains
        "discarded do keeps the last assignment"
        ( when_
            condE
            ( fromSyntax $ do
                _ <- toSyntax $ UnsafeObjectAssign (UnsafeObject "o") (expr (number 1))
                toSyntax $ UnsafeObjectAssign (UnsafeObject "p") (expr (number 2))
            )
        )
        ["o = 1", "p = 2"]
    , effectCodeCase
        "ifS of two CallMethods skips the result bind"
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
        "if (cond()) {el.setAttribute(\"k\", \"a\");}\nelse {el.setAttribute(\"k\", \"b\");}"
    , effectContains
        "ifE keeps impure prelude when condition folds"
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
        [".set("]
    , effectCodeCase
        "ifE of two getAttributes keeps the result bind"
        ( ifE
            condE
            (callMethod (UnsafeObject "el") "getAttribute" (arg (string "a") <: RecNil))
            (callMethod (UnsafeObject "el") "getAttribute" (arg (string "b") <: RecNil))
        )
        "let n0;\nif (cond()) {n0 = el.getAttribute(\"a\");}\nelse {n0 = el.getAttribute(\"b\");}\nn0"
    , effectCodeCase
        "ifE of assign vs number keeps the result bind"
        ( ifE
            condE
            (UnsafeObjectAssign (UnsafeObject "x") (expr (number 1)))
            (expr (number 2))
        )
        "let n0;\nif (cond()) {n0 = x = 1;}\nelse {n0 = 2;}\nn0"
    , effectCodeCase
        "try_ of two Unit arms skips the result bind"
        (try_ noOp noOp)
        "try {}\ncatch (n0) {}"
    , effectCodeCase
        "try_ of FFI vs Unit keeps the result bind"
        (try_ (ffi "foo" RecNil) noOp)
        "let n1;\ntry {n1 = foo();}\ncatch (n0) {}\nn1"
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
        T.isInfixOf " = 1" js @?= True
        T.isInfixOf " = 0" js @?= True
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
    , testCase "Array.index out of bounds throws" $
        assertThrows
          "evaluate: array index"
          (evaluateNumber (Array.index numArray (number 9)))
    , testCase "Array.index NaN is out of bounds" $
        assertThrows
          "evaluate: array index"
          (evaluateNumber (Array.index numArray (number (0 / 0))))
    , effectContains
        "Array.index truncates and throws out of bounds"
        (with2 (ffi "xs" RecNil) (ffi "i" RecNil) Array.index)
        ["$checkedIndex", "throw"]
    , evalBoolCase
        "Array.map evaluates"
        ( Eq
            (Array.map numArray (\x -> x + number 1))
            (Literal (ValueArray [ValueNumber 2, ValueNumber 3]))
        )
        True
    , evalBoolCase
        "Array.filter evaluates"
        ( Eq
            (Array.filter numArray (\x -> x .> number 1))
            (Literal (ValueArray [ValueNumber 2]))
        )
        True
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
    , testCase "Array.groupBy hoists $groupBy helper" $ do
        let
          js = renderJS (pureAST (Array.groupBy numArray (\_ -> string "k")))
        T.isInfixOf "const $groupBy =" js @?= True
        T.isInfixOf "=>" js @?= True
        T.isInfixOf ".reduce" js @?= True
        T.isInfixOf "key" js @?= True
        T.isInfixOf "($groupBy)(n0)(n1)" js @?= False
    , testCase "Array.groupBy hoists once when used twice" $ do
        let
          js =
            renderJS
              ( pureAST
                  ( let_ (Array.groupBy numArray (\_ -> string "a")) $ \g1 ->
                      let_ (Array.groupBy numArray (\_ -> string "b")) $ \g2 ->
                        Array.length g1 + Array.length g2
                  )
              )
        T.count "const $groupBy =" js @?= 1
        T.isInfixOf "const $groupBy = (arr, keyFn) =>" js @?= True
        T.isInfixOf "const $reduce = (seed, f) =>" js @?= True
    , testCase "binary hoists match in pureAST and effectfulAST" $ do
        let
          pureJs =
            renderJS (pureAST (Array.groupBy numArray (\_ -> string "k")))
          effJs =
            renderJS
              ( effectfulAST
                  (with2 (ffi "xs" RecNil) (ffi "i" RecNil) Array.index)
              )
        T.isInfixOf "=>" pureJs @?= True
        T.isInfixOf "($groupBy)(n0)(n1)" pureJs @?= False
        T.isInfixOf "const $checkedIndex =" effJs @?= True
        T.isInfixOf "$checkedIndex(" effJs @?= True
        T.isInfixOf "(($checkedIndex)(n0)(n1)" effJs @?= False
    , testCase "Array.zipWith hoists $zipWith helper" $ do
        let
          js = renderJS (pureAST (Array.zipWith (+) numArray numArray))
        T.isInfixOf "const $zipWith =" js @?= True
        T.isInfixOf "=>" js @?= True
        T.isInfixOf "($zipWith)(n0)(n1)" js @?= False
    , pureContains
        "Array.toSorted hoists $toSorted helper"
        (Array.toSorted numArray (\a b -> if_ (a .> b) (number 1) (number (-1))))
        ["const $toSorted =", "=>", ".toSorted"]
    , pureContains
        "Array.reduce hoists $reduce helper"
        (Array.reduce numArray (number 0) (\acc x -> acc + x))
        ["const $reduce = (seed, f) =>", ".reduce"]
    , testCase "Array.reduce hoists once when used twice" $ do
        let
          js =
            renderJS
              ( pureAST
                  ( let_ (Array.reduce numArray (number 0) (\acc x -> acc + x)) $ \a ->
                      let_ (Array.reduce numArray (number 1) (\acc x -> acc * x)) $ \b ->
                        a + b
                  )
              )
        T.count "const $reduce =" js @?= 1
        T.isInfixOf "const $reduce = (seed, f) =>" js @?= True
    , pureContains
        "hoisted $reduce keeps seed/f after a seed binder"
        ( Let (Just "seed") (number 1) $ \s ->
            Array.reduce numArray (Var s) (\acc x -> acc + x)
        )
        ["const seed = 1", "const $reduce = (seed, f) =>"]
    , evalBoolCase
        "Classes.fmap Array"
        ( Eq
            (C.fmap (\x -> x + number 1) numArray)
            (Literal (ValueArray [ValueNumber 2, ValueNumber 3]))
        )
        True
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
    , evalBoolCase
        "Classes.join Array"
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
        )
        True
    , testCase "Classes.fmap Function" $
        evaluateNumber (apply (C.fmap (\y -> y + 1) (lambda (\x -> x * 2))) (number 3))
          @?= 7
    , testCase "Classes.bimap Result" $
        case evaluate
          (C.bimap id (\x -> x + 1) (ok (number 2) :: Expr f ('Result 'String 'Number))) of
          ValueResult (Right (ValueNumber n)) -> n @?= 3
          _ -> assertFailure "expected Ok 3"
    , evalBoolCase
        "Classes Semigroup Array"
        ( Eq
            (numArray C.<> Literal (ValueArray [ValueNumber 3]))
            (Literal (ValueArray [ValueNumber 1, ValueNumber 2, ValueNumber 3]))
        )
        True
    , testCase "Classes Category Function" $
        evaluateNumber
          ( apply
              (C.fmap (\y -> y + 1) (lambda (\x -> x * 2)) C.. lambda (\x -> x + 1))
              (number 3)
          )
          @?= 9
    , evalBoolCase
        "Classes.mzipWith Array"
        ( Eq
            ( C.mzipWith
                (+)
                numArray
                (Literal (ValueArray [ValueNumber 10, ValueNumber 20, ValueNumber 30]))
            )
            (Literal (ValueArray [ValueNumber 11, ValueNumber 22]))
        )
        True
    , evalBoolCase
        "Classes.foldMap Array String"
        ( Eq
            (C.foldMap (\n -> if_ (n .== number 1) (string "a") (string "b")) numArray)
            (string "ab")
        )
        True
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
    , evalBoolCase
        "Classes.elem Array uses $valueEq"
        (C.elem (number 2) numArray)
        True
    , testCase "Array.singleton is a one-element array" $ do
        evaluateNumber (Array.length (Array.singleton (number 7))) @?= 1
        T.isInfixOf "[]" (renderJS (pureAST (Array.singleton (number 7))))
          @?= False
    , pureCodeCase
        "unit array literal keeps its slots"
        (Literal (ValueArray [ValueUnit, ValueUnit]))
        "[undefined, undefined]"
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
    , pureCodeCase "Array.length of a literal folds" (Array.length numArray) "2"
    , pureCodeCase
        "Array.length of a binder renders as .length"
        (lambda (\xs -> Array.length xs))
        "n0 => n0.length"
    , pureCodeCase
        "Array.map renders as .map with a callback"
        (Array.map numArray (\x -> x + number 1))
        "[1, 2].map(n0 => n0 + 1)"
    , effectCodeCase
        "Array.filterE renders an effectful callback"
        ( fromSyntax
            ( do
                toSyntax_ $ Array.filterE numArray (\x -> ffi "pred" (arg x <: RecNil))
                toSyntax noOp
            )
        )
        "[1, 2].filter(n0 => pred(n0));"
    , pureCodeCase
        "Array.map callback with an internal let is inlined when used once"
        (Array.map numArray (\x -> let_ (x + number 1) (\y -> y * 2)))
        "[1, 2].map(n0 => {const n1 = n0 + 1;\nreturn n1 * 2})"
    , pureCodeCase
        "Array.join renders as .join"
        (Array.join numArray (string ","))
        "[1, 2].join(\",\")"
    , effectCodeCase
        "Array.push renders as a mutating .push call"
        (fromSyntax (toSyntax (Array.push numArray (number 3)) *> toSyntax noOp))
        "[1, 2].push(3);"
    , effectCodeCase
        "Array.clear renders as length = 0"
        (fromSyntax (toSyntax (Array.clear numArray) *> toSyntax noOp))
        "[1, 2].length = 0;"
    , effectCodeCase
        "Array.pushMany renders one call with every argument"
        ( fromSyntax
            (toSyntax (Array.pushMany numArray [number 3, number 4]) *> toSyntax noOp)
        )
        "[1, 2].push(3, 4);"
    , effectCodeCase
        "Array.fromEffects renders an array literal"
        (Array.fromEffects [expr (number 1), expr (number 2)])
        "[1, 2]"
    , pureCodeCase
        "String.toUpper renders as .toUpperCase()"
        (Str.toUpper (string "hi"))
        "\"hi\".toUpperCase()"
    , effectCodeCase
        "Floating sin renders as Math.sin(x)"
        (with1 fooE sin)
        "const n0 = foo();\nMath.sin(n0)"
    , testCase "Floating sqrt evaluates" $
        evaluateNumber (sqrt (number 9)) @?= 3
    , testCase "Math.round matches JS half-toward-+Infinity semantics" $ do
        evaluateNumber (Math.round (number 2.5)) @?= 3
        evaluateNumber (Math.round (number (-2.5))) @?= (-2)
    , testCase "Floating (**) evaluates as Math.pow" $
        evaluateNumber (number 2 ** number 10) @?= 1024
    , pureCodeCase
        "Json.stringify renders as JSON.stringify(x)"
        (Json.stringify (number 1))
        "JSON.stringify(1)"
    , effectCodeCase
        "Console.log renders as console.log(x)"
        (fromSyntax (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp))
        "console.log(\"hi\");"
    , effectCodeCase
        "Dom appendChild inlines single-use handles"
        ( fromSyntax
            ( do
                p <- Dom.lookupId (string "p")
                c <- Dom.createElement (string "div")
                _ <- Dom.appendChild p c
                toSyntax noOp
            )
        )
        "const n0 = document.getElementById(\"p\");\nconst n1 = document.createElement(\"div\");\nn0.appendChild(n1);"
    , effectCodeCase
        "Dom appendChild keeps a handle that is used more than once"
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
        "const n0 = document.getElementById(\"p\");\nconst n1 = document.createElement(\"div\");\nconst n2 = document.createElement(\"span\");\nn0.appendChild(n1);\nn0.appendChild(n2);"
    , effectCodeCase
        "Canvas.getContext2d is typed as an Option"
        ( fromSyntax
            ( do
                c <- Dom.lookupId (string "c")
                ctx <- Canvas.getContext2d c
                toSyntax
                  ( Bind
                      Nothing
                      ctx
                      (\o -> Lift (optionCase (var o) (string "no") (\_ -> string "ok")))
                  )
            )
        )
        "const n0 = document.getElementById(\"c\");\nconst n1 = ((el,d)=>el.getContext('2d',{desynchronized:!!d,alpha:false,willReadFrequently:false}))(n0, false);\nconst n2 = n1;\nconst n3 = n2;\n(n3 === null ? \"no\" : \"ok\")"
    , effectCodeCase
        "Canvas.getContext2dDesync requests desynchronized context"
        ( fromSyntax
            ( do
                c <- Dom.lookupId (string "c")
                ctx <- Canvas.getContext2dDesync c
                toSyntax
                  ( Bind
                      Nothing
                      ctx
                      (\o -> Lift (optionCase (var o) (string "no") (\_ -> string "ok")))
                  )
            )
        )
        "const n0 = document.getElementById(\"c\");\nconst n1 = ((el,d)=>el.getContext('2d',{desynchronized:!!d,alpha:false,willReadFrequently:false}))(n0, true);\nconst n2 = n1;\nconst n3 = n2;\n(n3 === null ? \"no\" : \"ok\")"
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
                          Bind Nothing ctx $ \o ->
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
              , T.isInfixOf "getContext('2d'" (T.takeWhile (/= ';') chunk)
                  || T.isInfixOf "getContext(\"2d\")" (T.takeWhile (/= ';') chunk)
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
    , effectCodeCase
        "Canvas.fillRect renders a 2D call"
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
        "ctx.fillRect(0, 0, 10, 20);"
    , effectCodeCase
        "Canvas.rect renders a 2D call"
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
        "ctx.rect(1, 2, 3, 4);"
    , effectCodeCase
        "Canvas fillStyle is a Field"
        ( fromSyntax
            ( do
                _ <-
                  Object.set @"fillStyle"
                    (UnsafeObject "ctx" :: Effect f ('MutableObject Canvas.Context2D))
                    (string "#f00")
                toSyntax noOp
            )
        )
        "ctx.fillStyle = \"#f00\";"
    , effectCodeCase
        "Storage.getItem is typed as an Option and dispatches via optionCase"
        ( fromSyntax
            ( do
                v <- Storage.getItem Storage.localStorage (string "k")
                toSyntax (expr (optionCase v (string "missing") (\x -> x)))
            )
        )
        "const n0 = localStorage.getItem(\"k\");\nconst n1 = n0;\n(n1 === null ? \"missing\" : n1)"
    , effectCodeCase
        "Map.lookup treats undefined as None"
        ( fromSyntax
            ( Map.withMap $ \m ->
                do
                  v <- Map.lookup m (string "k")
                  toSyntax (expr (optionCase v (string "missing") (\x -> x)))
            )
        )
        "const n0 = new Map();\nconst n1 = ((m, k) => { const v = m.get(k); return v === undefined ? null : v; })(n0, \"k\");\nconst n2 = n1;\n(n2 === null ? \"missing\" : n2)"
    , effectCodeCase
        "Map.insert emits set"
        ( fromSyntax
            ( Map.withMap $ \m ->
                do
                  _ <- Map.insert m (string "a") (number 1)
                  toSyntax noOp
            )
        )
        "const n0 = new Map();\nn0.set(\"a\", 1);"
    , effectCodeCase
        "Set.insert emits add"
        ( fromSyntax
            ( Set.withSet $ \s ->
                do
                  _ <- Set.insert s (string "x")
                  toSyntax noOp
            )
        )
        "const n0 = new Set();\nn0.add(\"x\");"
    , effectCodeCase
        "Map.mapM_ emits forEach with (k,v) callback order"
        ( fromSyntax
            ( Map.withMap $ \m ->
                Map.mapM_ (\_ _ -> toSyntax noOp) m
            )
        )
        "const n0 = new Map();\n((m, f) => { m.forEach((v, k) => f(k)(v)); })(n0, n1 => n2 => {})"
    , effectCodeCase
        "multi-use Map.new stays one allocation (identity)"
        ( fromSyntax
            ( Map.withMap $ \m ->
                do
                  _ <- Map.insert m (string "a") (number 1)
                  _ <- Map.insert m (string "b") (number 2)
                  toSyntax noOp
            )
        )
        "const n0 = new Map();\nn0.set(\"a\", 1);\nn0.set(\"b\", 2);"
    , effectCodeCase
        "multi-use Set.new stays one allocation (identity)"
        ( fromSyntax
            ( Set.withSet $ \s ->
                do
                  _ <- Set.insert s (string "a")
                  _ <- Set.insert s (string "b")
                  toSyntax noOp
            )
        )
        "const n0 = new Set();\nn0.add(\"a\");\nn0.add(\"b\");"
    , effectCodeCase
        "Map.fromEntries emits new Map(entries)"
        (Map.fromEntries (emptyArray :: Expr f ('Array 'Number)))
        "new Map([])"
    , effectCodeCase
        "Set.fromList emits new Set(values)"
        (Set.fromList (emptyArray :: Expr f ('Array 'String)))
        "new Set([])"
    , effectCodeCase
        "Worker.newWorker emits new Worker(url)"
        ( fromSyntax $ do
            w <- Worker.newWorker (string "w.js")
            toSyntax_ w
            toSyntax noOp
        )
        "new Worker(\"w.js\");"
    , effectCodeCase
        "multi-use UnsafeObject stays one const (identity)"
        ( fromSyntax
            ( do
                o <- fmap (expr . Var) $ toSyntax $ UnsafeObject "{}"
                toSyntax_ $ UnsafeObjectAssign (UnsafeObjectGet o "a") (Lift (number 1))
                toSyntax_ $ UnsafeObjectAssign (UnsafeObjectGet o "b") (Lift (number 2))
                toSyntax noOp
            )
        )
        "const n0 = {};\nn0.a = 1;\nn0.b = 2;"
    , -- A Uint8Array is mutable, so propagating the literal to each use
      -- would hand out separate arrays: whoever fills one would not be
      -- seen by whoever reads the other. Guarded by `isCheapValue`.
      effectCodeCase
        "multi-use Uint8Array literal stays one array (identity)"
        ( fromSyntax
            ( do
                b <- yield (uint8Array (packUint8 [0, 0]))
                toSyntax_ (ffi "fill" (arg (var b) <: RecNil))
                toSyntax_ (ffi "read" (arg (var b) <: RecNil))
                toSyntax noOp
            )
        )
        "const n0 = new Uint8Array(2);\nfill(n0);\nread(n0);"
    , testCase "locationHash is window.location.hash, not a bracket key" $ do
        let
          js = renderJS (effectfulAST (fromSyntax (locationHash *> toSyntax noOp)))
        T.isInfixOf "window.location.hash" js @?= True
        T.isInfixOf "[\"location.hash\"]" js @?= False
    , effectCodeCase
        "forEach param name matches body uses"
        ( fromSyntax
            ( do
                toSyntax_ $ forEach numArray (\x -> ffi "foo" (arg x <: RecNil))
                toSyntax noOp
            )
        )
        "[1, 2].forEach(n0 => foo(n0));"
    , effectCodeCase
        "LambdaE of Unit does not emit return ()"
        ( fromSyntax
            ( do
                toSyntax_ $ forEach numArray (\_ -> noOp)
                toSyntax noOp
            )
        )
        "[1, 2].forEach(n0 => {});"
    , effectContains
        "onClick assigns the DOM onclick property"
        ( fromSyntax
            ( do
                el <- Dom.lookupId (string "b")
                onClick el $ \_ -> noOp
                toSyntax noOp
            )
        )
        [".onclick ="]
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
    , testCase "$valueEq shim includes null/object fast-path" $ do
        let
          body = builtinSrc ValueEq
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
    , effectContains
        ".!= is !$valueEq"
        (with2 fooE barE structuralNEq)
        ["$valueEq", "!($valueEq("]
    , effectCodeCase
        "ffi takes an effectful function via ArgEffect"
        ( ffi
            "setTimeout"
            (ArgEffect (LambdaE (\_ -> ffi "tick" RecNil)) <: arg (number 0) <: RecNil)
        )
        "setTimeout(n0 => tick(), 0)"
    , effectCodeCase
        "requestAnimationFrame takes ArgEffect"
        ( fromSyntax
            (Timers.requestAnimationFrame (\_ -> ffi "tick" RecNil) *> toSyntax noOp)
        )
        "requestAnimationFrame(n0 => tick());"
    , effectCodeCase
        "send emits xhr.send()"
        (fromSyntax (Ajax.send (UnsafeObject "xhr") *> toSyntax noOp))
        "xhr.send();"
    , effectCodeCase
        "sendPost emits xhr.send(body)"
        (fromSyntax (Ajax.sendPost (UnsafeObject "xhr") (string "hi") *> toSyntax noOp))
        "xhr.send(\"hi\");"
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
    , pureCodeCase
        "ok of Unit emits undefined, not an empty property"
        (ok (Literal ValueUnit) :: Expr f ('Result 'String 'Unit))
        "{ok: true, value: undefined}"
    , effectContains
        "resultCase picks .ok and unwraps .value"
        ( Bind
            Nothing
            (ffi "r" RecNil :: Effect f ('Result 'String 'Number))
            (\r -> Lift (resultCase (var r) (\_ -> number 0) id))
        )
        [".ok", ".value"]
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
    , effectCodeCase
        "try_ of two Unit arms still skips the result bind"
        (try_ noOp noOp)
        "try {}\ncatch (n0) {}"
    , effectCodeCase
        "throw_ renders throw"
        (throw_ (string "boom") :: Effect f 'Unit)
        "throw \"boom\";"
    , pureCodeCase
        "regex is new RegExp, not a literal"
        (Regex.test (Regex.regex "ab") (string "xab"))
        "new RegExp(\"ab\").test(\"xab\")"
    , pureCodeCase
        "regex source escapes quotes"
        (Regex.test (Regex.regex "a\"b") (string "x"))
        "new RegExp(\"a\\\"b\").test(\"x\")"
    , pureCodeCase
        "uint8Array is new Uint8Array, not a JS Array"
        (uint8Array sampleArray)
        "new Uint8Array([1, 2, 3])"
    , pureCodeCase
        "empty uint8Array is new Uint8Array(0)"
        (uint8Array emptyArray8)
        "new Uint8Array(0)"
    , pureCodeCase
        "zero-filled uint8Array uses length, not a literal"
        (uint8Array (packUint8 [0, 0, 0]))
        "new Uint8Array(3)"
    , effectCodeCase
        "newByteArray takes the size, not the bytes"
        (newByteArray (number 4))
        "(n => new Uint8Array(n))(4)"
    , -- Allocation has identity: folding two occurrences together would
      -- hand the writer and the reader different arrays.
      effectCodeCase
        "multi-use newByteArray stays one allocation (identity)"
        ( fromSyntax
            ( do
                b <- fmap var (toSyntax (newByteArray (number 2)))
                toSyntax_ (ffi "fill" (arg b <: RecNil))
                toSyntax_ (ffi "read" (arg b <: RecNil))
                toSyntax noOp
            )
        )
        "const n0 = (n => new Uint8Array(n))(2);\nfill(n0);\nread(n0);"
    , effectContains
        "hasOwn uses Object.prototype.hasOwnProperty.call"
        (Object.hasOwn (UnsafeObject "o") (string "k"))
        ["Object.prototype.hasOwnProperty.call"]
    , effectCodeCase
        "create is Object.create"
        (Object.create (UnsafeObject "p") :: Effect f ('MutableObject ()))
        "Object.create(p)"
    , effectCodeCase
        "obj literal quotes keys"
        (Object.obj [Object.field @"x" (number 1)] :: Effect f ('MutableObject LitRow))
        "{x: 1}"
    , pureCodeCase
        "frozen literal quotes keys"
        (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
        "{x: 1}"
    , effectCodeCase
        "effectful frozen literal emits field values"
        ( fromSyntax
            ( Array.push_
                (mempty :: Expr f ('Array ('Object LitRow)))
                ( Object.frozen
                    [Object.field @"x" (number 3), Object.field @"y" (number 7)] ::
                    Expr f ('Object LitRow)
                )
            )
        )
        "[].push({x: 3, y: 7})"
    , pureCodeCase
        "typeof of FrozenLit parenthesizes the literal"
        (typeOf (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow)))
        "typeof {x: 1}"
    , effectCodeCase
        "sort emits a binary compare callback"
        (Array.sort numArray (\a b -> a - b))
        "[1, 2].sort((a, b) => a - b)"
    , testCase "toSorted emits a binary compare callback" $ do
        let
          js = renderJS (pureAST (Array.toSorted numArray (\a b -> a - b)))
        T.isInfixOf "const $toSorted =" js @?= True
        T.isInfixOf "=>" js @?= True
        T.isInfixOf ".toSorted" js @?= True
        T.isInfixOf "($toSorted)([1.0, 2.0])" js @?= False
    , testCase "toSorted evaluates" $
        evaluateNumber
          (Array.index (Array.toSorted numArray (\a b -> a - b)) (number 1))
          @?= 2
    , effectCodeCase
        "toFn emits a binary function value"
        ( ffi
            "f"
            (arg (toFn (\(a :: Expr f 'Number) (b :: Expr f 'Number) -> a + b)) <: RecNil)
        )
        "f((a, b) => a + b)"
    , pureCodeCase
        "optimized toFn keeps param name hints"
        ( toFn
            ( \(a :: Expr f 'Number) (b :: Expr f 'Number) ->
                let_ (number 1) (\_ -> a + b)
            )
        )
        "(a, b) => a + b"
    , effectCodeCase
        "toFn emits a ternary function value"
        ( ffi
            "f"
            ( arg
                ( toFn
                    (\(a :: Expr f 'Number) (b :: Expr f 'Number) (c :: Expr f 'Number) -> a + b + c)
                )
                <: RecNil
            )
        )
        "f((a, b, c) => (a + b) + c)"
    , pureCodeCase
        "lambdaRow emits a nested unary function value"
        ( lambdaRow @('[Param "x" 'Number, Param "y" 'Number]) $
            \p -> p.x + p.y
        )
        "x => y => x + y"
    , effectCodeCase
        "ifE of throw vs number keeps the result bind"
        (ifE condE (throw_ "boom") (expr (number 1)))
        "let n0;\nif (cond()) {throw \"boom\";}\nelse {n0 = 1;}\nn0"
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
    , effectCodeCase
        "toObject renders record fields"
        (G.toObject (Person "Ada" 36))
        "{fullName: \"Ada\", years: 36}"
    , effectCodeCase
        "toObject renders a ByteArray field as Uint8Array"
        (G.toObject (Packet sampleArray))
        "{octets: new Uint8Array([1, 2, 3])}"
    , effectCodeCase
        "toObject renders list and Maybe fields"
        (G.toObject (Tagged "x" ["a", "b"] Nothing))
        "{label: \"x\", tags: [\"a\", \"b\"], nickname: null}"
    , effectCodeCase
        "toObject Maybe record field is nullable object not Some wrapper"
        (G.toObject (Team (Just (Person "Ada" 36))))
        "const n0 = {fullName: \"Ada\", years: 36};\n{lead: n0}"
    , effectCodeCase
        "get on a Generic object uses derived Field"
        ( fromSyntax $ do
            o <- hold (G.toObject (Person "Ada" 36))
            n <- Object.get @"fullName" o
            yieldString n
        )
        "const n0 = {fullName: \"Ada\", years: 36};\nn0.fullName"
    , effectCodeCase
        "record dot getField matches get"
        ( fromSyntax $ do
            o <- hold (G.toObject (Person "Ada" 36))
            n <- o.fullName
            yieldString n
        )
        "const n0 = {fullName: \"Ada\", years: 36};\nn0.fullName"
    , effectCodeCase
        "record dot getField on Expr"
        ( fromSyntax $ do
            o <- toSyntax (G.toObject (Person "Ada" 36))
            n <- (Var o).fullName
            yieldString n
        )
        "const n0 = {fullName: \"Ada\", years: 36};\nn0.fullName"
    , pureCodeCase
        "frozen record dot is a pure Expr"
        ((Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow)).x)
        "1"
    , effectCodeCase
        "newRecord is an empty object of the Generic row"
        (G.newRecord @Person)
        "{}"
    , effectCodeCase
        "toObjectArray is an array of records"
        (G.toObjectArray [Person "Ada" 36, Person "Bob" 40])
        "[{fullName: \"Ada\", years: 36}, {fullName: \"Bob\", years: 40}]"
    , effectCodeCase
        "toObjectArray of [] is a literal empty array"
        (G.toObjectArray ([] :: [Person]))
        "[]"
    , effectCodeCase
        "record field of [Person] uses toObjectArray"
        (G.toObject (Group [Person "Ada" 36]))
        "{members: [{fullName: \"Ada\", years: 36}]}"
    , effectCodeCase
        "toSum nullary is a tagged object"
        (G.toSum Red)
        "{tag: \"Red\"}"
    , effectCodeCase
        "toSum unary payload is the value"
        (G.toSum (Circle 1.5))
        "{tag: \"Circle\", payload: 1.5}"
    , effectCodeCase
        "toSum n-ary payload is a quoted object"
        (G.toSum (Rect 2 3))
        "{tag: \"Rect\", payload: {\"0\": 2, \"1\": 3}}"
    , effectCodeCase
        "toSumArray is an array of sums"
        (G.toSumArray [Red, Blue])
        "[{tag: \"Red\"}, {tag: \"Blue\"}]"
    , effectContains
        "record field of a sum uses toSum"
        (G.toObject (Badge Red))
        ["\"Red\""]
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
    , effectContains
        "whenTag unary payload is the value"
        (G.whenTag @"Circle" (G.toSum (Circle 1.5)) (\r -> expr r) (expr (number 0)))
        [".payload"]
    , effectContains
        "whenTag n-ary payload fields are gettable"
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
        ["[\"0\"]"]
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
    , effectContains
        "caseSum unary payload is the value"
        ( G.caseSum @Shape (ffi "shape" RecNil)
            $ G.on @"Circle" (\r -> expr r)
            $ G.on @"Rect" (\_ -> expr (number 0))
            $ G.CaseEnd
        )
        [".payload"]
    , effectContains
        "caseSum n-ary payload fields are gettable"
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
        ["[\"0\"]"]
    ]

optimizeTests :: TestTree
optimizeTests =
  testGroup
    "optimize"
    [ pureCodeCase "literal arithmetic folds" (number 1 + number 2) "3"
    , pureCodeCase
        "nested single-use lets fold"
        (let_ (number 1) (\x -> let_ (number 2) (\y -> y + x)))
        "3"
    , pureCodeCase
        "cheap multi-use let is propagated and folded"
        (let_ (number 5) (\x -> x + x))
        "10"
    , effectCodeCase
        "multi-use outer keeps inner folded let"
        (with1 fooE (\x -> let_ (number 1 + number 1) (\y -> x + x + y)))
        "const n0 = foo();\n(n0 + n0) + 2"
    , pureCodeCase
        "letRec rhs folds"
        (letRec (\_ -> number 1 + number 2) (\n -> n))
        "const n0 = 3;\nn0"
    , pureCodeCase "dead pure let is dropped" (let_ (number 1) (\_ -> number 2)) "2"
    , effectCodeCase
        "unused FFI let is kept as a statement"
        (Bind Nothing fooE (\_ -> Lift (number 1)))
        "foo();\n1"
    , testCase "top-level do-notation bind chain compiles" $ do
        let
          chain =
            foldr
              (\_ k -> toSyntax (ffi "step" RecNil) *> k)
              (toSyntax noOp)
              [1 .. 40 :: Int]
        out <- compileEffect readableConfig (fromSyntax chain)
        assertBool "emitted js" (T.length out > 20)
    , testCase "optIrEffect marks ForRange impure" $
        optIrEffectForRangeImpure @?= True
    , pureCodeCase
        "lambda application of a literal folds"
        (apply (lambda (\x -> x * 2)) (number 21))
        "const n0 = 21;\nn0 * 2"
    , pureCodeCase
        "if_ of True takes the true branch"
        (if_ (bool True) (number 1) (number 2))
        "1"
    , pureCodeCase
        "literal is propagated under a lambda"
        (let_ (number 5) (\x -> lambda (\_ -> x + number 1)))
        "n0 => 6"
    , pureCodeCase
        "let inside a lambda folds"
        (lambda (\x -> let_ (number 1) (\y -> y + x)))
        "n0 => 1 + n0"
    , pureCodeCase
        "multi-use let inside a lambda stays inside the function"
        (lambda (\x -> let_ (x + x) (\y -> y + y)))
        "n0 => {const n1 = n0 + n0;\nreturn n1 + n1}"
    , pureCodeCase
        "array index of a literal folds"
        (Array.index numArray (number 0))
        "1"
    , pureCodeCase
        "let-bound frozen field is cheap and folds"
        ( let_
            (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
            (\o -> o.x)
        )
        "const n0 = {x: 1};\nn0.x"
    , pureCodeCase
        "GetField does not DCE an impure sibling field"
        ( ( Object.frozen
              [ Object.field @"s" (Json.stringify (number 1))
              , Object.field @"y" (number 2)
              ] ::
              Expr f ('Object LitRow)
          ).y
        )
        "{s: JSON.stringify(1), y: 2}.y"
    , pureCodeCase
        "duplicate frozen keys fold last-wins"
        ( ( Object.frozen [Object.field @"x" (number 1), Object.field @"x" (number 2)] ::
              Expr f ('Object LitRow)
          ).x
        )
        "2"
    , pureCodeCase "sin of 0 folds" (sin (number 0)) "0"
    , pureCodeCase
        "sin of a non-zero literal is left to JS"
        (sin (number 1))
        "Math.sin(1)"
    , pureCodeCase "sinh of 0 folds" (sinh (number 0)) "0"
    , pureCodeCase
        "sinh of a non-zero literal is Math.sinh"
        (sinh (number 1))
        "Math.sinh(1)"
    , pureCodeCase
        "unused closed-name stdlib is dropped"
        (let_ (Str.toUpper (string "hi")) (\_ -> number 1))
        "1"
    , pureCodeCase
        "unused stringify is kept (can throw)"
        (let_ (Json.stringify (number 1)) (\_ -> number 2))
        "const n0 = JSON.stringify(1);\n2"
    , pureContains
        "impure && false keeps stringify"
        ((Json.stringify (number 1) .== string "1") .&& false_)
        ["JSON.stringify"]
    , pureContains
        "impure || true keeps stringify"
        ((Json.stringify (number 1) .== string "1") .|| true_)
        ["JSON.stringify"]
    , pureCodeCase
        "optionCase of a Literal ValueOption folds"
        ( optionCase
            (Literal (ValueOption (Just (ValueNumber 5))))
            (number 0)
            (\x -> x + 1)
        )
        "const n0 = 5;\nn0 + 1"
    , pureCodeCase
        "optionCase of some of a folded literal peels"
        (optionCase (some (number 1 + number 2)) (number 0) (\x -> x + 1))
        "const n0 = 3;\nn0 + 1"
    , pureCodeCase
        "if_ True takes the true branch"
        (if_ (bool True) (number 1) (number 99))
        "1"
    , pureCodeCase
        "false && folds the RHS"
        (And (bool False) (number 1 .== number 0))
        "false"
    , effectCodeCase
        "while false becomes a no-op"
        (while_ (expr (bool False)) (ffi "foo" RecNil))
        ""
    , effectCodeCase
        "ifE of True takes the true branch"
        (ifE (expr (bool True)) (ffi "foo" RecNil) (ffi "bar" RecNil))
        "foo()"
    , pureCodeCase
        "typeof of a literal folds"
        (typeOf (number 1))
        "\"number\""
    , pureCodeCase
        "typeof of Uint8Array folds to object"
        (typeOf (uint8Array sampleArray))
        "\"object\""
    , pureCodeCase
        "string Semigroup is Concat"
        (("a" :: Expr f 'String) <> "b")
        "\"ab\""
    , effectCodeCase
        "try_ renders try/catch"
        (try_ (ffi "foo" RecNil) (expr (number 0)))
        "let n1;\ntry {n1 = foo();}\ncatch (n0) {n1 = 0;}\nn1"
    , effectCodeCase
        "optionCaseE of none takes the none branch"
        ( optionCaseE
            (none :: Expr f ('Option 'Number))
            (ffi "missing" RecNil)
            (\x -> expr x)
        )
        "missing()"
    , effectCodeCase
        "stringCaseE of a literal takes the matching arm"
        ( stringCaseE
            (string "a")
            [("a", ffi "foo" RecNil), ("b", ffi "bar" RecNil)]
            (ffi "baz" RecNil)
        )
        "foo()"
    , effectCodeCase
        "stringCaseE of a literal miss takes default"
        ( stringCaseE
            (string "z")
            [("a", ffi "foo" RecNil)]
            (ffi "baz" RecNil)
        )
        "baz()"
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
        T.isInfixOf "$checkedIndex" js @?= True
        T.isInfixOf "(($checkedIndex)(n0)(n1)" js @?= False
    ]

flatSoATests :: TestTree
flatSoATests =
  testGroup
    "flat soa"
    [ testCase "optimize attaches pure flags" $
        flatSoaPureNodeCount (expr (number 1 + number 2)) > (0 :: Int) @?= True
    , testCase "constant fold chains" $
        renderJS
          (effectfulASTWith minifiedStyle (expr ((number 1 + number 2) + number 3)))
          @?= renderJS (effectfulASTWith minifiedStyle (expr (number 6)))
    , testCase "direct pack is deterministic (kernel)" $
        flatDirectPackDeterministic kernelAndLambdaUse @?= True
    , testCase "direct pack is deterministic (forRange u8set)" $
        flatDirectPackForRangeOk @?= True
    , testCase "optimize is stable on second pass" $
        flatDirectPackOptimizeStable kernelAndLambdaUse @?= True
    , testCase "every opcode decodes and re-encodes" $
        flatOpcodeRoundTripOk @?= True
    ]
 where
  kernelAndLambdaUse :: Effect f 'Number
  kernelAndLambdaUse = bindSyntax (fooE :: Effect f 'Number) $ \x ->
    expr (x + Apply (lambda (\_ -> x * number 2)) (number 1))

-- | Bind an effect and use its result in another effect ('with1' binds
-- in expression position; this binds in effect position).
bindSyntax :: Effect f a -> (Expr f a -> Effect f b) -> Effect f b
bindSyntax e k = fromSyntax $ do
  x <- toSyntax e
  toSyntax (k (Var x))

-- | Round-trips for the newer ergonomics surface ('toNumber',
-- 'whenNoneS', 'Dom.byId', typed event accessors, 'addEventListenerS',
-- 'compileEffectSyntax').
ergonomicsTests :: TestTree
ergonomicsTests =
  testGroup
    "ergonomics"
    [ effectCodeCase
        "toNumber coerces a string via Number()"
        ( fromSyntax
            ( do
                n <- toNumber (string "4.5")
                Console.log n
                done
            )
        )
        "const n0 = Number(\"4.5\");\nconsole.log(n0);"
    , effectCodeCase
        "whenNoneS runs the body only on none"
        ( fromSyntax
            ( do
                v <- Storage.getItem Storage.localStorage (string "k")
                _ <- whenNoneS v (toSyntax_ (ffi "seed" RecNil) *> done)
                done
            )
        )
        "const n0 = localStorage.getItem(\"k\");\nconst n1 = n0;\nif (n1 === null) {seed();}"
    , effectCodeCase
        "addEventListenerS + eventKey avoids stmts and annotations"
        ( fromSyntax
            ( do
                el <- Dom.byId "board"
                addEventListenerS "keydown" el $ \e -> do
                  k <- eventKey e
                  toSyntax_ (ffi "sink" (arg k <: RecNil))
                  done
                done
            )
        )
        "const n0 = document.getElementById(\"board\");\nn0.addEventListener(\"keydown\", n1 => {const n2 = n1.key;\nsink(n2);\nreturn});"
    , testCase "compileEffectSyntax absorbs fromSyntax" $ do
        out <-
          compileEffectSyntax
            readableConfig
            (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp)
        out @?= "console.log(\"hi\");"
    ]

compilerTests :: TestTree
compilerTests =
  testGroup
    "compiler"
    [ testCase "compilePure passthrough emits an IIFE" $ do
        out <- compilePure passthroughConfig (number 1 + number 2)
        out @?= renderJS (pureProgram (number 1 + number 2))
        assertBool "IIFE wrapper present" ("(() => {" `T.isInfixOf` out)
        assertBool
          "result is returned so minifiers cannot DCE it"
          ("return" `T.isInfixOf` out)
    , testCase "compilePure ignores configProgress stderr" $ do
        let
          prog = number 1 + number 2
          withProgress =
            defaultCompilerConfig {configProgress = True}
          eff =
            fromSyntax (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp)
        (_, capturedEffect) <-
          captureStderr $ compileEffectIO withProgress eff
        (_, capturedPure) <- captureStderr $ compilePure withProgress prog
        assertBool
          "effect timing line"
          (T.isInfixOf "compiled in" (T.pack capturedEffect))
        assertBool "pure silent" (not (T.isInfixOf "compiled in" (T.pack capturedPure)))
    , testCase "readableConfig compileEffect is a snippet, not an IIFE" $ do
        out <-
          compileEffect
            readableConfig
            (fromSyntax (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp))
        out @?= "console.log(\"hi\");"
    , testCase "readableConfig compilePure has no IIFE and inlines single-use lets" $ do
        out <- compileEffect readableConfig (with1 fooE (\x -> x + number 1))
        out @?= "const n0 = foo();\nn0 + 1;"
    , testCase "readableConfig keeps multi-use lets as const" $ do
        out <- compileEffect readableConfig (with1 fooE (\x -> x + x))
        out @?= "const n0 = foo();\nn0 + n0;"
    , testCase "callerBinderHint returns enclosing function name" $
        callerHintProbe () @?= Just "callerHintProbe"
    , testCase "readableConfig uses explicit let binder hints" $ do
        let
          prog :: Expr f 'Number
          prog = Let (Just "hintProbe") (sin (number 1)) (\x -> Var x + Var x)
        renderJS (pureAST prog)
          @?= "const hintProbe = Math.sin(1);\nhintProbe + hintProbe"
    , testCase "same-scope binder hints uniquify" $ do
        let
          prog :: Expr f 'Number
          prog =
            Let (Just "x") (number 1) $ \a ->
              Let (Just "x") (number 2) $ \b ->
                Var a + Var b
        renderJS (pureAST prog) @?= "const x = 1;\nconst n1 = 2;\nx + n1"
    , testCase "readableConfig names pure let binders from HasCallStack" $ do
        renderJS (pureAST readableLetSample)
          @?= "const readableLetSample = Math.sin(1);\nreadableLetSample + readableLetSample"
    , testCase "readableConfig names effect binders from HasCallStack" $ do
        out <- compileEffect readableConfig (fromSyntax readableBindSample)
        out
          @?= "const readableBindSample = foo();\nreadableBindSample + readableBindSample;"
    , testCase "prettyJS formats if/else when biome is on PATH" $ do
        requireBiome
        out <- prettyJS "if (cond()) {foo();} else {bar();}"
        out
          @?= "if (cond()) {\n  foo();\n} else {\n  bar();\n}"
    , testCase "prettyJS preserves braces inside strings" $ do
        requireBiome
        out <- prettyJS "foo(\"{;}\");"
        out @?= "foo(\"{;}\");"
    , testCase "prettyJS formats try/catch when biome is on PATH" $ do
        requireBiome
        out <- prettyJS "try {foo();} catch (n0) {bar();}"
        out
          @?= "try {\n  foo();\n} catch (n0) {\n  bar();\n}"
    , testCase "prettyJS leaves invalid IIFE unchanged when biome rejects it" $ do
        requireBiome
        out <- prettyJS "function () {return 1;}()"
        out @?= "function () {return 1;}()"
    , testCase "readableConfig pretty-prints ifE" $ do
        out <-
          compileEffect
            readableConfig
            ( fromSyntax
                (toSyntax (ifE condE (expr (number 1)) (expr (number 2))) *> toSyntax noOp)
            )
        out @?= "cond() ? 1 : 2;"
    , testCase "readableConfig Map.new is a snippet, not an IIFE" $ do
        out <-
          compileEffect
            readableConfig
            (fromSyntax (Map.withMap $ \m -> Map.clear m))
        out @?= "const n0 = new Map();\nn0.clear();"
        assertBool "no IIFE" (not ("(() => {" `T.isInfixOf` out))
    , testCase "readableConfig $valueEq shim is multiline" $ do
        out <- compileEffect readableConfig (with2 fooE barE structuralEq)
        assertBool "shim binding" ("const $valueEq =" `T.isInfixOf` out)
        assertBool "pretty body" ("{\n" `T.isInfixOf` out)
        assertBool "no IIFE" (not ("(() => {" `T.isInfixOf` out))
    , testCase "--readable sets OutputStyle Readable" $
        configStyle (applyCompilerArgs ["--readable"] defaultCompilerConfig)
          @?= Readable
    ]

emptyArray8 :: ByteArray
emptyArray8 = packUint8 []

sampleArray :: ByteArray
sampleArray = packUint8 [1, 2, 3]
