{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Control.Exception as Ex
import Data.Array.Byte (ByteArray)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
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
import qualified JShark.Console as Console
import qualified JShark.Dom as Dom
import JShark.FlatTest
  ( batchJobSlotTimingOk
  , flatDirectPackDeterministic
  , flatDirectPackForRangeOk
  , flatDirectPackOptimizeStable
  , flatSoaPureNodeCount
  , freezeEncColumnsOrderOk
  , lowerOptEffectRegressionOk
  , optIrEffectForRangeImpure
  )
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
          @?= "const n0 = foo();\nconst n1 = bar();\nn1 + n0"
    , testCase "let used more than once renders as a const binding" $
        renderJS (effectfulAST (with1 fooE (\x -> x + x)))
          @?= "const n0 = foo();\nn0 + n0"
    , testCase "let used once under a lambda is not inlined" $
        renderJS (effectfulAST (with1 fooE (\x -> lambda (\_ -> x + number 1))))
          @?= "const n0 = foo();\nn1 => n0 + 1"
    , testCase "let used once in an if_ branch is not inlined" $
        renderJS (effectfulAST (with2 fooE condE (\x c -> if_ c x (number 0))))
          @?= "const n0 = foo();\nconst n1 = cond();\n(n1 ? n0 : 0)"
    , testCase "let used once on the && RHS is not inlined" $
        renderJS (effectfulAST (with2 condE barE (\x y -> And y x)))
          @?= "const n0 = cond();\nconst n1 = bar();\nn1 && n0"
    , testCase "let used once on the && LHS is inlined" $
        renderJS (effectfulAST (with2 condE barE (\x y -> And x y)))
          @?= "const n0 = cond();\nconst n1 = bar();\nn0 && n1"
    , testCase "unknown function application renders as a direct call" $
        renderJS (effectfulAST (ApplyE (ffi "f" RecNil) fooE))
          @?= "(f())(foo())"
    , testCase "ffiExpr with no args omits trailing call parens" $
        renderJS
          (effectfulAST (ffiExpr "globalThis.crossOriginIsolated===true" RecNil))
          @?= "globalThis.crossOriginIsolated===true"
    , testCase "call FFI with no args appends trailing call parens" $
        renderJS (effectfulAST (ffi "performance.now" RecNil))
          @?= "performance.now()"
    , testCase "ffiExpr typeof omits trailing call parens" $
        renderJS
          ( effectfulAST
              (ffiExpr "typeof PIXI !== 'undefined'" RecNil)
          )
          @?= "typeof PIXI !== 'undefined'"
    , testCase "parenthesized IIFE FFI still invokes" $
        renderJS (effectfulAST (ffi "(function(){return 1})" RecNil))
          @?= "(function(){return 1})()"
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
        renderJS (pureAST (Literal (3 :: Value 'Number))) @?= "3"
    , testCase "Num Expr literal" $
        renderJS (pureAST (3 :: Expr f 'Number)) @?= "3"
    , testCase "Num Value host arithmetic" $
        case ((1 + 2 * 3) :: Value 'Number) of
          ValueNumber n -> n @?= 7
    , testCase "Fractional Value via Literal" $
        renderJS (pureAST (Literal ((1 / 2) :: Value 'Number))) @?= "0.5"
    , testCase "emptyArray renders as []" $
        renderJS (pureAST (emptyArray :: Expr f ('Array 'Number))) @?= "[]"
    , testCase "toString renders String(x)" $
        renderJS (effectfulAST (with1 fooE toString))
          @?= "const n0 = foo();\nString(n0)"
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
        T.isInfixOf "=>" js @?= True
        T.isInfixOf "console.log(\"p\")" js @?= True
        T.isInfixOf "console.log(\"w\")" js @?= True
    , testCase "foreverFrame reschedules requestAnimationFrame" $
        T.count
          "requestAnimationFrame"
          (renderJS (effectfulAST (fromSyntax (Timers.foreverFrame (\_ -> done)))))
          @?= 2
    , testCase "foreverTick reschedules setTimeout" $
        let
          js =
            renderJS (effectfulAST (fromSyntax (Timers.foreverTick (\_ -> done))))
         in
          and [T.isInfixOf needle js | needle <- ["setTimeout", "performance.now"]]
            @?= True
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
          @?= "const n0 = cond();\n(n0 ? 1 : 2)"
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
          @?= "(cond() ? 1 : 2);"
    , testCase "whileE re-emits an FFI condition" $ do
        let
          js =
            renderJS
              ( effectfulAST
                  (fromSyntax (toSyntax_ (while_ condE (ffi "foo" RecNil)) *> toSyntax noOp))
              )
        assertJSContains "while (cond())" js
        assertJSContains "foo();" js
    , testCase "forRange_ emits a C-style for loop" $ do
        let
          js =
            renderJS
              ( effectfulAST
                  ( fromSyntax
                      ( toSyntax_
                          ( forRange (number 0) (number 3) $ \i ->
                              discard (u8Set (uint8Array (packUint8 [0])) i (number 1))
                          )
                          *> toSyntax noOp
                      )
                  )
              )
        assertJSContains "for (let n0 = 0; n0 < 3; n0++)" js
        assertJSContains "new Uint8Array(1)[n0] = 1;" js
    , testCase "flat forRange_ emits u8Set in loop body" $ do
        let
          js =
            renderJS
              ( effectfulASTIr
                  ( fromSyntax
                      ( toSyntax_
                          ( forRange (number 0) (number 3) $ \i ->
                              discard (u8Set (uint8Array (packUint8 [0])) i (number 1))
                          )
                          *> toSyntax noOp
                      )
                  )
              )
        assertJSContains "for (let" js
        assertJSContains "[n" js
    , testCase "flat bindExpr forRange u8Set keeps loop" $ do
        let
          js =
            renderJS
              ( effectfulASTIr
                  ( fromSyntax $ do
                      buf <- bindExpr (newByteArray (number 4))
                      _ <-
                        forRange_ (number 0) (number 4) $ \i -> do
                          toSyntax_ (u8Set buf i (number 255))
                          done
                      toSyntax noOp
                  )
              )
        assertJSContains "for (let" js
        assertJSContains "[n" js
        assertJSContains "= 255;" js
    , testCase "flat initPaletteRgba pattern keeps fill loop" $ do
        let
          js =
            renderJS
              ( effectfulASTIr
                  ( fromSyntax $ do
                      pal <- bindExpr (newByteArray (number 12))
                      rgba <- bindExpr (newByteArray (number 16))
                      _ <-
                        forRange_ (number 0) (number 4) $ \s -> do
                          toSyntax_ (u8Set rgba (s * number 4) (u8Index pal (s * number 3)))
                          done
                      toSyntax noOp
                  )
              )
        assertJSContains "for (let" js
        assertJSContains "[n" js
    , testCase "flat nested forRange u8Set keeps both loops" $ do
        let
          w = number 3
          h = number 3
          js =
            renderJS
              ( effectfulASTIr
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
    , testCase "flat whenS u8Set keeps assignment" $ do
        let
          js =
            renderJS
              ( effectfulASTIr
                  ( fromSyntax $ do
                      buf <- bindExpr (newByteArray (number 1))
                      _ <-
                        whenS (number 1 .== number 1) $ do
                          toSyntax_ (u8Set buf (number 0) (number 42))
                          done
                      toSyntax noOp
                  )
              )
        assertJSContains "= 42;" js
    , testCase "multi-arg arrow FFI wraps IIFE" $
        renderJS
          ( effectfulAST
              ( ffi
                  ("(a,b)=>a+b")
                  (arg (number 1) <: arg (number 2) <: RecNil)
              )
          )
          @?= "((a,b)=>a+b)(1, 2)"
    , testCase "flat multi-arg arrow FFI wraps IIFE" $
        renderJS
          ( effectfulASTIr
              ( ffi
                  ("(a,b)=>a+b")
                  (arg (number 1) <: arg (number 2) <: RecNil)
              )
          )
          @?= "((a,b)=>a+b)(1, 2)"
    , testCase "u8Index renders direct Uint8Array indexing" $
        renderJS (pureAST (u8Index (uint8Array (packUint8 [7, 8, 9])) (number 1)))
          @?= "new Uint8Array([7, 8, 9])[1]"
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
        T.isInfixOf "o = 1" js @?= True
        T.isInfixOf "p = 2" js @?= True
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
         in
          T.isInfixOf ".set(" js @?= True
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
          @?= "let n0;\nif (cond()) {n0 = x = 1;}\nelse {n0 = 2;}\nn0"
    , testCase "try_ of two Unit arms skips the result bind" $
        renderJS (effectfulAST (try_ noOp noOp))
          @?= "try {}\ncatch (n0) {}"
    , testCase "try_ of FFI vs Unit keeps the result bind" $
        renderJS (effectfulAST (try_ (ffi "foo" RecNil) noOp))
          @?= "let n1;\ntry {n1 = foo();}\ncatch (n0) {}\nn1"
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
        T.isInfixOf "$checkedIndex" js @?= True
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
    , testCase "Array.toSorted hoists $toSorted helper" $ do
        let
          js =
            renderJS
              ( pureAST
                  (Array.toSorted numArray (\a b -> if_ (a .> b) (number 1) (number (-1))))
              )
        T.isInfixOf "const $toSorted =" js @?= True
        T.isInfixOf "=>" js @?= True
        T.isInfixOf ".toSorted" js @?= True
    , testCase "Array.reduce hoists $reduce helper" $ do
        let
          js =
            renderJS
              (pureAST (Array.reduce numArray (number 0) (\acc x -> acc + x)))
        T.isInfixOf "const $reduce = (seed, f) =>" js @?= True
        T.isInfixOf ".reduce" js @?= True
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
    , testCase "hoisted $reduce keeps seed/f after a seed binder" $ do
        let
          js =
            renderJS
              ( pureAST
                  ( Let (Just "seed") (number 1) $ \s ->
                      Array.reduce numArray (Var s) (\acc x -> acc + x)
                  )
              )
        T.isInfixOf "const seed = 1" js @?= True
        T.isInfixOf "const $reduce = (seed, f) =>" js @?= True
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
        renderJS (pureAST (Array.length numArray)) @?= "2"
    , testCase "Array.length of a binder renders as .length" $
        renderJS (pureAST (lambda (\xs -> Array.length xs)))
          @?= "n0 => n0.length"
    , testCase "Array.map renders as .map with a callback" $
        renderJS (pureAST (Array.map numArray (\x -> x + number 1)))
          @?= "[1, 2].map(x => x + 1)"
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
          @?= "[1, 2].filter(n0 => pred(n0));"
    , testCase "Array.map callback with an internal let is inlined when used once" $
        renderJS
          (pureAST (Array.map numArray (\x -> let_ (x + number 1) (\y -> y * 2))))
          @?= "[1, 2].map(x => {const n1 = x + 1;\nreturn n1 * 2})"
    , testCase "Array.join renders as .join" $
        renderJS (pureAST (Array.join numArray (string ",")))
          @?= "[1, 2].join(\",\")"
    , testCase "Array.push renders as a mutating .push call" $
        renderJS
          ( effectfulAST
              (fromSyntax (toSyntax (Array.push numArray (number 3)) *> toSyntax noOp))
          )
          @?= "[1, 2].push(3);"
    , testCase "Array.clear renders as length = 0" $
        renderJS
          ( effectfulAST
              (fromSyntax (toSyntax (Array.clear numArray) *> toSyntax noOp))
          )
          @?= "[1, 2].length = 0;"
    , testCase "Array.pushMany renders one call with every argument" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  (toSyntax (Array.pushMany numArray [number 3, number 4]) *> toSyntax noOp)
              )
          )
          @?= "[1, 2].push(3, 4);"
    , testCase "Array.fromEffects renders an array literal" $
        renderJS (effectfulAST (Array.fromEffects [expr (number 1), expr (number 2)]))
          @?= "[1, 2]"
    , testCase "String.toUpper renders as .toUpperCase()" $
        renderJS (pureAST (Str.toUpper (string "hi"))) @?= "\"hi\".toUpperCase()"
    , testCase "Floating sin renders as Math.sin(x)" $
        renderJS (effectfulAST (with1 fooE sin)) @?= "const n0 = foo();\nMath.sin(n0)"
    , testCase "Floating sqrt evaluates" $
        evaluateNumber (sqrt (number 9)) @?= 3
    , testCase "Math.round matches JS half-toward-+Infinity semantics" $ do
        evaluateNumber (Math.round (number 2.5)) @?= 3
        evaluateNumber (Math.round (number (-2.5))) @?= (-2)
    , testCase "Floating (**) evaluates as Math.pow" $
        evaluateNumber (number 2 ** number 10) @?= 1024
    , testCase "Json.stringify renders as JSON.stringify(x)" $
        renderJS (pureAST (Json.stringify (number 1))) @?= "JSON.stringify(1)"
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
          @?= "const n0 = document.getElementById(\"p\");\nconst n1 = document.createElement(\"div\");\nn0.appendChild(n1);"
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
          @?= "const n0 = document.getElementById(\"p\");\nconst n1 = document.createElement(\"div\");\nconst n2 = document.createElement(\"span\");\nn0.appendChild(n1);\nn0.appendChild(n2);"
    , testCase "Canvas.getContext2d is typed as an Option" $
        renderJS
          ( effectfulAST
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
          )
          @?= "const n0 = document.getElementById(\"c\");\nconst n1 = ((el,d)=>el.getContext('2d',{desynchronized:!!d,alpha:false,willReadFrequently:false}))(n0, false);\nconst n2 = n1;\nconst n3 = n2;\n(n3 === null ? \"no\" : \"ok\")"
    , testCase "Canvas.getContext2dDesync requests desynchronized context" $
        renderJS
          ( effectfulAST
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
          )
          @?= "const n0 = document.getElementById(\"c\");\nconst n1 = ((el,d)=>el.getContext('2d',{desynchronized:!!d,alpha:false,willReadFrequently:false}))(n0, true);\nconst n2 = n1;\nconst n3 = n2;\n(n3 === null ? \"no\" : \"ok\")"
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
          @?= "ctx.fillRect(0, 0, 10, 20);"
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
          @?= "ctx.rect(1, 2, 3, 4);"
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
          @?= "const n0 = localStorage.getItem(\"k\");\nconst n1 = n0;\n(n1 === null ? \"missing\" : n1)"
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
          @?= "const n0 = new Map();\nconst n1 = ((m, k) => { const v = m.get(k); return v === undefined ? null : v; })(n0, \"k\");\nconst n2 = n1;\n(n2 === null ? \"missing\" : n2)"
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
          @?= "const n0 = new Map();\nn0.set(\"a\", 1);"
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
          @?= "const n0 = new Set();\nn0.add(\"x\");"
    , testCase "Map.mapM_ emits forEach with (k,v) callback order" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( Map.withMap $ \m ->
                      Map.mapM_ (\_ _ -> toSyntax noOp) m
                  )
              )
          )
          @?= "const n0 = new Map();\n((m, f) => { m.forEach((v, k) => f(k)(v)); })(n0, n1 => n2 => {})"
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
          @?= "const n0 = new Map();\nn0.set(\"a\", 1);\nn0.set(\"b\", 2);"
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
          @?= "const n0 = new Set();\nn0.add(\"a\");\nn0.add(\"b\");"
    , testCase "Map.fromEntries emits new Map(entries)" $
        renderJS
          (effectfulAST (Map.fromEntries (emptyArray :: Expr f ('Array 'Number))))
          @?= "new Map([])"
    , testCase "Set.fromList emits new Set(values)" $
        renderJS
          (effectfulAST (Set.fromList (emptyArray :: Expr f ('Array 'String))))
          @?= "new Set([])"
    , testCase "Worker.newWorker emits new Worker(url)" $
        renderJS
          ( effectfulAST
              ( fromSyntax $ do
                  w <- Worker.newWorker (string "w.js")
                  toSyntax_ w
                  toSyntax noOp
              )
          )
          @?= "new Worker(\"w.js\");"
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
          @?= "const n0 = {};\nn0.a = 1;\nn0.b = 2;"
    , -- A Uint8Array is mutable, so propagating the literal to each use
      -- would hand out separate arrays: whoever fills one would not be
      -- seen by whoever reads the other. Guarded by `isCheapValue`.
      testCase "multi-use Uint8Array literal stays one array (identity)" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( do
                      b <- yield (uint8Array (packUint8 [0, 0]))
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
          @?= "[1, 2].forEach(n0 => foo(n0));"
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
          @?= "[1, 2].forEach(n0 => {});"
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
          @?= "setTimeout(n0 => tick(), 0)"
    , testCase "requestAnimationFrame takes ArgEffect" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  (Timers.requestAnimationFrame (\_ -> ffi "tick" RecNil) *> toSyntax noOp)
              )
          )
          @?= "requestAnimationFrame(n0 => tick());"
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
                  (Bind Nothing getR (\r -> Lift (resultCase (var r) (\_ -> number 0) id)))
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
        renderJS (pureAST (uint8Array (packUint8 [0, 0, 0])))
          @?= "new Uint8Array(3)"
    , testCase "newByteArray takes the size, not the bytes" $
        renderJS (effectfulAST (newByteArray (number 4)))
          @?= "(n => new Uint8Array(n))(4)"
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
          @?= "const n0 = (n => new Uint8Array(n))(2);\nfill(n0);\nread(n0);"
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
          @?= "{x: 1}"
    , testCase "frozen literal quotes keys" $
        renderJS
          ( pureAST
              (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
          )
          @?= "{x: 1}"
    , testCase "effectful frozen literal emits field values" $
        renderJS
          ( effectfulAST
              ( fromSyntax
                  ( Array.push_
                      (mempty :: Expr f ('Array ('Object LitRow)))
                      ( Object.frozen
                          [Object.field @"x" (number 3), Object.field @"y" (number 7)] ::
                          Expr f ('Object LitRow)
                      )
                  )
              )
          )
          @?= "[].push({x: 3, y: 7})"
    , testCase "typeof of FrozenLit parenthesizes the literal" $
        renderJS
          ( pureAST
              (typeOf (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow)))
          )
          @?= "typeof ({x: 1})"
    , testCase "sort emits a binary compare callback" $
        renderJS (effectfulAST (Array.sort numArray (\a b -> a - b)))
          @?= "[1, 2].sort((n0, n1) => n0 - n1)"
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
    , testCase "toFn emits a binary function value" $
        renderJS
          ( effectfulAST
              ( ffi
                  "f"
                  (arg (toFn (\(a :: Expr f 'Number) (b :: Expr f 'Number) -> a + b)) <: RecNil)
              )
          )
          @?= "f((n0, n1) => n0 + n1)"
    , testCase "optimized toFn keeps param name hints" $
        renderJS
          ( pureAST
              ( toFn
                  ( \(a :: Expr f 'Number) (b :: Expr f 'Number) ->
                      let_ (number 1) (\_ -> a + b)
                  )
              )
          )
          @?= "(a, b) => a + b"
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
          @?= "f((n0, n1, n2) => (n0 + n1) + n2)"
    , testCase "lambdaRow emits a nested unary function value" $
        renderJS
          ( pureAST
              ( lambdaRow @('[Param "x" 'Number, Param "y" 'Number]) $
                  \p -> p.x + p.y
              )
          )
          @?= "x => y => x + y"
    , testCase "ifE of throw vs number keeps the result bind" $
        renderJS (effectfulAST (ifE condE (throw_ "boom") (expr (number 1))))
          @?= "let n0;\nif (cond()) {throw \"boom\";}\nelse {n0 = 1;}\nn0"
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
          @?= "{fullName: \"Ada\", years: 36}"
    , testCase "toObject renders a ByteArray field as Uint8Array" $
        renderJS (effectfulAST (G.toObject (Packet sampleArray)))
          @?= "{octets: new Uint8Array([1, 2, 3])}"
    , testCase "toObject renders list and Maybe fields" $
        renderJS (effectfulAST (G.toObject (Tagged "x" ["a", "b"] Nothing)))
          @?= "{label: \"x\", tags: [\"a\", \"b\"], nickname: null}"
    , testCase "toObject Maybe record field is nullable object not Some wrapper" $
        renderJS (effectfulAST (G.toObject (Team (Just (Person "Ada" 36)))))
          @?= "const n0 = {fullName: \"Ada\", years: 36};\n{lead: n0}"
    , testCase "get on a Generic object uses derived Field" $
        renderJS
          ( effectfulAST
              ( fromSyntax $ do
                  o <- hold (G.toObject (Person "Ada" 36))
                  n <- Object.get @"fullName" o
                  yieldString n
              )
          )
          @?= "const n0 = {fullName: \"Ada\", years: 36};\nn0.fullName"
    , testCase "record dot getField matches get" $
        renderJS
          ( effectfulAST
              ( fromSyntax $ do
                  o <- hold (G.toObject (Person "Ada" 36))
                  n <- o.fullName
                  yieldString n
              )
          )
          @?= "const n0 = {fullName: \"Ada\", years: 36};\nn0.fullName"
    , testCase "record dot getField on Expr" $
        renderJS
          ( effectfulAST
              ( fromSyntax $ do
                  o <- toSyntax (G.toObject (Person "Ada" 36))
                  n <- (Var o).fullName
                  yieldString n
              )
          )
          @?= "const n0 = {fullName: \"Ada\", years: 36};\nn0.fullName"
    , testCase "frozen record dot is a pure Expr" $
        renderJS
          ( pureAST
              ((Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow)).x)
          )
          @?= "1"
    , testCase "newRecord is an empty object of the Generic row" $
        renderJS (effectfulAST (G.newRecord @Person))
          @?= "{}"
    , testCase "toObjectArray is an array of records" $
        renderJS (effectfulAST (G.toObjectArray [Person "Ada" 36, Person "Bob" 40]))
          @?= "[{fullName: \"Ada\", years: 36}, {fullName: \"Bob\", years: 40}]"
    , testCase "toObjectArray of [] is a literal empty array" $
        renderJS (effectfulAST (G.toObjectArray ([] :: [Person])))
          @?= "[]"
    , testCase "record field of [Person] uses toObjectArray" $
        renderJS (effectfulAST (G.toObject (Group [Person "Ada" 36])))
          @?= "{members: [{fullName: \"Ada\", years: 36}]}"
    , testCase "toSum nullary is a tagged object" $
        renderJS (effectfulAST (G.toSum Red))
          @?= "{tag: \"Red\"}"
    , testCase "toSum unary payload is the value" $
        renderJS (effectfulAST (G.toSum (Circle 1.5)))
          @?= "{tag: \"Circle\", payload: 1.5}"
    , testCase "toSum n-ary payload is a quoted object" $
        renderJS (effectfulAST (G.toSum (Rect 2 3)))
          @?= "{tag: \"Rect\", payload: {\"0\": 2, \"1\": 3}}"
    , testCase "toSumArray is an array of sums" $
        renderJS (effectfulAST (G.toSumArray [Red, Blue]))
          @?= "[{tag: \"Red\"}, {tag: \"Blue\"}]"
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
        renderJS (pureAST (number 1 + number 2)) @?= "3"
    , testCase "nested single-use lets fold" $
        renderJS (pureAST (let_ (number 1) (\x -> let_ (number 2) (\y -> y + x))))
          @?= "3"
    , testCase "cheap multi-use let is propagated and folded" $
        renderJS (pureAST (let_ (number 5) (\x -> x + x))) @?= "10"
    , testCase "multi-use outer keeps inner folded let" $
        renderJS
          (effectfulAST (with1 fooE (\x -> let_ (number 1 + number 1) (\y -> x + x + y))))
          @?= "const n0 = foo();\n(n0 + n0) + 2"
    , testCase "letRec rhs folds" $
        renderJS (pureAST (letRec (\_ -> number 1 + number 2) (\n -> n)))
          @?= "const n0 = 3;\nn0"
    , testCase "dead pure let is dropped" $
        renderJS (pureAST (let_ (number 1) (\_ -> number 2))) @?= "2"
    , testCase "unused FFI let is kept as a statement" $
        renderJS (effectfulAST (Bind Nothing fooE (\_ -> Lift (number 1))))
          @?= "foo();\n1"
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
    , testCase "lambda application of a literal folds" $
        renderJS (pureAST (apply (lambda (\x -> x * 2)) (number 21)))
          @?= "const n0 = 21;\nn0 * 2"
    , testCase "if_ of True takes the true branch" $
        renderJS (pureAST (if_ (bool True) (number 1) (number 2))) @?= "1"
    , testCase "literal is propagated under a lambda" $
        renderJS (pureAST (let_ (number 5) (\x -> lambda (\_ -> x + number 1))))
          @?= "n0 => 6"
    , testCase "let inside a lambda folds" $
        renderJS (pureAST (lambda (\x -> let_ (number 1) (\y -> y + x))))
          @?= "n0 => 1 + n0"
    , testCase "multi-use let inside a lambda stays inside the function" $
        renderJS (pureAST (lambda (\x -> let_ (x + x) (\y -> y + y))))
          @?= "n0 => {const n1 = n0 + n0;\nreturn n1 + n1}"
    , testCase "array index of a literal folds" $
        renderJS (pureAST (Array.index numArray (number 0))) @?= "1"
    , testCase "let-bound frozen field is cheap and folds" $
        renderJS
          ( pureAST
              ( let_
                  (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
                  (\o -> o.x)
              )
          )
          @?= "const n0 = {x: 1};\nn0.x"
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
          @?= "{s: JSON.stringify(1), y: 2}.y"
    , testCase "duplicate frozen keys fold last-wins" $
        renderJS
          ( pureAST
              ( ( Object.frozen [Object.field @"x" (number 1), Object.field @"x" (number 2)] ::
                    Expr f ('Object LitRow)
                ).x
              )
          )
          @?= "2"
    , testCase "sin of 0 folds" $
        renderJS (pureAST (sin (number 0))) @?= "0"
    , testCase "sin of a non-zero literal is left to JS" $
        renderJS (pureAST (sin (number 1))) @?= "Math.sin(1)"
    , testCase "sinh of 0 folds" $
        renderJS (pureAST (sinh (number 0))) @?= "0"
    , testCase "sinh of a non-zero literal is Math.sinh" $
        renderJS (pureAST (sinh (number 1))) @?= "Math.sinh(1)"
    , testCase "unused closed-name stdlib is dropped" $
        renderJS (pureAST (let_ (Str.toUpper (string "hi")) (\_ -> number 1)))
          @?= "1"
    , testCase "unused stringify is kept (can throw)" $
        renderJS (pureAST (let_ (Json.stringify (number 1)) (\_ -> number 2)))
          @?= "JSON.stringify(1);\n2"
    , testCase "impure && false keeps stringify" $
        T.isInfixOf
          "JSON.stringify"
          (renderJS (pureAST ((Json.stringify (number 1) .== string "1") .&& false_)))
          @?= True
    , testCase "impure || true keeps stringify" $
        T.isInfixOf
          "JSON.stringify"
          (renderJS (pureAST ((Json.stringify (number 1) .== string "1") .|| true_)))
          @?= True
    , testCase "optionCase of a Literal ValueOption folds" $
        renderJS
          ( pureAST
              ( optionCase
                  (Literal (ValueOption (Just (ValueNumber 5))))
                  (number 0)
                  (\x -> x + 1)
              )
          )
          @?= "const n0 = 5;\nn0 + 1"
    , testCase "optionCase of some of a folded literal peels" $
        renderJS
          (pureAST (optionCase (some (number 1 + number 2)) (number 0) (\x -> x + 1)))
          @?= "const n0 = 3;\nn0 + 1"
    , testCase "if_ True takes the true branch" $
        renderJS (pureAST (if_ (bool True) (number 1) (number 99)))
          @?= "1"
    , testCase "false && folds the RHS" $
        renderJS (pureAST (And (bool False) (number 1 .== number 0)))
          @?= "false"
    , testCase "while false becomes a no-op" $
        renderJS (effectfulAST (while_ (expr (bool False)) (ffi "foo" RecNil)))
          @?= "while (false) {foo();}"
    , testCase "ifE of True takes the true branch" $
        renderJS
          (effectfulAST (ifE (expr (bool True)) (ffi "foo" RecNil) (ffi "bar" RecNil)))
          @?= "let n0;\nif (true) {n0 = foo();}\nelse {n0 = bar();}\nn0"
    , testCase "typeof of a literal folds" $
        renderJS (pureAST (typeOf (number 1))) @?= "\"number\""
    , testCase "typeof of Uint8Array folds to object" $
        renderJS (pureAST (typeOf (uint8Array sampleArray))) @?= "\"object\""
    , testCase "string Semigroup is Concat" $
        renderJS (pureAST (("a" :: Expr f 'String) <> "b")) @?= "\"ab\""
    , testCase "try_ renders try/catch" $
        renderJS (effectfulAST (try_ (ffi "foo" RecNil) (expr (number 0))))
          @?= "let n1;\ntry {n1 = foo();}\ncatch (n0) {n1 = 0;}\nn1"
    , testCase "optionCaseE of none takes the none branch" $
        renderJS
          ( effectfulAST
              ( optionCaseE
                  (none :: Expr f ('Option 'Number))
                  (ffi "missing" RecNil)
                  (\x -> expr x)
              )
          )
          @?= "const n0 = null;\nlet n1;\nif (n0 === null) {n1 = missing();}\nelse {n1 = n0;}\nn1"
    , testCase "stringCaseE of a literal takes the matching arm" $
        renderJS
          ( effectfulAST
              ( stringCaseE
                  (string "a")
                  [("a", ffi "foo" RecNil), ("b", ffi "bar" RecNil)]
                  (ffi "baz" RecNil)
              )
          )
          @?= "let n0;\nswitch (\"a\") {case \"a\": {n0 = foo(); break;}\ncase \"b\": {n0 = bar(); break;}\ndefault: {n0 = baz();}}\nn0"
    , testCase "stringCaseE of a literal miss takes default" $
        renderJS
          ( effectfulAST
              ( stringCaseE
                  (string "z")
                  [("a", ffi "foo" RecNil)]
                  (ffi "baz" RecNil)
              )
          )
          @?= "let n0;\nswitch (\"z\") {case \"a\": {n0 = foo(); break;}\ndefault: {n0 = baz();}}\nn0"
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
        renderJS (effectfulASTIr (expr ((number 1 + number 2) + number 3)))
          @?= renderJS (effectfulASTIr (expr (number 6)))
    , testCase "freezeEncColumns preserves row order" $
        freezeEncColumnsOrderOk @?= True
    , testCase "direct pack is deterministic (kernel)" $
        flatDirectPackDeterministic kernelAndLambdaUse @?= True
    , testCase "direct pack is deterministic (forRange u8set)" $
        flatDirectPackForRangeOk @?= True
    , testCase "lowerOptEffectIr matches lower-then-opt on bind/forRange probe" $
        lowerOptEffectRegressionOk @?= True
    , testCase "optimize is stable on second pass" $
        flatDirectPackOptimizeStable kernelAndLambdaUse @?= True
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
        let
          src = "const x = 1 + 2;" :: Text
        out <- compileWith passthroughConfig src
        out @?= src
    , testCase "compilePure passthrough emits an IIFE" $ do
        out <- compilePure passthroughConfig (number 1 + number 2)
        out @?= renderJSCompact (pureProgram (number 1 + number 2))
        assertBool "IIFE wrapper present" ("(() => {" `T.isInfixOf` out)
        assertBool
          "result is returned so minifiers cannot DCE it"
          ("return" `T.isInfixOf` out)
    , testCase "disk cache roundtrips passthrough output" $ do
        tmp <- getTemporaryDirectory
        let
          dir = tmp </> "jshark-compiler-disk-test"
        removePathForcibly dir
        createDirectoryIfMissing True dir
        let
          cfg =
            CompilerConfig
              Passthrough
              (DiskCache dir)
              False
              Minified
              False
              False
              False
              Nothing
          src = "const x = 1 + 2;" :: Text
        a <- compileWith cfg src
        b <- compileWith cfg src
        a @?= src
        b @?= src
        files <- listDirectory dir
        assertBool "wrote a cache file" (not (null files))
        removePathForcibly dir
    , testCase "disk cache ignores a file whose stored key does not match" $ do
        tmp <- getTemporaryDirectory
        let
          dir = tmp </> "jshark-compiler-disk-mismatch"
        removePathForcibly dir
        createDirectoryIfMissing True dir
        let
          cfg =
            CompilerConfig
              Passthrough
              (DiskCache dir)
              False
              Minified
              False
              False
              False
              Nothing
        _ <- compileWith cfg "const a = 1;"
        files <- listDirectory dir
        mapM_ (\f -> writeFile (dir </> f) "not-a-cache-file") files
        out <- compileWith cfg "const b = 2;"
        out @?= "const b = 2;"
        removePathForcibly dir
    , testCase "esbuild minifies an IIFE when on PATH" $ do
        m <- findExecutable "esbuild"
        case m of
          Nothing -> pure ()
          Just _ -> do
            -- Constant-folded to a pure literal IIFE; esbuild DCE's that
            -- unless Compiler re-anchors via export default and strips it.
            let
              snippet = number 1 + number 2
              raw = renderJS (pureProgram snippet)
              cfg =
                CompilerConfig
                  (Esbuild defaultEsbuildConfig)
                  NoCache
                  False
                  Minified
                  False
                  False
                  False
                  Nothing
            out <- compilePure cfg snippet
            assertBool "non-empty" (not (T.null out))
            assertBool "minifier changed the IIFE" (out /= raw)
            assertBool "stripped ESM export anchor" (not ("export" `T.isInfixOf` out))
            assertBool
              "result still an expression (no var binding left)"
              (not ("var " `T.isPrefixOf` out))
    , testCase "tryCompileWith reports missing esbuild" $ do
        mExe <- findExecutable "esbuild"
        mNpx <- findExecutable "npx"
        case (mExe, mNpx) of
          (Nothing, Nothing) -> do
            res <-
              tryCompileWith
                ( CompilerConfig
                    (Esbuild defaultEsbuildConfig)
                    NoCache
                    False
                    Minified
                    False
                    False
                    False
                    Nothing
                )
                "1+2;"
            case res of
              Left _ -> pure ()
              Right _ -> assertFailure "expected Left when esbuild is missing"
          _ -> pure ()
    , testCase "configFallback False surfaces minifier errors" $ do
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
                  False
                  False
                  False
                  Nothing
            res <- tryCompileWith cfg "(() => { return 1; })();"
            case res of
              Left _ -> pure ()
              Right out -> assertFailure ("expected Left, got " <> T.unpack out)
    , testCase "configFallback True returns the original source" $ do
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
                  False
                  False
                  False
                  Nothing
            out <- compileWith cfg src
            out @?= src
    , testCase "compileWith logs minifier fallback on stderr" $ do
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
                  False
                  False
                  False
                  Nothing
            (_, captured) <- captureStderr $ compileWith cfg src
            assertBool
              "fallback notice"
              (T.isInfixOf "using unminified source" (T.pack captured))
    , testCase "compileWithPure suppresses minifier fallback stderr" $ do
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
                  False
                  False
                  False
                  Nothing
            (out, captured) <- captureStderr $ compileWithPure cfg src
            out @?= src
            assertBool
              "no fallback notice"
              (not (T.isInfixOf "using unminified source" (T.pack captured)))
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
    , testCase "batch job slot timing survives post-job snapshot" $
        batchJobSlotTimingOk >>= (@?= True)
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
    , testCase "Readable style skips the minifier even when a backend is set" $ do
        out <-
          compileEffect
            ( CompilerConfig
                (Esbuild defaultEsbuildConfig)
                NoCache
                False
                Readable
                False
                False
                False
                Nothing
            )
            fooE
        out @?= "foo();"
    , testCase "compileWith Readable skips the minifier even when a backend is set" $ do
        let
          src = "const x = 1 + 2;" :: Text
          cfg =
            CompilerConfig
              (Esbuild defaultEsbuildConfig)
              NoCache
              False
              Readable
              False
              False
              False
              Nothing
        out <- compileWith cfg src
        out @?= src
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
