{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Array.Byte (ByteArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
import JShark.Internal
  ( Builtin (ValueEq)
  , Ir (..)
  , Meta (..)
  , N (..)
  , SomeValue (..)
  , builtinSrc
  , effectfulASTWith
  , minifiedStyle
  , optIr
  , validateOptimizedEffect
  , validateOptimizedExpr
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
import Test.Support
import Test.Tasty
import Test.Tasty.HUnit
import TutorialSnippets (tutorialSnippets)

main :: IO ()
main =
  defaultMain $
    testGroup
      "jshark"
      [ evaluatorTests
      , evaluationOutcomeTests
      , validationTests
      , rewriteRuleTests
      , bigIntTests
      , codegenTests
      , controlFlowTests
      , stdlibTests
      , goodPartsTests
      , genericTests
      , optimizeTests
      , codegenFoldTests
      , compilerTests
      , ergonomicsTests
      , tutorialSnippets
      ]

-- | Host evaluation must separate success from JS-like failure and from
-- unsupported constructs, rather than collapsing all three into @error@.
evaluationOutcomeTests :: TestTree
evaluationOutcomeTests =
  testGroup
    "evaluation outcomes"
    [ testCase "success is Right" $
        tryEvaluate (number 1 + number 2 :: ClosedExpr 'Number) >>= \case
          Right (ValueNumber d) -> d @?= 3
          Left e -> assertFailure ("unexpected failure: " <> show e)
    , testCase "Error node is a JS-like failure" $
        failure (Error (string "boom") :: ClosedExpr 'Number) >>= \case
          EvalJsFailure msg -> msg @?= "boom"
          other -> assertFailure ("expected EvalJsFailure, got " <> show other)
    , testCase "out-of-bounds index is a JS-like failure" $
        failure
          (Index (Literal (ValueArray [ValueNumber 1])) (number 3) :: ClosedExpr 'Number)
          >>= \case
            EvalJsFailure msg ->
              assertBool
                ("bounds message: " <> T.unpack msg)
                ("out of bounds" `T.isInfixOf` msg)
            other -> assertFailure ("expected EvalJsFailure, got " <> show other)
    , testCase "an op with no host rule is unsupported" $
        failure (Str.toUpper (string "a") :: ClosedExpr 'String) >>= \case
          EvalUnsupported _ -> pure ()
          other -> assertFailure ("expected EvalUnsupported, got " <> show other)
    ]
 where
  failure :: ClosedExpr u -> IO EvalFailure
  failure e =
    tryEvaluate e
      >>= either pure (\_ -> assertFailure "expected a failure, got a value")

-- | Reads a byte and then overwrites it: the read must stay before the write.
u8ReadThenWrite :: Effect f 'Number
u8ReadThenWrite = fromSyntax $ do
  buf <- bindExpr (newByteArray (number 1))
  old <- bindExpr (expr (u8Index buf (number 0)))
  toSyntax_ (u8Set buf (number 0) (number 7))
  yield old

-- | The optimizer and substitution passes must keep every variable bound
-- with fresh binders. These cover mutation order, exceptions, loops,
-- short-circuiting, captures, name collisions, and shared inputs.
validationTests :: TestTree
validationTests =
  testGroup
    "ir validation"
    [ valid "mutation order" u8ReadThenWrite
    , valid "exception handling" $
        catch_ (throw_ (string "boom")) (\_ -> expr (number 7))
    , valid "short-circuit" $ fromSyntax $ do
        a <- bindExpr (ffi "condA" RecNil)
        b <- bindExpr (ffi "condB" RecNil)
        yield (if_ (And a b) (number 1) (number 2))
    , valid "captures" $ fromSyntax $ do
        x <- bindExpr (ffi "n" RecNil)
        let
          f = lambda (\y -> y + x)
        yield (apply f (number 1))
    , valid "name collisions" $ fromSyntax $ do
        a <- bindExpr (ffi "a" RecNil)
        b <- bindExpr (ffi "a" RecNil)
        yield (a + b)
    , valid "loop with conditional write" $ fromSyntax $ do
        buf <- bindExpr (newByteArray (number 4))
        _ <-
          forRange_ (number 0) (number 4) $ \i ->
            whenS (i .< number 2) (toSyntax (u8Set buf i (number 1)))
        yield (u8Index buf (number 1))
    , valid "option and result cases" $ fromSyntax $ do
        o <- bindExpr (expr (some (number 3)))
        n <- bindExpr (expr (optionCase o (number 0) (\x -> x + number 1)))
        yield n
    , testCase "pure capture is well-scoped" $
        validateOptimizedExpr
          (let_ (number 2) (\x -> apply (lambda (\y -> y * x)) (number 21)))
          @?= []
    ]
 where
  valid :: String -> ClosedEffect 'Number -> TestTree
  valid name e = testCase name (validateOptimizedEffect e @?= [])

bigIntTests :: TestTree
bigIntTests =
  testGroup
    "bigint"
    [ evalCase @Integer "bigInt 10 + bigInt 3 evaluates to 13" 13 $
        bigInt 10 + bigInt 3
    , evalCase @Integer "2^80+1 stays exact" (2 ^ (80 :: Int) + 1) $
        bigInt (2 ^ (80 :: Int) + 1)
    , pureJS "codegen emits 42n" "42n" (bigInt 42)
    , pureJS "negative literal is parenthesized" "(-42n)" (bigInt (-42))
    , evalCase @Double "Number inference still defaults" 2 $
        let_ (number 1) (\seqN -> seqN + 1)
    , evalCase @Integer "quot_ truncates toward 0" (-2) $
        quot_ (bigInt (-7)) (bigInt 3)
    , evalCase @Integer "rem_ is remainder after truncating division" (-1) $
        rem_ (bigInt (-7)) (bigInt 3)
    , testCase "bitwise and shifts evaluate" $ do
        evaluateBigInt (bitAnd (bigInt 7) (bigInt 3)) @?= 3
        evaluateBigInt (bitOr (bigInt 4) (bigInt 1)) @?= 5
        evaluateBigInt (bitXor (bigInt 7) (bigInt 3)) @?= 4
        evaluateBigInt (shl (bigInt 1) (bigInt 8)) @?= 256
        evaluateBigInt (shr (bigInt 256) (bigInt 3)) @?= 32
    , testCase "negative shift throws" $
        assertThrows "negative" (evaluateBigInt (shl (bigInt 1) (bigInt (-1))))
    , evalCase @Integer "toBigInt of an integer Number" 10 (toBigInt (number 10))
    , testCase "toBigInt of a non-integer Number throws" $
        assertThrows "not an integer" (evaluateBigInt (toBigInt (number 1.5)))
    , evalCase @Double "fromBigInt of a small value" 9 (fromBigInt (bigInt 9))
    , testCase "parseBigInt_ sign and prefixes" $ do
        evaluateBigInt (parseBigInt_ (string "-10")) @?= -10
        evaluateBigInt (parseBigInt_ (string "0x10")) @?= 16
        evaluateBigInt (parseBigInt_ (string "0b101")) @?= 5
        evaluateBigInt (parseBigInt_ (string "0o17")) @?= 15
        evaluateBigInt (parseBigInt_ (string "+0Xff")) @?= 255
    , testCase "comparisons and toString" $ do
        assertEval True (bigInt 3 .> bigInt 2)
        assertEval @Text "13" (toString (bigInt 10 + bigInt 3))
    , evalCase @Text "typeof bigint" "bigint" (typeOf (bigInt 1))
    , evalCase @Integer "Generic Integer is BigInt" 13 (G.toJS (13 :: Integer))
    ]

-- | GHC RULES fold literal-literal EDSL ops at compile time of the
-- client. These inspect the unoptimized tree; if a rule fails to fire
-- the case falls through to an AST node and the test fails.
rewriteRuleTests :: TestTree
rewriteRuleTests =
  testGroup
    "rewrite rules"
    [ testCase "plus folds literals" $
        folds @Double "jshark/plus/lit" 3 (number 1 + number 2)
    , testCase "times and minus fold literals" $ do
        folds @Double "jshark/times/lit" 12 (number 3 * number 4)
        folds @Double "jshark/minus/lit" 7 (number 10 - number 3)
    , testCase "div and negate fold literals" $ do
        folds @Double "jshark/div/lit" 4 (number 8 / number 2)
        folds @Double "jshark/negate/lit" (-5) (negate (number 5))
    , testCase "concat folds string literals" $
        folds @Text "jshark/concat/lit" "abcd" (string "ab" <> string "cd")
    , testCase "and/or fold boolean literals" $ do
        folds "jshark/and" False (bool True .&& bool False)
        folds "jshark/or" True (bool False .|| bool True)
    , testCase "and/or keep an impure left" $ do
        case (Json.stringifyPure (number 1) .== string "1") .&& false_ of
          And _ (Literal (ValueBool False)) -> pure ()
          _ -> assertFailure "andE dropped impure left"
        case (Json.stringifyPure (number 1) .== string "1") .|| true_ of
          Or _ (Literal (ValueBool True)) -> pure ()
          _ -> assertFailure "orE dropped impure left"
    , testCase "eq/ord fold number literals" $ do
        folds "jshark/eq/num" True (number 1 .== number 1)
        folds "jshark/lt/num" False (number 2 .< number 1)
    , testCase "if_ of a literal bool picks a branch" $
        folds @Double "jshark/if/true" 1 (if_ (bool True) (number 1) (number 2))
    , testCase "rem/bitAnd/shl fold literals" $ do
        folds @Double "jshark/rem/lit" 1 (rem_ (number 10) (number 3))
        folds @Double "jshark/bitand/lit" 3 (bitAnd (number 7) (number 3))
        folds @Double "jshark/shl/lit" 5888 (shl (number 23) (number 8))
        folds @Double "jshark/rem/neg" (-1) (rem_ (number (-10)) (number 3))
        folds @Double "jshark/ushr/lit" 4294967295 (ushr (number (-1)) (number 0))
    , testCase "let_ of a literal betas" $
        folds @Double "jshark/let/lit" 2 (let_ (number 1) (\x -> x + x))
    ]
 where
  -- The named rule turned @e@ into a literal holding @expected@.
  folds ::
    (G.ToValue a, Eq a, Show a) =>
    String -> a -> Expr f (G.UniverseOf a) -> Assertion
  folds rule expected = \case
    Literal v -> G.fromValue v @?= expected
    _ -> assertFailure rule

evaluatorTests :: TestTree
evaluatorTests =
  testGroup
    "evaluate"
    [ evalCase @Double "addition" 3 (number 1 + number 2)
    , evalCase @Double "subtraction" 3 ((number 5 :: Expr f 'Number) - number 2)
    , evalCase @Double "multiplication and division" 21 $
        (number 6 :: Expr f 'Number) * number 7 / number 2
    , evalCase @Double "abs and negate" 5 (abs (negate (number 5) :: Expr f 'Number))
    , evalCase @Double "let bindings" 42 (let_ (number 21) (\x -> x + x))
    , evalCase @Double "lambda application" 42 $
        apply (lambda (\x -> x * 2)) (number 21)
    , testCase "frozen records use $deepEqual in codegen" $ do
        let
          o1 =
            Object.frozen [Object.field @"x" (number 1), Object.field @"y" (number 2)] ::
              Expr f ('Object LitRow)
          o2 =
            Object.frozen [Object.field @"x" (number 1), Object.field @"y" (number 2)] ::
              Expr f ('Object LitRow)
        assertEval True (structuralEq o1 o2)
        assertJSContains "$deepEqual" $
          pureText (toLambda (\(a :: Expr f u) (b :: Expr f u) -> structuralEq a b))
    , evalCase @Double "GetField of FrozenLit evaluates" 21 $
        (Object.frozen [Object.field @"x" (number 21)] :: Expr f ('Object LitRow)).x
    , evalCase @Double "let-bound frozen field evaluates" 21 $
        let_
          (Object.frozen [Object.field @"x" (number 21)] :: Expr f ('Object LitRow))
          (\o -> o.x)
    , evalCase @Double "if_ of frozen fields evaluates" 21 $
        ( if_
            (bool True)
            (Object.frozen [Object.field @"x" (number 21)] :: Expr f ('Object LitRow))
            (Object.frozen [Object.field @"x" (number 0)])
        ).x
    , evalCase @Double "duplicate frozen keys last-wins" 2 $
        ( Object.frozen [Object.field @"x" (number 1), Object.field @"x" (number 2)] ::
            Expr f ('Object LitRow)
        ).x
    , evalCase "frozen records compare by last-wins fields" True $
        Eq
          (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
          (Object.frozen [Object.field @"x" (number 1)])
    , evalCase @Text "Show of Result is JS String(object)" "[object Object]" $
        Show (ok (number 5) :: Expr f ('Result 'String 'Number))
    , testCase "Uint8Array literals compare by contents" $ do
        assertEval True $
          structuralEq
            (uint8Array (packUint8 [1, 2, 3]))
            (uint8Array (packUint8 [1, 2, 3]))
        assertEval False $
          structuralEq (uint8Array (packUint8 [1, 2])) (uint8Array (packUint8 [1, 2, 3]))
    , evalCase @Text "Show of Uint8Array is comma-joined bytes" "1,2,3" $
        Show (uint8Array sampleArray)
    , evalCase @Text "typeof of Uint8Array is object" "object" $
        typeOf (uint8Array sampleArray)
    ]

codegenTests :: TestTree
codegenTests =
  testGroup
    "codegen"
    [ effectJS
        "nested single-use lets are both inlined"
        "const n0 = foo();\nconst n1 = bar();\nn1 + n0"
        (with2 fooE barE (\x y -> y + x))
    , effectJS
        "let used more than once renders as a const binding"
        "const n0 = foo();\nn0 + n0"
        (with1 fooE (\x -> x + x))
    , effectJS
        "let used once under a lambda is not inlined"
        "const n0 = foo();\nn1 => n0 + 1"
        (with1 fooE (\x -> lambda (\_ -> x + number 1)))
    , effectJS
        "let used once in an if_ branch is not inlined"
        "const n0 = foo();\nconst n1 = cond();\n(n1 ? n0 : 0)"
        (with2 fooE condE (\x c -> if_ c x (number 0)))
    , effectJS
        "let used once on the && RHS is not inlined"
        "const n0 = cond();\nconst n1 = bar();\nn1 && n0"
        (with2 condE barE (\x y -> And y x))
    , effectJS
        "let used once on the && LHS is inlined"
        "const n0 = cond();\nconst n1 = bar();\nn0 && n1"
        (with2 condE barE (\x y -> And x y))
    , effectJS "unknown function application is a direct call" "(f())(foo())" $
        ApplyE (ffi "f" RecNil) fooE
    , effectJS
        "ffiExpr with no args omits trailing call parens"
        "globalThis.crossOriginIsolated===true"
        (ffiExpr "globalThis.crossOriginIsolated===true" RecNil)
    , effectJS
        "call FFI with no args appends trailing call parens"
        "performance.now()"
        (ffi "performance.now" RecNil)
    , effectJS
        "ffiExpr typeof omits trailing call parens"
        "typeof PIXI !== 'undefined'"
        (ffiExpr "typeof PIXI !== 'undefined'" RecNil)
    , effectJS "parenthesized IIFE FFI still invokes" "(function(){return 1})()" $
        ffi "(function(){return 1})" RecNil
    , testCase "effectfulProgram wraps decls and the result in a JS IIFE" $
        jsText (effectfulProgram (with1 fooE (\x -> x + x)))
          @?= "(() => {\n  const n0 = foo();\n  return n0 + n0;\n})()"
    , syntaxHas "sequenced do-block keeps its effects" ["foo()", "bar("] $
        ( do
            x <- toSyntax fooE
            toSyntax_ (ffi "bar" (arg (Var x) <: RecNil))
        )
          *> done
    , pureJS "OverloadedStrings Expr literal" "\"hi\"" ("hi" :: Expr f 'String)
    , pureJS "OverloadedStrings Value via Literal" "\"hi\"" $
        Literal ("hi" :: Value 'String)
    , pureJS "Num Value literal via Literal" "3" (Literal (3 :: Value 'Number))
    , pureJS "Num Expr literal" "3" (3 :: Expr f 'Number)
    , testCase "Num Value host arithmetic" $
        G.fromValue ((1 + 2 * 3) :: Value 'Number) @?= (7 :: Double)
    , pureJS "Fractional Value via Literal" "0.5" (Literal ((1 / 2) :: Value 'Number))
    , pureJS "emptyArray renders as []" "[]" (emptyArray :: Expr f ('Array 'Number))
    , effectJS "toString renders String(x)" "const n0 = foo();\nString(n0)" $
        with1 fooE toString
    , syntaxJS "assign is Object.assign" "Object.assign(dst, src);" $
        assign (UnsafeObject "dst") (UnsafeObject "src")
    , syntaxHas "whenSomeE binds then option-cases" ["opt()", ".some"] $
        whenSomeE (ffi "opt" RecNil :: Effect f ('Option 'String)) $ \x ->
          Console.log x *> done
    , syntaxHas
        "loop0 is a recursive zero-arg function"
        ["=>", "console.log(\"p\")", "console.log(\"w\")"]
        $ loop0
          (\_ -> Console.log ("p" :: Expr f 'String) *> done)
          (\_ -> Console.log ("w" :: Expr f 'String) *> done)
    , testCase "foreverFrame reschedules requestAnimationFrame" $
        T.count
          "requestAnimationFrame"
          (effectText (fromSyntax (Timers.foreverFrame (\_ -> done))))
          @?= 2
    , syntaxHas "foreverTick reschedules setTimeout" ["setTimeout", "performance.now"] $
        Timers.foreverTick (\_ -> done)
    ]

-- | Minified-style emit of a do-block program.
minified :: (forall f. EffectSyntax f (f u)) -> JS
minified body = effectfulASTWith minifiedStyle (fromSyntax body)

-- | Fills a fresh one-byte array inside a three-step for loop.
forRangeFill :: Effect f 'Unit
forRangeFill =
  fromSyntax
    ( toSyntax_
        ( forRange (number 0) (number 3) $ \i ->
            discard (u8Set (uint8Array (packUint8 [0])) i (number 1))
        )
        *> toSyntax noOp
    )

controlFlowTests :: TestTree
controlFlowTests =
  testGroup
    "control flow"
    [ evalCase @Double "if_ picks the true branch" 1 $
        if_ (bool True) (number 1) (number 2)
    , evalCase @Double "if_ picks the false branch" 2 $
        if_ (bool False) (number 1) (number 2)
    , effectJS "if_ renders as a ternary" "const n0 = cond();\n(n0 ? 1 : 2)" $
        with1 condE (\c -> if_ c (number 1) (number 2))
    , evalCase @Double "optionCase on Some" 6 $
        optionCase
          (JShark.Api.some (number 5) :: Expr f ('Option 'Number))
          (number 0)
          (\x -> x + 1)
    , evalCase @Double "optionCase on None" 0 $
        optionCase (none :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1)
    , evalCase @Double "unsafeNullable of tagged none is a present value" 1 $
        optionCase
          ( unsafeNullable (none :: Expr f ('Option 'Number)) ::
              Expr f ('Option ('Option 'Number))
          )
          (number 0)
          (const (number 1))
    , evalCase @Double "unsafeNullable of undefined is none" 0 $
        optionCase (unsafeNullable (Literal ValueUnit)) (number 0) (const (number 1))
    , effectJS
        "ifE renders an if/else statement with a shared result variable"
        "(cond() ? 1 : 2);"
        ( fromSyntax
            (toSyntax (ifE condE (expr (number 1)) (expr (number 2))) *> toSyntax noOp)
        )
    , syntaxHas "whileE re-emits an FFI condition" ["while (cond())", "foo();"] $
        toSyntax_ (while_ condE (ffi "foo" RecNil)) *> toSyntax noOp
    , effectHas
        "forRange_ emits a C-style for loop"
        ["for (let n0 = 0; n0 < 3; n0++)", "new Uint8Array(1)[n0] = 1;"]
        forRangeFill
    , effectHas
        "u8 read bound before a write is not moved after it"
        ["const n1 = n0[0];\nn0[0] = 7;"]
        u8ReadThenWrite
    , jsHas "flat forRange_ emits u8Set in loop body" ["for (let", "[n"] $
        effectfulASTWith minifiedStyle forRangeFill
    , jsHas "flat bindExpr forRange u8Set keeps loop" ["for (let", "[n", "= 255;"] $
        minified $ do
          buf <- bindExpr (newByteArray (number 4))
          _ <-
            forRange_ (number 0) (number 4) $ \i -> do
              toSyntax_ (u8Set buf i (number 255))
              done
          toSyntax noOp
    , jsHas "flat initPaletteRgba pattern keeps fill loop" ["for (let", "[n"] $
        minified $ do
          pal <- bindExpr (newByteArray (number 12))
          rgba <- bindExpr (newByteArray (number 16))
          _ <-
            forRange_ (number 0) (number 4) $ \s -> do
              toSyntax_ (u8Set rgba (s * number 4) (u8Index pal (s * number 3)))
              done
          toSyntax noOp
    , testCase "flat nested forRange u8Set keeps both loops" $ do
        let
          w = number 3
          h = number 3
          js =
            jsText $ minified $ do
              buf <- bindExpr (newByteArray (w * h))
              _ <-
                forRange_ (number 0) h $ \y ->
                  forRange_ (number 0) w $ \x -> do
                    toSyntax_ (u8Set buf (y * w + x) (number 1))
                    done
              toSyntax noOp
        assertJSContains "for (let" js
        T.count "for (let" js @?= 2
    , jsHas "flat whenS u8Set keeps assignment" ["= 42;"] $
        minified $ do
          buf <- bindExpr (newByteArray (number 1))
          _ <-
            whenS (number 1 .== number 1) $ do
              toSyntax_ (u8Set buf (number 0) (number 42))
              done
          toSyntax noOp
    , effectJS "multi-arg arrow FFI wraps IIFE" "((a,b)=>a+b)(1, 2)" $
        ffi ("(a,b)=>a+b") (arg (number 1) <: arg (number 2) <: RecNil)
    , jsIs "flat multi-arg arrow FFI wraps IIFE" "((a,b)=>a+b)(1, 2)"
        $ effectfulASTWith minifiedStyle
        $ ffi ("(a,b)=>a+b") (arg (number 1) <: arg (number 2) <: RecNil)
    , pureJS
        "u8Index renders direct Uint8Array indexing"
        "new Uint8Array([7, 8, 9])[1]"
        (u8Index (uint8Array (packUint8 [7, 8, 9])) (number 1))
    , effectJS "when_ of Unit skips the result bind" "if (cond()) {foo();}" $
        when_ condE (ffi "foo" RecNil)
    , effectHas "discarded do keeps the last assignment" ["o = 1", "p = 2"] $
        when_ condE $
          fromSyntax $ do
            _ <- toSyntax $ UnsafeObjectAssign (UnsafeObject "o") (expr (number 1))
            toSyntax $ UnsafeObjectAssign (UnsafeObject "p") (expr (number 2))
    , effectJS
        "ifS of two CallMethods skips the result bind"
        "if (cond()) {el.setAttribute(\"k\", \"a\");}\nelse {el.setAttribute(\"k\", \"b\");}"
        (ifE condE (setK "a") (setK "b"))
    , syntaxHas "ifE keeps impure prelude when condition folds" [".set("] $ do
        dst <- bindExpr (newByteArray (number 4))
        src <- bindExpr (newByteArray (number 4))
        toSyntax_ $ ifE (expr (number 1 .== number 1)) (u8Copy dst src) noOp
        toSyntax noOp
    , effectJS
        "ifE of two getAttributes keeps the result bind"
        "let n0;\nif (cond()) {n0 = el.getAttribute(\"a\");}\nelse {n0 = el.getAttribute(\"b\");}\nn0"
        ( ifE
            condE
            (callMethod (UnsafeObject "el") "getAttribute" (arg (string "a") <: RecNil))
            (callMethod (UnsafeObject "el") "getAttribute" (arg (string "b") <: RecNil))
        )
    , effectJS
        "ifE of assign vs number keeps the result bind"
        "let n0;\nif (cond()) {n0 = x = 1;}\nelse {n0 = 2;}\nn0"
        ( ifE
            condE
            (UnsafeObjectAssign (UnsafeObject "x") (expr (number 1)))
            (expr (number 2))
        )
    , effectJS "try_ of Unit arms skips the result bind" "try {}\ncatch (n0) {}" $
        try_ noOp noOp
    , effectJS
        "try_ of FFI vs Unit keeps the result bind"
        "let n1;\ntry {n1 = foo();}\ncatch (n0) {}\nn1"
        (try_ (ffi "foo" RecNil) noOp)
    , syntaxHas
        "stringCaseE of Unit arms is a switch statement"
        [ "switch ("
        , "case \"a\":"
        , "case \"b\":"
        , "default:"
        , "break;"
        , "foo()"
        , lacks "=;"
        ]
        $ do
          k <- toSyntax (ffi "key" RecNil)
          toSyntax $
            stringCaseE
              (var k)
              [("a", discard (ffi "foo" RecNil)), ("b", discard (ffi "bar" RecNil))]
              (discard (ffi "baz" RecNil))
    , syntaxHas
        "stringCaseE of values keeps the result bind"
        ["let n", "switch (", "case \"a\":", " = 1", " = 0", "break;", lacks "=;"]
        $ do
          k <- toSyntax (ffi "key" RecNil)
          toSyntax (stringCaseE (var k) [("a", expr (number 1))] (expr (number 0)))
    , syntaxHas
        "stringCaseE switches on the scrutinee ref"
        ["switch (typeof ", lacks " = typeof", "case \"number\":"]
        $ do
          x <- toSyntax (ffi "val" RecNil)
          toSyntax $
            stringCaseE
              (typeOf (var x))
              [("number", discard (ffi "foo" RecNil))]
              (discard (ffi "bar" RecNil))
    ]
 where
  setK v =
    discard $
      callMethod
        (UnsafeObject "el")
        "setAttribute"
        (arg (string "k") <: arg (string v) <: RecNil)

stdlibTests :: TestTree
stdlibTests =
  testGroup
    "stdlib"
    [ evalCase @Double "Array.index evaluates" 2 (Array.index numArray (number 1))
    , evalCase @Double "Array.index 1.9 is the integer slot" 2 $
        Array.index numArray (number 1.9)
    , testCase "Array.index out of bounds throws" $
        assertThrows
          "array index out of bounds"
          (evaluateNumber (Array.index numArray (number 9)))
    , testCase "Array.index NaN is out of bounds" $
        assertThrows
          "array index out of bounds"
          (evaluateNumber (Array.index numArray (number (0 / 0))))
    , effectHas
        "Array.index truncates and throws out of bounds"
        ["$checkedIndex", "throw"]
        (with2 (ffi "xs" RecNil) (ffi "i" RecNil) Array.index)
    , evalCase "Array.map evaluates" True $
        Eq
          (Array.map numArray (\x -> x + number 1))
          (Literal (ValueArray [ValueNumber 2, ValueNumber 3]))
    , evalCase "Array.filter evaluates" True $
        Eq
          (Array.filter numArray (\x -> x .> number 1))
          (Literal (ValueArray [ValueNumber 2]))
    , testCase "Array.groupBy is first-seen [{key, items}]" $ do
        let
          xs = Literal (ValueArray [ValueNumber 1, ValueNumber 2, ValueNumber 1])
          grouped =
            Array.groupBy xs (\n -> if_ (n .== number 1) (string "one") (string "two"))
          keys = Array.map grouped (\g -> GetField @"key" g)
          firstItems = GetField @"items" (Array.index grouped (number 0))
        assertEval True $
          Eq keys (Literal (ValueArray [ValueString "one", ValueString "two"]))
        evaluateNumber (Array.length firstItems) @?= 2
    , pureHas
        "Array.groupBy emits the $groupBy shim"
        [ "const $groupBy ="
        , "new Map()"
        , "items:[]"
        , lacks ".reduce"
        , "key"
        , lacks "($groupBy)(n0)(n1)"
        ]
        (Array.groupBy numArray (\_ -> string "k"))
    , testCase "Array.groupBy hoists once when used twice" $ do
        let
          js =
            pureText $
              let_ (Array.groupBy numArray (\_ -> string "a")) $ \g1 ->
                let_ (Array.groupBy numArray (\_ -> string "b")) $ \g2 ->
                  Array.length g1 + Array.length g2
        T.count "const $groupBy =" js @?= 1
        assertJSContains "const $groupBy = function(arr,key)" js
    , testCase "binary hoists match in pureAST and effectfulAST" $ do
        assertJS ["=>", lacks "($groupBy)(n0)(n1)"] $
          pureText (Array.groupBy numArray (\_ -> string "k"))
        assertJS
          [ "const $checkedIndex ="
          , "$checkedIndex("
          , lacks "(($checkedIndex)(n0)(n1)"
          ]
          (effectText (with2 (ffi "xs" RecNil) (ffi "i" RecNil) Array.index))
    , pureHas
        "Array.zipWith hoists $zipWith helper"
        ["const $zipWith =", "=>", lacks "($zipWith)(n0)(n1)"]
        (Array.zipWith (+) numArray numArray)
    , pureHas
        "Array.toSorted hoists $toSorted helper"
        ["const $toSorted =", "=>", ".toSorted"]
        (Array.toSorted numArray (\a b -> if_ (a .> b) (number 1) (number (-1))))
    , pureHas
        "Array.reduce hoists $reduce helper"
        ["const $reduce = (seed, f) =>", ".reduce"]
        (Array.reduce numArray (number 0) (\acc x -> acc + x))
    , testCase "Array.reduce hoists once when used twice" $ do
        let
          js =
            pureText $
              let_ (Array.reduce numArray (number 0) (\acc x -> acc + x)) $ \a ->
                let_ (Array.reduce numArray (number 1) (\acc x -> acc * x)) $ \b ->
                  a + b
        T.count "const $reduce =" js @?= 1
        assertJSContains "const $reduce = (seed, f) =>" js
    , pureHas
        "hoisted $reduce keeps seed/f after a seed binder"
        ["const seed = 1", "const $reduce = (seed, f) =>"]
        ( Let (Just "seed") (number 1) $ \s ->
            Array.reduce numArray (Var s) (\acc x -> acc + x)
        )
    , evalCase "Classes.fmap Array" True $
        Eq
          (C.fmap (\x -> x + number 1) numArray)
          (Literal (ValueArray [ValueNumber 2, ValueNumber 3]))
    , testCase "Classes.liftA2 Option" $ do
        assertEval @(Maybe Double) (Just 5) $
          C.liftA2 (+) (some (number 2)) (some (number 3))
        assertEval @(Maybe Double) Nothing $
          C.liftA2 (+) (none :: Expr f ('Option 'Number)) (some (number 3))
    , testCase "Classes.traverse Array Option" $ do
        let
          pos x = if_ (x .> number 0) (some x) none
        assertEval True $
          Eq
            (C.traverse pos numArray)
            (some (Literal (ValueArray [ValueNumber 1, ValueNumber 2])))
        assertEval @(Maybe [Double]) Nothing $
          C.traverse pos (Literal (ValueArray [ValueNumber 1, ValueNumber (-1)]))
    , evalCase "Classes.join Array" True $
        Eq
          ( C.join
              ( Literal
                  ( ValueArray
                      [ValueArray [ValueNumber 1], ValueArray [ValueNumber 2, ValueNumber 3]]
                  )
              )
          )
          (Literal (ValueArray [ValueNumber 1, ValueNumber 2, ValueNumber 3]))
    , evalCase @Double "Classes.fmap Function" 7 $
        apply (C.fmap (\y -> y + 1) (lambda (\x -> x * 2))) (number 3)
    , evalCase @(Either Text Double) "Classes.bimap Result" (Right 3) $
        C.bimap id (\x -> x + 1) (ok (number 2) :: Expr f ('Result 'String 'Number))
    , evalCase "Classes Semigroup Array" True $
        Eq
          (numArray C.<> Literal (ValueArray [ValueNumber 3]))
          (Literal (ValueArray [ValueNumber 1, ValueNumber 2, ValueNumber 3]))
    , evalCase @Double "Classes Category Function" 9 $
        apply
          (C.fmap (\y -> y + 1) (lambda (\x -> x * 2)) C.. lambda (\x -> x + 1))
          (number 3)
    , evalCase "Classes.mzipWith Array" True $
        Eq
          ( C.mzipWith
              (+)
              numArray
              (Literal (ValueArray [ValueNumber 10, ValueNumber 20, ValueNumber 30]))
          )
          (Literal (ValueArray [ValueNumber 11, ValueNumber 22]))
    , evalCase "Classes.foldMap Array String" True $
        Eq
          (C.foldMap (\n -> if_ (n .== number 1) (string "a") (string "b")) numArray)
          (string "ab")
    , testCase "Classes.foldr is reduceRight" $ do
        evaluateNumber (C.foldr (-) (number 0) numArray) @?= -1
        evaluateNumber (C.foldl (-) (number 0) numArray) @?= -3
        assertJSContains ".reduceRight" (pureText (C.foldr (+) (number 0) numArray))
    , evalCase @Double "LetRec value rhs evaluates" 3 $
        letRec (\_ -> number 1 + number 2) (\n -> n)
    , evalCase @Double "Classes.mfix Function" 1 $
        apply
          (C.mfix (\a -> lambda (\r -> if_ (r .== number 0) (number 1) a)))
          (number 0)
    , testCase "Classes Semigroup Option is Maybe" $ do
        assertEval @(Maybe Text) (Just "ab") (some (string "a") C.<> some (string "b"))
        assertEval @(Maybe Text) (Just "x") $
          (none :: Expr f ('Option 'String)) C.<> some (string "x")
    , evalCase "Classes.elem Array uses $valueEq" True (C.elem (number 2) numArray)
    , testCase "Array.singleton is a one-element array" $ do
        evaluateNumber (Array.length (Array.singleton (number 7))) @?= 1
        pureText (Array.singleton (number 7)) @?= "[7]"
    , pureJS "unit array literal keeps its slots" "[undefined, undefined]" $
        Literal (ValueArray [ValueUnit, ValueUnit])
    , testCase "Array.join renders tagged options as objects" $ do
        let
          opts =
            Literal
              (ValueArray [ValueOption Nothing, ValueOption (Just (ValueNumber 1))])
        assertEval @Text "[object Object]-[object Object]" $
          Array.join opts (string "-")
        assertEval @Text "[object Object],[object Object]" (Show opts)
    , testCase "$valueEq helpers are defined once for two comparisons" $ do
        let
          js =
            jsText $ pureProgram $ toLambda $ \(a :: Expr f u) (b :: Expr f u) ->
              structuralEq a b .|| structuralEq b a
        T.count "const $valueEq" js @?= 1
        T.count "const $arrayEq" js @?= 1
        T.count "const $deepEqual" js @?= 1
        T.count "const $uint8ArrayEq" js @?= 1
        T.count "$valueEq(n" js @?= 2
    , pureJS "Array.length of a literal folds" "2" (Array.length numArray)
    , pureJS "Array.length of a binder renders as .length" "n0 => n0.length" $
        lambda (\xs -> Array.length xs)
    , pureJS
        "Array.map renders as .map with a callback"
        "[1, 2].map(n0 => n0 + 1)"
        (Array.map numArray (\x -> x + number 1))
    , syntaxJS
        "Array.filterE renders an effectful callback"
        "[1, 2].filter(n0 => pred(n0));"
        $ do
          toSyntax_ $ Array.filterE numArray (\x -> ffi "pred" (arg x <: RecNil))
          toSyntax noOp
    , pureJS
        "Array.map callback with an internal let is inlined when used once"
        "[1, 2].map(n0 => {const n1 = n0 + 1;\nreturn n1 * 2})"
        (Array.map numArray (\x -> let_ (x + number 1) (\y -> y * 2)))
    , pureJS "Array.join renders as .join" "[1, 2].join(\",\")" $
        Array.join numArray (string ",")
    , syntaxJS "Array.push renders as a mutating .push call" "[1, 2].push(3);" $
        toSyntax (Array.push numArray (number 3)) *> toSyntax noOp
    , syntaxJS "Array.clear renders as length = 0" "[1, 2].length = 0;" $
        toSyntax (Array.clear numArray) *> toSyntax noOp
    , syntaxJS
        "Array.pushMany renders one call with every argument"
        "[1, 2].push(3, 4);"
        $ toSyntax (Array.pushMany numArray [number 3, number 4]) *> toSyntax noOp
    , syntaxJS "Array.pushLen keeps the native new length" "[1, 2].push(3)" $ do
        n <- bindExpr (Array.pushLen numArray (number 3))
        yield n
    , pureHas "Array.indexChecked uses the checked index" ["$checkedIndex"] $
        toLambda
          ( \(a :: Expr f ('Array 'Number)) (i :: Expr f 'Number) ->
              Array.indexChecked a i
          )
    , effectJS "Array.fromEffects renders an array literal" "[1, 2]" $
        Array.fromEffects [expr (number 1), expr (number 2)]
    , pureJS "String.toUpper renders as .toUpperCase()" "\"hi\".toUpperCase()" $
        Str.toUpper (string "hi")
    , effectJS "Floating sin renders as Math.sin(x)" "const n0 = foo();\nMath.sin(n0)" $
        with1 fooE sin
    , evalCase @Double "Floating sqrt evaluates" 3 (sqrt (number 9))
    , testCase "Math.round matches JS half-toward-+Infinity semantics" $ do
        evaluateNumber (Math.round (number 2.5)) @?= 3
        evaluateNumber (Math.round (number (-2.5))) @?= (-2)
    , evalCase @Double "Floating (**) evaluates as Math.pow" 1024 $
        number 2 ** number 10
    , pureJS "Json.stringifyPure renders as JSON.stringify(x)" "JSON.stringify(1)" $
        Json.stringifyPure (number 1)
    , syntaxJS "Console.log renders as console.log(x)" "console.log(\"hi\");" $
        Console.log ("hi" :: Expr f 'String) *> toSyntax noOp
    , syntaxJS
        "Dom appendChild inlines single-use handles"
        "const n0 = document.getElementById(\"p\");\nconst n1 = document.createElement(\"div\");\nn0.appendChild(n1);"
        $ do
          p <- Dom.lookupId (string "p")
          c <- Dom.createElement (string "div")
          _ <- Dom.appendChild p c
          toSyntax noOp
    , syntaxJS
        "Dom appendChild keeps a handle that is used more than once"
        "const n0 = document.getElementById(\"p\");\nconst n1 = document.createElement(\"div\");\nconst n2 = document.createElement(\"span\");\nn0.appendChild(n1);\nn0.appendChild(n2);"
        $ do
          p <- Dom.lookupId (string "p")
          c1 <- Dom.createElement (string "div")
          c2 <- Dom.createElement (string "span")
          _ <- Dom.appendChild p c1
          _ <- Dom.appendChild p c2
          toSyntax noOp
    , effectJS
        "Canvas.getContext2d is typed as an Option"
        "const n0 = document.getElementById(\"c\");\nconst n1 = ((el,d)=>el.getContext('2d',{desynchronized:!!d,alpha:false,willReadFrequently:false}))(n0, false);\nconst n2 = ((v) => v == null ? {some: false} : {some: true, value: v})(n1);\nconst n4 = n2;\nlet n5;\nif (n4 .some) {const n3 = n4.value;\nn5 = \"ok\";}\nelse {n5 = \"no\";}\nn5"
        (canvasOkNo Canvas.getContext2d)
    , effectJS
        "Canvas.getContext2dDesync requests desynchronized context"
        "const n0 = document.getElementById(\"c\");\nconst n1 = ((el,d)=>el.getContext('2d',{desynchronized:!!d,alpha:false,willReadFrequently:false}))(n0, true);\nconst n2 = ((v) => v == null ? {some: false} : {some: true, value: v})(n1);\nconst n4 = n2;\nlet n5;\nif (n4 .some) {const n3 = n4.value;\nn5 = \"ok\";}\nelse {n5 = \"no\";}\nn5"
        (canvasOkNo Canvas.getContext2dDesync)
    , testCase
        "optionCaseE of getContext plus a large object array tests that context"
        $ do
          let
            people =
              [Person ("p" <> T.pack (show i)) (fromIntegral i) | i <- [1 .. 15 :: Int]]
            js =
              effectText $ fromSyntax $ do
                c <- Dom.lookupId (string "c")
                ctx <- Canvas.getContext2d c
                toSyntax $
                  Bind Nothing ctx $ \o ->
                    optionCaseE (var o) noOp $ \_ ->
                      stmts $ do
                        _ <- toSyntax (G.toObject (Group people))
                        done
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
                pre = fst (T.breakOn " .some" js)
               in
                T.drop (T.length (T.dropWhileEnd (\c -> c == 'n' || isDigit c) pre)) pre
            -- The scrutinee may be the context directly, an alias of it, or
            -- a conversion wrapper around it. Follow a plain-identifier
            -- alias, and treat any RHS that mentions a context id as
            -- resolving to it.
            aliasOf i =
              concat
                [ if rhs == jsIdent rhs
                    then [rhs | not (T.null rhs)]
                    else [cid | cid <- ctxIds, cid `T.isInfixOf` rhs]
                | chunk <- T.splitOn "const " js
                , let
                    (lhs, rest) = T.breakOn " = " chunk
                , lhs == i
                , let
                    rhs = T.takeWhile (/= ';') (T.drop 3 rest)
                ]
            resolvesToCtx fuel i
              | i `elem` ctxIds = True
              | fuel <= (0 :: Int) = False
              | otherwise = any (resolvesToCtx (fuel - 1)) (aliasOf i)
          assertJSOmits "=;" js
          (not (null ctxIds) && resolvesToCtx 8 nullId) @?= True
    , syntaxJS "Canvas.fillRect renders a 2D call" "ctx.fillRect(0, 0, 10, 20);" $ do
        _ <-
          Canvas.fillRect
            (UnsafeObject "ctx")
            (number 0)
            (number 0)
            (number 10)
            (number 20)
        toSyntax noOp
    , syntaxJS "Canvas.rect renders a 2D call" "ctx.rect(1, 2, 3, 4);" $ do
        _ <-
          Canvas.rect (UnsafeObject "ctx") (number 1) (number 2) (number 3) (number 4)
        toSyntax noOp
    , syntaxJS "Canvas fillStyle is a Field" "ctx.fillStyle = \"#f00\";" $ do
        _ <-
          Object.set @"fillStyle"
            (UnsafeObject "ctx" :: Effect f ('MutableObject Canvas.Context2D))
            (string "#f00")
        toSyntax noOp
    , syntaxJS
        "Storage.getItem is typed as an Option and dispatches via optionCase"
        "const n0 = localStorage.getItem(\"k\");\nconst n2 = ((v) => v == null ? {some: false} : {some: true, value: v})(n0);\nlet n3;\nif (n2 .some) {const n1 = n2.value;\nn3 = n1;}\nelse {n3 = \"missing\";}\nn3"
        $ do
          v <- Storage.getItem Storage.localStorage (string "k")
          toSyntax (expr (optionCase v (string "missing") (\x -> x)))
    , syntaxJS
        "Map.lookup treats undefined as None"
        "const n0 = new Map();\nconst n1 = ((m, k) => { const v = m.get(k); return v === undefined ? null : v; })(n0, \"k\");\nconst n3 = ((v) => v == null ? {some: false} : {some: true, value: v})(n1);\nlet n4;\nif (n3 .some) {const n2 = n3.value;\nn4 = n2;}\nelse {n4 = \"missing\";}\nn4"
        $ Map.withMap
        $ \m -> do
          v <- Map.lookup m (string "k")
          toSyntax (expr (optionCase v (string "missing") (\x -> x)))
    , syntaxJS "Map.insert emits set" "const n0 = new Map();\nn0.set(\"a\", 1);" $
        Map.withMap $ \m -> do
          _ <- Map.insert m (string "a") (number 1)
          toSyntax noOp
    , syntaxJS "Set.insert emits add" "const n0 = new Set();\nn0.add(\"x\");" $
        Set.withSet $ \s -> do
          _ <- Set.insert s (string "x")
          toSyntax noOp
    , syntaxHas "Map.deleteReturning emits delete" [".delete(\"k\")"] $
        Map.withMap (\m -> Map.deleteReturning m (string "k") >>= yield)
    , syntaxHas "Set.deleteReturning emits delete" [".delete(\"x\")"] $
        Set.withSet (\s -> Set.deleteReturning s (string "x") >>= yield)
    , syntaxJS
        "Map.mapM_ emits forEach with (k,v) callback order"
        "const n0 = new Map();\n((m, f) => { m.forEach((v, k) => f(k)(v)); })(n0, n1 => n2 => {})"
        (Map.withMap $ \m -> Map.mapM_ (\_ _ -> toSyntax noOp) m)
    , syntaxJS
        "multi-use Map.new stays one allocation (identity)"
        "const n0 = new Map();\nn0.set(\"a\", 1);\nn0.set(\"b\", 2);"
        $ Map.withMap
        $ \m -> do
          _ <- Map.insert m (string "a") (number 1)
          _ <- Map.insert m (string "b") (number 2)
          toSyntax noOp
    , syntaxJS
        "multi-use Set.new stays one allocation (identity)"
        "const n0 = new Set();\nn0.add(\"a\");\nn0.add(\"b\");"
        $ Set.withSet
        $ \s -> do
          _ <- Set.insert s (string "a")
          _ <- Set.insert s (string "b")
          toSyntax noOp
    , effectJS "Map.fromEntries emits new Map(entries)" "new Map([])" $
        Map.fromEntries (emptyArray :: Expr f ('Array 'Number))
    , effectJS "Set.fromList emits new Set(values)" "new Set([])" $
        Set.fromList (emptyArray :: Expr f ('Array 'String))
    , syntaxJS "Worker.newWorker emits new Worker(url)" "new Worker(\"w.js\");" $ do
        w <- Worker.newWorker (string "w.js")
        toSyntax_ w
        toSyntax noOp
    , syntaxJS
        "multi-use UnsafeObject stays one const (identity)"
        "const n0 = {};\nn0.a = 1;\nn0.b = 2;"
        $ do
          o <- fmap (expr . Var) $ toSyntax $ UnsafeObject "{}"
          toSyntax_ $ UnsafeObjectAssign (UnsafeObjectGet o "a") (Lift (number 1))
          toSyntax_ $ UnsafeObjectAssign (UnsafeObjectGet o "b") (Lift (number 2))
          toSyntax noOp
    , pureJS
        "clamped array literal renders new Uint8ClampedArray"
        "new Uint8ClampedArray([0, 0, 5])"
        (uint8ClampedArray (packUint8 [0, 0, 5]))
    , pureJS
        "u8Len works on a clamped array"
        "new Uint8ClampedArray([1, 2, 3]).length"
        (u8Len (uint8ClampedArray (packUint8 [1, 2, 3])))
    , -- A Uint8Array is mutable, so propagating the literal to each use
      -- would hand out separate arrays: whoever fills one would not be
      -- seen by whoever reads the other. Guarded by `isCheapValue`.
      syntaxJS
        "multi-use Uint8Array literal stays one array (identity)"
        "const n0 = new Uint8Array(2);\nfill(n0);\nread(n0);"
        $ do
          b <- yield (uint8Array (packUint8 [0, 0]))
          toSyntax_ (ffi "fill" (arg (var b) <: RecNil))
          toSyntax_ (ffi "read" (arg (var b) <: RecNil))
          toSyntax noOp
    , syntaxHas
        "locationHash is window.location.hash, not a bracket key"
        ["window.location.hash", lacks "[\"location.hash\"]"]
        (locationHash *> toSyntax noOp)
    , syntaxJS "forEach param name matches body uses" "[1, 2].forEach(n0 => foo(n0));" $ do
        toSyntax_ $ forEach numArray (\x -> ffi "foo" (arg x <: RecNil))
        toSyntax noOp
    , syntaxJS "LambdaE of Unit does not emit return ()" "[1, 2].forEach(n0 => {});" $ do
        toSyntax_ $ forEach numArray (\_ -> noOp)
        toSyntax noOp
    , syntaxHas "onClick assigns the DOM onclick property" [".onclick ="] $ do
        el <- Dom.lookupId (string "b")
        onClick el $ \_ -> noOp
        toSyntax noOp
    , testCase "NaN .== NaN is false" $ do
        assertEval False (number (0 / 0) .== number (0 / 0))
        pureText (number (0 / 0) .== number (0 / 0)) @?= "false"
    , pureHas
        ".== on number exprs uses === without eq helpers"
        [lacks "const $valueEq", "==="]
        $ toLambda (\(a :: Expr f 'Number) (b :: Expr f 'Number) -> (a + b) .== (a + b))
    , pureHas "bound Number .== uses === (not $valueEq)" [lacks "$valueEq", "==="] $
        toLambda (\(a :: Expr f 'Number) (_ :: Expr f 'Number) -> a .== number 1)
    , testCase "$valueEq shim includes null/object fast-path" $
        assertJS ["typeof", "null"] (builtinSrc ValueEq)
    , pureHas "frozen Number literals fold to === in .==" ["true", lacks "$valueEq"] $
        number 1 .== number 1
    , effectHas
        ".== hoists $valueEq (=== then structural; never ==)"
        ["$valueEq", lacks " == "]
        $ with2 fooE barE structuralEq
    , effectHas ".!= is !$valueEq" ["$valueEq", "!($valueEq("] $
        with2 fooE barE structuralNEq
    , effectJS
        "ffi takes an effectful function via ArgEffect"
        "setTimeout(n0 => tick(), 0)"
        $ ffi
          "setTimeout"
          (ArgEffect (LambdaE (\_ -> ffi "tick" RecNil)) <: arg (number 0) <: RecNil)
    , syntaxJS
        "requestAnimationFrame takes ArgEffect"
        "requestAnimationFrame(n0 => tick());"
        $ Timers.requestAnimationFrame (\_ -> ffi "tick" RecNil) *> toSyntax noOp
    , syntaxJS "send emits xhr.send()" "xhr.send();" $
        Ajax.send (UnsafeObject "xhr") *> toSyntax noOp
    , syntaxJS "sendPost emits xhr.send(body)" "xhr.send(\"hi\");" $
        Ajax.sendPost (UnsafeObject "xhr") (string "hi") *> toSyntax noOp
    ]
 where
  canvasOkNo getContext = fromSyntax $ do
    c <- Dom.lookupId (string "c")
    ctx <- getContext c
    toSyntax $ Bind Nothing ctx $ \o ->
      Lift (optionCase (var o) (string "no") (\_ -> string "ok"))

goodPartsTests :: TestTree
goodPartsTests =
  testGroup
    "good parts"
    [ testCase "rem and bitwise evaluate" $ do
        evaluateNumber (rem_ (number 7) (number 3)) @?= 1
        evaluateNumber (bitAnd (number 7) (number 3)) @?= 3
        evaluateNumber (ushr (number (-1)) (number 0)) @?= 4294967295
    , evalCase @Double "parseInt_ requires a radix and evaluates" 16 $
        parseInt_ (string "10") (number 16)
    , evalCase @Double "parseInt_ keeps an optional sign" (-10) $
        parseInt_ (string "-10") (number 10)
    , evalCase @Double "resultCase on ok" 6 $
        resultCase
          (ok (number 5) :: Expr f ('Result 'String 'Number))
          (\_ -> number 0)
          (\x -> x + 1)
    , pureJS
        "ok of Unit emits undefined, not an empty property"
        "{ok: true, value: undefined}"
        (ok (Literal ValueUnit) :: Expr f ('Result 'String 'Unit))
    , effectHas "resultCase picks .ok and unwraps .value" [".ok", ".value"] $
        Bind
          Nothing
          (ffi "r" RecNil :: Effect f ('Result 'String 'Number))
          (\r -> Lift (resultCase (var r) (\_ -> number 0) id))
    , evalCase @Double "orElse on none" 3 $
        orElse (none :: Expr f ('Option 'Number)) (number 3)
    , evalCase @Double "reduce evaluates" 3 $
        Array.reduce numArray (number 0) (\a x -> a + x)
    , evalCase @Double "arraySlice evaluates" 2 $
        Array.index (Array.arraySlice numArray (number 1) (number 2)) (number 0)
    , evalCase @Double "arraySlice negatives count from the end" 2 $
        Array.index (Array.arraySlice numArray (number (-1)) (number 2)) (number 0)
    , evalCase @Double "apply2 is curried Apply" 3 $
        apply2
          (toLambda (\(x :: Expr f 'Number) (y :: Expr f 'Number) -> x + y))
          (number 1)
          (number 2)
    , effectJS "throw_ renders throw" "throw \"boom\";" $
        (throw_ (string "boom") :: Effect f 'Unit)
    , pureJS "regex is new RegExp, not a literal" "new RegExp(\"ab\").test(\"xab\")" $
        Regex.test (Regex.regex "ab") (string "xab")
    , pureJS "regex source escapes quotes" "new RegExp(\"a\\\"b\").test(\"x\")" $
        Regex.test (Regex.regex "a\"b") (string "x")
    , pureJS
        "uint8Array is new Uint8Array, not a JS Array"
        "new Uint8Array([1, 2, 3])"
        $ uint8Array sampleArray
    , pureJS "empty uint8Array is new Uint8Array(0)" "new Uint8Array(0)" $
        uint8Array (packUint8 [])
    , pureJS "zero-filled uint8Array uses length, not a literal" "new Uint8Array(3)" $
        uint8Array (packUint8 [0, 0, 0])
    , effectJS
        "newByteArray takes the size, not the bytes"
        "(n => new Uint8Array(n))(4)"
        $ newByteArray (number 4)
    , -- Allocation has identity: folding two occurrences together would
      -- hand the writer and the reader different arrays.
      syntaxJS
        "multi-use newByteArray stays one allocation (identity)"
        "const n0 = (n => new Uint8Array(n))(2);\nfill(n0);\nread(n0);"
        $ do
          b <- fmap var (toSyntax (newByteArray (number 2)))
          toSyntax_ (ffi "fill" (arg b <: RecNil))
          toSyntax_ (ffi "read" (arg b <: RecNil))
          toSyntax noOp
    , effectHas
        "hasOwn uses Object.prototype.hasOwnProperty.call"
        ["Object.prototype.hasOwnProperty.call"]
        (Object.hasOwn (UnsafeObject "o") (string "k"))
    , effectJS "create is Object.create" "Object.create(p)" $
        (Object.create (UnsafeObject "p") :: Effect f ('MutableObject ()))
    , effectJS "obj literal quotes keys" "{x: 1}" $
        (Object.obj [Object.field @"x" (number 1)] :: Effect f ('MutableObject LitRow))
    , pureJS "frozen literal quotes keys" "{x: 1}" $
        (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
    , effectJS "effectful frozen literal emits field values" "[].push({x: 3, y: 7})" $
        fromSyntax
          ( Array.push_
              (mempty :: Expr f ('Array ('Object LitRow)))
              ( Object.frozen
                  [Object.field @"x" (number 3), Object.field @"y" (number 7)] ::
                  Expr f ('Object LitRow)
              )
          )
    , pureJS "typeof of FrozenLit parenthesizes the literal" "typeof {x: 1}" $
        typeOf (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
    , effectJS "sort emits a binary compare callback" "[1, 2].sort((a, b) => a - b)" $
        Array.sort numArray (\a b -> a - b)
    , pureHas
        "toSorted emits a binary compare callback"
        ["const $toSorted =", "=>", ".toSorted", lacks "($toSorted)([1.0, 2.0])"]
        (Array.toSorted numArray (\a b -> a - b))
    , evalCase @Double "toSorted evaluates" 2 $
        Array.index (Array.toSorted numArray (\a b -> a - b)) (number 1)
    , effectJS "toFn emits a binary function value" "f((a, b) => a + b)" $
        ffi
          "f"
          (arg (toFn (\(a :: Expr f 'Number) (b :: Expr f 'Number) -> a + b)) <: RecNil)
    , pureJS "optimized toFn keeps param name hints" "(a, b) => a + b" $
        toFn
          ( \(a :: Expr f 'Number) (b :: Expr f 'Number) ->
              let_ (number 1) (\_ -> a + b)
          )
    , effectJS "toFn emits a ternary function value" "f((a, b, c) => (a + b) + c)" $
        ffi
          "f"
          ( arg
              ( toFn
                  (\(a :: Expr f 'Number) (b :: Expr f 'Number) (c :: Expr f 'Number) -> a + b + c)
              )
              <: RecNil
          )
    , pureJS "lambdaRow emits a nested unary function value" "x => y => x + y" $
        lambdaRow @('[Param "x" 'Number, Param "y" 'Number]) $
          \p -> p.x + p.y
    , effectJS
        "ifE of throw vs number keeps the result bind"
        "let n0;\nif (cond()) {throw \"boom\";}\nelse {n0 = 1;}\nn0"
        (ifE condE (throw_ "boom") (expr (number 1)))
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
    , effectJS "toObject renders record fields" "{fullName: \"Ada\", years: 36}" $
        G.toObject (Person "Ada" 36)
    , effectJS
        "toObject renders a ByteArray field as Uint8Array"
        "{octets: new Uint8Array([1, 2, 3])}"
        (G.toObject (Packet sampleArray))
    , effectJS
        "toObject renders list and Maybe fields"
        "{label: \"x\", tags: [\"a\", \"b\"], nickname: {some: false}}"
        (G.toObject (Tagged "x" ["a", "b"] Nothing))
    , effectJS
        "toObject Maybe field is a tagged Option"
        "const n0 = {fullName: \"Ada\", years: 36};\n{lead: ((v) => v == null ? {some: false} : {some: true, value: v})(n0)}"
        (G.toObject (Team (Just (Person "Ada" 36))))
    , syntaxJS "get on a Generic object uses derived Field" adaFullName $ do
        o <- hold (G.toObject (Person "Ada" 36))
        n <- Object.get @"fullName" o
        yieldString n
    , syntaxJS "record dot getField matches get" adaFullName $ do
        o <- hold (G.toObject (Person "Ada" 36))
        n <- o.fullName
        yieldString n
    , syntaxJS "record dot getField on Expr" adaFullName $ do
        o <- toSyntax (G.toObject (Person "Ada" 36))
        n <- (Var o).fullName
        yieldString n
    , pureJS "frozen record dot is a pure Expr" "1" $
        (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow)).x
    , effectJS "newRecord is an empty object of the Generic row" "{}" $
        G.newRecord @Person
    , effectJS
        "toObjectArray is an array of records"
        "[{fullName: \"Ada\", years: 36}, {fullName: \"Bob\", years: 40}]"
        (G.toObjectArray [Person "Ada" 36, Person "Bob" 40])
    , effectJS "toObjectArray of [] is a literal empty array" "[]" $
        G.toObjectArray ([] :: [Person])
    , effectJS
        "record field of [Person] uses toObjectArray"
        "{members: [{fullName: \"Ada\", years: 36}]}"
        (G.toObject (Group [Person "Ada" 36]))
    , effectJS "toSum nullary is a tagged object" "{tag: \"Red\"}" (G.toSum Red)
    , effectJS "toSum unary payload is the value" "{tag: \"Circle\", payload: 1.5}" $
        G.toSum (Circle 1.5)
    , effectJS
        "toSum n-ary payload is a quoted object"
        "{tag: \"Rect\", payload: {\"0\": 2, \"1\": 3}}"
        (G.toSum (Rect 2 3))
    , effectJS "toSumArray is an array of sums" "[{tag: \"Red\"}, {tag: \"Blue\"}]" $
        G.toSumArray [Red, Blue]
    , effectHas "record field of a sum uses toSum" ["\"Red\""] $
        G.toObject (Badge Red)
    , effectHas
        "whenTag on a nullary ctor compares .tag"
        [".tag", "\"Red\"", "===", lacks "$valueEq"]
        (G.whenTag @"Red" (G.toSum Red) (\_ -> expr (string "yes")) (expr (string "no")))
    , effectHas "whenTag unary payload is the value" [".payload"] $
        G.whenTag @"Circle" (G.toSum (Circle 1.5)) (\r -> expr r) (expr (number 0))
    , syntaxHas "whenTag n-ary payload fields are gettable" ["[\"0\"]"] $ do
        s <- hold (G.toSum (Rect 2 3))
        toSyntax $
          G.whenTag @"Rect"
            s
            ( \p -> fromSyntax $ do
                w <- Object.get @"0" (Lift p)
                yield w
            )
            (expr (number 0))
    , effectHas
        "caseSum nullary checks every named tag"
        [".tag", "===", lacks "$valueEq", "\"Red\"", "\"Green\"", "\"Blue\"", "throw"]
        $ G.caseSum @Color (ffi "color" RecNil)
        $ G.on @"Red" (\_ -> expr (string "r"))
        $ G.on @"Green" (\_ -> expr (string "g"))
        $ G.on @"Blue" (\_ -> expr (string "b"))
        $ G.CaseEnd
    , effectHas
        "caseSum Case_ is a suffix wildcard"
        ["\"Red\"", lacks "\"Green\"", lacks "\"Blue\""]
        $ G.caseSum @Color (ffi "color" RecNil)
        $ G.on @"Red" (\_ -> expr (string "r"))
        $ G.Case_ (\_ -> expr (string "other"))
    , effectHas "caseSum unary payload is the value" [".payload"]
        $ G.caseSum @Shape (ffi "shape" RecNil)
        $ G.on @"Circle" (\r -> expr r)
        $ G.on @"Rect" (\_ -> expr (number 0))
        $ G.CaseEnd
    , syntaxHas "caseSum n-ary payload fields are gettable" ["[\"0\"]"] $ do
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
    ]
 where
  adaFullName = "const n0 = {fullName: \"Ada\", years: 36};\nn0.fullName"

optimizeTests :: TestTree
optimizeTests =
  testGroup
    "optimize"
    [ pureJS "literal arithmetic folds" "3" (number 1 + number 2)
    , pureJS "negative zero keeps its sign" "-0.0" (Literal (ValueNumber (-0.0)))
    , pureJS "nested single-use lets fold" "3" $
        let_ (number 1) (\x -> let_ (number 2) (\y -> y + x))
    , pureJS "cheap multi-use let is propagated and folded" "10" $
        let_ (number 5) (\x -> x + x)
    , effectJS
        "multi-use outer keeps inner folded let"
        "const n0 = foo();\n(n0 + n0) + 2"
        $ with1 fooE (\x -> let_ (number 1 + number 1) (\y -> x + x + y))
    , pureJS "letRec rhs folds" "const n0 = 3;\nn0" $
        letRec (\_ -> number 1 + number 2) (\n -> n)
    , pureJS "dead pure let is dropped" "2" (let_ (number 1) (\_ -> number 2))
    , effectJS "unused FFI let is kept as a statement" "foo();\n1" $
        Bind Nothing fooE (\_ -> Lift (number 1))
    , jsIs "minified bind does not reorder effects" "const n0 = foo();\nbar();\nn0" $
        minified $
          toSyntax fooE >>= \x ->
            toSyntax_ (ffi "bar" RecNil) *> toSyntax (expr (Var x))
    , testCase "top-level do-notation bind chain compiles" $ do
        let
          chain =
            foldr
              (\_ k -> toSyntax (ffi "step" RecNil) *> k)
              (toSyntax noOp)
              [1 .. 40 :: Int]
        out <- compileEffect readableConfig (fromSyntax chain)
        assertBool "emitted js" (BS.length out > 20)
    , pureJS "lambda application of a literal folds" "const n0 = 21;\nn0 * 2" $
        apply (lambda (\x -> x * 2)) (number 21)
    , pureJS "if_ of True takes the true branch" "1" $
        if_ (bool True) (number 1) (number 2)
    , pureJS "literal is propagated under a lambda" "n0 => 6" $
        let_ (number 5) (\x -> lambda (\_ -> x + number 1))
    , pureJS "let inside a lambda folds" "n0 => 1 + n0" $
        lambda (\x -> let_ (number 1) (\y -> y + x))
    , pureJS
        "multi-use let inside a lambda stays inside the function"
        "n0 => {const n1 = n0 + n0;\nreturn n1 + n1}"
        (lambda (\x -> let_ (x + x) (\y -> y + y)))
    , pureJS
        "fnLit body keeps the enclosing binding distinct"
        "const n0 = Math.sin(1);\np => {const n2 = p + 2;\nreturn ((n2 + n2) + n0) + n0}"
        $ let_ (sin (number 1))
        $ \z ->
          fnLit @'[Param "p" 'Number] $ \p ->
            let_ (p.p + number 2) (\q -> q + q + z + z)
    , pureJS
        "capturing named lambda is not hoisted"
        "const n0 = Math.sin(1);\nn1 => (n1 + n0) + n0"
        (let_ (sin (number 1)) (\n -> namedLambda "f" (\x -> (x + n) + n)))
    , pureJS "array index of a literal folds" "1" (Array.index numArray (number 0))
    , pureJS "let-bound frozen field is cheap and folds" "const n0 = {x: 1};\nn0.x" $
        let_
          (Object.frozen [Object.field @"x" (number 1)] :: Expr f ('Object LitRow))
          (\o -> o.x)
    , pureJS
        "GetField does not DCE an impure sibling field"
        "{s: JSON.stringify(1), y: 2}.y"
        $ ( Object.frozen
              [ Object.field @"s" (Json.stringifyPure (number 1))
              , Object.field @"y" (number 2)
              ] ::
              Expr f ('Object LitRow)
          ).y
    , pureJS "duplicate frozen keys fold last-wins" "2" $
        ( Object.frozen [Object.field @"x" (number 1), Object.field @"x" (number 2)] ::
            Expr f ('Object LitRow)
        ).x
    , pureJS "sin of 0 folds" "0" (sin (number 0))
    , pureJS "sin of a non-zero literal is left to JS" "Math.sin(1)" (sin (number 1))
    , pureJS "sinh of 0 folds" "0" (sinh (number 0))
    , pureJS "sinh of a non-zero literal is Math.sinh" "Math.sinh(1)" $
        sinh (number 1)
    , pureJS "unused closed-name stdlib is dropped" "1" $
        let_ (Str.toUpper (string "hi")) (\_ -> number 1)
    , pureJS "unused stringify is kept (can throw)" "const n0 = JSON.stringify(1);\n2" $
        let_ (Json.stringifyPure (number 1)) (\_ -> number 2)
    , pureHas "impure && false keeps stringify" ["JSON.stringify"] $
        (Json.stringifyPure (number 1) .== string "1") .&& false_
    , pureHas "impure || true keeps stringify" ["JSON.stringify"] $
        (Json.stringifyPure (number 1) .== string "1") .|| true_
    , pureJS "optionCase of a Literal ValueOption folds" "const n0 = 5;\nn0 + 1" $
        optionCase
          (Literal (ValueOption (Just (ValueNumber 5))))
          (number 0)
          (\x -> x + 1)
    , pureJS "optionCase of some of a folded literal peels" "const n0 = 3;\nn0 + 1" $
        optionCase (some (number 1 + number 2)) (number 0) (\x -> x + 1)
    , pureJS "some none nests faithfully" "{some: true, value: {some: false}}" $
        some (none :: Expr f ('Option 'Number))
    , pureJS "none is tagged none" "{some: false}" (none :: Expr f ('Option 'Number))
    , pureJS "if_ True takes the true branch" "1" $
        if_ (bool True) (number 1) (number 99)
    , pureJS "false && folds the RHS" "false" $
        And (bool False) (number 1 .== number 0)
    , effectJS "while false becomes a no-op" "" $
        while_ (expr (bool False)) (ffi "foo" RecNil)
    , effectJS
        "while re-evaluates a condition that needs declarations"
        "while (true) {const n0 = tick();\nif (!((n0 + 1) > 1)) {break;}\nbody();}"
        ( while_
            (Bind Nothing (ffi "tick" RecNil) (\n -> expr ((Var n + number 1) .> number 1)))
            (ffi "body" RecNil)
        )
    , pureJS
        "resultCase keeps branch declarations conditional"
        "n0 => {const n4 = n0;\nconst n2 = n4.value;\nlet n5;\nif (n4.ok) {const n3 = Math.sin(1);\nn5 = (n3 + n3) + n2;}\nelse {n5 = 0;}\nreturn n5}"
        $ lambda
        $ \r ->
          resultCase r (\_ -> number 0) (\x -> let_ (sin (number 1)) (\y -> y + y + x))
    , effectJS "ifE of True takes the true branch" "foo()" $
        ifE (expr (bool True)) (ffi "foo" RecNil) (ffi "bar" RecNil)
    , pureJS "typeof of a literal folds" "\"number\"" (typeOf (number 1))
    , pureJS "typeof of Uint8Array folds to object" "\"object\"" $
        typeOf (uint8Array sampleArray)
    , pureJS "string Semigroup is Concat" "\"ab\"" (("a" :: Expr f 'String) <> "b")
    , effectJS
        "try_ renders try/catch"
        "let n1;\ntry {n1 = foo();}\ncatch (n0) {n1 = 0;}\nn1"
        $ try_ (ffi "foo" RecNil) (expr (number 0))
    , effectJS "optionCaseE of none takes the none branch" "missing()" $
        optionCaseE
          (none :: Expr f ('Option 'Number))
          (ffi "missing" RecNil)
          (\x -> expr x)
    , effectJS "stringCaseE of a literal takes the matching arm" "foo()" $
        stringCaseE
          (string "a")
          [("a", ffi "foo" RecNil), ("b", ffi "bar" RecNil)]
          (ffi "baz" RecNil)
    , effectJS "stringCaseE of a literal miss takes default" "baz()" $
        stringCaseE (string "z") [("a", ffi "foo" RecNil)] (ffi "baz" RecNil)
    , -- Row index must depend on the loop counter (not constant-folded to
      -- the first coordinate); column index 0 is expected to stay literal.
      syntaxHas
        "forRange array index uses the loop variable"
        ["sink(", lacks "sink(1.0)", "$checkedIndex", lacks "(($checkedIndex)(n0)(n1)"]
        $ do
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
    ]

codegenFoldTests :: TestTree
codegenFoldTests =
  testGroup
    "codegen folds"
    [ testCase "constant fold chains" $
        jsText
          (effectfulASTWith minifiedStyle (expr ((number 1 + number 2) + number 3)))
          @?= jsText (effectfulASTWith minifiedStyle (expr (number 6)))
    , testCase "optIr keeps a mutating loop effectful" $
        let
          ?keepLets = False
         in
          mDrop (snd (optIr loop)) @?= False
    ]
 where
  lit :: Double -> Ir
  lit = Ir . NLit . SomeValue . ValueNumber
  loop = Ir (NFor (lit 0) (lit 4) 0 (Ir (NU8Set (Ir (NVar 99)) (lit 0) (lit 1))))

-- | Round-trips for the newer ergonomics surface ('toNumber',
-- 'whenNoneS', 'Dom.byId', typed event accessors, 'addEventListenerS',
-- 'compileEffectSyntax').
ergonomicsTests :: TestTree
ergonomicsTests =
  testGroup
    "ergonomics"
    [ syntaxJS
        "toNumber coerces a string via Number()"
        "const n0 = Number(\"4.5\");\nconsole.log(n0);"
        $ do
          n <- toNumber (string "4.5")
          Console.log n
          done
    , syntaxJS
        "whenNoneS runs the body only on none"
        "const n0 = localStorage.getItem(\"k\");\nconst n2 = ((v) => v == null ? {some: false} : {some: true, value: v})(n0);\nconst n1 = n2.value;\nif (n2 .some) {}\nelse {seed();}"
        $ do
          v <- Storage.getItem Storage.localStorage (string "k")
          _ <- whenNoneS v (toSyntax_ (ffi "seed" RecNil) *> done)
          done
    , syntaxJS
        "addEventListenerS + eventKey avoids stmts and annotations"
        "const n0 = document.getElementById(\"board\");\nn0.addEventListener(\"keydown\", n1 => {const n2 = n1.key;\nsink(n2);\nreturn});"
        $ do
          el <- Dom.byId "board"
          addEventListenerS "keydown" el $ \e -> do
            k <- eventKey e
            toSyntax_ (ffi "sink" (arg k <: RecNil))
            done
          done
    , testCase "compileEffectSyntax absorbs fromSyntax" $
        compileEffectSyntax
          readableConfig
          (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp)
          >>= (@?= "console.log(\"hi\");")
    ]

compilerTests :: TestTree
compilerTests =
  testGroup
    "compiler"
    [ testCase "compilePure passthrough emits an IIFE" $ do
        out <- compilePure defaultCompilerConfig (number 1 + number 2)
        out @?= renderJS (pureProgram (number 1 + number 2))
        assertBool "IIFE wrapper present" ("(() => {" `BS.isInfixOf` out)
        assertBool
          "result is returned so minifiers cannot DCE it"
          ("return" `BS.isInfixOf` out)
    , testCase "compilePure ignores configProgress stderr" $ do
        let
          withProgress = defaultCompilerConfig {configProgress = True}
          eff = fromSyntax (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp)
        (_, capturedEffect) <- captureStderr $ compileEffectIO withProgress eff
        (_, capturedPure) <-
          captureStderr $ compilePure withProgress (number 1 + number 2)
        assertBool
          "effect timing line"
          (T.isInfixOf "compiled in" (T.pack capturedEffect))
        assertBool "pure silent" (not (T.isInfixOf "compiled in" (T.pack capturedPure)))
    , readable
        "readableConfig compileEffect is a snippet, not an IIFE"
        "console.log(\"hi\");"
        $ fromSyntax (Console.log ("hi" :: Expr f 'String) *> toSyntax noOp)
    , readable
        "readableConfig compilePure has no IIFE and inlines single-use lets"
        "const n0 = foo();\nn0 + 1;"
        (with1 fooE (\x -> x + number 1))
    , readable
        "readableConfig keeps multi-use lets as const"
        "const n0 = foo();\nn0 + n0;"
        $ with1 fooE (\x -> x + x)
    , testCase "callerBinderHint returns enclosing function name" $
        callerHintProbe () @?= Just "callerHintProbe"
    , pureJS
        "readableConfig uses explicit let binder hints"
        "const hintProbe = Math.sin(1);\nhintProbe + hintProbe"
        (Let (Just "hintProbe") (sin (number 1)) (\x -> Var x + Var x))
    , pureJS "same-scope binder hints uniquify" "const x = 1;\nconst n1 = 2;\nx + n1" $
        Let (Just "x") (number 1) $ \a ->
          Let (Just "x") (number 2) $ \b ->
            Var a + Var b
    , pureJS
        "readableConfig names pure let binders from HasCallStack"
        "const readableLetSample = Math.sin(1);\nreadableLetSample + readableLetSample"
        readableLetSample
    , readable
        "readableConfig names effect binders from HasCallStack"
        "const readableBindSample = foo();\nreadableBindSample + readableBindSample;"
        (fromSyntax readableBindSample)
    , pretty
        "prettyJS formats if/else when biome is on PATH"
        "if (cond()) {foo();} else {bar();}"
        "if (cond()) {\n  foo();\n} else {\n  bar();\n}"
    , pretty
        "prettyJS preserves braces inside strings"
        "foo(\"{;}\");"
        "foo(\"{;}\");"
    , pretty
        "prettyJS formats try/catch when biome is on PATH"
        "try {foo();} catch (n0) {bar();}"
        "try {\n  foo();\n} catch (n0) {\n  bar();\n}"
    , pretty
        "prettyJS leaves invalid IIFE unchanged when biome rejects it"
        "function () {return 1;}()"
        "function () {return 1;}()"
    , readable "readableConfig pretty-prints ifE" "cond() ? 1 : 2;" $
        fromSyntax
          (toSyntax (ifE condE (expr (number 1)) (expr (number 2))) *> toSyntax noOp)
    , testCase "readableConfig Map.new is a snippet, not an IIFE" $ do
        out <-
          compileEffect readableConfig (fromSyntax (Map.withMap $ \m -> Map.clear m))
        out @?= "const n0 = new Map();\nn0.clear();"
        assertBool "no IIFE" (not ("(() => {" `BS.isInfixOf` out))
    , testCase "readableConfig $valueEq shim is multiline" $ do
        out <- compileEffect readableConfig (with2 fooE barE structuralEq)
        assertBool "shim binding" ("const $valueEq =" `BS.isInfixOf` out)
        assertBool "pretty body" ("{\n" `BS.isInfixOf` out)
        assertBool "no IIFE" (not ("(() => {" `BS.isInfixOf` out))
    , testCase "--readable sets OutputStyle Readable" $
        configStyle (applyCompilerArgs ["--readable"] defaultCompilerConfig)
          @?= Readable
    ]
 where
  readable :: String -> ByteString -> ClosedEffect u -> TestTree
  readable name golden e =
    testCase name (compileEffect readableConfig e >>= (@?= golden))
  pretty :: String -> ByteString -> ByteString -> TestTree
  pretty name input golden =
    testCase name (requireBiome >> prettyJS input >>= (@?= golden))

sampleArray :: ByteArray
sampleArray = packUint8 [1, 2, 3]
