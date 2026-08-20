{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}

module Main (main) where

import JShark
import JShark.Api
import JShark.Types
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
  ]

codegenTests :: TestTree
codegenTests = testGroup "codegen"
  [ testCase "pure arithmetic" $
      renderJS (pureAST (plus (number 1) (number 2))) @?= "1.0 + 2.0"
  , testCase "let renders as a const binding" $
      renderJS (pureAST (let_ (number 5) (\x -> x + x)))
        @?= "const n0 = 5.0;\nn0 + n0"
  , testCase "lambda application renders as an IIFE-style call" $
      renderJS (pureAST (apply (lambda (\x -> x * 2)) (number 21)))
        @?= "const n1 = function (n0) {return (n0 * 2.0)};\nn1(21.0)"
  , testCase "effectful console.log FFI call" $
      renderJS (effectfulAST (fromSyntax (consoleLog (string "hi" :: Expr f 'String) *> toSyntax noOp)))
        @?= "const n0 = console.log(\"hi\");\nn0"
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
        @?= "let n0;\nif (true) {n0 = 1.0;}\nelse {n0 = 2.0;}\nconst n1 = n0;\nn1"
  , testCase "while_ renders a while loop" $
      renderJS (effectfulAST (fromSyntax (toSyntax_ (while_ (bool False) (Bind (expr (number 1)) (\_ -> Lift (Literal ValueUnit)))) *> toSyntax noOp)))
        @?= "while (false) {const n0 = 1.0;}\nn0"
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
  , testCase "Array.map_ callback with an internal let doesn't collide with the parameter" $
      renderJS (pureAST (Array.map_ numArray (\x -> let_ (x + number 1) (\y -> y * 2))))
        @?= "[1.0, 2.0].map(function (n0) {const n1 = n0 + 1.0;\n                              return n1 * 2.0})"
  , testCase "Array.join renders as .join" $
      renderJS (pureAST (Array.join numArray (string ",")))
        @?= "[1.0, 2.0].join(\",\")"
  , testCase "Array.push renders as a mutating .push call" $
      renderJS (effectfulAST (fromSyntax (toSyntax (Array.push numArray (number 3)) *> toSyntax noOp)))
        @?= "const n0 = [1.0, 2.0].push(3.0);\nn0"
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
        @?= "const n0 = console.log(\"hi\");\nn0"
  , testCase "Dom appendChild reuses bound handles instead of re-running lookups" $
      renderJS (effectfulAST (fromSyntax (do
        p <- Dom.lookupId (string "p")
        c <- Dom.createElement (string "div")
        _ <- Dom.appendChild p c
        toSyntax noOp)))
        @?= "const n0 = document.getElementById(\"p\");\nconst n1 = document.createElement(\"div\");\nconst n2 = n0.appendChild(n1);\nn2"
  , testCase "Storage.getItem is typed as an Option and dispatches via optionCase" $
      renderJS (effectfulAST (fromSyntax (do
        v <- Storage.getItem Storage.localStorage (string "k")
        toSyntax (expr (optionCase v (string "missing") (\x -> x))))))
        @?= "const n0 = localStorage.getItem(\"k\");\nconst n1 = n0;\nconst n2 = (n1 === null ? \"missing\" : n1);\nn2"
  ]
