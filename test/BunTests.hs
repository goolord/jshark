{-# LANGUAGE
    DataKinds
  , GADTs
  , LambdaCase
  , OverloadedRecordDot
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
#-}

module BunTests (bunEvalTests) where

import Control.Exception (IOException, bracket, catch)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import JShark
import JShark.Api
import JShark.Compiler
import qualified JShark.Array as Array
import qualified JShark.Math as Math
import JShark.Object ()
import Support
import System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

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
            , bunCase getBun "array map" (Array.map numArray (\x -> x + number 1))
            , bunCase getBun "array reduceRight"
                (Array.reduceRight numArray (number 0) (\acc x -> acc - x))
            , bunCase getBun "array singleton"
                (Array.singleton (number 7))
            , bunCase getBun "array singleton length"
                (Array.length (Array.singleton (number 7)))
            , bunCase getBun "array join over options"
                ( Array.join
                    (Literal (ValueArray [ValueOption Nothing, ValueOption (Just (ValueNumber 1))]))
                    (string "-")
                )
            , bunCase getBun "two comparisons share one $eq"
                (And (number 1 .== number 1) (number 2 .== number 2))
            , bunCase getBun "letRec value rhs"
                (letRec (\_ -> number 1 + number 2) (\n -> n))
            , bunCase getBun "option semigroup Maybe"
                (JShark.Api.some (string "a") <> JShark.Api.some (string "b"))
            , bunCase getBun "array groupBy keys"
                ( Array.map
                    (Array.groupBy numArray $ \n ->
                      if_ (n .== number 1) (string "one") (string "two"))
                    (\g -> g.key)
                )
            , bunCase getBun "array index" (Array.index numArray (number 1))
            , bunCase getBun "array index 1.9 is the integer slot"
                (Array.index numArray (number 1.9))
            , bunCase getBun "Math.sqrt" (sqrt (number 9))
            , bunCase getBun "Math.round half toward +Infinity" (Math.round (number 2.5))
            , bunCase getBun "Math.round negative half" (Math.round (number (-2.5)))
            , bunCase getBun "Math.pow" (number 2 ** number 10)
            , bunCase getBun "Math.sin 0" (sin (number 0))
            , bunCase getBun "result ok number"
                (ok (number 5) :: Expr f ('Result 'String 'Number))
            , bunCase getBun "result ok unit"
                (ok (Literal ValueUnit) :: Expr f ('Result 'String 'Unit))
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
  ValueResult (Right x) -> encodeResult True x
  ValueResult (Left x) -> encodeResult False x
  ValueRegex s -> encodeJSString (T.unpack s)
  ValueFrozen{} -> error "encodeJSValue: frozen objects are not JSON"
  ValueFunction _ -> error "encodeJSValue: functions are not JSON"

encodeResult :: Bool -> Value u -> String
encodeResult okFlag payload =
  let payloadJS = encodeJSValue payload
      okJS = if okFlag then "true" else "false"
   in if payloadJS == "undefined"
        then "{\"ok\":" ++ okJS ++ "}"
        else "{\"ok\":" ++ okJS ++ ",\"value\":" ++ payloadJS ++ "}"

encodeJSNumber :: Double -> String
encodeJSNumber d
  | isNaN d || isInfinite d = "null"
  | isInt = show (truncate d :: Integer)
  | otherwise = show d
  where
    isInt = d == fromInteger (truncate d)

encodeJSString :: String -> String
encodeJSString s = '"' : escapeJsString s ++ "\""

ignoreIO :: IOException -> IO ()
ignoreIO _ = pure ()
