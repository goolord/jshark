{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module BunTests (bunEvalTests) where

import qualified Control.Exception as Ex
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import JShark
import JShark.Api
import qualified JShark.Array as Array
import JShark.Bun
  ( BunConfig (..)
  , BunEnv (..)
  , HappyDomOptions (..)
  , defaultHappyDomOptions
  , domBunConfig
  , evaluateEffectJSON
  , evaluateEffectJSONWith
  )
import JShark.Bun.Internal (runJS, runJSWith)
import qualified JShark.Canvas as Canvas
import JShark.Compiler
import qualified JShark.Console as Console
import qualified JShark.Dom as Dom
import qualified JShark.Map as Map
import qualified JShark.Math as Math
import qualified JShark.Object as Object
import JShark.Rec (Rec (..), (<:))
import qualified JShark.Set as Set
import qualified JShark.Storage as Storage
import Support
import System.Directory (findExecutable)
import Test.Tasty
import Test.Tasty.HUnit

bunEvalTests :: TestTree
bunEvalTests =
  withResource (findExecutable "bun") (const (pure ())) $ \getBun ->
    testGroup
      "bun"
      [ testCase "bun is on PATH" $ do
          m <- getBun
          case m of
            Just _ -> pure ()
            Nothing ->
              assertFailure "bun not found on PATH; install https://bun.sh"
      , after AllSucceed "bun is on PATH" $
          testGroup
            "eval"
            [ bunCase "addition" (number 1 + number 2)
            , bunCase "subtraction" ((number 5 :: Expr f 'Number) - number 2)
            , bunCase
                "multiplication and division"
                ((number 6 :: Expr f 'Number) * number 7 / number 2)
            , bunCase "abs and negate" (abs (negate (number 5) :: Expr f 'Number))
            , bunCase "let used twice" (let_ (number 21) (\x -> x + x))
            , bunCase
                "nested single-use lets"
                (let_ (number 1) (\x -> let_ (number 2) (\y -> y + x)))
            , bunCase
                "lambda application"
                (apply (lambda (\x -> x * 2)) (number 21))
            , bunCase "if_ true" (if_ (bool True) (number 1) (number 2))
            , bunCase "if_ false" (if_ (bool False) (number 1) (number 2))
            , bunCase
                "&& short-circuit false"
                (And (bool False) (bool True))
            , bunCase
                "|| short-circuit true"
                (Or (bool True) (bool False))
            , bunCase "let on && LHS" (let_ (bool True) (\x -> And x (bool False)))
            , bunCase "let on && RHS" (let_ (bool True) (\x -> And (bool False) x))
            , bunCase
                "let in if_ branch"
                (let_ (number 5) (\x -> if_ (bool True) x (number 0)))
            , bunCase
                "optionCase Some"
                ( optionCase
                    (JShark.Api.some (number 5) :: Expr f ('Option 'Number))
                    (number 0)
                    (\x -> x + 1)
                )
            , bunCase
                "optionCase None"
                (optionCase (none :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1))
            , bunCase
                "some is the wrapped value"
                (JShark.Api.some (number 5) :: Expr f ('Option 'Number))
            , bunCase "none is null" (none :: Expr f ('Option 'Number))
            , bunCase "string concat" (Concat (string "a") (string "b"))
            , bunCase "Show number" (Show (number 3))
            , bunCase "Eq numbers" (Eq (number 1) (number 1))
            , bunCase "NEq numbers" (NEq (number 1) (number 2))
            , bunCase "array map" (Array.map numArray (\x -> x + number 1))
            , bunCase
                "array reduceRight"
                (Array.reduceRight numArray (number 0) (\acc x -> acc - x))
            , bunCase
                "array singleton"
                (Array.singleton (number 7))
            , bunCase
                "array singleton length"
                (Array.length (Array.singleton (number 7)))
            , bunCase
                "array join over options"
                ( Array.join
                    (Literal (ValueArray [ValueOption Nothing, ValueOption (Just (ValueNumber 1))]))
                    (string "-")
                )
            , bunCase
                "two comparisons share one $valueEq"
                (And (number 1 .== number 1) (number 2 .== number 2))
            , bunCase
                "letRec value rhs"
                (letRec (\_ -> number 1 + number 2) (\n -> n))
            , bunCase
                "option semigroup Maybe"
                (JShark.Api.some (string "a") <> JShark.Api.some (string "b"))
            , bunCase
                "array groupBy keys"
                ( Array.map
                    ( Array.groupBy numArray $ \n ->
                        if_ (n .== number 1) (string "one") (string "two")
                    )
                    (\g -> g.key)
                )
            , bunCase "array index" (Array.index numArray (number 1))
            , bunCase
                "array index 1.9 is the integer slot"
                (Array.index numArray (number 1.9))
            , bunCase
                "array zipWith"
                ( Array.zipWith
                    (+)
                    numArray
                    (Literal (ValueArray [ValueNumber 10, ValueNumber 20, ValueNumber 30]))
                )
            , bunCase "Math.sqrt" (sqrt (number 9))
            , bunCase "Math.round half toward +Infinity" (Math.round (number 2.5))
            , bunCase "Math.round negative half" (Math.round (number (-2.5)))
            , bunCase "Math.pow" (number 2 ** number 10)
            , bunCase "Math.sin 0" (sin (number 0))
            , bunCase
                "result ok number"
                (ok (number 5) :: Expr f ('Result 'String 'Number))
            , bunCase
                "result ok unit"
                (ok (Literal ValueUnit) :: Expr f ('Result 'String 'Unit))
            , effectCase
                "newByteArray is zeroed and the right length"
                ( fromSyntax
                    ( do
                        buf <- toSyntax (newByteArray (number 3))
                        yield (Eq (var buf) (uint8Array (bytes [0, 0, 0])))
                    )
                )
                "true"
            , bunCase "Uint8Array contents" (uint8Array (bytes [1, 2, 3]))
            , bunCase
                "Uint8Array Eq"
                (uint8Array (bytes [1, 2]) .== uint8Array (bytes [1, 2]))
            , bunCase
                "Show Uint8Array"
                (Show (uint8Array (bytes [1, 2, 3])))
            , testCase "prettyJS compileEffect ifE+LambdaE" $ do
                clearCompilerCache
                out <- compileEffect readableConfig prettyIfLambda
                assertBool "indented if body" ("{\n" `T.isInfixOf` out)
                got <- T.unpack <$> runJS (wrapReadableExpr out)
                assertEqual
                  ("expected 6\nbun JSON: " <> got <> "\njs:\n" <> T.unpack out)
                  "6"
                  got
            ]
      , after AllSucceed "bun is on PATH" $
          testGroup
            "evaluateEffectJSON"
            [ effectCase "Lift of addition" (expr (number 1 + number 2)) "3"
            , effectCase "unit is undefined" noOp "undefined"
            , effectCase
                "ifE true"
                (ifE (expr (bool True)) (expr (number 1)) (expr (number 2)))
                "1"
            , effectCase
                "FFI Math.max"
                (ffi "Math.max" (arg (number 2) <: arg (number 9) <: RecNil) :: Effect f 'Number)
                "9"
            , effectCase "object set then get" mutSetGet "21"
            , effectCase "Map insert then lookup" mapRoundTrip "\"v\""
            , effectCase "Set insert then member" setMember "true"
            , effectCase "Map foldM sums values" mapFold "3"
            , effectCase "Map mapM_ runs" mapForEach "undefined"
            , effectCase
                "catch_ of throw_"
                (catch_ (throw_ (string "boom")) (\_ -> expr (number 7)))
                "7"
            , effectCase
                "Array.fromEffects"
                (Array.fromEffects [expr (number 1), expr (number 2)])
                "[1,2]"
            , effectCase "program stdout does not corrupt the result" logHi "undefined"
            , effectCase
                "a promise result is awaited, not stringified as {}"
                (ffi "Promise.resolve" (arg (number 7) <: RecNil) :: Effect f 'Number)
                "7"
            , testCase "a rejected promise fails the run" $ do
                r <-
                  Ex.try
                    ( evaluateEffectJSON
                        (ffi "Promise.reject" (arg (string "nope") <: RecNil))
                    )
                case r of
                  Right out -> assertFailure ("expected a failure, got " <> T.unpack out)
                  Left (e :: Ex.IOException) -> do
                    let
                      msg = T.pack (show e)
                    assertBool
                      ("expected a non-zero exit, got " <> show e)
                      ("bun exited" `T.isInfixOf` msg)
                    -- Only emitted when bun actually wrote to stderr, and
                    -- never present in the echoed program.
                    assertBool
                      ("expected a stderr section, got " <> show e)
                      ("stderr:" `T.isInfixOf` msg)
            , effectCase
                "logged value is not mistaken for the result"
                (fromSyntax (Console.log (string "7") *> toSyntax (expr (number 1))))
                "1"
            , testCase "Lift agrees with evaluate" $ do
                got <- T.unpack <$> evaluateEffectJSON (expr mulDiv)
                assertEqual "Lift JSON" (encodeJSValue (evaluate mulDiv)) got
            , testCase "prettyIfLambda" $ do
                got <- T.unpack <$> evaluateEffectJSON prettyIfLambda
                assertEqual "expected 6" "6" got
            , testCase "no DOM in the sandbox" $ do
                r <- Ex.try (evaluateEffectJSON domInnerText)
                case r of
                  Right out -> assertFailure ("expected a failure, got " <> T.unpack out)
                  -- Not "document": the error echoes the program, which
                  -- contains document.getElementById whatever went wrong.
                  Left (e :: Ex.IOException) ->
                    assertBool
                      ("expected a ReferenceError, got " <> show e)
                      ("document is not defined" `T.isInfixOf` T.pack (show e))
            , testCase "a non-terminating program hits the timeout" $ do
                let
                  spin = renderJSCompact (effectfulProgram (while_ (expr (bool True)) noOp))
                r <- Ex.try (runJSWith 1000000 spin)
                case r of
                  Right out -> assertFailure ("expected a timeout, got " <> T.unpack out)
                  Left (e :: Ex.IOException) ->
                    assertBool
                      ("expected a timeout error, got " <> show e)
                      ("timed out" `T.isInfixOf` T.pack (show e))
            ]
      , after AllSucceed "bun is on PATH" $
          testGroup
            "happy-dom"
            [ testCase "happy-dom is available" $ do
                -- Also warms bun's install cache for the cases below.
                got <- T.unpack <$> evaluateEffectJSONWith domBunConfig (expr (number 1))
                assertEqual "registration works" "1" got
            , after AllSucceed "happy-dom is available" $
                testGroup
                  "dom"
                  [ domCase
                      "setInnerText then innerText"
                      "<div id=\"a\"></div>"
                      domInnerText
                      "\"hello\""
                  , domCase
                      "classAdd shows up in the class attribute"
                      "<div id=\"a\"></div>"
                      domClass
                      "\"on\""
                  , domCase
                      "createElement and appendChild are visible to querySelectorAll"
                      "<div id=\"a\"></div>"
                      domAppend
                      "1"
                  , domCase
                      "getElementById of a missing id is null"
                      "<div id=\"other\"></div>"
                      domMissing
                      "null"
                  , domCase "localStorage round trip" "" domStorage "\"v\""
                  , domCase
                      "happy-dom has no 2D canvas, and the Option says so"
                      "<canvas id=\"a\"></canvas>"
                      domCanvas
                      "\"no 2d\""
                  , testCase "window.location.hash" $ do
                      let
                        cfg =
                          domBunConfig
                            { bunEnv =
                                HappyDom
                                  defaultHappyDomOptions
                                    { happyDomUrl = "http://localhost/#done"
                                    }
                            }
                      got <- T.unpack <$> evaluateEffectJSONWith cfg domHash
                      assertEqual "hash" "\"#done\"" got
                  ]
            ]
      ]

bunCase :: String -> (forall f. Expr f u) -> TestTree
bunCase name e = testCase name (assertBunAgrees e)

effectCase :: String -> (forall f. Effect f u) -> String -> TestTree
effectCase name e expected = testCase name $ do
  got <- T.unpack <$> evaluateEffectJSON e
  assertEqual name expected got

mutSetGet :: forall f. Effect f 'Number
mutSetGet = fromSyntax $ do
  o <- toSyntax (Object.newObject :: Effect f ('MutableObject LitRow))
  _ <- Object.set @"x" (Lift (Var o)) (number 21)
  x <- (Var o).x
  yield x

mapRoundTrip :: forall f. Effect f 'String
mapRoundTrip = fromSyntax $ Map.withMap $ \m -> do
  _ <- Map.insert m (string "k") (string "v")
  v <- Map.lookup m (string "k")
  yield (orElse v (string "missing"))

setMember :: forall f. Effect f 'Bool
setMember = fromSyntax $ Set.withSet $ \s -> do
  _ <- Set.insert s (string "x")
  b <- Set.member s (string "x")
  yield b

mapFold :: forall f. Effect f 'Number
mapFold = fromSyntax $ Map.withMap $ \m -> do
  _ <- Map.insert m (string "a") (number 1)
  _ <- Map.insert m (string "b") (number 2)
  acc <- bindExpr $ Map.foldM (\a _ v -> a + v) (number 0) m
  yield acc

mapForEach :: forall f. Effect f 'Unit
mapForEach = fromSyntax $ Map.withMap $ \m -> do
  _ <- Map.insert m (string "x") (number 1)
  Map.mapM_ (\_ _ -> toSyntax noOp) m

logHi :: forall f. Effect f 'Unit
logHi = fromSyntax (Console.log (string "hi" :: Expr f 'String) *> done)

-- | An effect run against a seeded @document.body@.
domCase :: String -> Text -> (forall f. Effect f u) -> String -> TestTree
domCase name body e expected = testCase name $ do
  let
    cfg =
      domBunConfig
        { bunEnv = HappyDom defaultHappyDomOptions {happyDomBody = body}
        }
  got <- T.unpack <$> evaluateEffectJSONWith cfg e
  assertEqual name expected got

domInnerText :: forall f. Effect f 'String
domInnerText = fromSyntax $ do
  el <- Dom.lookupId (string "a")
  _ <- Dom.setInnerText el (string "hello")
  t <- Dom.innerText el
  yield t

domClass :: forall f. Effect f 'String
domClass = fromSyntax $ do
  el <- Dom.lookupId (string "a")
  _ <- Dom.classAdd el (string "on")
  c <- Dom.getAttribute el "class"
  yield c

domAppend :: forall f. Effect f 'Number
domAppend = fromSyntax $ do
  parent <- Dom.lookupId (string "a")
  child <- Dom.createElement (string "span")
  _ <- Dom.appendChild parent child
  nodes <- Dom.lookupSelector (string "#a span")
  n <- toSyntax nodes
  yield (Array.length (Var n))

domMissing :: forall f. Effect f ('Option ('MutableObject Dom.DomElement))
domMissing = fromSyntax $ do
  el <- Dom.lookupId (string "a")
  handle <- toSyntax el
  yield (unsafeNullable (Var handle))

domStorage :: forall f. Effect f 'String
domStorage = fromSyntax $ do
  _ <- Storage.setItem Storage.localStorage (string "k") (string "v")
  v <- Storage.getItem Storage.localStorage (string "k")
  yield (orElse v (string "missing"))

domHash :: forall f. Effect f 'String
domHash = fromSyntax (locationHash >>= yield)

-- | happy-dom implements no 2D context, so @getContext@ is @null@ —
-- which is what 'Canvas.getContext2d''s 'Option' already models.
domCanvas :: forall f. Effect f 'String
domCanvas = fromSyntax $ do
  el <- Dom.lookupId (string "a")
  ctx <- Canvas.getContext2d el
  handle <- toSyntax ctx
  yield (optionCase (Var handle) (string "no 2d") (\_ -> string "2d"))

wrapReadableExpr :: Text -> String
wrapReadableExpr src =
  let
    ls = filter (not . T.null) (T.lines (T.strip src))
   in
    case reverse ls of
      [] -> "undefined"
      result : revStmts ->
        T.unpack
          $ T.unlines
          $ "(() => {" : reverse revStmts ++ ["return " <> result, "})()"]

assertBunAgrees :: (forall f. Expr f u) -> IO ()
assertBunAgrees e = do
  let
    expected = encodeJSValue (evaluate e)
    program = renderJS (pureProgram e)
  got <- T.unpack <$> runJS program
  assertEqual
    ("evaluate JSON: " <> expected <> "\nbun JSON: " <> got <> "\njs:\n" <> program)
    expected
    got

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
  ValueUint8Array ba ->
    "{"
      ++ intercalate
        ","
        [ encodeJSString (show i) ++ ":" ++ show w
        | (i, w) <- zip [0 :: Int ..] (byteElems ba)
        ]
      ++ "}"
  ValueFrozen {} -> error "encodeJSValue: frozen objects are not JSON"
  ValueFunction _ -> error "encodeJSValue: functions are not JSON"

encodeResult :: Bool -> Value u -> String
encodeResult okFlag payload =
  let
    payloadJS = encodeJSValue payload
    okJS = if okFlag then "true" else "false"
   in
    if payloadJS == "undefined"
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
