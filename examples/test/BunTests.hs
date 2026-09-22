{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module BunTests (bunEvalTests) where

import BunGate (bunGroup)
import qualified Control.Exception as Ex
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import JShark
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
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
import JShark.Bun.Internal (runJS, runJSTagged, runJSWith)
import qualified JShark.Canvas as Canvas
import JShark.Compiler
import qualified JShark.Console as Console
import qualified JShark.Dom as Dom
import JShark.Example.Life (initialCatalogCells, initialPop, soupSeedPop)
import JShark.Example.Life.GridApi (seedLiveCells, seedSoupRegion)
import qualified JShark.Json as Json
import qualified JShark.Map as Map
import qualified JShark.Math as Math
import qualified JShark.Object as Object
import JShark.Promise (Promise, promiseCatch, promiseThen)
import qualified JShark.Set as Set
import qualified JShark.Storage as Storage
import Test.Support
import Test.Tasty
import Test.Tasty.HUnit

bunEvalTests :: TestTree
bunEvalTests =
  bunGroup
    "bun"
    [ testGroup "eval" evalCases
    , testGroup "evaluateEffectJSON" effectCases
    , testGroup
        "happy-dom"
        [ -- Also warms bun's install cache for the cases below.
          testCase "happy-dom is available" $
            evaluateEffectJSONWith domBunConfig (expr (number 1)) >>= (@?= "1")
        , after AllSucceed "happy-dom is available" (testGroup "dom" domCases)
        ]
    ]

evalCases :: [TestTree]
evalCases =
  [ bunCase "addition" (number 1 + number 2)
  , bunCase "bigint add via toString" (toString (bigInt 10 + bigInt 3))
  , bunCase "bigint exact via toString" $
      toString (bigInt (2 ^ (80 :: Int) + 1))
  , bunCase "bigint literal via toString" (toString (bigInt 42))
  , taggedCase "tagged NaN" "number:NaN" (Literal (ValueNumber (0 / 0)))
  , taggedCase "tagged +Infinity" "number:Infinity" $
      Literal (ValueNumber (1 / 0))
  , taggedCase "tagged -Infinity" "number:-Infinity" $
      Literal (ValueNumber (-1 / 0))
  , taggedCase "tagged negative zero" "number:-0" $
      Literal (ValueNumber (-0.0))
  , taggedCase "tagged undefined" "undefined" (Literal ValueUnit)
  , taggedCase "tagged bigint" "bigint:42" (bigInt 42)
  , bunCase "subtraction" ((number 5 :: Expr f 'Number) - number 2)
  , bunCase "multiplication and division" $
      (number 6 :: Expr f 'Number) * number 7 / number 2
  , bunCase "abs and negate" (abs (negate (number 5) :: Expr f 'Number))
  , bunCase "let used twice" (let_ (number 21) (\x -> x + x))
  , bunCase "nested single-use lets" $
      let_ (number 1) (\x -> let_ (number 2) (\y -> y + x))
  , bunCase "lambda application" (apply (lambda (\x -> x * 2)) (number 21))
  , bunCase "if_ true" (if_ (bool True) (number 1) (number 2))
  , bunCase "if_ false" (if_ (bool False) (number 1) (number 2))
  , bunCase "&& short-circuit false" (And (bool False) (bool True))
  , bunCase "|| short-circuit true" (Or (bool True) (bool False))
  , bunCase "let on && LHS" (let_ (bool True) (\x -> And x (bool False)))
  , bunCase "let on && RHS" (let_ (bool True) (\x -> And (bool False) x))
  , bunCase "let in if_ branch" $
      let_ (number 5) (\x -> if_ (bool True) x (number 0))
  , bunCase "if_ does not evaluate its untaken branch" $
      untaken (bool False) (\b v -> if_ b (let_ v (\x -> x + x)) (number 0))
  , bunCase "|| does not evaluate a short-circuited right side" $
      untaken (bool True) (\b v -> Or b (let_ v (\x -> x .== x)))
  , bunCase "&& does not evaluate a short-circuited right side" $
      untaken (bool False) (\b v -> And b (let_ v (\x -> x .== x)))
  , bunCase "optionCase Some" $
      optionCase
        (JShark.Api.some (number 5) :: Expr f ('Option 'Number))
        (number 0)
        (\x -> x + 1)
  , bunCase "optionCase None" $
      optionCase (none :: Expr f ('Option 'Number)) (number 0) (\x -> x + 1)
  , bunCase "some is the wrapped value" $
      (JShark.Api.some (number 5) :: Expr f ('Option 'Number))
  , bunCase "none is tagged none" (none :: Expr f ('Option 'Number))
  , bunCase "some none nests faithfully" $
      JShark.Api.some (none :: Expr f ('Option 'Number))
  , bunCase "unsafeNullable of undefined is none" $
      unsafeNullable (Literal ValueUnit)
  , bunCase "unsafeNullable preserves a tagged none as present" $
      unsafeNullable (none :: Expr f ('Option 'Number))
  , bunCase "string concat" (Concat (string "a") (string "b"))
  , bunCase "Show number" (Show (number 3))
  , bunCase "Eq numbers" (number 1 .== number 1)
  , bunCase "NEq numbers" (number 1 .!= number 2)
  , bunCase "array map" (Array.map numArray (\x -> x + number 1))
  , bunCase "array reduceRight" $
      Array.reduceRight numArray (number 0) (\acc x -> acc - x)
  , bunCase "array singleton" (Array.singleton (number 7))
  , bunCase "array singleton length" $
      Array.length (Array.singleton (number 7))
  , bunCase "array join over options" $
      Array.join
        ( Literal
            (ValueArray [ValueOption Nothing, ValueOption (Just (ValueNumber 1))])
        )
        (string "-")
  , bunCase "two comparisons share one $valueEq" $
      And (number 1 .== number 1) (number 2 .== number 2)
  , bunCase "letRec value rhs" (letRec (\_ -> number 1 + number 2) (\n -> n))
  , bunCase "option semigroup Maybe" $
      JShark.Api.some (string "a") <> JShark.Api.some (string "b")
  , bunCase "array groupBy keys" $
      Array.map
        ( Array.groupBy numArray $ \n ->
            if_ (n .== number 1) (string "one") (string "two")
        )
        (\g -> g.key)
  , bunCase "array index" (Array.index numArray (number 1))
  , bunCase "array index 1.9 is the integer slot" $
      Array.index numArray (number 1.9)
  , bunCase "id then apply stays curried" $
      apply (apply (lambda (\f -> f)) (lambda (\x -> x + number 1))) (number 2)
  , effectCase "capturing reduce indexes with both args" "\"100\"" $
      capturingReduceIndex
  , bunCase "Math.sqrt" (sqrt (number 9))
  , bunCase "Math.round half toward +Infinity" (Math.round (number 2.5))
  , bunCase "Math.round negative half" (Math.round (number (-2.5)))
  , bunCase "Math.pow" (number 2 ** number 10)
  , bunCase "Math.sin 0" (sin (number 0))
  , bunCase "result ok number" (ok (number 5) :: Expr f ('Result 'String 'Number))
  , bunCase "result ok unit" $
      (ok (Literal ValueUnit) :: Expr f ('Result 'String 'Unit))
  , effectCase "newByteArray is zeroed and the right length" "true" $
      fromSyntax $ do
        buf <- toSyntax (newByteArray (number 3))
        yield (structuralEq (var buf) (uint8Array (packUint8 [0, 0, 0])))
  , effectCase "u8Set wraps a Uint8Array and clamps a Uint8ClampedArray" "44255" $
      clampVsWrap
  , effectCase "catalog seed stamps non-zero species" "46" $
      fromSyntax (seeded >>= \(_, s) -> yield (u8Index s (number 196928)))
  , effectCase "soup seed pop matches Haskell reference" (tshow soupSeedPop) $
      fromSyntax $ do
        a <- grid
        seedSoup a
        countAlive a >>= yield
  , effectCase "full init pop matches Haskell reference" (tshow initialPop) $
      fromSyntax (seeded >>= countAlive . fst >>= yield)
  , bunCase "Uint8Array contents" (uint8Array (packUint8 [1, 2, 3]))
  , bunCase "Uint8Array Eq" $
      structuralEq (uint8Array (packUint8 [1, 2])) (uint8Array (packUint8 [1, 2]))
  , bunCase "Show Uint8Array" (Show (uint8Array (packUint8 [1, 2, 3])))
  , testCase "compileEffect ifE+LambdaE evaluates" $
      compileEffect defaultCompilerConfig prettyIfLambda >>= assertRuns "6"
  ]

effectCases :: [TestTree]
effectCases =
  [ effectCase "Lift of addition" "3" (expr (number 1 + number 2))
  , effectCase "unit is undefined" "undefined" noOp
  , effectCase "ifE true" "1" $
      ifE (expr (bool True)) (expr (number 1)) (expr (number 2))
  , effectCase "FFI Math.max" "9" $
      (ffi "Math.max" (arg (number 2) <: arg (number 9) <: RecNil) :: Effect f 'Number)
  , effectCase "object set then get" "21" $ fromSyntax $ do
      o <- toSyntax (Object.newObject :: Effect f ('MutableObject LitRow))
      _ <- Object.set @"x" (Lift (Var o)) (number 21)
      (Var o).x >>= yield
  , effectCase "u8 read bound before a write keeps the pre-write value" "0" $
      u8ReadBeforeWrite
  , effectCase "runtime-indexed read bound before a write keeps the value" "0" $
      runtimeIndexReadBeforeWrite
  , effectCase "two allocations keep distinct identity" "true" allocationIdentity
  , effectCase "unsafeOptionToNative passes null / value to a foreign call" "42" $
      optionArgNative
  , effectCase "Map insert then lookup" "\"v\"" $
      fromSyntax . Map.withMap $ \m -> do
        _ <- Map.insert m (string "k") (string "v")
        v <- Map.lookup m (string "k")
        yield (orElse v (string "missing"))
  , effectCase "groupBy skips holes and keeps first-seen key order" "\"a,b\"" $
      groupBySparseKeys
  , effectCase "Set insert then member" "true" $
      fromSyntax . Set.withSet $ \s -> do
        _ <- Set.insert s (string "x")
        Set.member s (string "x") >>= yield
  , effectCase "Map foldM sums values" "3" $
      fromSyntax . Map.withMap $ \m -> do
        _ <- Map.insert m (string "a") (number 1)
        _ <- Map.insert m (string "b") (number 2)
        bindExpr (Map.foldM (\a _ v -> a + v) (number 0) m) >>= yield
  , effectCase "Map mapM_ runs" "undefined" $
      fromSyntax . Map.withMap $ \m -> do
        _ <- Map.insert m (string "x") (number 1)
        Map.mapM_ (\_ _ -> toSyntax noOp) m
  , effectCase "JSON.stringify of a number" "\"1\"" $
      jsonOr "none" (Json.stringify (number 1))
  , effectCase "JSON.stringify of undefined is none" "\"none\"" $
      jsonOr "none" (Json.stringify (Literal ValueUnit))
  , effectCase "JSON.stringify of BigInt throws" "\"caught\"" $
      jsonOr "caught" (catch_ (Json.stringify (bigInt 1)) (\_ -> expr none))
  , effectCase "catch_ of throw_" "7" $
      catch_ (throw_ (string "boom")) (\_ -> expr (number 7))
  , effectCase "Array.fromEffects" "[1,2]" $
      Array.fromEffects [expr (number 1), expr (number 2)]
  , effectCase "program stdout does not corrupt the result" "undefined" $
      fromSyntax (Console.log (string "hi" :: Expr f 'String) *> done)
  , effectCase "a promise result is awaited, not stringified as {}" "7" $
      (ffi "Promise.resolve" (arg (number 7) <: RecNil) :: Effect f 'Number)
  , effectCase "promiseThen resolves the handler result" "6" $
      chained (resolved 5) (\p -> promiseThen p (\x -> expr (Var x + number 1)))
  , effectCase "promiseCatch receives the rejection reason" "\"caught:boom\"" $
      chained (ffi "Promise.reject" (arg (string "boom") <: RecNil)) $ \p ->
        promiseCatch p (\e -> expr (Concat (string "caught:") (Var e)))
  , -- A fulfilled promise passes its value through @.catch@ untouched; the
    -- handler only runs on rejection. Recovery must preserve the resolution
    -- type, which is why @promiseCatch@ returns @Promise u@.
    effectCase "promiseCatch passes a fulfilled value through" "5" $
      chained (resolved 5) (\p -> promiseCatch p (\_ -> expr (number 99)))
  , effectCase "promiseThen adopts a returned promise" "7" $
      chained (resolved 1) (\p -> promiseThen p (\_ -> resolved 7))
  , -- Adoption keeps the resolution type usable: chain another @.then@ on
    -- the adopted promise and map its number.
    effectCase "promiseThen adopts then chains the adopted promise" "8" $
      chained (resolved 1) $ \p -> do
        adopted <- promiseThen p (\_ -> resolved 7)
        promiseThen adopted (\x -> expr (Var x + number 1))
  , -- "stderr:" appears only when bun wrote to stderr, never in the echoed
    -- program.
    testCase "a rejected promise fails the run"
      $ failsWith ["bun exited", "stderr:"]
      $ evaluateEffectJSON (ffi "Promise.reject" (arg (string "nope") <: RecNil))
  , effectCase "logged value is not mistaken for the result" "1" $
      fromSyntax (Console.log (string "7") *> toSyntax (expr (number 1)))
  , testCase "Lift agrees with evaluate" $
      evaluateEffectJSON (expr mulDiv)
        >>= (@?= T.pack (encodeJSValue (evaluate mulDiv)))
  , effectCase "prettyIfLambda" "6" prettyIfLambda
  , -- Not "document": the error echoes the program, which contains
    -- document.getElementById whatever went wrong.
    testCase "no DOM in the sandbox" $
      failsWith ["document is not defined"] (evaluateEffectJSON domInnerText)
  , testCase "a non-terminating program hits the timeout"
      $ failsWith ["timed out"] . runJSWith 1000000 . BC.unpack . renderJS
      $ effectfulProgram (while_ (expr (bool True)) noOp)
  ]

domCases :: [TestTree]
domCases =
  [ domCase "setInnerText then innerText" divA "\"hello\"" domInnerText
  , domCase "classAdd shows up in the class attribute" divA "\"on\"" domClass
  , domCase "a missing attribute is None" divA "\"missing\"" domAttrMissing
  , domCase
      "createElement and appendChild are visible to querySelectorAll"
      divA
      "1"
      domAppend
  , domCase
      "lookupSelector yields a real Array (map works)"
      (withBody "<div id=\"a\"><span></span><span></span><span></span></div>")
      "3"
      domSelectorMap
  , domCase "getElementById of a missing id is none" divOther "{\"some\":false}" $
      domMissing
  , domCase "lookupIdOption of a missing id is none" divOther "\"absent\"" $
      domMissingOption
  , domCase "localStorage round trip" (withBody "") "\"v\"" domStorage
  , domCase
      "happy-dom has no 2D canvas, and the Option says so"
      (withBody "<canvas id=\"a\"></canvas>")
      "\"no 2d\""
      domCanvas
  , domCase
      "window.location.hash"
      defaultHappyDomOptions {happyDomUrl = "http://localhost/#done"}
      "\"#done\""
      (fromSyntax (locationHash >>= yield))
  ]
 where
  divA = withBody "<div id=\"a\"></div>"
  divOther = withBody "<div id=\"other\"></div>"
  withBody b = defaultHappyDomOptions {happyDomBody = b}

-- | Assert bun's JSON for a pure expression matches the Haskell evaluator.
bunCase :: String -> (forall f. Expr f u) -> TestTree
bunCase name e =
  testCase name $
    assertRuns (encodeJSValue (evaluate e)) (renderJS (pureProgram e))

-- | Run a program with bun and compare its JSON result.
assertRuns :: String -> BC.ByteString -> Assertion
assertRuns expected js = do
  got <- T.unpack <$> runJS (BC.unpack js)
  assertEqual
    ("expected: " <> expected <> "\nbun JSON: " <> got <> "\njs:\n" <> BC.unpack js)
    expected
    got

-- | Assert the tagged observation of a compiled expression. Unlike the
-- plain-JSON path this keeps NaN, the infinities, @-0@, @undefined@, and
-- @BigInt@ distinct.
taggedCase :: String -> Text -> (forall f. Expr f u) -> TestTree
taggedCase name expected e =
  testCase name $
    runJSTagged (BC.unpack (renderJS (pureProgram e))) >>= assertEqual name expected

effectCase :: String -> Text -> (forall f. Effect f u) -> TestTree
effectCase name expected e =
  testCase name $ evaluateEffectJSON e >>= assertEqual name expected

-- | An effect run under happy-dom with the given options.
domCase ::
  String -> HappyDomOptions -> Text -> (forall f. Effect f u) -> TestTree
domCase name opts expected e =
  testCase name $
    evaluateEffectJSONWith domBunConfig {bunEnv = HappyDom opts} e
      >>= assertEqual name expected

-- | Assert a run fails with an 'Ex.IOException' mentioning every needle.
failsWith :: [Text] -> IO Text -> Assertion
failsWith needles run =
  Ex.try run >>= \case
    Right out -> assertFailure ("expected a failure, got " <> T.unpack out)
    Left (e :: Ex.IOException) -> forM_ needles $ \n ->
      assertBool
        ("expected " <> show n <> ", got " <> show e)
        (n `T.isInfixOf` T.pack (show e))

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Apply @k@ to @flag@ and an out-of-bounds 'numArray' read, which throws
-- if @k@ evaluates it.
untaken ::
  Expr f 'Bool -> (Expr f 'Bool -> Expr f 'Number -> Expr f v) -> Expr f v
untaken flag k =
  apply
    (apply (lambda $ \b -> lambda $ \i -> k b (Array.index numArray i)) flag)
    (number 99)

-- | Life HUD path: capturing 'Array.reduce' then 'Array.index'.
-- Uncurrying an opaque @f(a, b)@ dropped @b@ and @$checkedIndex@ threw.
capturingReduceIndex :: Effect f 'String
capturingReduceIndex =
  expr $
    let_ (lit (map ValueNumber [0.5, 1, 2])) $ \levels ->
      let_ (lit (map ValueNumber [0, 1, 2])) $ \indices ->
        let_ (lit (map ValueString ["50", "100", "200"])) $ \labels ->
          Array.index labels $
            Array.reduce indices (number 0) $ \bestIdx i ->
              let
                dist j = abs (Array.index levels j - number 1)
               in
                if_ (dist i .< dist bestIdx) i bestIdx
 where
  lit :: [Value u] -> Expr f ('Array u)
  lit = Literal . ValueArray

-- | Read a byte before writing it. The read is a mutable access and must
-- not be moved across the write, so the bound value stays the zeroed byte.
u8ReadBeforeWrite :: forall f. Effect f 'Number
u8ReadBeforeWrite = fromSyntax $ do
  buf <- bindExpr (newByteArray (number 1))
  old <- bindExpr (expr (u8Index buf (number 0)))
  toSyntax_ (u8Set buf (number 0) (number 7))
  yield old

-- | 'u8ReadBeforeWrite' with the index from a runtime call, so constant
-- folding cannot hide a reordering.
runtimeIndexReadBeforeWrite :: forall f. Effect f 'Number
runtimeIndexReadBeforeWrite = fromSyntax $ do
  buf <- bindExpr (newByteArray (number 4))
  i <- bindExpr (ffi "(() => 0)" RecNil)
  old <- bindExpr (expr (u8Index buf i))
  toSyntax_ (u8Set buf i (number 9))
  yield old

-- | Writing one of two independent allocations does not touch the other.
allocationIdentity :: forall f. Effect f 'Bool
allocationIdentity = fromSyntax $ do
  a <- bindExpr (newByteArray (number 1))
  b <- bindExpr (newByteArray (number 1))
  toSyntax_ (u8Set a (number 0) (number 1))
  yield
    ((u8Index a (number 0) .== number 1) .&& (u8Index b (number 0) .== number 0))

-- | A foreign callee that expects @number | null@. The tagged 'Option'
-- must be unwrapped at the boundary: @none@ becomes native @null@ and
-- @some n@ becomes @n@.
optionArgNative :: Effect f 'Number
optionArgNative = fromSyntax $ do
  absent <- bindExpr (orOne none)
  present <- bindExpr (orOne (some (number 41)))
  yield (absent + present)
 where
  orOne :: Expr g ('Option 'Number) -> Effect g 'Number
  orOne o =
    ffi "((x) => x === null ? 1 : x)" (arg (unsafeOptionToNative o) <: RecNil)

-- | A sparse array (hole at 0) with first-seen keys @a@, @b@, @a@: the
-- generated @$groupBy@ must skip the hole and keep first-seen key order.
groupBySparseKeys :: forall f. Effect f 'String
groupBySparseKeys = fromSyntax $ do
  arr <- bindExpr (ffiExpr sparse RecNil :: Effect f ('Array 'String))
  g <- bindExpr (expr (Array.groupBy arr (\x -> x)))
  keys <- bindExpr (expr (Array.map g (\grp -> grp.key)))
  yield (Array.join keys (string ","))
 where
  sparse = "(function(){var a=[];a[1]='a';a[2]='b';a[3]='a';return a;})()"

-- | A 300 write wraps to 44 on a @Uint8Array@ and clamps to 255 on a
-- @Uint8ClampedArray@: @44*1000 + 255@.
clampVsWrap :: forall f. Effect f 'Number
clampVsWrap = fromSyntax $ do
  u8 <- bindExpr (newByteArray (number 1))
  c8 <-
    bindExpr
      (ffi "(() => new Uint8ClampedArray(1))" RecNil :: Effect f 'Uint8ClampedArray)
  toSyntax_ (u8Set u8 (number 0) (number 300))
  toSyntax_ (u8Set c8 (number 0) (number 300))
  a <- bindExpr (expr (u8Index u8 (number 0)))
  b <- bindExpr (expr (u8Index c8 (number 0)))
  yield (a * number 1000 + b)

jsonOr :: Text -> Effect f ('Option 'String) -> Effect f 'String
jsonOr dflt m = fromSyntax $ do
  s <- bindExpr m
  yield (orElse s (string dflt))

-- | @Promise.resolve(n)@.
resolved :: Double -> Effect f ('MutableObject (Promise 'Number))
resolved n = ffi "Promise.resolve" (arg (number n) <: RecNil)

-- | Hold a promise, chain onto it with @k@, and return the result.
chained ::
  Effect f ('MutableObject (Promise u))
  -> (Effect f ('MutableObject (Promise u)) -> EffectSyntax f (Effect f v))
  -> Effect f v
chained p k = fromSyntax (hold p >>= k >>= toSyntax)

domInnerText :: Effect f 'String
domInnerText = fromSyntax $ do
  el <- Dom.lookupId (string "a")
  _ <- Dom.setInnerText el (string "hello")
  Dom.innerText el >>= yield

domClass :: Effect f 'String
domClass = fromSyntax $ do
  el <- Dom.lookupId (string "a")
  _ <- Dom.classAdd el (string "on")
  c <- Dom.getAttribute el "class"
  yield (orElse c (string ""))

domAttrMissing :: Effect f 'String
domAttrMissing = fromSyntax $ do
  el <- Dom.lookupId (string "a")
  c <- Dom.getAttribute el "data-nope"
  yield (orElse c (string "missing"))

domAppend :: Effect f 'Number
domAppend = fromSyntax $ do
  parent <- Dom.lookupId (string "a")
  child <- Dom.createElement (string "span")
  _ <- Dom.appendChild parent child
  nodes <- Dom.lookupSelector (string "#a span")
  n <- toSyntax nodes
  yield (Array.length (Var n))

-- | @Array.map@ only exists on a real array. If @lookupSelector@ returned a
-- raw @NodeList@ this program would fail at runtime.
domSelectorMap :: Effect f 'Number
domSelectorMap = fromSyntax $ do
  nodes <- Dom.lookupSelector (string "#a span")
  n <- toSyntax nodes
  yield (Array.length (Array.map (Var n) (\_ -> number 1)))

domMissing :: Effect f ('Option ('MutableObject Dom.DomElement))
domMissing = fromSyntax $ do
  el <- Dom.lookupId (string "a")
  handle <- toSyntax el
  yield (unsafeNullable (Var handle))

domMissingOption :: Effect f 'String
domMissingOption = fromSyntax $ do
  opt <- Dom.lookupIdOption (string "nope") >>= bindExpr
  yield (optionCase opt (string "absent") (\_ -> string "present"))

domStorage :: Effect f 'String
domStorage = fromSyntax $ do
  _ <- Storage.setItem Storage.localStorage (string "k") (string "v")
  v <- Storage.getItem Storage.localStorage (string "k")
  yield (orElse v (string "missing"))

-- | happy-dom implements no 2D context, so @getContext@ is @null@ —
-- which is what 'Canvas.getContext2d''s 'Option' already models.
domCanvas :: Effect f 'String
domCanvas = fromSyntax $ do
  el <- Dom.lookupId (string "a")
  ctx <- Canvas.getContext2d el
  handle <- toSyntax ctx
  yield (optionCase (Var handle) (string "no 2d") (\_ -> string "2d"))

-- | What bun's @JSON.stringify@ prints for a value.
encodeJSValue :: Value u -> String
encodeJSValue = \case
  ValueNumber d
    | isNaN d || isInfinite d -> "null"
    | d == fromInteger (truncate d) -> show (truncate d :: Integer)
    | otherwise -> show d
  ValueBool b -> map toLower (show b)
  ValueString s -> encodeJSString (T.unpack s)
  ValueUnit -> "undefined"
  ValueArray xs -> "[" ++ intercalate "," (map encodeJSValue xs) ++ "]"
  ValueOption Nothing -> "{\"some\":false}"
  ValueOption (Just x) -> "{\"some\":true,\"value\":" ++ encodeJSValue x ++ "}"
  ValueResult r -> either (encodeResult "false") (encodeResult "true") r
  ValueRegex s -> encodeJSString (T.unpack s)
  ValueUint8Array ba -> encodeU8 (uint8Elems ba)
  ValueUint8ClampedArray ba -> encodeU8 (uint8Elems ba)
  ValueFrozen {} -> error "encodeJSValue: frozen objects are not JSON"
  ValueFunction _ -> error "encodeJSValue: functions are not JSON"
  ValueBigInt {} -> error "encodeJSValue: bigint is not JSON"
 where
  encodeJSString s = '"' : escapeJsString s ++ "\""
  encodeU8 ws =
    "{"
      ++ intercalate
        ","
        [encodeJSString (show i) ++ ":" ++ show w | (i, w) <- zip [0 :: Int ..] ws]
      ++ "}"
  encodeResult :: String -> Value v -> String
  encodeResult okJS payload = case encodeJSValue payload of
    "undefined" -> "{\"ok\":" ++ okJS ++ "}"
    js -> "{\"ok\":" ++ okJS ++ ",\"value\":" ++ js ++ "}"

-- | A 1024x768 Life grid.
grid :: EffectSyntax f (Expr f 'Uint8Array)
grid = fmap var (toSyntax (newByteArray (number 786432)))

seedSoup :: Expr f 'Uint8Array -> EffectSyntax f ()
seedSoup a =
  toSyntax_ $
    seedSoupRegion
      a
      (number 256)
      (number 192)
      (number 512)
      (number 384)
      (number 1024)
      (number 42)

countAlive :: forall f. Expr f 'Uint8Array -> EffectSyntax f (Expr f 'Number)
countAlive buf = do
  popRef <- hold newObject
  _ <- setProp popRef "n" (number 0)
  _ <- forRange_ (number 0) (u8Len buf) $ \i -> do
    whenS (u8Index buf i .== 1) $ do
      n <- getProp popRef "n"
      setProp popRef "n" (n + 1)
  getProp popRef "n"

-- | Soup plus catalog stamp: the alive and species grids.
-- 'initialCatalogCells' must match @examples/Life/js/catalog.js@ (see
-- CatalogTests).
seeded :: EffectSyntax f (Expr f 'Uint8Array, Expr f 'Uint8Array)
seeded = do
  a <- grid
  s <- grid
  seedSoup a
  toSyntax_ (seedLiveCells a s initialCatalogCells)
  pure (a, s)
