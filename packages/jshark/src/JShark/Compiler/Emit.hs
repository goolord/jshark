{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JavaScript text for codegen: a 'Builder' with O(1) emptiness, JS number
-- semantics for constant folding, string quoting, and the runtime preamble.
--
-- The preamble holds compiler-owned shims ('Builtin', a closed catalog) and
-- program-hoisted @$name@ helpers. Hoisted bodies deduplicate after
-- alpha-renaming ('canonicalHoistSrc'), and everything prints name-sorted as
-- @const $name = …;@, so call order does not matter.
module JShark.Compiler.Emit
  ( JS
  , renderJS
  , ($$)
  , (<+>)
  , parens
  , brackets
  , braces
  , semi
  , colon
  , dquotes
  , jsText
  , jsString
  , jsDouble
  , jsDecimal
  , hcat
  , vcat
  , blockBody
  , iifeBody
  , nonEmpty
  , escapeJsString
  , jsQuote

    -- * JS number semantics
  , jsRem
  , jsBit2
  , jsShl
  , jsShr
  , jsUShr

    -- * Preamble
  , Builtin (..)
  , Preamble
  , emptyPreamble
  , useShim
  , insertHoisted
  , renderPreambleStyled
  , builtinSrc
  , hoistTagName
  )
where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import qualified Data.Char as Char
import Data.Int (Int32)
import Data.List (intersperse, nub, sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import Numeric (showFFloat, showHex)

-- | JavaScript codegen. Empty is tracked separately so 'renderJS' stays a
-- single materialization and '$$' needs no per-node allocation.
data JS
  = Empty
  | NonEmpty !BB.Builder

instance Semigroup JS where
  Empty <> b = b
  a <> Empty = a
  NonEmpty a <> NonEmpty b = NonEmpty (a <> b)

instance Monoid JS where
  mempty = Empty

instance IsString JS where
  fromString = NonEmpty . BB.stringUtf8

-- | Materialize a codegen tree to strict 'ByteString'.
renderJS :: JS -> ByteString
renderJS Empty = BS.empty
renderJS (NonEmpty b) = BL.toStrict (BB.toLazyByteString b)

infixl 5 $$

($$) :: JS -> JS -> JS
a $$ b
  | isEmpty a = b
  | isEmpty b = a
  | otherwise = a <> "\n" <> b

infixl 6 <+>

(<+>) :: JS -> JS -> JS
a <+> b = a <> " " <> b

parens :: JS -> JS
parens b = "(" <> b <> ")"

brackets :: JS -> JS
brackets b = "[" <> b <> "]"

braces :: JS -> JS
braces b = "{" <> b <> "}"

-- | Wrap statement(s) in @{@/@}@ without extra indentation.
blockBody :: JS -> JS
blockBody = braces

-- | Indent a multi-line IIFE/interior body once (single 'renderJS' at end).
iifeBody :: JS -> JS
iifeBody body
  | isEmpty body = mempty
  | otherwise = "\n" <> indentLines 2 body <> "\n"

semi :: JS
semi = ";"

colon :: JS
colon = ":"

dquotes :: JS -> JS
dquotes b = "\"" <> b <> "\""

jsText :: Text -> JS
jsText = NonEmpty . BB.byteString . TE.encodeUtf8

jsString :: String -> JS
jsString = NonEmpty . BB.stringUtf8

jsDouble :: Double -> JS
jsDouble d = jsString (showFFloat Nothing d "")

jsDecimal :: Integral a => a -> JS
jsDecimal = NonEmpty . BB.integerDec . toInteger

hcat :: [JS] -> JS
hcat = mconcat

vcat :: [JS] -> JS
vcat [] = mempty
vcat ds = foldr1 ($$) ds

nonEmpty :: JS -> Maybe JS
nonEmpty b = if isEmpty b then Nothing else Just b

isEmpty :: JS -> Bool
isEmpty Empty = True
isEmpty (NonEmpty _) = False

indentLines :: Int -> JS -> JS
indentLines n body
  | isEmpty body = mempty
  | otherwise =
      let
        pad = BS.replicate n 32
        ls = dropTrailingEmpty (BS.split 10 (renderJS body))
       in
        NonEmpty
          (BB.byteString (BS.intercalate (BS.singleton 10) (map (pad <>) ls)))

-- | Match 'Data.Text.lines': split on @\n@ and drop one trailing empty line.
dropTrailingEmpty :: [ByteString] -> [ByteString]
dropTrailingEmpty xs =
  case reverse xs of
    (l : rest) | BS.null l -> reverse rest
    _ -> xs

jsQuote :: Text -> JS
jsQuote s = dquotes (jsString (escapeJsString (T.unpack s)))

escapeJsString :: String -> String
escapeJsString = concatMap esc
 where
  esc = \case
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    c
      | Char.ord c < 32 ->
          let h = showHex (Char.ord c) "" in "\\u" ++ replicate (4 - length h) '0' ++ h
      | otherwise -> [c]

-- JS numbers ------------------------------------------------------------------

-- | JS ToInt32 / ToUint32 for bitwise ops and @>>>@.
toInt32 :: Double -> Int32
toInt32 d
  | isNaN d || isInfinite d = 0
  | otherwise = fromInteger (truncate d)

toUint32 :: Double -> Word32
toUint32 d
  | isNaN d || isInfinite d = 0
  | otherwise = fromInteger (truncate d)

jsBit2 :: (Int32 -> Int32 -> Int32) -> Double -> Double -> Double
jsBit2 f a b = fromIntegral (f (toInt32 a) (toInt32 b))

jsShl, jsShr, jsUShr :: Double -> Double -> Double
jsShl a b =
  fromIntegral (shiftL (toInt32 a) (fromIntegral (toUint32 b .&. 31)))
jsShr a b =
  fromIntegral (shiftR (toInt32 a) (fromIntegral (toUint32 b .&. 31)))
jsUShr a b =
  fromIntegral (shiftR (toUint32 a) (fromIntegral (toUint32 b .&. 31)))

-- | JS @%@ : remainder after truncating division, not Haskell @mod@.
jsRem :: Double -> Double -> Double
jsRem a b
  | isNaN a || isNaN b || isInfinite a || b == 0 = 0 / 0
  | isInfinite b = a
  | otherwise = a - b * fromInteger (truncate (a / b))

-- Preamble --------------------------------------------------------------------

-- | Closed runtime shims. 'ValueEq' pulls the whole eq clique
-- ('ArrayEq', 'DeepEqual', 'Uint8ArrayEq') because those bodies call
-- each other.
data Builtin
  = CheckedIndex
  | ValueEq
  | ArrayEq
  | DeepEqual
  | Uint8ArrayEq
  | GroupBy
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Runtime shims plus hoisted program lambdas, printed once ahead of
-- the body.
data Preamble = Preamble
  { pBuiltins :: !(S.Set Builtin)
  , pHoisted :: !(M.Map Text Text)
  }
  deriving Eq

emptyPreamble :: Preamble
emptyPreamble = Preamble S.empty M.empty

builtinNameSet :: S.Set Text
builtinNameSet = S.fromList (map builtinName [minBound .. maxBound])

needBuiltin :: Builtin -> Preamble -> Preamble
needBuiltin b p
  | b `S.member` eqClique =
      p {pBuiltins = pBuiltins p <> eqClique}
  | otherwise =
      p {pBuiltins = S.insert b (pBuiltins p)}

eqClique :: S.Set Builtin
eqClique = S.fromList [ValueEq, ArrayEq, DeepEqual, Uint8ArrayEq]

useShim :: Builtin -> [JS] -> Preamble -> (Preamble, JS)
useShim b args p = (needBuiltin b p, callShim b args)

insertHoisted :: Text -> Text -> Preamble -> Preamble
insertHoisted name src p
  | name `S.member` builtinNameSet =
      error $
        "JShark.insertHoisted: "
          <> T.unpack name
          <> " is a reserved shim name"
  | otherwise =
      p
        { pHoisted =
            M.insertWith
              (\incoming existing -> mergeHoistedSrc name existing incoming)
              name
              src
              (pHoisted p)
        }

mergeHoistedSrc :: Text -> Text -> Text -> Text
mergeHoistedSrc name existing incoming
  | existing == incoming = existing
  | canonicalHoistSrc existing == canonicalHoistSrc incoming = existing
  | otherwise =
      error $
        "JShark.mergeHoistedSrc: conflicting body for "
          <> T.unpack name

-- | Name of a hoisted shared lambda binding.
hoistTagName :: Text -> Text
hoistTagName tag = "$" <> tag

-- | Alpha-rename @n0@, @n1@, … so the same hoisted lambda compares equal
-- across codegen sites that picked different binder ids.
canonicalHoistSrc :: Text -> Text
canonicalHoistSrc src =
  foldl' (\t (from, to) -> T.replace from to t) src renames
 where
  renames =
    sortBy (\(a, _) (b, _) -> compare (T.length b) (T.length a)) $
      zip ids (map (\i -> "p" <> T.pack (show (i :: Int))) [0 .. length ids - 1])
  ids = nub (hoistNIdents src)

hoistNIdents :: Text -> [Text]
hoistNIdents src = go 0 []
 where
  len = T.length src
  go i acc
    | i >= len = acc
    | otherwise =
        case T.uncons (T.drop i src) of
          Nothing -> acc
          Just ('n', rest) ->
            case span isDigit (T.unpack rest) of
              ([], _) -> go (i + 1) acc
              (ds, _) ->
                let
                  ident = "n" <> T.pack ds
                  prev = if i > 0 then Just (T.index src (i - 1)) else Nothing
                 in
                  if isIdentCont prev
                    then go (i + 1) acc
                    else
                      go (i + 1 + length ds) $
                        if ident `elem` acc
                          then acc
                          else acc ++ [ident]
          Just _ -> go (i + 1) acc
  isIdentCont (Just c) = Char.isAlphaNum c || c == '_'
  isIdentCont Nothing = False

-- | Render preamble bindings compactly; Biome formats the full emit.
renderPreambleStyled :: Preamble -> JS
renderPreambleStyled p =
  vcat
    [ ("const" <+> jsText name <+> "=" <+> jsText src) <> semi
    | (name, src) <- preambleToList p
    ]

preambleToList :: Preamble -> [(Text, Text)]
preambleToList p =
  M.toAscList (builtinMap <> pHoisted p)
 where
  builtinMap =
    M.fromList
      [ (builtinName b, builtinSrc b)
      | b <- S.toAscList (pBuiltins p)
      ]

callShim :: Builtin -> [JS] -> JS
callShim b args =
  jsText (builtinName b) <> parens (hcat (intersperse ", " args))

builtinName :: Builtin -> Text
builtinName = \case
  CheckedIndex -> "$checkedIndex"
  ValueEq -> "$valueEq"
  ArrayEq -> "$arrayEq"
  DeepEqual -> "$deepEqual"
  Uint8ArrayEq -> "$uint8ArrayEq"
  GroupBy -> "$groupBy"

builtinSrc :: Builtin -> Text
builtinSrc = \case
  CheckedIndex ->
    "function(a,i){var n=Math.trunc(i);if(!(n>=0&&n<a.length))throw new Error(\"jshark: index\");return a[n];}"
  ValueEq ->
    "function(a,b){if(a===b)return true;if(a===null||b===null||typeof a!==\"object\"||typeof b!==\"object\")return false;if(Array.isArray(a)&&Array.isArray(b))return $arrayEq(a,b);if(a instanceof Uint8Array&&b instanceof Uint8Array)return $uint8ArrayEq(a,b);if(a.constructor===Object&&b.constructor===Object)return $deepEqual(a,b);return false}"
  ArrayEq ->
    "function(a,b){if(a===b)return true;if(!Array.isArray(b))return false;if(a.length!==b.length)return false;for(var i=0;i<a.length;i++)if(!$valueEq(a[i],b[i]))return false;return true}"
  DeepEqual ->
    "function(a,b){if(a===b)return true;if(a instanceof Date&&b instanceof Date)return a.getTime()===b.getTime();if(a instanceof RegExp&&b instanceof RegExp)return a.toString()===b.toString();var ka=Object.keys(a),kb=Object.keys(b);if(ka.length!==kb.length)return false;for(var i=0;i<ka.length;i++){var k=ka[i];if(!Object.prototype.hasOwnProperty.call(b,k))return false;var v1=a[k],v2=b[k],o=v1&&v2&&typeof v1==='object'&&typeof v2==='object';if(o){if(Array.isArray(v1)){if(!$arrayEq(v1,v2))return false}else if(v1 instanceof Uint8Array){if(!$uint8ArrayEq(v1,v2))return false}else if(!$deepEqual(v1,v2))return false}else if(v1!==v2&&!(Number.isNaN(v1)&&Number.isNaN(v2)))return false}return true}"
  Uint8ArrayEq ->
    "function(a,b){if(a===b)return true;if(a.length!==b.length)return false;for(var i=0;i<a.length;i++)if(a[i]!==b[i])return false;return true}"
  GroupBy ->
    "function(arr,key){var m=new Map(),out=[];for(var i=0;i<arr.length;i++){if(!(i in arr))continue;var x=arr[i],k=key(x),e=m.get(k);if(e===undefined){e={key:k,items:[]};m.set(k,e);out.push(e)}e.items.push(x)}return out}"
