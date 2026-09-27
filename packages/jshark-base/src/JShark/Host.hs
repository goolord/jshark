{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | JavaScript semantics shared by the host evaluators ('libEval') of the
-- library ops in "JShark.Api" and "JShark.Array".
module JShark.Host
  ( jsFailure
  , elemAt
  , u8Elems
  , exactInteger
  , jsParseInt
  , parseBigIntString
  )
where

import Control.Exception (throw)
import Data.Char (digitToInt, isSpace)
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import JShark.Api.Types (U8Buffer (..), Value (..))
import JShark.Core (EvalFailure (..), uint8Elems)
import Numeric (readInt)

-- | A JavaScript exception under 'JShark.Core.evaluate'.
jsFailure :: String -> a
jsFailure = throw . EvalJsFailure . T.pack

-- | @xs[trunc d]@ when @d@ is finite and in range.
elemAt :: [a] -> Double -> Maybe a
elemAt xs d
  | not (isNaN d) && not (isInfinite d), i >= 0, i < length xs = Just (xs !! i)
  | otherwise = Nothing
 where
  i = truncate d :: Int

-- | The bytes of a @Uint8Array@ or @Uint8ClampedArray@.
u8Elems :: U8Buffer u => Value u -> [Word8]
u8Elems = uint8Elems . bufferBytes

-- | @BigInt(d)@ for a finite integral Number.
exactInteger :: Double -> Maybe Integer
exactInteger d
  | not (isNaN d) && not (isInfinite d), d == fromInteger n = Just n
  | otherwise = Nothing
 where
  n = truncate d

-- | JS @parseInt(s, radix)@.
jsParseInt :: Text -> Int -> Double
jsParseInt s r
  | r < 2 || r > 36 = 0 / 0
  | otherwise =
      let
        (neg, t) = sign (dropWhile isSpace (T.unpack s))
       in
        case readInt (fromIntegral r :: Integer) (digitBelow r) digitToInt t of
          (n, _) : _ -> fromInteger (if neg then negate n else n)
          [] -> 0 / 0

sign :: String -> (Bool, String)
sign = \case
  '-' : xs -> (True, xs)
  '+' : xs -> (False, xs)
  xs -> (False, xs)

-- | JS @BigInt(s)@: optional sign and @0x@ \/ @0b@ \/ @0o@ prefix.
parseBigIntString :: String -> Maybe Integer
parseBigIntString raw =
  let
    (neg, rest) = sign (dropWhile isSpace (reverse (dropWhile isSpace (reverse raw))))
    (base, digits) = case map Char.toLower (take 2 rest) of
      "0x" -> (16, drop 2 rest)
      "0b" -> (2, drop 2 rest)
      "0o" -> (8, drop 2 rest)
      _ -> (10, rest)
   in
    case readInt (fromIntegral base :: Integer) (digitBelow base) digitToInt digits of
      (n, []) : _ | not (null digits) -> Just (if neg then negate n else n)
      _ -> Nothing

digitBelow :: Int -> Char -> Bool
digitBelow base c
  | Char.isDigit c = Char.ord c - Char.ord '0' < base
  | Char.isAsciiLower c = Char.ord c - Char.ord 'a' + 10 < base
  | Char.isAsciiUpper c = Char.ord c - Char.ord 'A' + 10 < base
  | otherwise = False
