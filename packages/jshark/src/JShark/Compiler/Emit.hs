{-# LANGUAGE OverloadedStrings #-}

-- | Strict 'ByteString' construction for JavaScript codegen.
--
-- 'JS' is a 'Data.ByteString.Builder.Builder' with O(1) emptiness tracking so
-- '$$' and 'nonEmpty' can skip stray newlines without materializing the tree.
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
  , vcatNonEmpty
  , punctuate
  , blockBody
  , iifeBody
  , nonEmpty
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Numeric (showFFloat)

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

vcatNonEmpty :: [JS] -> JS
vcatNonEmpty = mconcat . mapMaybe nonEmpty

nonEmpty :: JS -> Maybe JS
nonEmpty b = if isEmpty b then Nothing else Just b

isEmpty :: JS -> Bool
isEmpty Empty = True
isEmpty (NonEmpty _) = False

punctuate :: JS -> [JS] -> [JS]
punctuate _ [] = []
punctuate _ [x] = [x]
punctuate sep (x : xs) = x : concatMap (\y -> [sep, y]) xs

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
