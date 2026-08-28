{-# LANGUAGE OverloadedStrings #-}

-- | Strict 'TextBuilder' helpers for JavaScript codegen.
module JShark.Compiler.Emit
  ( JS
  , renderJS
  , renderJSCompact
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
  , isEmpty
  )
where

import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)
import TextBuilder (TextBuilder)
import qualified TextBuilder as TB

type JS = TextBuilder

-- | Materialize a codegen tree to strict 'Text'.
renderJS :: JS -> Text
renderJS = TB.toText

-- | Backward-compatible alias for 'renderJS' (layout is fixed at emit time).
renderJSCompact :: JS -> Text
renderJSCompact = renderJS

infixl 5 $$

($$) :: JS -> JS -> JS
a $$ b
  | isEmpty a = b
  | isEmpty b = a
  | otherwise = a <> TB.char '\n' <> b

infixl 6 <+>

(<+>) :: JS -> JS -> JS
a <+> b = a <> TB.char ' ' <> b

parens :: JS -> JS
parens b = "(" <> b <> ")"

brackets :: JS -> JS
brackets b = "[" <> b <> "]"

braces :: JS -> JS
braces b = "{" <> b <> "}"

-- | Wrap statement(s) in @{@/@}@ without extra indentation.
blockBody :: JS -> JS
blockBody = braces

-- | Indent a multi-line IIFE/interior body once (single 'toText' at end).
iifeBody :: JS -> JS
iifeBody body
  | isEmpty body = mempty
  | otherwise = TB.char '\n' <> indentLines 2 body <> TB.char '\n'

semi :: JS
semi = ";"

colon :: JS
colon = ":"

dquotes :: JS -> JS
dquotes b = "\"" <> b <> "\""

jsText :: Text -> JS
jsText = TB.text

jsString :: String -> JS
jsString = TB.string

jsDouble :: Double -> JS
jsDouble d = jsString (showFFloat Nothing d "")

jsDecimal :: Integral a => a -> JS
jsDecimal = TB.decimal

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
isEmpty = TB.isEmpty

punctuate :: JS -> [JS] -> [JS]
punctuate _ [] = []
punctuate _ [x] = [x]
punctuate sep (x : xs) = x : concatMap (\y -> [sep, y]) xs

indentLines :: Int -> JS -> JS
indentLines n body =
  let
    pad = TB.text (T.replicate n " ")
    ls = T.lines (TB.toText body)
   in
    case ls of
      [] -> mempty
      _ -> mconcat (intersperse (TB.char '\n') (map (pad <>) (map TB.text ls)))
