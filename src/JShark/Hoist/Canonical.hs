{-# LANGUAGE OverloadedStrings #-}

-- | Alpha-rename hoisted JS so @$tag@ dedup ignores binder ids.
module JShark.Hoist.Canonical
  ( canonicalHoistSrc
  , hoistTagName
  )
where

import Data.Char (isDigit)
import qualified Data.Char as Char
import Data.List (nub, sortBy)
import Data.Text (Text)
import qualified Data.Text as T

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

hoistTagName :: Text -> Text
hoistTagName tag = "$" <> tag
