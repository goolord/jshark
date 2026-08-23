{-# LANGUAGE OverloadedStrings #-}

{- | Canonical shape hashes for catalog patterns. Used at runtime to
   recognize emergent soup formations as known taxa before minting novel
   species ids.
-}
module Catalog
  ( shapeHash
  , knownCatalogJson
  )
where

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Patterns (PatternSpec (..), allPatterns)

shapeHash :: [(Int, Int)] -> Text
shapeHash cells =
  let
    minX = minimum (map fst cells)
    minY = minimum (map snd cells)
    norm =
      sort
        [ (x - minX, y - minY)
        | (x, y) <- cells
        ]
   in
    T.intercalate ";" [T.pack (show x ++ "," ++ show y) | (x, y) <- norm]

knownCatalogJson :: Text
knownCatalogJson =
  T.concat
    [ "["
    , T.intercalate "," [entry p | p <- allPatterns]
    , "]"
    ]
 where
  entry p =
    let
      h = shapeHash (patCells p)
      sid = patId p
     in
      "[\"" <> h <> "\"," <> T.pack (show sid) <> "]"
