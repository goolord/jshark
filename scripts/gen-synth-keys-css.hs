{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import JShark.Example.Synth.Keys
  ( Key (..)
  , black
  , blackLeft
  , blackWidth
  , keys
  , noteId
  )
import Numeric (showFFloat)

noteIdStr :: Key -> String
noteIdStr = T.unpack . noteId

pct :: Double -> String
pct w = showFFloat (Just 1) w ""

main :: IO ()
main = do
  putStrLn "/* Generated — run scripts/gen-synth-keys-css.sh */"
  putStrLn ""
  putStrLn $ ".synth .key.black { width: " ++ pct blackWidth ++ "%; }"
  putStrLn ""
  mapM_ rule (filter black keys)
 where
  rule k =
    putStrLn $
      "#"
        ++ noteIdStr k
        ++ " { left: "
        ++ showFFloat (Just 2) (blackLeft k) ""
        ++ "%; }"
