{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Keys (Key (..), black, blackLeft, keys, noteId)
import Numeric (showFFloat)

noteIdStr :: Key -> String
noteIdStr = T.unpack . noteId

main :: IO ()
main = do
  putStrLn "/* Generated — run scripts/gen-synth-keys-css.sh */"
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
