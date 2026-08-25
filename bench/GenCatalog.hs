{-# LANGUAGE OverloadedStrings #-}

-- | Regenerate @examples/Life/js/catalog.js@ from Haskell catalog data.
--
--   cabal run gen-life-catalog
module Main (main) where

import Life (catalogJs)
import qualified Data.Text.IO as T
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let
    path = case args of
      (p : _) -> p
      _ -> "examples/Life/js/catalog.js"
  T.writeFile path catalogJs
  putStrLn $ "wrote " ++ path
