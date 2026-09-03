{-# LANGUAGE OverloadedStrings #-}

-- | Regenerate @examples/src/JShark/Example/Life/js/catalog.js@ from Haskell catalog data.
--
--   cabal run gen-life-catalog
module Main (main) where

import qualified Data.Text.IO as T
import JShark.Example.Life (catalogJs)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let
    path = case args of
      (p : _) -> p
      _ -> "examples/src/JShark/Example/Life/js/catalog.js"
  T.writeFile path catalogJs
  putStrLn $ "wrote " ++ path
