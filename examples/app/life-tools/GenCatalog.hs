{-# LANGUAGE OverloadedStrings #-}

-- | Regenerate @examples/src/JShark/Example/Life/js/catalog.js@ from Haskell catalog data.
--
--   cabal run gen-life-catalog
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import JShark.Example.Life (catalogJs)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let
    path = case args of
      (p : _) -> p
      _ -> "examples/src/JShark/Example/Life/js/catalog.js"
  BS.writeFile path (TE.encodeUtf8 catalogJs)
  putStrLn $ "wrote " ++ path
