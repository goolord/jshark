{-# LANGUAGE OverloadedStrings #-}

-- | Regenerate @examples/src/JShark/Example/Life/js/EngineWorker.js@ from Haskell.
--
--   cabal run gen-life-worker
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import JShark.Example.Life (engineWorkerJs)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let
    path = case args of
      (p : _) -> p
      _ -> "examples/src/JShark/Example/Life/js/EngineWorker.js"
  BS.writeFile path (TE.encodeUtf8 engineWorkerJs)
  putStrLn $ "wrote " ++ path
