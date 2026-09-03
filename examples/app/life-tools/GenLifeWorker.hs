{-# LANGUAGE OverloadedStrings #-}

-- | Regenerate @examples/src/JShark/Example/Life/js/EngineWorker.js@ from Haskell.
--
--   cabal run gen-life-worker
module Main (main) where

import qualified Data.Text.IO as T
import JShark.Example.Life (engineWorkerJs)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let
    path = case args of
      (p : _) -> p
      _ -> "examples/src/JShark/Example/Life/js/EngineWorker.js"
  T.writeFile path engineWorkerJs
  putStrLn $ "wrote " ++ path
