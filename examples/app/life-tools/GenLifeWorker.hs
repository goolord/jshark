-- | Regenerate @examples/src/JShark/Example/Life/js/EngineWorker.js@ from Haskell.
--
--   cabal run gen-life-worker
module Main (main) where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text.Encoding as TE
import JShark.Example.Life (engineWorkerJs)
import System.Environment (getArgs)

main :: IO ()
main = do
  path <-
    fromMaybe "examples/src/JShark/Example/Life/js/EngineWorker.js" . listToMaybe
      <$> getArgs
  BS.writeFile path (TE.encodeUtf8 engineWorkerJs)
  putStrLn ("wrote " ++ path)
