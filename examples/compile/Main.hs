{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | One-shot compile of example JShark programs into
-- @.jshark-cache/<name>.js@. Used by @examples --hot@ to pick up
-- @Client.hs@ edits without restarting the HTTP server (Mode B).
module Main (main) where

import qualified Breakout
import Control.Monad (forM_, unless)
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Hvm2Demo
import JShark.Api.Types (fromSyntax)
import JShark.Compiler
  ( CompilerConfig
  , applyCompilerArgs
  , compileEffect
  , defaultCompilerConfig
  , isCompilerFlag
  )
import qualified Life
import qualified Synth
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import qualified TodoMvc

cacheDir :: FilePath
cacheDir = ".jshark-cache"

allLabels :: [String]
allLabels = ["breakout", "todo-mvc", "synth", "life", "hvm2-demo"]

main :: IO ()
main = do
  args <- getArgs
  let
    (flags, rest) = partition isCompilerFlag args
    cfg = applyCompilerArgs flags defaultCompilerConfig
    labels = if null rest then allLabels else rest
  forM_ labels $ \lab ->
    unless (lab `elem` allLabels) $
      die
        ( "jshark-compile: unknown example "
            <> lab
            <> " (want "
            <> unwords allLabels
            <> ")"
        )
  createDirectoryIfMissing True cacheDir
  forM_ labels $ \lab -> do
    js <- compileLabel cfg lab
    let
      out = cacheDir </> (lab <> ".js")
    T.writeFile out js
    putStrLn out
    hFlush stdout

compileLabel :: CompilerConfig -> String -> IO Text
compileLabel cfg = \case
  "breakout" -> compileEffect cfg (fromSyntax Breakout.mainJS)
  "todo-mvc" -> compileEffect cfg (fromSyntax TodoMvc.mainJS)
  "synth" -> compileEffect cfg (fromSyntax Synth.mainJS)
  "life" -> compileEffect cfg (fromSyntax Life.mainJS)
  "hvm2-demo" -> compileEffect cfg (fromSyntax Hvm2Demo.mainJS)
  other -> die ("jshark-compile: unknown example " <> other)
