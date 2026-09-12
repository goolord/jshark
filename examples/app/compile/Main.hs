{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | One-shot compile of example JShark programs + Lucid shells into
-- @.jshark-cache/<name>.{js,html}@. Used by @examples --hot@ (Mode B).
module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import JShark.Compiler
  ( CompilerConfig
  , applyCompilerArgs
  , compileEffect
  , defaultCompilerConfig
  , isCompilerFlag
  )
import qualified JShark.Example.Breakout as Breakout
import qualified JShark.Example.Life as Life
import JShark.Example.Registry (exampleLabels, exampleMainJS)
import qualified JShark.Example.Synth as Synth
import qualified JShark.Example.TodoMvc as TodoMvc
import Lucid (Html, renderText)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

cacheDir :: FilePath
cacheDir = ".jshark-cache"

staticRoot :: Text
staticRoot = "/static"

allLabels :: [String]
allLabels = map T.unpack exampleLabels

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
      script = "/" <> T.pack lab <> "/app.js"
      jsOut = cacheDir </> (lab <> ".js")
      htmlOut = cacheDir </> (lab <> ".html")
      html = TL.toStrict (renderText (pageLabel lab script))
    BS.writeFile jsOut js
    T.writeFile htmlOut html
    putStrLn jsOut
    putStrLn htmlOut
    hFlush stdout

compileLabel :: CompilerConfig -> String -> IO BS.ByteString
compileLabel cfg lab = compileEffect cfg (exampleMainJS (T.pack lab))

-- | Hot-reload shells use empty source-pane slots; the live app script URL
-- still points at @/<name>/app.js@.
pageLabel :: String -> Text -> Html ()
pageLabel = \case
  "breakout" -> \script -> Breakout.page staticRoot mempty mempty script
  "todo-mvc" -> \script -> TodoMvc.page staticRoot mempty mempty script
  "synth" -> \script -> Synth.page staticRoot mempty mempty script
  "life" -> \script -> Life.page staticRoot (Life.frameSrcFor script)
  other -> \_ -> error ("jshark-compile: unknown page " <> other)
