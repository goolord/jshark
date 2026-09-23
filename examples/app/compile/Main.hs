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
import JShark.Build
  ( applyCompilerArgs
  , compileEffect
  , defaultCompilerConfig
  , isCompilerFlag
  )
import qualified JShark.Example.Breakout as Breakout
import qualified JShark.Example.Life as Life
import JShark.Example.Registry (exampleLabels, exampleMainJS)
import qualified JShark.Example.Synth as Synth
import qualified JShark.Example.TodoMvc as TodoMvc
import Lucid (Html, renderToFile)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  (flags, rest) <- partition isCompilerFlag <$> getArgs
  let
    cfg = applyCompilerArgs flags defaultCompilerConfig
    known = map T.unpack exampleLabels
    labels = if null rest then known else rest
  forM_ labels $ \lab ->
    unless (lab `elem` known) . die $
      "jshark-compile: unknown example "
        <> lab
        <> " (want "
        <> unwords known
        <> ")"
  createDirectoryIfMissing True ".jshark-cache"
  forM_ labels $ \lab -> do
    let
      out ext = ".jshark-cache" </> (lab <> ext)
    BS.writeFile (out ".js") =<< compileEffect cfg (exampleMainJS (T.pack lab))
    -- Written as UTF-8 bytes: Data.Text.IO would use the locale encoding,
    -- which cannot represent the page on a non-UTF-8 machine.
    renderToFile (out ".html") (page lab ("/" <> T.pack lab <> "/app.js"))
    putStrLn (out ".js")
    putStrLn (out ".html")
    hFlush stdout

-- | Hot-reload shells use empty source-pane slots; the live app script URL
-- still points at @/<name>/app.js@.
page :: String -> Text -> Html ()
page = \case
  "breakout" -> Breakout.page "/static" mempty mempty
  "todo-mvc" -> TodoMvc.page "/static" mempty mempty
  "synth" -> Synth.page "/static" mempty mempty
  "life" -> Life.page "/static" . Life.frameSrcFor
  other -> error ("jshark-compile: unknown page " <> other)
