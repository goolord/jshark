{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import DevServer (Example (..), ServeMode (..), exportExamples, serveExamples)
import JShark.Compiler
  ( CompilerConfig (..)
  , OutputStyle (..)
  , applyCompilerArgs
  , compileJobsLabeled
  , defaultCompilerConfig
  , isCompilerFlag
  , readableConfig
  )
import qualified JShark.Example.Breakout as Breakout
import qualified JShark.Example.Life as Life
import JShark.Example.Registry (exampleJobs, exampleLabels)
import qualified JShark.Example.Synth as Synth
import JShark.Example.Theme (sourceLinks, sourceLinksLite)
import qualified JShark.Example.TodoMvc as TodoMvc
import JShark.HotReload.Core (defaultHotReloadConfig)
import SourcePane (sourcePane)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  let
    (flags, rest) = partition isCompilerFlag args
    (hotFlags, cmd) = partition (`elem` ["--hot", "--watch"]) rest
    mode =
      if null hotFlags then StaticServe else HotServe defaultHotReloadConfig
    cfg = applyCompilerArgs ("--progress" : flags) defaultCompilerConfig
    paneCfg = readableConfig {configProgress = configProgress cfg}
  compiled <- compileJobsLabeled cfg (exampleJobs cfg)
  -- The source panes always show readable JS.
  paneCompiled <-
    if configStyle cfg == Readable
      then pure compiled
      else compileJobsLabeled paneCfg (exampleJobs paneCfg)
  let
    output outputs name =
      maybe
        (error (T.unpack ("examples: missing compile output for " <> name)))
        TE.decodeUtf8
        (lookup name (zip exampleLabels outputs))
    example name title page =
      Example name title (page src) (output compiled name) src
     where
      src = output paneCompiled name
    withPane pageFn headLinks src script static =
      pageFn static (headLinks static) (sourcePane static src) script
    examples =
      [ example "breakout" "Breakout" (withPane Breakout.page sourceLinks)
      , example "todo-mvc" "TodoMVC" (withPane TodoMvc.page sourceLinksLite)
      , example "synth" "Synthesizer" (withPane Synth.page sourceLinks)
      , example "life" "Game of Life" $ \_ script static ->
          Life.page static (Life.frameSrcFor script)
      ]
  case cmd of
    [] -> serveExamples mode 3000 examples
    ["export", dest] -> exportExamples dest examples
    _ ->
      die
        "usage: jshark-examples [--progress] [--readable] [--hot|--watch] | jshark-examples [...] export DIR\n  --hot/--watch: SSE + CSS swap + Haskell recompile via jshark-compile"
