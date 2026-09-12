{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (partition)
import qualified Data.List as List
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
import qualified JShark.Example.TodoMvc as TodoMvc
import JShark.HotReload.Core (defaultHotReloadConfig)
import SourcePane (sourceHead, sourceHeadLite, sourcePane)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  let
    (flags, rest) = partition isCompilerFlag args
    (hotFlags, cmd) =
      partition (\a -> a == "--hot" || a == "--watch") rest
    mode =
      if null hotFlags
        then StaticServe
        else HotServe defaultHotReloadConfig
    cfg =
      applyCompilerArgs ("--progress" : flags) defaultCompilerConfig
    paneCfg = readableConfig {configProgress = configProgress cfg}
    labels = exampleLabels
  compiled <- compileJobsLabeled cfg (exampleJobs cfg)
  paneCompiled <-
    if configStyle cfg == Readable
      then pure compiled
      else
        compileJobsLabeled
          paneCfg
          (exampleJobs paneCfg)
  let
    lookupIn srcs label =
      TE.decodeUtf8 $
        case List.lookup label (zip labels srcs) of
          Just js -> js
          Nothing ->
            error (T.unpack ("examples: missing compile output for " <> label))
    lookupCompiled = lookupIn compiled
    lookupPane = lookupIn paneCompiled
    breakoutJs = lookupCompiled "breakout"
    todoJs = lookupCompiled "todo-mvc"
    synthJs = lookupCompiled "synth"
    lifeJs = lookupCompiled "life"
    breakoutSrc = lookupPane "breakout"
    todoSrc = lookupPane "todo-mvc"
    synthSrc = lookupPane "synth"
    lifeSrc = lookupPane "life"
    examples =
      [ Example
          "breakout"
          "Breakout"
          ( \script static ->
              Breakout.page
                static
                (sourceHead static)
                (sourcePane static breakoutSrc)
                script
          )
          breakoutJs
          (Just breakoutSrc)
          Nothing
      , Example
          "todo-mvc"
          "TodoMVC"
          ( \script static ->
              TodoMvc.page
                static
                (sourceHeadLite static)
                (sourcePane static todoSrc)
                script
          )
          todoJs
          (Just todoSrc)
          Nothing
      , Example
          "synth"
          "Synthesizer"
          ( \script static ->
              Synth.page
                static
                (sourceHead static)
                (sourcePane static synthSrc)
                script
          )
          synthJs
          (Just synthSrc)
          Nothing
      , Example
          "life"
          "Game of Life"
          ( \script static ->
              Life.page static (Life.frameSrcFor script)
          )
          lifeJs
          (Just lifeSrc)
          Nothing
      ]
  case cmd of
    [] ->
      serveExamples mode 3000 examples
    ["export", dest] -> exportExamples dest examples
    _ ->
      die
        "usage: jshark-examples [--progress] [--readable] [--hot|--watch] | jshark-examples [...] export DIR\n  --hot/--watch: SSE + CSS swap + Haskell recompile via jshark-compile"
