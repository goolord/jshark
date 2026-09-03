{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (partition)
import qualified Data.List as List
import qualified Data.Text as T
import DevServer (Example (..), ServeMode (..), exportExamples, serveExamples)
import JShark.Api.Types (fromSyntax)
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
import qualified JShark.Example.Hvm2Demo as Hvm2Demo
import JShark.Example.Hvm2Demo.Kernels (hvm2Entries, mandelJsSource)
import qualified JShark.Example.Life as Life
import qualified JShark.Example.Synth as Synth
import qualified JShark.Example.TodoMvc as TodoMvc
import JShark.HotReload.Core (defaultHotReloadConfig)
import JShark.Hvm2 (bendModule)
import SourcePane (hvm2SourcePanes, sourceHead, sourceHeadLite, sourcePane)
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
    labels =
      [ "breakout"
      , "todo-mvc"
      , "synth"
      , "life"
      , "hvm2-demo"
      ]
  (compiled, _stats) <-
    compileJobsLabeled
      cfg
      [ ("breakout", cfg, fromSyntax Breakout.mainJS)
      , ("todo-mvc", cfg, fromSyntax TodoMvc.mainJS)
      , ("synth", cfg, fromSyntax Synth.mainJS)
      , ("life", cfg, fromSyntax Life.mainJS)
      , ("hvm2-demo", cfg, fromSyntax Hvm2Demo.mainJS)
      ]
  paneCompiled <-
    if configStyle cfg == Readable
      then pure compiled
      else
        fmap
          fst
          ( compileJobsLabeled
              paneCfg
              [ ("breakout", paneCfg, fromSyntax Breakout.mainJS)
              , ("todo-mvc", paneCfg, fromSyntax TodoMvc.mainJS)
              , ("synth", paneCfg, fromSyntax Synth.mainJS)
              , ("life", paneCfg, fromSyntax Life.mainJS)
              , ("hvm2-demo", paneCfg, fromSyntax Hvm2Demo.mainJS)
              ]
          )
  let
    lookupIn srcs label =
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
    hvm2Js = lookupCompiled "hvm2-demo"
    breakoutSrc = lookupPane "breakout"
    todoSrc = lookupPane "todo-mvc"
    synthSrc = lookupPane "synth"
    lifeSrc = lookupPane "life"
    hvm2Src = lookupPane "hvm2-demo"
  hvm2Bend <-
    case bendModule hvm2Entries of
      Left err -> die ("hvm2-demo bend: " <> show err)
      Right bend -> pure bend
  let
    hvm2MandelJs = T.pack mandelJsSource
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
      , Example
          "hvm2-demo"
          "HVM2 Lab"
          ( \script static ->
              let
                demoBase =
                  if "/" `T.isPrefixOf` script then "/hvm2-demo" else ""
               in
                Hvm2Demo.page
                  static
                  demoBase
                  (sourceHead static)
                  (hvm2SourcePanes static hvm2Src hvm2Bend hvm2MandelJs)
                  script
          )
          hvm2Js
          (Just hvm2Src)
          Nothing
      ]
  case cmd of
    [] ->
      serveExamples mode 3000 examples
    ["export", dest] -> exportExamples dest examples
    _ ->
      die
        "usage: jshark-examples [--progress] [--readable] [--warn-hvm2-candidates] [--hot|--watch] | jshark-examples [...] export DIR\n  --hot/--watch: SSE + CSS swap + Haskell recompile via jshark-compile"
