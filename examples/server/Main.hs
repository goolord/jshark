{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Breakout
import Data.List (partition)
import qualified Data.List as List
import qualified Data.Text as T
import DevServer (Example (..), exportExamples, serveExamples)
import qualified Hvm2Demo
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
import JShark.Hvm2 (bendModule)
import Kernels (hvm2Entries, mandelJsSource)
import qualified Life
import SourcePane (hvm2SourcePanes, sourceHead, sourceHeadLite, sourcePane)
import qualified Synth
import System.Environment (getArgs)
import System.Exit (die)
import qualified TodoMvc

main :: IO ()
main = do
  args <- getArgs
  let
    (flags, cmd) = partition isCompilerFlag args
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
    lookupJs = lookupIn compiled
    lookupPane = lookupIn paneCompiled
    breakoutJs = lookupJs "breakout"
    todoJs = lookupJs "todo-mvc"
    synthJs = lookupJs "synth"
    lifeJs = lookupJs "life"
    hvm2Js = lookupJs "hvm2-demo"
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
      , Example
          "life"
          "Game of Life"
          ( \script static ->
              Life.page static (Life.frameSrcFor script)
          )
          lifeJs
          (Just lifeSrc)
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
      ]
  case cmd of
    [] ->
      serveExamples 3000 examples
    ["export", dest] -> exportExamples dest examples
    _ ->
      die
        "usage: examples [--progress] [--readable] [--warn-hvm2-candidates] | examples [--progress] [--readable] [--warn-hvm2-candidates] export DIR"
