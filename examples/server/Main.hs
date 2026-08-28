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
  ( applyCompilerArgs
  , compileJobsLabeled
  , defaultCompilerConfig
  , isCompilerFlag
  , prettyJS
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
  let
    jsByLabel = zip labels compiled
    lookupJs label =
      case List.lookup label jsByLabel of
        Just js -> js
        Nothing ->
          error (T.unpack ("examples: missing compile output for " <> label))
    breakoutJs = lookupJs "breakout"
    todoJs = lookupJs "todo-mvc"
    synthJs = lookupJs "synth"
    lifeJs = lookupJs "life"
    hvm2Js = lookupJs "hvm2-demo"
    sourceJs = prettyJS
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
                (sourcePane static (sourceJs breakoutJs))
                script
          )
          breakoutJs
          (Just (sourceJs breakoutJs))
      , Example
          "todo-mvc"
          "TodoMVC"
          ( \script static ->
              TodoMvc.page
                static
                (sourceHeadLite static)
                (sourcePane static (sourceJs todoJs))
                script
          )
          todoJs
          (Just (sourceJs todoJs))
      , Example
          "synth"
          "Synthesizer"
          ( \script static ->
              Synth.page
                static
                (sourceHead static)
                (sourcePane static (sourceJs synthJs))
                script
          )
          synthJs
          (Just (sourceJs synthJs))
      , Example
          "life"
          "Game of Life"
          ( \script static ->
              Life.page static (Life.frameSrcFor script)
          )
          lifeJs
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
                  (hvm2SourcePanes static (sourceJs hvm2Js) hvm2Bend hvm2MandelJs)
                  script
          )
          hvm2Js
          (Just (sourceJs hvm2Js))
      ]
  case cmd of
    [] ->
      serveExamples 3000 examples
    ["export", dest] -> exportExamples dest examples
    _ ->
      die
        "usage: examples [--progress] [--warn-hvm2-candidates] | examples [--progress] [--warn-hvm2-candidates] export DIR"
