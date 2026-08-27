{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Breakout
import Data.List (partition)
import qualified Data.Text as T
import DevServer (Example (..), exportExamples, serveExamples)
import qualified Hvm2Demo
import JShark.Compiler
  ( applyCompilerArgs
  , compileJobsLabeled
  , defaultCompilerConfig
  , isCompilerFlag
  , readableConfig
  )
import JShark.Hvm2 (bendModule)
import JShark.Types (fromSyntax)
import Kernels (hvm2Entries, mandelJsSource)
import qualified Life
import SourcePane (hvm2SourcePanes, sourceHead, sourcePane)
import qualified Synth
import System.Environment (getArgs)
import System.Exit (die)
import qualified TodoMvc

main :: IO ()
main = do
  args <- getArgs
  let
    (flags, cmd) = partition isCompilerFlag args
    baseCfg =
      applyCompilerArgs ("--progress" : flags) defaultCompilerConfig
  (compiled, _stats) <-
    compileJobsLabeled
      baseCfg
      [ ("breakout (source)", readableConfig, fromSyntax Breakout.mainJS)
      , ("todo-mvc (source)", readableConfig, fromSyntax TodoMvc.mainJS)
      , ("synth (source)", readableConfig, fromSyntax Synth.mainJS)
      , ("hvm2-demo (source)", readableConfig, fromSyntax Hvm2Demo.mainJS)
      , ("breakout", defaultCompilerConfig, fromSyntax Breakout.mainJS)
      , ("todo-mvc", defaultCompilerConfig, fromSyntax TodoMvc.mainJS)
      , ("synth", defaultCompilerConfig, fromSyntax Synth.mainJS)
      , ("life", defaultCompilerConfig, fromSyntax Life.mainJS)
      , ("hvm2-demo", defaultCompilerConfig, fromSyntax Hvm2Demo.mainJS)
      ]
  let
    breakoutSrc = compiled !! 0
    todoSrc = compiled !! 1
    synthSrc = compiled !! 2
    hvm2Src = compiled !! 3
    breakoutJs = compiled !! 4
    todoJs = compiled !! 5
    synthJs = compiled !! 6
    lifeJs = compiled !! 7
    hvm2Js = compiled !! 8
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
                (sourceHead static)
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
              Life.page static script (Life.frameSrcFor script)
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
        "usage: examples [--progress] [--warn-hvm2-candidates] | examples [--progress] [--warn-hvm2-candidates] export DIR"
