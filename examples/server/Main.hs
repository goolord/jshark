{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Breakout
import Data.List (partition)
import qualified Data.Text as T
import DevServer (Example (..), exportExamples, serveExamples)
import qualified Hvm2Demo
import JShark.Compiler
  ( applyCompilerArgs
  , compileEffectsLabeled
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
    progressFlags = "--progress" : flags
  [breakoutSrc, todoSrc, synthSrc, hvm2Src] <-
    compileEffectsLabeled
      (applyCompilerArgs progressFlags readableConfig)
      [ ("breakout (source)", fromSyntax Breakout.mainJS)
      , ("todo-mvc (source)", fromSyntax TodoMvc.mainJS)
      , ("synth (source)", fromSyntax Synth.mainJS)
      , ("hvm2-demo (source)", fromSyntax Hvm2Demo.mainJS)
      ]
  hvm2Bend <-
    case bendModule hvm2Entries of
      Left err -> die ("hvm2-demo bend: " <> show err)
      Right bend -> pure bend
  let
    hvm2MandelJs = T.pack mandelJsSource
  [breakoutJs, todoJs, synthJs, lifeJs, hvm2Js] <-
    compileEffectsLabeled
      (applyCompilerArgs progressFlags defaultCompilerConfig)
      [ ("breakout", fromSyntax Breakout.mainJS)
      , ("todo-mvc", fromSyntax TodoMvc.mainJS)
      , ("synth", fromSyntax Synth.mainJS)
      , ("life", fromSyntax Life.mainJS)
      , ("hvm2-demo", fromSyntax Hvm2Demo.mainJS)
      ]
  let
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
