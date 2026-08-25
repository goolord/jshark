{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Breakout
import DevServer (Example (..), exportExamples, serveExamples)
import JShark.Compiler (applyCompilerArgs, compileEffects, isCompilerFlag, readableConfig)
import JShark.Types (fromSyntax)
import qualified Life
import SourcePane (sourceHead, sourcePane)
import qualified Synth
import System.Environment (getArgs)
import System.Exit (die)
import Data.List (partition)
import qualified TodoMvc

main :: IO ()
main = do
  args <- getArgs
  let
    (flags, cmd) = partition isCompilerFlag args
    cfg = applyCompilerArgs flags readableConfig
  [breakoutJs, todoJs, synthJs, lifeJs] <-
    compileEffects
      cfg
      [ fromSyntax Breakout.mainJS
      , fromSyntax TodoMvc.mainJS
      , fromSyntax Synth.mainJS
      , fromSyntax Life.mainJS
      ]
  let
    examples =
      [ Example
          "breakout"
          "Breakout"
          ( \script static ->
              Breakout.page static (sourceHead static) (sourcePane static breakoutJs) script
          )
          breakoutJs
      , Example
          "todo-mvc"
          "TodoMVC"
          ( \script static ->
              TodoMvc.page static (sourceHead static) (sourcePane static todoJs) script
          )
          todoJs
      , Example
          "synth"
          "Synthesizer"
          ( \script static ->
              Synth.page static (sourceHead static) (sourcePane static synthJs) script
          )
          synthJs
      , Example
          "life"
          "Game of Life"
          ( \script static ->
              Life.page static script
          )
          lifeJs
      ]
  case cmd of
    [] ->
      serveExamples 3000 "Examples on http://localhost:3000" examples
    ["export", dest] -> exportExamples dest examples
    _ -> die "usage: examples [--warn-hvm2-candidates] | examples [--warn-hvm2-candidates] export DIR"
