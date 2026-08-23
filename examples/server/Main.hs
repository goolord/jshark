{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Breakout
import DevServer (Example (..), exportExamples, serveExamples)
import JShark.Compiler (compileEffect, readableConfig)
import JShark.Types (fromSyntax)
import SourcePane (sourceHead, sourcePane)
import qualified Synth
import System.Environment (getArgs)
import System.Exit (die)
import qualified TodoMvc

main :: IO ()
main = do
  breakoutJs <- compileEffect readableConfig (fromSyntax Breakout.mainJS)
  todoJs <- compileEffect readableConfig (fromSyntax TodoMvc.mainJS)
  synthJs <- compileEffect readableConfig (fromSyntax Synth.mainJS)
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
      ]
  args <- getArgs
  case args of
    [] ->
      serveExamples 3000 "Examples on http://localhost:3000" examples
    ["export", dest] -> exportExamples dest examples
    _ -> die "usage: examples | examples export DIR"
