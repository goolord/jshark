{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Breakout
import DevServer (Example (..), serveExamples)
import JShark.Compiler (compileEffect, readableConfig)
import JShark.Types (fromSyntax)
import SourcePane (sourceHead, sourcePane)
import qualified TodoMvc

main :: IO ()
main = do
  breakoutJs <- compileEffect readableConfig (fromSyntax Breakout.mainJS)
  todoJs <- compileEffect readableConfig (fromSyntax TodoMvc.mainJS)
  serveExamples 3000 "Examples on http://localhost:3000"
    [ Example
        "breakout"
        "Breakout"
        (Breakout.page sourceHead (sourcePane breakoutJs))
        breakoutJs
    , Example
        "todo-mvc"
        "TodoMVC"
        (TodoMvc.page sourceHead (sourcePane todoJs))
        todoJs
    ]
