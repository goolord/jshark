{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Client (mainJS)
import DevServer (serveCompiled)
import JShark.Compiler (compileEffect, readableConfig)
import JShark.Types (fromSyntax)
import Page (page)

main :: IO ()
main = do
  js <- compileEffect readableConfig (fromSyntax mainJS)
  serveCompiled 3001 "Breakout on http://localhost:3001" page js
