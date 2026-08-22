{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Client (mainJS)
import qualified Data.Text.Lazy as TL
import JShark.Compiler (compileEffect, readableConfig)
import JShark.Types (fromSyntax)
import Lucid (renderText)
import Page (page)
import System.IO (hFlush, stdout)
import Web.Scotty

main :: IO ()
main = do
  js <- compileEffect readableConfig (fromSyntax mainJS)
  putStrLn "Breakout on http://localhost:3001"
  hFlush stdout
  scotty 3001 $ do
    get "/app.js" $ do
      setHeader "Content-Type" "application/javascript; charset=utf-8"
      text (TL.fromStrict js)
    get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      html $ renderText page
