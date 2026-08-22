{-# LANGUAGE OverloadedStrings #-}

module DevServer (serveCompiled) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Lucid (Html, renderText)
import System.IO (hFlush, stdout)
import Web.Scotty

-- | Serve compiled client JS at @/app.js@ and an HTML shell at @/@.
serveCompiled :: Int -> String -> Html () -> T.Text -> IO ()
serveCompiled port banner page js = do
  putStrLn banner
  hFlush stdout
  scotty port $ do
    get "/app.js" $ do
      setHeader "Content-Type" "application/javascript; charset=utf-8"
      text (TL.fromStrict js)
    get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      html $ renderText page
