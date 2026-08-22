{-# LANGUAGE OverloadedStrings #-}

module DevServer (Example (..), serveExamples) where

import Control.Monad (forM_)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Lucid
import Paths_jshark (getDataFileName)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Web.Scotty

-- | One compiled example, mounted at @/<name>@.
data Example = Example
  { exampleName :: T.Text
  , exampleTitle :: T.Text
  , examplePage :: Html ()
  , exampleJs :: T.Text
  }

-- | Serve every example and a screenshot directory at @/@.
serveExamples :: Int -> String -> [Example] -> IO ()
serveExamples port banner examples = do
  putStrLn banner
  hFlush stdout
  shots <- traverse exampleShot examples
  assets <- fmap concat $ traverse staticAsset
    [ "highlight.min.js"
    , "github-dark.min.css"
    ]
  scotty port $ do
    get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      html $ renderText (indexPage shots)
    forM_ examples $ \ex -> do
      let base = "/" <> T.unpack (exampleName ex)
      get (fromString base) $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        html $ renderText (examplePage ex)
      get (fromString (base <> "/app.js")) $ do
        setHeader "Content-Type" "application/javascript; charset=utf-8"
        text (TL.fromStrict (exampleJs ex))
    forM_ assets $ \(name, path) ->
      get (fromString ("/static/" <> name)) $ do
        setHeader "Content-Type" (staticType name)
        file path
    forM_ shots $ \(ex, path) ->
      case path of
        Nothing -> pure ()
        Just filePath ->
          get (fromString ("/static/" <> T.unpack (exampleName ex) <> ".png")) $ do
            setHeader "Content-Type" "image/png"
            file filePath

exampleShot :: Example -> IO (Example, Maybe FilePath)
exampleShot ex = do
  path <- getDataFileName ("examples/static/" <> T.unpack (exampleName ex) <> ".png")
  exists <- doesFileExist path
  pure (ex, if exists then Just path else Nothing)

staticAsset :: FilePath -> IO [(String, FilePath)]
staticAsset name = do
  path <- getDataFileName ("examples/static/" <> name)
  exists <- doesFileExist path
  pure [(name, path) | exists]

staticType :: FilePath -> TL.Text
staticType name
  | ".js" `T.isSuffixOf` T.pack name = "application/javascript; charset=utf-8"
  | ".css" `T.isSuffixOf` T.pack name = "text/css; charset=utf-8"
  | otherwise = "application/octet-stream"

indexPage :: [(Example, Maybe FilePath)] -> Html ()
indexPage shots = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark examples"
    style_ indexCss
  body_ $
    main_ $ do
      h1_ "examples"
      p_ "JShark compiled to JS. Click a card."
      ul_ $ mapM_ exampleCard shots

exampleCard :: (Example, Maybe FilePath) -> Html ()
exampleCard (ex, shot) =
  li_ $
    a_ [href_ ("/" <> exampleName ex)] $ do
      case shot of
        Nothing -> mempty
        Just _ ->
          img_
            [ src_ ("/static/" <> exampleName ex <> ".png")
            , alt_ (exampleTitle ex)
            ]
      span_ (toHtml (exampleTitle ex))

indexCss :: T.Text
indexCss =
  "html,body{margin:0;min-height:100%;background:#0f172a;color:#e2e8f0;"
    <> "font-family:Georgia,serif}"
    <> "main{max-width:52rem;margin:2.5rem auto;padding:0 1.25rem}"
    <> "h1{font-weight:400;letter-spacing:.04em;margin:0 0 .4rem}"
    <> "p{color:#94a3b8;margin:0 0 1.75rem}"
    <> "ul{list-style:none;margin:0;padding:0;display:grid;"
    <> "grid-template-columns:repeat(2,minmax(0,1fr));gap:1.25rem}"
    <> "@media(max-width:30rem){ul{grid-template-columns:1fr}}"
    <> "a{color:inherit;text-decoration:none;display:block;background:#1e293b;"
    <> "border-radius:6px;overflow:hidden;outline:1px solid #334155}"
    <> "a:hover{outline-color:#38bdf8}"
    <> "img{display:block;width:100%;aspect-ratio:4/3;object-fit:cover;"
    <> "object-position:top center;background:#0b1220}"
    <> "span{display:block;padding:.85rem 1rem .95rem}"
