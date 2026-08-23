{-# LANGUAGE OverloadedStrings #-}

module DevServer
  ( Example (..)
  , SitePaths (..)
  , exportExamples
  , serveExamples
  , exportPaths
  , serverPaths
  )
where

import Control.Monad (forM_, unless, when)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lucid
import Paths_jshark (getDataFileName)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesFileExist
  , doesPathExist
  , removePathForcibly
  )
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import Web.Scotty

-- | One compiled example, mounted at @/<name>@ (or @<name>/@ on a static site).
data Example = Example
  { exampleName :: T.Text
  , exampleTitle :: T.Text
  , examplePage :: T.Text -> T.Text -> Html ()
  , exampleJs :: T.Text
  }

-- | URL prefixes so the same HTML works on Scotty (@/@) and GitHub Pages (@/jshark/@).
data SitePaths = SitePaths
  { hrefExample :: T.Text -> T.Text
  , srcShot :: T.Text -> T.Text
  , srcScript :: T.Text -> T.Text
  , indexStatic :: T.Text
  , srcStatic :: T.Text
  }

serverPaths :: SitePaths
serverPaths =
  SitePaths
    { hrefExample = ("/" <>)
    , srcShot = \n -> "/static/" <> n <> ".png"
    , srcScript = \n -> "/" <> n <> "/app.js"
    , indexStatic = "/static"
    , srcStatic = "/static"
    }

-- | Relative URLs from @index.html@ and @<name>/index.html@.
exportPaths :: SitePaths
exportPaths =
  SitePaths
    { hrefExample = (<> "/")
    , srcShot = \n -> "static/" <> n <> ".png"
    , srcScript = const "app.js"
    , indexStatic = "static"
    , srcStatic = "../static"
    }

-- | Extra JS assets for an example (route suffix, data-file path under package).
lifeEngineJs :: [(FilePath, FilePath)]
lifeEngineJs =
  [ ("js/LUTGenerator.js", "examples/Life/js/LUTGenerator.js")
  , ("js/Main.js", "examples/Life/js/Main.js")
  , ("js/EngineWorker.js", "examples/Life/js/EngineWorker.js")
  ]

lifeIsolationHeaders :: ActionM ()
lifeIsolationHeaders = do
  setHeader "Cross-Origin-Opener-Policy" "same-origin"
  setHeader "Cross-Origin-Embedder-Policy" "require-corp"
  setHeader "Cross-Origin-Resource-Policy" "cross-origin"

-- | Serve every example and a screenshot directory at @/@.
serveExamples :: Int -> String -> [Example] -> IO ()
serveExamples port banner examples = do
  putStrLn banner
  hFlush stdout
  shots <- traverse exampleShot examples
  assets <-
    fmap concat $
      traverse staticAsset staticFiles
  lifeJs <- traverse lifeJsAsset lifeEngineJs
  scotty port $ do
    get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      html $ renderText (indexPage serverPaths shots)
    forM_ examples $ \ex -> do
      let
        base = "/" <> T.unpack (exampleName ex)
        page =
          examplePage ex (srcScript serverPaths (exampleName ex)) (srcStatic serverPaths)
        isLife = exampleName ex == "life"
      get (fromString base) $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        when isLife lifeIsolationHeaders
        html $ renderText page
      get (fromString (base <> "/")) $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        when isLife lifeIsolationHeaders
        html $ renderText page
      get (fromString (base <> "/app.js")) $ do
        setHeader "Content-Type" "application/javascript; charset=utf-8"
        when isLife lifeIsolationHeaders
        text (TL.fromStrict (exampleJs ex))
      when isLife $
        forM_ lifeJs $ \(route, path) ->
          get (fromString (base <> "/" <> route)) $ do
            setHeader "Content-Type" "application/javascript; charset=utf-8"
            lifeIsolationHeaders
            file path
    forM_ assets $ \(name, path) ->
      get (fromString ("/static/" <> name)) $ do
        setHeader "Content-Type" (staticType name)
        setHeader "Cross-Origin-Resource-Policy" "cross-origin"
        file path
    forM_ shots $ \(ex, path) ->
      case path of
        Nothing -> pure ()
        Just filePath ->
          get (fromString ("/static/" <> T.unpack (exampleName ex) <> ".png")) $ do
            setHeader "Content-Type" "image/png"
            setHeader "Cross-Origin-Resource-Policy" "cross-origin"
            file filePath

-- | Write a static tree GitHub Pages can host.
exportExamples :: FilePath -> [Example] -> IO ()
exportExamples dest examples = do
  setLocaleEncoding utf8
  destExists <- doesPathExist dest
  when destExists (removePathForcibly dest)
  createDirectoryIfMissing True (dest </> "static")
  shots <- traverse exampleShot examples
  TL.writeFile (dest </> "index.html") (renderText (indexPage exportPaths shots))
  writeFile (dest </> ".nojekyll") ""
  forM_ staticFiles (copyStatic dest)
  forM_ shots $ \(ex, path) ->
    case path of
      Nothing -> pure ()
      Just filePath ->
        copyFile
          filePath
          (dest </> "static" </> (T.unpack (exampleName ex) <> ".png"))
  forM_ examples $ \ex -> do
    let
      name = T.unpack (exampleName ex)
      dir = dest </> name
    createDirectoryIfMissing True dir
    writeFile (dest </> name <> ".html") (slashRedirect name)
    TL.writeFile
      (dir </> "index.html")
      ( renderText
          (examplePage ex (srcScript exportPaths (exampleName ex)) (srcStatic exportPaths))
      )
    T.writeFile (dir </> "app.js") (exampleJs ex)
    when (exampleName ex == "life") $ do
      createDirectoryIfMissing True (dir </> "js")
      forM_ lifeEngineJs $ \(route, rel) -> do
        src <- getDataFileName rel
        copyFile src (dir </> route)

-- | Pretty URL without a trailing slash (@/breakout@) would otherwise resolve
-- @app.js@ as a sibling. GitHub Pages serves @<name>.html@ for that path.
slashRedirect :: FilePath -> String
slashRedirect name =
  "<!DOCTYPE html><meta charset=\"utf-8\">"
    <> "<meta http-equiv=\"refresh\" content=\"0;url="
    <> name
    <> "/\">"
    <> "<link rel=\"canonical\" href=\""
    <> name
    <> "/\">"
    <> "<script>location.replace("
    <> show (name <> "/")
    <> ")</script>"

copyStatic :: FilePath -> FilePath -> IO ()
copyStatic dest name = do
  src <- getDataFileName ("examples/static/" <> name)
  exists <- doesFileExist src
  if exists
    then copyFile src (dest </> "static" </> name)
    else fail ("export: missing data-file examples/static/" <> name)

exampleShot :: Example -> IO (Example, Maybe FilePath)
exampleShot ex = do
  path <-
    getDataFileName ("examples/static/" <> T.unpack (exampleName ex) <> ".png")
  exists <- doesFileExist path
  pure (ex, if exists then Just path else Nothing)

staticAsset :: FilePath -> IO [(String, FilePath)]
staticAsset name = do
  path <- getDataFileName ("examples/static/" <> name)
  exists <- doesFileExist path
  pure [(name, path) | exists]

lifeJsAsset :: (FilePath, FilePath) -> IO (FilePath, FilePath)
lifeJsAsset (route, rel) = do
  path <- getDataFileName rel
  exists <- doesFileExist path
  unless exists (fail ("serve: missing data-file " <> rel))
  pure (route, path)

staticType :: FilePath -> TL.Text
staticType name
  | ".js" `T.isSuffixOf` T.pack name = "application/javascript; charset=utf-8"
  | ".css" `T.isSuffixOf` T.pack name = "text/css; charset=utf-8"
  | otherwise = "application/octet-stream"

indexPage :: SitePaths -> [(Example, Maybe FilePath)] -> Html ()
indexPage paths shots = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark examples"
    link_ [rel_ "stylesheet", href_ (indexStatic paths <> "/index.css")]
  body_ $
    main_ $ do
      h1_ "examples"
      p_ "JShark compiled to JS. Click a card."
      ul_ $ mapM_ (exampleCard paths) shots

exampleCard :: SitePaths -> (Example, Maybe FilePath) -> Html ()
exampleCard paths (ex, shot) =
  li_ $
    a_ [href_ (hrefExample paths (exampleName ex))] $ do
      case shot of
        Nothing -> mempty
        Just _ ->
          img_
            [ src_ (srcShot paths (exampleName ex))
            , alt_ (exampleTitle ex)
            ]
      span_ (toHtml (exampleTitle ex))

staticFiles :: [FilePath]
staticFiles =
  [ "highlight.min.js"
  , "github-dark.min.css"
  , "source.css"
  , "index.css"
  , "breakout.css"
  , "synth.css"
  , "todo-mvc.css"
  , "life.css"
  ]
