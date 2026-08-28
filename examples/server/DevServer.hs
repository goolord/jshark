{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DevServer
  ( Example (..)
  , SitePaths (..)
  , exportExamples
  , serveExamples
  , exportPaths
  , serverPaths
  )
where

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Control.Monad (forM, forM_, when)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Life
import Lucid
import Lucid.Base (makeAttribute)
import Network.Wai.Handler.Warp (setHost, setPort)
import Paths_jshark (getDataFileName)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , listDirectory
  , removePathForcibly
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (isAlreadyInUseError)
import ThemeHead (themeLinks)
import Web.Scotty

resolveDataFile :: FilePath -> IO FilePath
resolveDataFile rel = do
  installed <- getDataFileName rel
  installedOk <- pathExists installed
  if installedOk
    then pure installed
    else do
      cwdOk <- pathExists rel
      if cwdOk then pure rel else fail ("serve: missing data-file " <> rel)

pathExists :: FilePath -> IO Bool
pathExists p = do
  ok <- doesFileExist p
  if ok then pure True else doesDirectoryExist p

-- | One compiled example, mounted at @/<name>@ (or @<name>/@ on a static site).
data Example = Example
  { exampleName :: T.Text
  , exampleTitle :: T.Text
  , examplePage :: T.Text -> T.Text -> Html ()
  , exampleJs :: T.Text
  , exampleSourceJs :: Maybe T.Text
  -- ^ Display JS for the collapsible source pane ('prettyJS' of compiled
  --   output when set). 'Nothing' when the page has no source viewer (Life).
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
  [ ("js/pixi.min.js", "examples/Life/js/pixi.min.js")
  , ("js/EngineWorker.js", "examples/Life/js/EngineWorker.js")
  , ("js/shaders/cell.frag.glsl", "examples/Life/shaders/cell.frag.glsl")
  ]

-- | HVM2 demo assets served under @/hvm2-demo/@ (COOP/COEP + CORP) so worker
-- | scripts are not blocked and do not depend on @/static/@ data-file install.
hvm2DemoAssets :: [(FilePath, FilePath)]
hvm2DemoAssets =
  [ ("hvm2-worker.js", "examples/static/hvm2-worker.js")
  , ("hvm2-wasm.js", "examples/static/hvm2-wasm.js")
  , ("hvm2-demo.wasm", "examples/static/hvm2-demo.wasm")
  ]

-- | Sandboxed frame fetches @app.js@ / wasm from the example origin; CORP +
-- | ACAO are required. COOP/COEP stay off the shell HTML so the frame is not
-- | blocked by require-corp (SharedArrayBuffer workers need headers on the
-- | frame document itself).
-- | COOP/COEP enable SharedArrayBuffer + wasm threads for the HVM2 demo.
hvm2ThreadHeaders :: ActionM ()
hvm2ThreadHeaders = do
  setHeader "Cross-Origin-Opener-Policy" "same-origin"
  setHeader "Cross-Origin-Embedder-Policy" "require-corp"
  setHeader "Cross-Origin-Resource-Policy" "cross-origin"

lifeAssetHeaders :: ActionM ()
lifeAssetHeaders = do
  setHeader "Cross-Origin-Resource-Policy" "cross-origin"
  setHeader "Access-Control-Allow-Origin" "*"

serverHost :: String
serverHost = "127.0.0.1"

serverOpts :: Int -> Options
serverOpts port =
  defaultOptions
    { settings =
        setHost "127.0.0.1" . setPort port $ settings defaultOptions
    }

-- | Serve every example and a screenshot directory at @/@.
-- Tries @startPort@, then successive ports until warp binds.
serveExamples :: Int -> [Example] -> IO ()
serveExamples startPort examples = do
  shots <- traverse exampleShot examples
  assets <-
    fmap concat $
      traverse staticAsset staticFiles
  treeAssets <- speedHighlightAssets
  let
    allAssets = assets ++ treeAssets
  lifeJs <- traverse demoAssetPath lifeEngineJs
  hvm2Js <- traverse demoAssetPath hvm2DemoAssets
  tryServe
    startPort
    startPort
    (startPort + 100)
    shots
    allAssets
    lifeJs
    hvm2Js
    examples

tryServe ::
  Int
  -> Int
  -> Int
  -> [(Example, Maybe FilePath)]
  -> [(String, FilePath)]
  -> [(FilePath, FilePath)]
  -> [(FilePath, FilePath)]
  -> [Example]
  -> IO ()
tryServe startPort port maxPort shots assets lifeJs hvm2Js examples
  | port > maxPort =
      fail $
        "no free port in range "
          <> show startPort
          <> ".."
          <> show maxPort
  | otherwise = do
      putStrLn ("Examples on http://" <> serverHost <> ":" <> show port)
      hFlush stdout
      Exception.catch
        (scottyOpts (serverOpts port) (exampleRoutes shots assets lifeJs hvm2Js examples))
        $ \e ->
          if isAlreadyInUseError e
            then do
              hPutStrLn
                stderr
                ("port " <> show port <> " in use, trying " <> show (port + 1))
              tryServe startPort (port + 1) maxPort shots assets lifeJs hvm2Js examples
            else Exception.throwIO (e :: IOException)

exampleRoutes ::
  [(Example, Maybe FilePath)]
  -> [(String, FilePath)]
  -> [(FilePath, FilePath)]
  -> [(FilePath, FilePath)]
  -> [Example]
  -> ScottyM ()
exampleRoutes shots assets lifeJs hvm2Js examples = do
  get "/" $ do
    setHeader "Content-Type" "text/html; charset=utf-8"
    html $ renderText (indexPage serverPaths shots)
  forM_ examples $ \ex -> do
    let
      base = "/" <> T.unpack (exampleName ex)
      page =
        examplePage ex (srcScript serverPaths (exampleName ex)) (srcStatic serverPaths)
      isLife = exampleName ex == "life"
      isHvm2 = exampleName ex == "hvm2-demo"
    get (fromString base) $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      when isHvm2 hvm2ThreadHeaders
      html $ renderText page
    get (fromString (base <> "/")) $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      when isHvm2 hvm2ThreadHeaders
      html $ renderText page
    get (fromString (base <> "/app.js")) $ do
      setHeader "Content-Type" "application/javascript; charset=utf-8"
      setHeader "Cache-Control" "no-store"
      when isLife lifeAssetHeaders
      when isHvm2 hvm2ThreadHeaders
      text (TL.fromStrict (exampleJs ex))
    when isLife $ do
      let
        static = srcStatic serverPaths
        script = srcScript serverPaths (exampleName ex)
        frame =
          Life.framePage static script (Life.assetBaseFor script)
      get (fromString (base <> "/frame")) $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        lifeAssetHeaders
        html $ renderText frame
      get (fromString (base <> "/frame/")) $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        lifeAssetHeaders
        html $ renderText frame
      forM_ lifeJs $ \(route, path) ->
        get (fromString (base <> "/" <> route)) $ do
          setHeader "Content-Type" (lifeAssetType route)
          lifeAssetHeaders
          file path
    when isHvm2 $
      forM_ hvm2Js $ \(route, path) ->
        get (fromString (base <> "/" <> route)) $ do
          setHeader "Content-Type" (staticType route)
          hvm2ThreadHeaders
          file path
  forM_ assets $ \(name, path) ->
    get (fromString ("/static/" <> name)) $ do
      setHeader "Content-Type" (staticType name)
      setHeader "Cross-Origin-Resource-Policy" "cross-origin"
      setHeader "Access-Control-Allow-Origin" "*"
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
  copySpeedHighlight dest
  forM_ shots $ \(ex, path) ->
    case path of
      Nothing -> pure ()
      Just filePath ->
        copyFileInto
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
      createDirectoryIfMissing True (dir </> "frame")
      let
        static = srcStatic exportPaths
        script = srcScript exportPaths (exampleName ex)
      TL.writeFile
        (dir </> "frame" </> "index.html")
        (renderText (Life.framePage static script (Life.assetBaseFor script)))
      createDirectoryIfMissing True (dir </> "js")
      createDirectoryIfMissing True (dir </> "js/shaders")
      forM_ lifeEngineJs $ \(route, rel) -> do
        src <- resolveDataFile rel
        copyFileInto src (dir </> route)
    when (exampleName ex == "hvm2-demo") $
      forM_ hvm2DemoAssets $ \(route, rel) -> do
        src <- resolveDataFile rel
        copyFileInto src (dir </> route)

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

copyFileInto :: FilePath -> FilePath -> IO ()
copyFileInto src dest = do
  createDirectoryIfMissing True (takeDirectory dest)
  copyFile src dest

copyStatic :: FilePath -> FilePath -> IO ()
copyStatic dest name = do
  src <- resolveDataFile ("examples/static/" <> name)
  exists <- doesFileExist src
  if exists
    then copyFileInto src (dest </> "static" </> name)
    else fail ("export: missing data-file examples/static/" <> name)

exampleShot :: Example -> IO (Example, Maybe FilePath)
exampleShot ex = do
  path <-
    getDataFileName ("examples/static/" <> T.unpack (exampleName ex) <> ".png")
  exists <- doesFileExist path
  pure (ex, if exists then Just path else Nothing)

staticAsset :: FilePath -> IO [(String, FilePath)]
staticAsset name = do
  path <- resolveDataFile ("examples/static/" <> name)
  exists <- doesFileExist path
  pure [(name, path) | exists]

-- | URL segment under @/static/@ for vendored speed-highlight. Must match
-- @source-pane.js@ import @./speed-highlight/index.js@.
speedHighlightPrefix :: FilePath
speedHighlightPrefix = "speed-highlight"

speedHighlightMissing :: String
speedHighlightMissing =
  "missing speed-highlight tree — run scripts/vendor-speed-highlight.sh"

speedHighlightAssets :: IO [(String, FilePath)]
speedHighlightAssets = do
  root <- resolveDataFile ("examples/static/" <> speedHighlightPrefix)
  exists <- doesDirectoryExist root
  if not exists
    then fail ("serve: " <> speedHighlightMissing)
    else do
      assets <- walkStaticTree root speedHighlightPrefix
      when (null assets) $
        fail ("serve: " <> speedHighlightMissing)
      pure assets

walkStaticTree :: FilePath -> FilePath -> IO [(String, FilePath)]
walkStaticTree dir routePrefix = do
  entries <- listDirectory dir
  fmap concat $
    forM entries $ \entry -> do
      let
        path = dir </> entry
        route = routePrefix </> entry
      isDir <- doesDirectoryExist path
      if isDir then walkStaticTree path route else pure [(route, path)]

copySpeedHighlight :: FilePath -> IO ()
copySpeedHighlight dest = do
  assets <- speedHighlightAssets
  when (null assets) $
    fail ("export: " <> speedHighlightMissing)
  forM_ assets $ \(route, src) -> do
    let
      out = dest </> "static" </> route
    copyFileInto src out

demoAssetPath :: (FilePath, FilePath) -> IO (FilePath, FilePath)
demoAssetPath (route, rel) = do
  path <- resolveDataFile rel
  pure (route, path)

lifeAssetType :: FilePath -> TL.Text
lifeAssetType route
  | ".wasm" `T.isSuffixOf` T.pack route = "application/wasm"
  | ".glsl" `T.isSuffixOf` T.pack route = "text/plain; charset=utf-8"
  | otherwise = "application/javascript; charset=utf-8"

staticType :: FilePath -> TL.Text
staticType name
  | ".js" `T.isSuffixOf` T.pack name = "application/javascript; charset=utf-8"
  | ".css" `T.isSuffixOf` T.pack name = "text/css; charset=utf-8"
  | ".wasm" `T.isSuffixOf` T.pack name = "application/wasm"
  | otherwise = "application/octet-stream"

indexPage :: SitePaths -> [(Example, Maybe FilePath)] -> Html ()
indexPage paths shots = doctypehtml_ $
  html_ [makeAttribute "data-theme" "dark"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ "Examples"
      themeLinks (indexStatic paths)
      link_ [rel_ "stylesheet", href_ (indexStatic paths <> "/index.css")]
    body_ $
      main_ [class_ "page examples-index"] $ do
        header_ [class_ "page-header"] $ do
          h1_ "Examples"
          p_ [class_ "page-meta"] "JShark → JavaScript"
        div_ [class_ "example-grid"] $ mapM_ (exampleCard paths) shots

exampleCard :: SitePaths -> (Example, Maybe FilePath) -> Html ()
exampleCard paths (ex, shot) =
  div_ $
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
  [ "source-pane.js"
  , "tokens.css"
  , "base.css"
  , "pico/pico.min.css"
  , "source.css"
  , "index.css"
  , "breakout.css"
  , "synth.css"
  , "todo-mvc.css"
  , "todomvc-common-base.css"
  , "todomvc-app-index.css"
  , "life.css"
  , "life-shell.css"
  , "life-tool-preview.css"
  , "synth-keys.css"
  , "hvm2-demo.css"
  , "hvm2-demo.wasm"
  , "hvm2-wasm.js"
  , "hvm2-worker.js"
  ]
