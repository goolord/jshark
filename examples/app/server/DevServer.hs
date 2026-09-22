{-# LANGUAGE OverloadedStrings #-}

module DevServer
  ( Example (..)
  , ServeMode (..)
  , exportExamples
  , serveExamples
  )
where

import qualified Control.Exception as E
import Control.Monad (filterM, forM, forM_, guard, when)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified JShark.Example.Life as Life
import JShark.Example.Theme (githubCorner, themeLinks)
import JShark.Example.Watch (exampleWatchTargets)
import JShark.HotReload.Core
import JShark.HotReload.Wai (hotReloadMiddleware)
import JShark.HotReload.Watcher (WatchTargets (..), startWatcher)
import Lucid
import Lucid.Base (makeAttribute)
import Network.Wai.Handler.Warp (setHost, setPort)
import Paths_jshark_examples (getDataFileName)
import Recompile (CabalHot (..), prepareCabalHot, startHsRecompiler)
import System.Directory
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (isAlreadyInUseError)
import Web.Scotty

-- | One compiled example, mounted at @/<name>@ (or @<name>/@ on a static site).
data Example = Example
  { exampleName :: T.Text
  , exampleTitle :: T.Text
  , examplePage :: T.Text -> T.Text -> Html ()
  -- ^ The page, given its script URL and static root.
  , exampleJs :: T.Text
  , exampleSourceJs :: T.Text
  -- ^ Display JS ('prettyJS' of the compiled output), served at
  --   @source.js@; Life loads it there, the others embed it in the page.
  }

-- | Static assets only, or hot-reload hub + filesystem watcher.
data ServeMode
  = StaticServe
  | HotServe HotReloadConfig
  deriving (Show, Eq)

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
    ("/" <>)
    (\n -> "/static/img/" <> n <> ".png")
    (\n -> "/" <> n <> "/app.js")
    "/static"
    "/static"

-- | Relative URLs from @index.html@ and @<name>/index.html@.
exportPaths :: SitePaths
exportPaths =
  SitePaths
    (<> "/")
    (\n -> "static/img/" <> n <> ".png")
    (const "app.js")
    "static"
    "../static"

pageAt :: SitePaths -> Example -> Html ()
pageAt paths ex =
  examplePage ex (srcScript paths (exampleName ex)) (srcStatic paths)

-- | Life's sandboxed frame document.
frameAt :: SitePaths -> Example -> Html ()
frameAt paths ex =
  Life.framePage (srcStatic paths) script (Life.assetBaseFor script)
 where
  script = srcScript paths (exampleName ex)

-- | Extra JS assets for Life (route suffix, data-file path under package).
lifeEngineJs :: [(FilePath, FilePath)]
lifeEngineJs =
  [ ("js/pixi.min.js", "src/JShark/Example/Life/js/pixi.min.js")
  , ("js/EngineWorker.js", "src/JShark/Example/Life/js/EngineWorker.js")
  , ("js/shaders/cell.frag.glsl", "src/JShark/Example/Life/shaders/cell.frag.glsl")
  ]

-- | Serve every example and a screenshot directory at @/@.
-- Tries @startPort@, then successive ports until warp binds.
serveExamples :: ServeMode -> Int -> [Example] -> IO ()
serveExamples mode startPort examples = do
  shots <- traverse exampleShot examples
  assets <- staticAssets
  lifeJs <- traverse (traverse resolveDataFile) lifeEngineJs
  mHub <- case mode of
    StaticServe -> pure Nothing
    HotServe cfg -> Just <$> startHot cfg examples
  let
    maxPort = startPort + 100
    routes = exampleRoutes mHub shots assets lifeJs examples
    opts port =
      defaultOptions
        { settings = setHost "127.0.0.1" . setPort port $ settings defaultOptions
        }
    serveOn port
      | port > maxPort =
          fail ("no free port in range " <> show startPort <> ".." <> show maxPort)
      | otherwise = do
          putStrLn ("Examples on http://127.0.0.1:" <> show port)
          hFlush stdout
          scottyOpts (opts port) routes `E.catch` \e ->
            if isAlreadyInUseError e
              then do
                hPutStrLn
                  stderr
                  ("port " <> show port <> " in use, trying " <> show (port + 1))
                serveOn (port + 1)
              else E.throwIO (e :: E.IOException)
  serveOn startPort

-- | Seed a hub with every example's JS and page, then watch the sources.
startHot :: HotReloadConfig -> [Example] -> IO HotReloadHub
startHot cfg examples = do
  hub <- newHotReloadHub cfg
  forM_ examples $ \ex -> do
    _ <- registerJs hub (exampleName ex) (exampleJs ex)
    registerHtml
      hub
      (exampleName ex)
      (TL.toStrict (renderText (pageAt serverPaths ex)))
  hot <- prepareCabalHot
  onHs <- startHsRecompiler hub hot
  _ <-
    startWatcher hub (exampleWatchTargets ["examples"]) {onHaskellSource = onHs}
  putStrLn . unwords $
    ["hot-reload: haskell recompiler via", hotCompileBin hot, "using", hotCabal hot]
  hFlush stdout
  pure hub

exampleRoutes ::
  Maybe HotReloadHub
  -> [(Example, Maybe FilePath)]
  -> [(FilePath, FilePath)]
  -> [(FilePath, FilePath)]
  -> [Example]
  -> ScottyM ()
exampleRoutes mHub shots assets lifeJs examples = do
  forM_ mHub $ \hub ->
    middleware (hotReloadMiddleware (hotReloadConfig hub) hub)
  get "/" $ html (renderText (indexPage serverPaths shots))
  forM_ examples $ \ex -> do
    let
      name = exampleName ex
      route suffix = get (fromString ("/" <> T.unpack name <> suffix))
      isLife = name == "life"
      page = TL.toStrict (renderText (pageAt serverPaths ex))
      -- The hub's latest build of an artifact, else the startup one.
      latest lookupFn fallback = case mHub of
        Nothing -> pure fallback
        Just hub -> maybe fallback fst <$> lookupFn hub name
      js load = do
        setHeader "Content-Type" "application/javascript; charset=utf-8"
        setHeader "Cache-Control" "no-store"
        when isLife crossOrigin
        text . TL.fromStrict =<< liftIO load
    forM_ ["", "/"] $ \suffix ->
      route suffix $ html . TL.fromStrict =<< liftIO (latest lookupHtml page)
    route "/app.js" $ js (latest lookupJs (exampleJs ex))
    route "/source.js" $ js (pure (exampleSourceJs ex))
    when isLife $ do
      forM_ ["/frame", "/frame/"] $ \suffix ->
        route suffix $ crossOrigin >> html (renderText (frameAt serverPaths ex))
      forM_ lifeJs $ \(sub, path) -> route ("/" <> sub) (asset sub path)
  forM_ assets $ \(sub, path) ->
    get (fromString ("/static/" <> sub)) (asset sub path)
  forM_ shots $ \(ex, shot) -> forM_ shot $ \path ->
    get (fromString (T.unpack (srcShot serverPaths (exampleName ex)))) $ do
      setHeader "Content-Type" "image/png"
      setHeader "Cross-Origin-Resource-Policy" "cross-origin"
      file path
 where
  asset route path = do
    setHeader "Content-Type" $ case takeExtension route of
      ".js" -> "application/javascript; charset=utf-8"
      ".css" -> "text/css; charset=utf-8"
      ".wasm" -> "application/wasm"
      ".glsl" -> "text/plain; charset=utf-8"
      _ -> "application/octet-stream"
    crossOrigin
    file path

-- | Life's sandboxed frame fetches @app.js@ / wasm from the example origin,
-- so CORP + ACAO are required. COOP/COEP stay off the shell HTML so the
-- frame is not blocked by require-corp (SharedArrayBuffer workers need
-- headers on the frame document itself).
crossOrigin :: ActionM ()
crossOrigin = do
  setHeader "Cross-Origin-Resource-Policy" "cross-origin"
  setHeader "Access-Control-Allow-Origin" "*"

-- | Write a static tree GitHub Pages can host.
exportExamples :: FilePath -> [Example] -> IO ()
exportExamples dest examples = do
  setLocaleEncoding utf8
  removePathForcibly dest
  createDirectoryIfMissing True (dest </> "static")
  shots <- traverse exampleShot examples
  TL.writeFile (dest </> "index.html") (renderText (indexPage exportPaths shots))
  writeFile (dest </> ".nojekyll") ""
  assets <- staticAssets
  forM_ assets $ \(name, src) -> copyInto src (dest </> "static" </> name)
  forM_ shots $ \(ex, shot) -> forM_ shot $ \src ->
    copyInto
      src
      (dest </> "static" </> "img" </> T.unpack (exampleName ex) <> ".png")
  forM_ examples $ \ex -> do
    let
      name = T.unpack (exampleName ex)
      dir = dest </> name
    createDirectoryIfMissing True dir
    writeFile (dest </> name <> ".html") (slashRedirect name)
    TL.writeFile (dir </> "index.html") (renderText (pageAt exportPaths ex))
    T.writeFile (dir </> "app.js") (exampleJs ex)
    when (exampleName ex == "life") $ do
      createDirectoryIfMissing True (dir </> "frame")
      TL.writeFile
        (dir </> "frame" </> "index.html")
        (renderText (frameAt exportPaths ex))
      T.writeFile (dir </> "source.js") (exampleSourceJs ex)
      forM_ lifeEngineJs $ \(route, rel) -> do
        src <- resolveDataFile rel
        copyInto src (dir </> route)
 where
  copyInto src out = do
    createDirectoryIfMissing True (takeDirectory out)
    copyFile src out

-- | Pretty URL without a trailing slash (@/breakout@) would otherwise resolve
-- @app.js@ as a sibling. GitHub Pages serves @<name>.html@ for that path.
slashRedirect :: FilePath -> String
slashRedirect name =
  concat
    [ "<!DOCTYPE html><meta charset=\"utf-8\">"
    , "<meta http-equiv=\"refresh\" content=\"0;url=" <> name <> "/\">"
    , "<link rel=\"canonical\" href=\"" <> name <> "/\">"
    , "<script>location.replace(" <> show (name <> "/") <> ")</script>"
    ]

-- | The installed data file, else the source-tree copy (run from the
-- package or the repo root).
resolveDataFile :: FilePath -> IO FilePath
resolveDataFile rel = do
  installed <- getDataFileName rel
  found <- filterM doesPathExist [installed, rel, "examples" </> rel]
  case found of
    p : _ -> pure p
    [] -> fail ("serve: missing data-file " <> rel)

exampleShot :: Example -> IO (Example, Maybe FilePath)
exampleShot ex = do
  path <- getDataFileName ("static/img/" <> T.unpack (exampleName ex) <> ".png")
  exists <- doesFileExist path
  pure (ex, path <$ guard exists)

-- | Everything under @/static/@ as (route, path): 'staticFiles' plus the
-- vendored speed-highlight tree, whose route must match @source-pane.js@'s
-- @../speed-highlight/index.js@ import. Routes always use @/@.
staticAssets :: IO [(FilePath, FilePath)]
staticAssets = do
  listed <- forM staticFiles $ \name ->
    (,) name <$> resolveDataFile ("static/" <> name)
  root <- resolveDataFile "static/speed-highlight"
  isTree <- doesDirectoryExist root
  tree <- if isTree then walk "speed-highlight" root else pure []
  when (null tree) $
    fail
      "serve: missing speed-highlight tree — run \
      \scripts/vendor-speed-highlight.sh"
  pure (listed ++ tree)
 where
  walk route dir = do
    entries <- listDirectory dir
    fmap concat . forM entries $ \entry -> do
      let
        path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then walk (route <> "/" <> entry) path
        else pure [(route <> "/" <> entry, path)]

indexPage :: SitePaths -> [(Example, Maybe FilePath)] -> Html ()
indexPage paths shots = doctypehtml_ $
  html_ [makeAttribute "data-theme" "dark"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ "Examples"
      themeLinks (indexStatic paths)
      link_ [rel_ "stylesheet", href_ (indexStatic paths <> "/css/index.css")]
    body_ $ do
      githubCorner
      main_ [class_ "page examples-index"] $ do
        header_ [class_ "page-header"] $ do
          h1_ "Examples"
          p_ [class_ "page-meta"] "JShark → JavaScript"
        div_ [class_ "example-grid"] . forM_ shots $ \(ex, shot) ->
          div_ . a_ [href_ (hrefExample paths (exampleName ex))] $ do
            forM_ shot $ \_ ->
              img_ [src_ (srcShot paths (exampleName ex)), alt_ (exampleTitle ex)]
            span_ (toHtml (exampleTitle ex))

staticFiles :: [FilePath]
staticFiles =
  [ "js/source-pane.js"
  , "css/tokens.css"
  , "css/base.css"
  , "pico/pico.min.css"
  , "css/source.css"
  , "css/index.css"
  , "css/breakout.css"
  , "css/synth.css"
  , "css/todo-mvc.css"
  , "css/todomvc-common-base.css"
  , "css/todomvc-app-index.css"
  , "css/life.css"
  , "css/life-shell.css"
  , "css/life-tool-preview.css"
  , "css/synth-keys.css"
  ]
