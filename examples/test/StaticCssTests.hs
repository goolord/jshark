{-# LANGUAGE OverloadedStrings #-}

module StaticCssTests (staticCssTests) where

import Data.List (intercalate, isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import JShark.Example.Life.Patterns
  ( disturbPatterns
  , gliderSpeciesSid
  , patId
  , speciesColor
  )
import JShark.Example.Life.Types (eraserToolSid, gliderToolSid, mouseToolSid)
import JShark.Example.Synth.Keys
  ( black
  , blackLeft
  , blackWidth
  , keyChar
  , keys
  , noteId
  , primaryKey
  )
import Numeric (showFFloat)
import System.Directory (doesFileExist, getCurrentDirectory, getFileSize)
import System.FilePath (takeDirectory, (</>))
import System.IO
  ( IOMode (ReadMode)
  , hSetEncoding
  , utf8
  , withFile
  )
import Test.Tasty
import Test.Tasty.HUnit

-- | Read a committed/generated text asset as UTF-8 (files embed UTF-8
-- characters; do not depend on the process locale).
readUtf8 :: FilePath -> IO T.Text
readUtf8 p = withFile p ReadMode (\h -> hSetEncoding h utf8 >> TIO.hGetContents h)

-- | Repo root (contains @cabal.project@), independent of the test CWD.
repoRoot :: IO FilePath
repoRoot = getCurrentDirectory >>= go
 where
  go dir = do
    let
      proj = dir </> "cabal.project"
    ok <- doesFileExist proj
    if ok
      then pure dir
      else do
        let
          up = takeDirectory dir
        if up == dir
          then fail "static css test: cabal.project not found above cwd"
          else go up

staticCssTests :: TestTree
staticCssTests =
  testGroup
    "static css"
    [ testCase "required static assets exist" assertStaticAssets
    , testCase "pico version matches pin" assertPicoVersion
    , testCase "synth key labels match keyBindings" assertSynthKeyLabels
    , testCase "synth-keys.css matches Keys layout" $
        assertCssFile "examples/static/synth-keys.css" genSynthKeysCss
    , testCase "life-tool-preview.css matches species colors" $
        assertCssFile
          "examples/static/life-tool-preview.css"
          genLifeToolPreviewCss
    ]

assertStaticAssets :: IO ()
assertStaticAssets = do
  root <- repoRoot
  mapM_ (assertAsset root) requiredStaticAssets
 where
  requiredStaticAssets =
    [ "examples/static/tokens.css"
    , "examples/static/base.css"
    , "examples/static/pico/pico.min.css"
    , "examples/static/pico/VERSION"
    , "examples/static/source-pane.js"
    , "examples/static/synth-keys.css"
    , "examples/static/life-tool-preview.css"
    , "examples/static/speed-highlight/index.js"
    , "examples/static/speed-highlight/themes/github-dark.css"
    ]
  assertAsset root rel = do
    let
      path = root </> rel
    exists <- doesFileExist path
    assertBool (rel ++ " missing — run " ++ vendorHint rel) exists

vendorHint :: FilePath -> String
vendorHint rel
  | "pico" `isInfixOf` rel =
      "commit examples/static/pico/ (or scripts/vendor-pico.sh to refresh)"
  | "speed-highlight" `isInfixOf` rel = "scripts/vendor-speed-highlight.sh"
  | otherwise = "scripts/gen-*-css.sh"

assertPicoVersion :: IO ()
assertPicoVersion = do
  root <- repoRoot
  let
    pinPath = root </> "scripts/pico-version"
    vendoredPath = root </> "examples/static/pico/VERSION"
    cssPath = root </> "examples/static/pico/pico.min.css"
  pinExists <- doesFileExist pinPath
  assertBool "scripts/pico-version missing" pinExists
  cssExists <- doesFileExist cssPath
  assertBool
    ( "examples/static/pico/pico.min.css missing — commit examples/static/pico/"
        ++ " (or scripts/vendor-pico.sh to refresh)"
    )
    cssExists
  size <- getFileSize cssPath
  assertBool
    ( "examples/static/pico/pico.min.css looks empty — commit examples/static/pico/"
        ++ " (or scripts/vendor-pico.sh to refresh)"
    )
    (size > 50000)
  vendoredExists <- doesFileExist vendoredPath
  assertBool
    ( "examples/static/pico/VERSION missing — commit examples/static/pico/"
        ++ " (or scripts/vendor-pico.sh to refresh)"
    )
    vendoredExists
  pin <- readUtf8 pinPath
  ver <- readUtf8 vendoredPath
  assertEqual
    ( "pico version drift — commit examples/static/pico/"
        ++ " (or scripts/vendor-pico.sh to refresh)"
    )
    (T.strip pin)
    (T.strip ver)

assertSynthKeyLabels :: IO ()
assertSynthKeyLabels =
  mapM_ assertKeyLabel keys
 where
  assertKeyLabel k =
    assertEqual
      ("key label for " ++ T.unpack (noteId k))
      (primaryKey (noteId k))
      (keyChar k)

assertCssFile :: FilePath -> T.Text -> IO ()
assertCssFile rel expected = do
  root <- repoRoot
  onDisk <- readUtf8 (root </> rel)
  assertEqual
    (rel ++ " drift — rerun scripts/gen-*-css.sh")
    (normalizeCss expected)
    (normalizeCss onDisk)

normalizeCss :: T.Text -> T.Text
normalizeCss = T.dropWhileEnd (== '\n')

genSynthKeysCss :: T.Text
genSynthKeysCss =
  T.unlines $
    [ "/* Generated — run scripts/gen-synth-keys-css.sh */"
    , ""
    , T.pack $
        ".synth .key.black { width: "
          ++ showFFloat (Just 1) blackWidth ""
          ++ "%; }"
    , ""
    ]
      ++ map rule (filter black keys)
 where
  rule k =
    T.pack $
      "#"
        ++ T.unpack (noteId k)
        ++ " { left: "
        ++ showFFloat (Just 2) (blackLeft k) ""
        ++ "%; }"

genLifeToolPreviewCss :: T.Text
genLifeToolPreviewCss =
  T.unlines $
    [ "/* Generated — run scripts/gen-life-tool-preview-css.sh */"
    , ""
    , ".life-tool-preview {"
    , "  display: grid;"
    , "  grid-auto-rows: 3px;"
    , "  gap: 1px;"
    , "  justify-content: center;"
    , "}"
    , ""
    ]
      ++ [ T.pack $
             ".life-tool-preview[data-tw=\""
               ++ show n
               ++ "\"] { grid-template-columns: repeat("
               ++ show n
               ++ ", 3px); }"
         | n <- ([1 .. 12] :: [Int])
         ]
      ++ [""]
      ++ map (T.pack . colorRule) (map patId disturbPatterns)
      ++ map (T.pack . colorRule) [eraserToolSid, mouseToolSid, gliderToolSid]
 where
  colorRule sid
    | sid == eraserToolSid =
        ".life-tool[data-tool=\""
          ++ show sid
          ++ "\"] .life-tool-cell.is-on { background: #e55; }"
    | sid == mouseToolSid =
        ".life-tool[data-tool=\""
          ++ show sid
          ++ "\"] .life-tool-cell.is-on { background: #aaa; }"
    | sid == gliderToolSid =
        let
          (r, g, b) = speciesColor gliderSpeciesSid
         in
          ".life-tool[data-tool=\""
            ++ show sid
            ++ "\"] .life-tool-cell.is-on { background: rgb("
            ++ intercalate ", " [show r, show g, show b]
            ++ "); }"
    | otherwise =
        let
          (r, g, b) = speciesColor sid
          s = show sid
         in
          ".life-tool[data-tool=\""
            ++ s
            ++ "\"] .life-tool-cell.is-on { background: rgb("
            ++ intercalate ", " [show r, show g, show b]
            ++ "); }"
