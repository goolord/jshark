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
import Paths_jshark_examples (getDataFileName)
import System.Directory (doesFileExist, getFileSize)
import System.IO (IOMode (ReadMode), hSetEncoding, utf8, withFile)
import Test.Tasty
import Test.Tasty.HUnit

staticCssTests :: TestTree
staticCssTests =
  testGroup
    "static css"
    [ testCase "required static assets exist" $
        mapM_ (\rel -> assertExists (vendorHint rel) rel) requiredStaticAssets
    , testCase "pico version matches pin" $ do
        assertExists picoHint "static/pico/pico.min.css"
        size <- getFileSize =<< getDataFileName "static/pico/pico.min.css"
        assertBool ("pico.min.css looks empty — " ++ picoHint) (size > 50000)
        assertExists picoHint "static/pico/VERSION"
        ver <- readUtf8 =<< getDataFileName "static/pico/VERSION"
        assertEqual
          ("pico version drift — " ++ picoHint)
          picoVersionPin
          (T.strip ver)
    , testCase "synth key labels match keyBindings" $
        mapM_
          ( \k ->
              assertEqual
                ("key label for " ++ T.unpack (noteId k))
                (primaryKey (noteId k))
                (keyChar k)
          )
          keys
    , testCase "synth-keys.css matches Keys layout" $
        assertCssFile "static/css/synth-keys.css" genSynthKeysCss
    , testCase "life-tool-preview.css matches species colors" $
        assertCssFile "static/css/life-tool-preview.css" genLifeToolPreviewCss
    ]

-- | The pinned Pico version. The repo-level @scripts/pico-version@ is not
-- part of this package's data files, so the pin is kept here; the vendored
-- @static/pico/VERSION@ must match it.
picoVersionPin :: T.Text
picoVersionPin = "2.1.1"

requiredStaticAssets :: [FilePath]
requiredStaticAssets =
  [ "static/css/tokens.css"
  , "static/css/base.css"
  , "static/pico/pico.min.css"
  , "static/pico/VERSION"
  , "static/js/source-pane.js"
  , "static/css/synth-keys.css"
  , "static/css/life-tool-preview.css"
  , "static/speed-highlight/index.js"
  , "static/speed-highlight/themes/github-dark.css"
  ]

-- | Assert a data file exists, with a hint on how to restore it.
assertExists :: String -> FilePath -> Assertion
assertExists hint rel = do
  exists <- doesFileExist =<< getDataFileName rel
  assertBool ("examples/" ++ rel ++ " missing — " ++ hint) exists

vendorHint :: FilePath -> String
vendorHint rel
  | "pico" `isInfixOf` rel = picoHint
  | "speed-highlight" `isInfixOf` rel = "run scripts/vendor-speed-highlight.sh"
  | otherwise = "run scripts/gen-*-css.sh"

picoHint :: String
picoHint =
  "commit examples/static/pico/ (or scripts/vendor-pico.sh to refresh)"

-- | Read a committed/generated text asset as UTF-8 (files embed UTF-8
-- characters; do not depend on the process locale).
readUtf8 :: FilePath -> IO T.Text
readUtf8 p = withFile p ReadMode (\h -> hSetEncoding h utf8 >> TIO.hGetContents h)

assertCssFile :: FilePath -> T.Text -> IO ()
assertCssFile rel expected = do
  onDisk <- readUtf8 =<< getDataFileName rel
  assertEqual
    (rel ++ " drift — rerun scripts/gen-*-css.sh")
    (T.dropWhileEnd (== '\n') expected)
    (T.dropWhileEnd (== '\n') onDisk)

genSynthKeysCss :: T.Text
genSynthKeysCss =
  T.unlines . map T.pack $
    [ "/* Generated — run scripts/gen-synth-keys-css.sh */"
    , ""
    , ".synth .key.black { width: " ++ showFFloat (Just 1) blackWidth "%; }"
    , ""
    ]
      ++ [ "#"
             ++ T.unpack (noteId k)
             ++ " { left: "
             ++ showFFloat (Just 2) (blackLeft k) "%; }"
         | k <- filter black keys
         ]

genLifeToolPreviewCss :: T.Text
genLifeToolPreviewCss =
  T.unlines . map T.pack $
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
      ++ [ ".life-tool-preview[data-tw=\""
             ++ show n
             ++ "\"] { grid-template-columns: repeat("
             ++ show n
             ++ ", 3px); }"
         | n <- [1 .. 12 :: Int]
         ]
      ++ [""]
      ++ map colorRule (map patId disturbPatterns ++ toolSids)
 where
  toolSids = [eraserToolSid, mouseToolSid, gliderToolSid]
  colorRule sid =
    ".life-tool[data-tool=\""
      ++ show sid
      ++ "\"] .life-tool-cell.is-on { background: "
      ++ background sid
      ++ "; }"
  background sid
    | sid == eraserToolSid = "#e55"
    | sid == mouseToolSid = "#aaa"
    | sid == gliderToolSid = rgb (speciesColor gliderSpeciesSid)
    | otherwise = rgb (speciesColor sid)
  rgb (r, g, b) = "rgb(" ++ intercalate ", " [show r, show g, show b] ++ ")"
