{-# LANGUAGE OverloadedStrings #-}

module StaticCssTests (staticCssTests) where

import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Keys (black, blackLeft, keyChar, keys, noteId, primaryKey)
import Numeric (showFFloat)
import Patterns (disturbPatterns, gliderSpeciesSid, patId, speciesColor)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Types (eraserToolSid, gliderToolSid, mouseToolSid)

staticCssTests :: TestTree
staticCssTests =
  testGroup
    "static css"
    [ testCase "required static assets exist" assertStaticAssets
    , testCase "synth key labels match keyBindings" assertSynthKeyLabels
    , testCase "synth-keys.css matches Keys.blackLeft" $
        assertCssFile "examples/static/synth-keys.css" genSynthKeysCss
    , testCase "life-tool-preview.css matches species colors" $
        assertCssFile
          "examples/static/life-tool-preview.css"
          genLifeToolPreviewCss
    ]

assertStaticAssets :: IO ()
assertStaticAssets = do
  root <- getCurrentDirectory
  mapM_ (assertAsset root) requiredStaticAssets
 where
  requiredStaticAssets =
    [ "examples/static/base.css"
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
    assertBool
      (rel ++ " missing — run scripts/vendor-speed-highlight.sh or gen-*-css.sh")
      exists

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
  root <- getCurrentDirectory
  onDisk <- TIO.readFile (root </> rel)
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
