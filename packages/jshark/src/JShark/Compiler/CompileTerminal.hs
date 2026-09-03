{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ANSI terminal styling for compile progress and stats output.
module JShark.Compiler.CompileTerminal
  ( TerminalStyle (..)
  , terminalStyleIO
  , boldSGR
  , cyanSGR
  , dimSGR
  , greenSGR
  , clearLine
  , cursorUp
  , formatDuration
  , renderBatchDone
  , renderSingleDone
  , renderStatsTable
  , styled
  )
where

import Data.Char (toLower)
import Data.List (intercalate, isInfixOf, sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import GHC.IO.Encoding (TextEncoding, textEncodingName)
import JShark.Compiler.CompileTiming (CompileForm (..), CompileJobStats (..))
import Numeric (showFFloat)
import System.Console.ANSI
  ( Color (..)
  , ColorIntensity (..)
  , ConsoleIntensity (..)
  , ConsoleLayer (..)
  , SGR (..)
  , hSupportsANSI
  )
import System.Console.ANSI.Codes
  ( clearLineCode
  , cursorUpCode
  , setSGRCode
  )
import System.IO (hGetEncoding, stderr)

data TerminalStyle = TerminalPlain | TerminalTTY
  deriving (Eq, Show)

encodingSupportsUnicode :: Maybe TextEncoding -> Bool
encodingSupportsUnicode Nothing = False
encodingSupportsUnicode (Just enc) =
  let
    name = map toLower (textEncodingName enc)
   in
    "utf-8" `isInfixOf` name || "utf8" `isInfixOf` name

stderrSupportsUnicode :: IO Bool
stderrSupportsUnicode = do
  mEnc <- hGetEncoding stderr
  pure (encodingSupportsUnicode mEnc)

terminalStyleIO :: IO TerminalStyle
terminalStyleIO = do
  ansi <- hSupportsANSI stderr
  unicode <- stderrSupportsUnicode
  pure (if ansi && unicode then TerminalTTY else TerminalPlain)

boldSGR, dimSGR, cyanSGR, greenSGR :: [SGR]
boldSGR = [SetConsoleIntensity BoldIntensity]
dimSGR = [SetConsoleIntensity FaintIntensity]
cyanSGR = [SetColor Foreground Vivid Cyan]
greenSGR = [SetColor Foreground Vivid Green]

styled :: TerminalStyle -> [SGR] -> String -> String
styled TerminalPlain _ s = s
styled TerminalTTY sgr s = setSGRCode sgr ++ s ++ setSGRCode [Reset]

clearLine :: String
clearLine = "\r" ++ clearLineCode

cursorUp :: Int -> String
cursorUp n = if n > 0 then cursorUpCode n else ""

formatDuration :: Double -> String
formatDuration s
  | s < 0.001 = show (round (s * 1e6 :: Double) :: Integer) ++ "us"
  | s < 1 = show (round (s * 1000 :: Double) :: Integer) ++ "ms"
  | otherwise = showFFloat (Just 2) s ""

renderSingleDone :: TerminalStyle -> Double -> String
renderSingleDone style secs =
  let
    dur = formatDuration secs
   in
    case style of
      TerminalPlain ->
        "JShark.Compiler: compiled in " ++ dur
      TerminalTTY ->
        styled style greenSGR "✅"
          ++ " "
          ++ "JShark compiled in "
          ++ styled style cyanSGR dur

renderBatchDone :: TerminalStyle -> Int -> Double -> String
renderBatchDone style total secs =
  let
    dur = formatDuration secs
   in
    case style of
      TerminalPlain ->
        "JShark.Compiler: compiled "
          ++ show total
          ++ " programs in "
          ++ dur
      TerminalTTY ->
        styled style greenSGR "✅"
          ++ " "
          ++ "JShark compiled "
          ++ styled style boldSGR (show total)
          ++ " programs in "
          ++ styled style cyanSGR dur

renderStatsTable :: TerminalStyle -> Maybe Double -> [CompileJobStats] -> String
renderStatsTable style mBatchWall stats =
  case style of
    TerminalPlain -> renderPlainStatsTable mBatchWall stats
    TerminalTTY -> renderTTYStatsTable mBatchWall stats

renderPlainStatsTable :: Maybe Double -> [CompileJobStats] -> String
renderPlainStatsTable mBatchWall stats =
  unlines (header : map (renderPlainRow cols) sorted ++ footerRows)
 where
  sorted = sortBy (comparing cjsLabel) stats
  cols = statsColumns
  header = renderPlainHeader cols
  footerRows =
    renderPlainFooter cols sorted
      ++ case mBatchWall of
        Nothing -> []
        Just w -> [renderPlainWallRow cols w]

renderTTYStatsTable :: Maybe Double -> [CompileJobStats] -> String
renderTTYStatsTable mBatchWall stats =
  unlines
    ( styled TerminalTTY dimSGR (boxLine cols "╭" "┬" "╮" '─')
        : styledRow boldSGR cols (map colHeader cols)
        : styled TerminalTTY dimSGR (boxLine cols "├" "┼" "┤" '─')
        : map (renderDataRow cols) sorted
        ++ [ styled TerminalTTY dimSGR (boxLine cols "├" "┼" "┤" '─')
           , styledRow boldSGR cols (footerCells cols sorted)
           ]
        ++ case mBatchWall of
          Nothing -> []
          Just w ->
            [ styledRow boldSGR cols (wallCells cols w)
            , styled TerminalTTY dimSGR (boxLine cols "╰" "┴" "╯" '─')
            ]
        ++ case mBatchWall of
          Nothing ->
            [styled TerminalTTY dimSGR (boxLine cols "╰" "┴" "╯" '─')]
          Just _ -> []
    )
 where
  sorted = sortBy (comparing cjsLabel) stats
  cols = statsColumns

renderDataRow :: [Column] -> CompileJobStats -> String
renderDataRow cols stat =
  styledRow [] cols (rowCells cols stat)

styledRow :: [SGR] -> [Column] -> [String] -> String
styledRow base cols cells =
  styled TerminalTTY (base ++ dimSGR) "│"
    ++ intercalate
      (styled TerminalTTY dimSGR "│")
      (zipWith renderCell cols cells)
 where
  renderCell col raw =
    styled TerminalTTY base (padCell (colAlign col raw))

data Column = Column
  { colHeader :: !String
  , colWidth :: !Int
  , colAlign :: !(String -> String)
  , colValue :: CompileJobStats -> String
  }

statsColumns :: [Column]
statsColumns =
  [ programCol
  , secCol "job-total" 8 cjsTotalSec
  , secCol "lint" 7 cjsLintSec
  , secCol "irprep" 7 cjsIrPrepareSec
  , secCol "pack" 7 cjsPackSec
  , secCol "fopt" 7 cjsFlatOptSec
  , secCol "phopt" 7 cjsPhoasOptSec
  , secCol "emit" 7 cjsEmitSec
  , secCol "min" 7 cjsMinifySec
  , bytesCol
  ]

programCol :: Column
programCol =
  Column
    { colHeader = "program"
    , colWidth = 24
    , colAlign = padRight 24
    , colValue = \stat ->
        T.unpack (cjsLabel stat) ++ formSuffix (cjsForm stat)
    }

secCol :: String -> Int -> (CompileJobStats -> Double) -> Column
secCol hdr w field =
  Column
    { colHeader = hdr
    , colWidth = w
    , colAlign = padLeft w
    , colValue = fmtSec . field
    }

bytesCol :: Column
bytesCol =
  Column
    { colHeader = "bytes"
    , colWidth = 9
    , colAlign = padLeft 9
    , colValue = show . cjsJsBytes
    }

rowCells :: [Column] -> CompileJobStats -> [String]
rowCells cols stat = [colValue c stat | c <- cols]

renderPlainHeader :: [Column] -> String
renderPlainHeader cols = concat [colAlign c (colHeader c) | c <- cols]

renderPlainRow :: [Column] -> CompileJobStats -> String
renderPlainRow cols stat =
  concat [colAlign c (colValue c stat) | c <- cols]

renderPlainFooter :: [Column] -> [CompileJobStats] -> [String]
renderPlainFooter (progCol : restCols) sorted =
  [ padRight (colWidth progCol) "totals"
      ++ concat
        [ padLeft (colWidth c) (footerValue c sorted)
        | c <- restCols
        ]
  ]
renderPlainFooter [] _ = ["totals"]

renderPlainWallRow :: [Column] -> Double -> String
renderPlainWallRow (progCol : restCols) w =
  padRight (colWidth progCol) "wall"
    ++ concat
      [ padLeft (colWidth c) (wallValue c w)
      | c <- restCols
      ]
renderPlainWallRow [] _ = "wall"

footerCells :: [Column] -> [CompileJobStats] -> [String]
footerCells (_ : restCols) sorted =
  "totals" : [footerValue c sorted | c <- restCols]
footerCells [] _ = ["totals"]

wallCells :: [Column] -> Double -> [String]
wallCells (_ : restCols) w =
  "wall" : [wallValue c w | c <- restCols]
wallCells [] _ = ["wall"]

footerValue :: Column -> [CompileJobStats] -> String
footerValue c sorted
  | colHeader c == "job-total" = fmtSec (sum (map cjsTotalSec sorted))
  | colHeader c == "bytes" = show (sum (map cjsJsBytes sorted))
  | otherwise = fmtSec (sum (map (phaseSec c) sorted))

wallValue :: Column -> Double -> String
wallValue c w
  | colHeader c == "job-total" = fmtSec w
  | otherwise = "-"

phaseSec :: Column -> CompileJobStats -> Double
phaseSec c = case colHeader c of
  "lint" -> cjsLintSec
  "irprep" -> cjsIrPrepareSec
  "pack" -> cjsPackSec
  "fopt" -> cjsFlatOptSec
  "phopt" -> cjsPhoasOptSec
  "emit" -> cjsEmitSec
  "min" -> cjsMinifySec
  _ -> const 0

boxLine :: [Column] -> String -> String -> String -> Char -> String
boxLine cols left mid right fill =
  left
    ++ intercalate mid [replicate (cellWidth c) fill | c <- cols]
    ++ right

cellWidth :: Column -> Int
cellWidth c = colWidth c + 2

padCell :: String -> String
padCell s = " " ++ s ++ " "

formSuffix :: CompileForm -> String
formSuffix = \case
  FormReadable -> " [read]"
  FormMinified -> " [min]"

fmtSec :: Double -> String
fmtSec s
  | s <= 0 = "-"
  | s < 0.001 = show (round (s * 1e6 :: Double) :: Integer) ++ "us"
  | s < 1 = show (round (s * 1000 :: Double) :: Integer) ++ "ms"
  | otherwise = showFFloat (Just 2) s ""

padLeft :: Int -> String -> String
padLeft w s =
  let
    k = w - length s
   in
    if k > 0 then replicate k ' ' ++ s else take w s

padRight :: Int -> String -> String
padRight w s =
  let
    k = w - length s
   in
    if k > 0 then s ++ replicate k ' ' else take w s
