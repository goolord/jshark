{-# LANGUAGE OverloadedStrings #-}

module CatalogTests (catalogTests) where

import BunGate (bunGroup)
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import JShark.Bun (evaluateEffectJSON)
import JShark.Example.Life (canonicalShapeHash, catalogJs, shapeHash)
import JShark.Example.Life.DiscoverCore
  ( ResolveResult (..)
  , classifyAndResolve
  , collectPhaseKey
  , extractCoords
  )
import JShark.Example.Life.LifeTestSupport
  ( runtimeBlockPhaseHashLen
  , runtimeBlockPhaseKey
  )
import JShark.Example.Life.Patterns
  ( PatternSpec (..)
  , allPatterns
  , glider
  , speciesColor
  )
import Paths_jshark_examples (getDataFileName)
import Test.Tasty
import Test.Tasty.HUnit

catalogTests :: TestTree
catalogTests =
  testGroup
    "life catalog sidecar"
    [ testCase "catalog.js matches Haskell catalogJs" $ do
        onDisk <- T.readFile =<< getDataFileName "src/JShark/Example/Life/js/catalog.js"
        onDisk @?= catalogJs
    , testCase "glider orientations share canonical hash" $
        canonicalShapeHash glider @?= canonicalShapeHash (patternCells 57)
    , testCase "toad phases share empirical phase key" $
        phaseKey (patternCells 26) @?= phaseKey (stepPattern (patternCells 26))
    , testCase "block stays single-phase" $
        length (phaseHashes block) @?= 1
    , testCase "glider classifies via drift stop" $
        assertBool "glider has a phase key" (not (T.null (phaseKey glider)))
    , testCase "unstable pattern rejects without stable phase" $
        assertBool "cross has no phase key" (T.null (phaseKey cross))
    , testCase "shapeHash normalizes and sorts coords" $
        shapeHash block @?= "0,0;0,1;1,0;1,1"
    , testCase "classifyAndResolve waits for second sighting" $ do
        let
          first = resolve Map.empty Map.empty 100 blockCells
          second = resolve Map.empty (Map.singleton blockKey 1) 100 blockCells
        rrAction first @?= 0
        rrKey first @?= blockKey
        rrAction second @?= 2
        rrSid second @?= 100
        speciesColor 100 @?= (rrR second, rrG second, rrB second)
    , testCase "classifyAndResolve at cap asks to steal a slot" $ do
        let
          res = resolve Map.empty (Map.singleton blockKey 1) 256 blockCells
        rrAction res @?= 3
        rrKey res @?= blockKey
    , testCase "glider is one 8-connected component" $
        eightComponentSize glider @?= 5
    , testCase "diehard is classic 8x3 methuselah" $
        shapeHash (patternCells 62) @?= "0,1;1,1;2,1;2,2;6,0;6,2;7,2"
    , testCase "classifyAndResolve blinker hits catalog on first sight" $ do
        let
          key = canonicalShapeHash [(0, 0), (1, 0), (2, 0)]
          res = resolve (Map.singleton key 25) Map.empty 100 [0, 1, 2]
        rrAction res @?= 1
        rrSid res @?= 25
    , testCase "classifyAndResolve known catalog hits on first sight" $ do
        let
          known = Map.singleton (canonicalShapeHash block) 42
          res = resolve known Map.empty 100 blockCells
        rrAction res @?= 1
        rrSid res @?= 42
    , bunGroup
        "runtime classifier parity"
        [ testCase "runtime collectPhaseKey key matches DiscoverCore for block" $ do
            got <- evaluateEffectJSON runtimeBlockPhaseKey
            -- The JSON string's contents, unquoted.
            maybe got (T.takeWhile (/= '"')) (T.stripPrefix "\"" got)
              @?= phaseKey block
        , testCase "runtime collectPhaseKey hash count matches DiscoverCore" $ do
            got <- evaluateEffectJSON runtimeBlockPhaseHashLen
            got @?= T.pack (show (length (phaseHashes block)))
        ]
    ]
 where
  block = [(0, 0), (0, 1), (1, 0), (1, 1)]
  cross = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
  -- The block on a width-10 grid, and its phase key.
  blockCells = [0, 1, 10, 11]
  blockKey = phaseKey (extractCoords 10 blockCells)
  resolve known pending nextSid =
    classifyAndResolve known Map.empty pending nextSid 255 10
  patternCells n =
    maybe
      (error ("pattern " ++ show n ++ " missing"))
      patCells
      (find ((== n) . patId) allPatterns)
  phaseKey = fst . collectPhaseKey
  phaseHashes = snd . collectPhaseKey

-- | Size of the 8-connected component holding the first live cell.
eightComponentSize :: [(Int, Int)] -> Int
eightComponentSize [] = 0
eightComponentSize live@(s : _) = length (flood [s] [s])
 where
  flood [] seen = seen
  flood ((x, y) : ps) seen =
    let
      new =
        [ q
        | dx <- [-1 .. 1]
        , dy <- [-1 .. 1]
        , (dx, dy) /= (0, 0)
        , let
            q = (x + dx, y + dy)
        , q `elem` live
        , q `notElem` seen
        ]
     in
      flood (new ++ ps) (new ++ seen)

-- | One Life generation, row-major. Births need three live neighbours, so
-- only the bounding box grown by one can come alive.
stepPattern :: [(Int, Int)] -> [(Int, Int)]
stepPattern cells =
  [ (x, y)
  | y <- [minimum ys - 1 .. maximum ys + 1]
  , x <- [minimum xs - 1 .. maximum xs + 1]
  , let
      n =
        length
          [ ()
          | dy <- [-1 .. 1]
          , dx <- [-1 .. 1]
          , (dx, dy) /= (0, 0)
          , (x + dx, y + dy) `elem` cells
          ]
  , n == 3 || (n == 2 && (x, y) `elem` cells)
  ]
 where
  xs = map fst cells
  ys = map snd cells
