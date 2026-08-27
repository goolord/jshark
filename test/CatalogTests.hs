{-# LANGUAGE OverloadedStrings #-}

module CatalogTests (catalogTests) where

import Data.List (find)
import qualified Data.Text.IO as T
import DiscoverCore (collectPhaseKey)
import Life (canonicalShapeHash, catalogJs)
import Patterns (PatternSpec (..), allPatterns, glider)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

catalogTests :: TestTree
catalogTests =
  testGroup
    "life catalog sidecar"
    [ testCase "catalog.js matches Haskell catalogJs" $ do
        root <- getCurrentDirectory
        onDisk <- T.readFile (root </> "examples/Life/js/catalog.js")
        onDisk @?= catalogJs
    , testCase "glider orientations share canonical hash" $
        canonicalShapeHash glider
          @?= canonicalShapeHash (patCells gliderUpPat)
    , testCase "toad phases share empirical phase key" $ do
        let
          toadCells = patCells toadPat
        phaseKey toadCells @?= phaseKey (stepPattern toadCells)
    , testCase "block stays single-phase" $
        length (phaseHashes block) @?= 1
    ]
 where
  block = [(0, 0), (0, 1), (1, 0), (1, 1)]
  gliderUpPat =
    case find ((== 57) . patId) allPatterns of
      Just p -> p
      Nothing -> error "gliderUp missing from catalog"
  toadPat =
    case find ((== 26) . patId) allPatterns of
      Just p -> p
      Nothing -> error "toad missing from catalog"

  phaseKey coords = fst (collectPhaseKey coords)

  phaseHashes coords = snd (collectPhaseKey coords)

  stepPattern coords =
    let
      (minX, minY, maxX, maxY) = bounds coords
      pad = 2
      ox = minX - pad
      oy = minY - pad
      gw = maxX - minX + 1 + 2 * pad
      gh = maxY - minY + 1 + 2 * pad
      grid1 = stepGrid (stamp coords ox oy gw gh) gw gh
     in
      [ (x + ox, y + oy)
      | y <- [0 .. gh - 1]
      , x <- [0 .. gw - 1]
      , grid1 !! (y * gw + x)
      ]

  bounds coords =
    ( minimum (map fst coords)
    , minimum (map snd coords)
    , maximum (map fst coords)
    , maximum (map snd coords)
    )

  stamp coords ox oy gw gh =
    foldr
      (\(x, y) g -> setCell g gw (x - ox) (y - oy))
      (replicate (gw * gh) False)
      coords

  setCell g gw x y =
    let
      i = y * gw + x
     in
      take i g ++ [True] ++ drop (i + 1) g

  stepGrid grid gw gh =
    [alive x y | y <- [0 .. gh - 1], x <- [0 .. gw - 1]]
   where
    alive x y =
      let
        n =
          sum
            [ if dx == 0 && dy == 0 then 0 else count nx ny
            | dy <- [-1 .. 1]
            , dx <- [-1 .. 1]
            , let
                nx = x + dx
                ny = y + dy
            , nx >= 0
            , ny >= 0
            , nx < gw
            , ny < gh
            ]
        i = y * gw + x
       in
        n == (3 :: Int) || (grid !! i && n == (2 :: Int))
    count nx ny =
      if grid !! (ny * gw + nx) then 1 else 0
