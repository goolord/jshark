{-# LANGUAGE OverloadedStrings #-}

module CatalogTests (catalogTests) where

import Data.List (find, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Life (canonicalShapeHash, catalogJs, shapeHash)
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

  phaseKey coords =
    case phaseHashes coords of
      [_] -> canonicalShapeHash coords
      hashes -> T.intercalate "|" (sort hashes)

  phaseHashes coords =
    let
      (minX, minY, maxX, maxY) = bounds coords
      pad = 2
      ox = minX - pad
      oy = minY - pad
      gw = maxX - minX + 1 + 2 * pad
      gh = maxY - minY + 1 + 2 * pad
      grid0 = stamp coords ox oy gw gh
     in
      collect grid0 gw gh ox oy coords [] []

  collect grid gw gh ox oy origCoords history acc
    | null liveLocal = acc
    | exact `elem` history = acc
    | length acc >= 32 = acc
    | otherwise =
        let
          nextAcc = exact : acc
          (ocx, ocy) = centroid origCoords
          (cx, cy) = centroid [(x + ox, y + oy) | (x, y) <- liveLocal]
          moved = abs (cx - ocx) + abs (cy - ocy) > (0.75 :: Double)
         in
          if moved && not (null acc)
            then acc
            else
              collect
                (stepGrid grid gw gh)
                gw
                gh
                ox
                oy
                origCoords
                (exact : history)
                nextAcc
   where
    liveLocal =
      [ (x, y)
      | y <- [0 .. gh - 1]
      , x <- [0 .. gw - 1]
      , grid !! (y * gw + x)
      ]
    exact = shapeHash liveLocal

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

  centroid cs =
    let
      n = fromIntegral (length cs) :: Double
      (sx, sy) = foldr (\(x, y) (a, b) -> (a + fromIntegral x, b + fromIntegral y)) (0, 0) cs
     in
      (sx / n, sy / n)
