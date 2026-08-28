{-# LANGUAGE OverloadedStrings #-}

module CatalogTests (catalogTests) where

import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import DiscoverCore
  ( ResolveResult (..)
  , classifyAndResolve
  , collectPhaseKey
  , discoverRgb
  , extractCoords
  )
import JShark.Bun (evaluateEffectJSON)
import Life (canonicalShapeHash, catalogJs, shapeHash)
import LifeTestSupport (runtimeBlockPhaseHashLen, runtimeBlockPhaseKey)
import Patterns (PatternSpec (..), allPatterns, glider, speciesColor)
import System.Directory (findExecutable, getCurrentDirectory)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Types (discoverMin)

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
    , testCase "shapeHash normalizes and sorts coords" $
        shapeHash block @?= "0,0;0,1;1,0;1,1"
    , testCase "discoverRgb matches speciesColor golden angle" $ do
        let
          sid = discoverMin + 7
        discoverRgb sid @?= speciesColor sid
    , testCase "classifyAndResolve waits for second sighting" $ do
        let
          w = 10
          cells = [0, 1, w, w + 1]
          key = fst (collectPhaseKey (extractCoords w cells))
          first =
            classifyAndResolve Map.empty Map.empty Map.empty 100 255 w cells
          second =
            classifyAndResolve
              Map.empty
              Map.empty
              (Map.singleton key 1)
              100
              255
              w
              cells
        rrAction first @?= 0
        rrKey first @?= key
        rrAction second @?= 2
        rrSid second @?= 100
        discoverRgb 100 @?= (rrR second, rrG second, rrB second)
    , testCase "classifyAndResolve known catalog hits on first sight" $ do
        let
          w = 10
          cells = [0, 1, w, w + 1]
          key = canonicalShapeHash block
          sid = 42
          res =
            classifyAndResolve
              (Map.singleton key sid)
              Map.empty
              Map.empty
              100
              255
              w
              cells
        rrAction res @?= 1
        rrSid res @?= sid
    , withResource (findExecutable "bun") (const (pure ())) $ \getBun ->
        testGroup
          "runtime classifier parity"
          [ testCase "bun is on PATH" $ do
              m <- getBun
              case m of
                Nothing -> assertFailure "bun not found on PATH"
                Just _ -> pure ()
          , testCase "runtime collectPhaseKey key matches DiscoverCore for block" $ do
              m <- getBun
              case m of
                Nothing -> pure ()
                Just _ -> do
                  let
                    expected = phaseKey block
                  got <- evaluateEffectJSON runtimeBlockPhaseKey
                  jsonString got @?= expected
          , testCase "runtime collectPhaseKey hash count matches DiscoverCore" $ do
              m <- getBun
              case m of
                Nothing -> pure ()
                Just _ -> do
                  let
                    expected = length (phaseHashes block)
                  got <- evaluateEffectJSON runtimeBlockPhaseHashLen
                  got @?= T.pack (show expected)
          ]
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

  jsonString :: T.Text -> T.Text
  jsonString t =
    case T.uncons t of
      Just ('"', rest) ->
        case T.break (== '"') rest of
          (s, _) -> s
      _ -> t

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
