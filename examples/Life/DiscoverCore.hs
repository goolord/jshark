{-# LANGUAGE OverloadedStrings #-}

-- | Pure reference for species shape / phase keys (mirrors former Discover.js).
module DiscoverCore
  ( discoverRgb
  , extractCoords
  , collectPhaseKey
  , classifyAndResolve
  , ResolveResult (..)
  )
where

import Catalog (canonicalShapeHash, shapeHash)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

data ResolveResult = ResolveResult
  { rrAction :: Int
  , rrSid :: Int
  , rrR :: Int
  , rrG :: Int
  , rrB :: Int
  , rrKey :: Text
  }
  deriving (Eq, Show)

defaultResolve :: ResolveResult
defaultResolve = ResolveResult 0 0 0 0 0 ""

discoverRgb :: Int -> (Int, Int, Int)
discoverRgb n =
  let
    hue = fromIntegral ((n * 137508) `mod` 360000) / (1000 :: Double)
    s = 0.62 :: Double
    l = 0.41 :: Double
    c = (1 - abs (2 * l - 1)) * s
    hp = hue / 60
    hpMod = hp - 2 * fromIntegral (floor (hp / 2) :: Int)
    x = c * (1 - abs (hpMod - 1))
    m = l - c / 2
    (r1, g1, b1) =
      if hp < 1
        then (c, x, 0)
        else
          if hp < 2
            then (x, c, 0)
            else
              if hp < 3
                then (0, c, x)
                else
                  if hp < 4
                    then (0, x, c)
                    else
                      if hp < 5
                        then (x, 0, c)
                        else (c, 0, x)
    clamp t =
      max 0 (min 255 (round (255 * (t + m) :: Double) :: Integer))
   in
    (fromIntegral (clamp r1), fromIntegral (clamp g1), fromIntegral (clamp b1))

extractCoords :: Int -> [Int] -> [(Int, Int)]
extractCoords w cells =
  [ (i `mod` w, i `div` w)
  | i <- cells
  ]

collectPhaseKey :: [(Int, Int)] -> (Text, [Text])
collectPhaseKey [] = ("", [])
collectPhaseKey coords =
  let
    (minX, minY, maxX, maxY) = bounds coords
    pad = 2
    ox = minX - pad
    oy = minY - pad
    gw = maxX - minX + 1 + 2 * pad
    gh = maxY - minY + 1 + 2 * pad
    grid0 = stamp coords ox oy gw gh
    (c0x, c0y) = centroid coords
    (key, hashes) = go grid0 gw gh ox oy coords c0x c0y Set.empty [] (0 :: Int)
   in
    (key, hashes)
 where
  go ::
    [Bool]
    -> Int
    -> Int
    -> Int
    -> Int
    -> [(Int, Int)]
    -> Double
    -> Double
    -> Set.Set Text
    -> [Text]
    -> Int
    -> (Text, [Text])
  go grid gw gh ox oy origCoords c0x c0y history acc step
    | step >= 32 = finish acc origCoords
    | otherwise =
        let
          liveLocal =
            [ (x, y)
            | y <- [0 .. gh - 1]
            , x <- [0 .. gw - 1]
            , grid !! (y * gw + x)
            ]
         in
          if null liveLocal
            then finish acc origCoords
            else
              let
                exact = shapeHash liveLocal
               in
                if exact `Set.member` history
                  then finish acc origCoords
                  else
                    let
                      history' = Set.insert exact history
                      acc' = exact : acc
                     in
                      if step > 0
                        then
                          let
                            absCoords = [(x + ox, y + oy) | (x, y) <- liveLocal]
                            (cx, cy) = centroid absCoords
                           in
                            if abs (cx - c0x) + abs (cy - c0y) > (0.75 :: Double)
                              then finish acc' origCoords
                              else
                                go
                                  (stepGrid grid gw gh)
                                  gw
                                  gh
                                  ox
                                  oy
                                  origCoords
                                  c0x
                                  c0y
                                  history'
                                  acc'
                                  (step + 1)
                        else
                          go
                            (stepGrid grid gw gh)
                            gw
                            gh
                            ox
                            oy
                            origCoords
                            c0x
                            c0y
                            history'
                            acc'
                            (step + 1)

  finish acc originCells =
    let
      hashes = reverse acc
     in
      if length hashes > 1
        then (T.intercalate "|" (sort hashes), hashes)
        else (canonicalShapeHash originCells, hashes)

  bounds cs =
    ( minimum (map fst cs)
    , minimum (map snd cs)
    , maximum (map fst cs)
    , maximum (map snd cs)
    )

  stamp cs ox oy gw gh =
    foldr
      (\(x, y) g -> setCell g gw (x - ox) (y - oy))
      (replicate (gw * gh) False)
      cs

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
        n == (3 :: Int) || (grid !! i && n == 2)
    count nx ny =
      if grid !! (ny * gw + nx) then 1 else 0

  centroid cs =
    let
      n = fromIntegral (length cs) :: Double
      (sx, sy) = foldr (\(x, y) (a, b) -> (a + fromIntegral x, b + fromIntegral y)) (0, 0) cs
     in
      (sx / n, sy / n)

classifyAndResolve ::
  Map Text Int
  -> Map Text Int
  -> Map Text Int
  -> Int
  -> Int
  -> Int
  -> [Int]
  -> ResolveResult
classifyAndResolve known seen0 pending0 _nextId maxSid w cells =
  let
    coords = extractCoords w cells
    (key, hashes) = collectPhaseKey coords
   in
    if T.null key
      then defaultResolve
      else
        resolveSpecies' known seen0 pending0 _nextId maxSid key hashes
 where
  resolveSpecies' knownMap seen pending nextId maxSidLimit key hashes =
    case Map.lookup key knownMap of
      Just sid ->
        ResolveResult 1 sid 0 0 0 key
      Nothing ->
        case Map.lookup key seen of
          Just sid ->
            ResolveResult 1 sid 0 0 0 key
          Nothing ->
            case lookupHash seen hashes of
              Just sid ->
                ResolveResult 1 sid 0 0 0 key
              Nothing ->
                let
                  cnt = Map.findWithDefault 0 key pending + 1
                 in
                  if cnt < 2
                    then defaultResolve {rrKey = key}
                    else
                      if nextId > maxSidLimit
                        then defaultResolve {rrKey = key}
                        else
                          let
                            (r, g, b) = discoverRgb nextId
                           in
                            ResolveResult 2 nextId r g b key

  lookupHash _ [] = Nothing
  lookupHash seen (h : hs) =
    case Map.lookup h seen of
      Just sid -> Just sid
      Nothing -> lookupHash seen hs
