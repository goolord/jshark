{-# LANGUAGE OverloadedStrings #-}

-- | Pure species shape / phase keys and classify.
module JShark.Example.Life.DiscoverCore
  ( extractCoords
  , collectPhaseKey
  , classifyAndResolve
  , ResolveResult (..)
  )
where

import Control.Applicative (asum)
import Control.Monad (mfilter)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Example.Life.Catalog (canonicalShapeHash, shapeHash)
import JShark.Example.Life.Palette (speciesColor)
import JShark.Example.Life.Types (methuselahMax, methuselahMin)

data ResolveResult = ResolveResult
  { rrAction :: Int
  , rrSid :: Int
  , rrR :: Int
  , rrG :: Int
  , rrB :: Int
  , rrKey :: Text
  }
  deriving (Eq, Show)

extractCoords :: Int -> [Int] -> [(Int, Int)]
extractCoords w cells = [(i `mod` w, i `div` w) | i <- cells]

data StopReason = Cycle | Drift | Extinct | MaxSteps

phaseStructured :: StopReason -> Int -> Bool
phaseStructured Cycle _ = True
phaseStructured Drift hLen = hLen >= 2 && hLen <= 8
phaseStructured Extinct _ = False
phaseStructured MaxSteps _ = False

collectPhaseKey :: [(Int, Int)] -> (Text, [Text])
collectPhaseKey [] = ("", [])
collectPhaseKey coords = go (stamp coords) Set.empty [] (0 :: Int)
 where
  (minX, minY, maxX, maxY) = bounds coords
  pad = 2
  ox = minX - pad
  oy = minY - pad
  gw = maxX - minX + 1 + 2 * pad
  gh = maxY - minY + 1 + 2 * pad
  (c0x, c0y) = centroid coords
  go grid history acc step
    | step >= 32 = finish MaxSteps acc
    | null liveLocal = finish Extinct acc
    | exact `Set.member` history = finish Cycle acc
    | step > 0 && abs (cx - c0x) + abs (cy - c0y) > (0.75 :: Double) =
        finish Drift acc'
    | otherwise = go (stepGrid grid) (Set.insert exact history) acc' (step + 1)
   where
    liveLocal =
      [(x, y) | y <- [0 .. gh - 1], x <- [0 .. gw - 1], grid !! (y * gw + x)]
    exact = shapeHash liveLocal
    acc' = exact : acc
    (cx, cy) = centroid [(x + ox, y + oy) | (x, y) <- liveLocal]

  finish reason acc
    | not (phaseStructured reason hLen) = ("", hashes)
    | hLen > 1 = (T.intercalate "|" (sort hashes), hashes)
    | otherwise = (canonicalShapeHash coords, hashes)
   where
    hashes = reverse acc
    hLen = length hashes

  bounds cs =
    ( minimum (map fst cs)
    , minimum (map snd cs)
    , maximum (map fst cs)
    , maximum (map snd cs)
    )

  stamp =
    foldr
      (\(x, y) g -> setCell g (x - ox) (y - oy))
      (replicate (gw * gh) False)

  setCell g x y = let i = y * gw + x in take i g ++ [True] ++ drop (i + 1) g

  stepGrid grid = [alive x y | y <- [0 .. gh - 1], x <- [0 .. gw - 1]]
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
       in
        n == (3 :: Int) || (grid !! (y * gw + x) && n == 2)
    count nx ny = if grid !! (ny * gw + nx) then 1 else 0

  centroid cs =
    let
      n = fromIntegral (length cs) :: Double
      (sx, sy) =
        foldr
          (\(x, y) (a, b) -> (a + fromIntegral x, b + fromIntegral y))
          (0, 0)
          cs
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
classifyAndResolve known seen pending nextId maxSid w cells =
  fromMaybe minted . asum $
    [ hit key <$> catalogHit key
    , hit snap <$> catalogHit snap
    , hit key <$> mfilter (not . isMethuselah) (lookupHash known)
    , hit key <$> Map.lookup key seen
    , hit snap <$> Map.lookup snap seen
    , hit key <$> lookupHash seen
    ]
 where
  coords = extractCoords w cells
  (phaseKey, hashes) = collectPhaseKey coords
  snap = canonicalShapeHash coords
  key = if T.null phaseKey then snap else phaseKey
  hit k sid = ResolveResult 1 sid 0 0 0 k
  isMethuselah sid = sid >= methuselahMin && sid <= methuselahMax
  catalogHit k = mfilter (not . isMethuselah) (Map.lookup k known)
  lookupHash m = asum [Map.lookup h m | h <- hashes]
  minted
    | T.null phaseKey = ResolveResult 0 0 0 0 0 ""
    | Map.findWithDefault 0 key pending + 1 < 2 = ResolveResult 0 0 0 0 0 key
    | nextId > maxSid = ResolveResult 3 0 0 0 0 key
    | otherwise =
        let (r, g, b) = speciesColor nextId in ResolveResult 2 nextId r g b key
