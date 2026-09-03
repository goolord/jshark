-- | Species RGB table. Kept off 'Patterns' so 'DiscoverCore' can
--   color mints without pulling the catalog.
module JShark.Example.Life.Palette
  ( speciesColor
  )
where

import JShark.Example.Life.Types (manualSpecies, soupSpecies)

speciesColor :: Int -> (Int, Int, Int)
speciesColor n
  | n == soupSpecies = hslRgb 0 0 0.5
  | n == manualSpecies = hslRgb 292 0.84 0.56
  | otherwise =
      let
        hue =
          fromIntegral ((n * 137508) `mod` 360000) / 1000
        sat
          | unit01 40503 n < 0.38 =
              0.10 + 0.28 * unit01 2246822519 n
          | otherwise =
              0.50 + 0.45 * unit01 2246822519 n
        lit =
          0.30
            + 0.40 * unit01 3266489917 n
            + (1 - sat) * 0.10
       in
        hslRgb hue sat lit

unit01 :: Int -> Int -> Double
unit01 salt sid =
  fromIntegral (n `mod` 100003) / 100003
 where
  n = toInteger salt * toInteger sid + 1013904223

hslRgb :: Double -> Double -> Double -> (Int, Int, Int)
hslRgb h s l =
  let
    c = (1 - abs (2 * l - 1)) * s
    hp = h / 60
    hpMod = hp - 2 * fromIntegral (floor (hp / 2) :: Int)
    x = c * (1 - abs (hpMod - 1))
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
    m = l - c / 2
    clamp t = max 0 (min 255 (round (255 * (t + m))))
   in
    (clamp r1, clamp g1, clamp b1)
