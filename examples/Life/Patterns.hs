{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Pattern catalog and species palette for the life demo.

Species 0 is random soup; 1–10 still lifes (blue hues), 11–20 oscillators
(green), 21–30 spaceships (warm). Each pattern type gets its own id so
hue shifts within the category.
-}
module Patterns
  ( allPatterns
  , initialAlive
  , initialSpecies
  , initialPop
  , paletteBytes
  )
where

import Control.Monad (replicateM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.Byte (ByteArray (..))
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word8)
import GHC.Exts (Int (I#), newByteArray#, unsafeFreezeByteArray#, writeWord8Array#, (+#))
import GHC.ST (ST (..))
import GHC.Word (Word8 (W8#))
import Types (gridH, gridN, gridW, manualSpecies)

data PatternSpec = PatternSpec
  { patId :: Int
  , patCount :: Int
  , patCells :: [(Int, Int)]
  }

allPatterns :: [PatternSpec]
allPatterns =
  stillLifes ++ oscillators ++ spaceships
 where
  stillLifes =
    [ pat 1 35 block
    , pat 2 35 beehive
    , pat 3 30 loaf
    , pat 4 30 boat
    , pat 5 30 tub
    , pat 6 25 pond
    , pat 7 25 ship
    , pat 8 20 longBoat
    , pat 9 15 mango
    , pat 10 15 hat
    ]
  oscillators =
    [ pat 11 40 blinker
    , pat 12 35 toad
    , pat 13 30 beacon
    , pat 14 10 pulsar
    , pat 15 8 pentadecathlon
    , pat 16 6 queenBee
    , pat 17 25 figureEight
    , pat 18 20 sparkles
    , pat 19 15 unix
    , pat 20 15 tumbler
    ]
  spaceships =
    [ pat 21 50 glider
    , pat 22 25 lwss
    , pat 23 15 mwss
    , pat 24 10 hwss
    , pat 25 40 gliderAlt
    , pat 26 20 lwssAlt
    ]

pat :: Int -> Int -> [(Int, Int)] -> PatternSpec
pat i n cells = PatternSpec i n cells

-- Still lifes ---------------------------------------------------------------

block :: [(Int, Int)]
block = [(0, 0), (1, 0), (0, 1), (1, 1)]

beehive :: [(Int, Int)]
beehive = [(1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (2, 2)]

loaf :: [(Int, Int)]
loaf = [(1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (3, 2), (2, 3)]

boat :: [(Int, Int)]
boat = [(0, 0), (1, 0), (0, 1), (2, 1), (1, 2)]

tub :: [(Int, Int)]
tub = [(1, 0), (0, 1), (2, 1), (1, 2)]

pond :: [(Int, Int)]
pond = [(1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (2, 2), (2, 3)]

ship :: [(Int, Int)]
ship = [(0, 0), (1, 0), (0, 1), (2, 1), (1, 2), (2, 2)]

longBoat :: [(Int, Int)]
longBoat = [(0, 0), (1, 0), (0, 1), (3, 1), (1, 2), (2, 2), (3, 2)]

mango :: [(Int, Int)]
mango = [(1, 0), (2, 0), (3, 0), (0, 1), (4, 1), (0, 2), (4, 2), (1, 3), (2, 3), (3, 3)]

hat :: [(Int, Int)]
hat = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (3, 1), (0, 2), (3, 2)]

-- Oscillators ---------------------------------------------------------------

blinker :: [(Int, Int)]
blinker = [(0, 0), (1, 0), (2, 0)]

toad :: [(Int, Int)]
toad = [(1, 0), (2, 0), (3, 0), (0, 1), (1, 1), (2, 1)]

beacon :: [(Int, Int)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

pulsar :: [(Int, Int)]
pulsar =
  [ (2, 0)
  , (3, 0)
  , (4, 0)
  , (8, 0)
  , (9, 0)
  , (10, 0)
  , (0, 2)
  , (5, 2)
  , (7, 2)
  , (12, 2)
  , (0, 3)
  , (5, 3)
  , (7, 3)
  , (12, 3)
  , (0, 4)
  , (5, 4)
  , (7, 4)
  , (12, 4)
  , (2, 5)
  , (3, 5)
  , (4, 5)
  , (8, 5)
  , (9, 5)
  , (10, 5)
  , (2, 7)
  , (3, 7)
  , (4, 7)
  , (8, 7)
  , (9, 7)
  , (10, 7)
  , (0, 8)
  , (5, 8)
  , (7, 8)
  , (12, 8)
  , (0, 9)
  , (5, 9)
  , (7, 9)
  , (12, 9)
  , (0, 10)
  , (5, 10)
  , (7, 10)
  , (12, 10)
  , (2, 12)
  , (3, 12)
  , (4, 12)
  , (8, 12)
  , (9, 12)
  , (10, 12)
  ]

pentadecathlon :: [(Int, Int)]
pentadecathlon =
  [ (1, 0)
  , (2, 0)
  , (3, 0)
  , (4, 0)
  , (5, 0)
  , (6, 0)
  , (7, 0)
  , (8, 0)
  , (9, 0)
  , (0, 1)
  , (3, 1)
  , (10, 1)
  , (0, 2)
  , (5, 2)
  , (7, 2)
  , (10, 2)
  , (0, 3)
  , (5, 3)
  , (7, 3)
  , (10, 3)
  , (3, 4)
  , (4, 4)
  , (5, 4)
  , (6, 4)
  , (7, 4)
  ]

queenBee :: [(Int, Int)]
queenBee =
  [ (0, 0)
  , (1, 0)
  , (2, 0)
  , (3, 0)
  , (4, 0)
  , (5, 0)
  , (6, 0)
  , (0, 1)
  , (2, 1)
  , (5, 1)
  , (6, 1)
  , (0, 2)
  , (6, 2)
  , (0, 3)
  , (1, 3)
  , (2, 3)
  , (3, 3)
  , (4, 3)
  , (5, 3)
  , (6, 3)
  ]

figureEight :: [(Int, Int)]
figureEight = [(1, 0), (2, 0), (0, 1), (3, 1), (0, 2), (3, 2), (1, 3), (2, 3)]

sparkles :: [(Int, Int)]
sparkles = [(1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (2, 2), (1, 3), (2, 3)]

unix :: [(Int, Int)]
unix = [(0, 0), (1, 0), (1, 1), (2, 1), (1, 2), (2, 2), (2, 3)]

tumbler :: [(Int, Int)]
tumbler = [(1, 0), (2, 0), (0, 1), (3, 1), (0, 2), (1, 2), (2, 2), (3, 2), (1, 3), (2, 3)]

-- Spaceships ----------------------------------------------------------------

glider :: [(Int, Int)]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

gliderAlt :: [(Int, Int)]
gliderAlt = [(0, 0), (1, 0), (2, 0), (0, 1), (1, 2)]

lwss :: [(Int, Int)]
lwss = [(1, 0), (4, 0), (0, 1), (0, 2), (4, 2), (0, 3), (1, 3), (2, 3), (3, 3)]

lwssAlt :: [(Int, Int)]
lwssAlt = [(0, 0), (3, 0), (4, 1), (0, 2), (4, 2), (0, 3), (1, 3), (2, 3), (3, 3)]

mwss :: [(Int, Int)]
mwss =
  [ (2, 0)
  , (0, 1)
  , (0, 2)
  , (1, 2)
  , (2, 2)
  , (3, 2)
  , (4, 2)
  , (0, 3)
  , (4, 3)
  , (1, 4)
  , (2, 4)
  , (3, 4)
  ]

hwss :: [(Int, Int)]
hwss =
  [ (3, 0)
  , (4, 0)
  , (0, 1)
  , (0, 2)
  , (1, 2)
  , (2, 2)
  , (3, 2)
  , (4, 2)
  , (5, 2)
  , (0, 3)
  , (5, 3)
  , (1, 4)
  , (2, 4)
  , (3, 4)
  , (4, 4)
  ]

-- Host-built initial grid and flat RGB palette -----------------------------

initialAlive, initialSpecies :: ByteArray
initialPop :: Int
(initialAlive, initialSpecies, initialPop) = buildInitialGrid

buildInitialGrid :: (ByteArray, ByteArray, Int)
buildInitialGrid = runST $ do
  alive <- newGrid 0
  species <- newGrid 0
  popRef <- newSTRef (0 :: Int)
  rngRef <- newSTRef (42 :: Int)
  mapM_ (seedCell alive species popRef rngRef) [0 .. gridN - 1]
  mapM_ (stampPatterns alive species popRef rngRef) allPatterns
  pop <- readSTRef popRef
  aliveBA <- freezeGrid alive
  speciesBA <- freezeGrid species
  pure (aliveBA, speciesBA, pop)

type Grid s = STUArray s Int Word8

newGrid :: Word8 -> ST s (Grid s)
newGrid v = newArray (0, gridN - 1) v

freezeGrid :: Grid s -> ST s ByteArray
freezeGrid arr = packBytes <$> mapM (readArray arr) [0 .. gridN - 1]

seedCell ::
  Grid s
  -> Grid s
  -> STRef s Int
  -> STRef s Int
  -> Int
  -> ST s ()
seedCell alive species popRef rngRef i = do
  rng <- readSTRef rngRef
  let
    (rng', v) = lcg01 rng
  writeSTRef rngRef rng'
  when (v < 0.20) $ do
    writeArray alive i 1
    writeArray species i 0
    modifySTRef popRef (+ 1)

stampPatterns ::
  Grid s
  -> Grid s
  -> STRef s Int
  -> STRef s Int
  -> PatternSpec
  -> ST s ()
stampPatterns alive species popRef rngRef p =
  replicateM_ (patCount p) (stampOne alive species popRef rngRef p)

stampOne ::
  Grid s
  -> Grid s
  -> STRef s Int
  -> STRef s Int
  -> PatternSpec
  -> ST s ()
stampOne alive species popRef rngRef p = do
  rng <- readSTRef rngRef
  let
    (ox, rng1) = lcgRange rng gridW
    (oy, rng2) = lcgRange rng1 gridH
  writeSTRef rngRef rng2
  mapM_
    ( \(dx, dy) ->
        let
          x = wrapCoord (ox + dx) gridW
          y = wrapCoord (oy + dy) gridH
          i = y * gridW + x
          sid = fromIntegral (patId p)
         in
          stampCell alive species popRef i sid
    )
    (patCells p)

stampCell :: Grid s -> Grid s -> STRef s Int -> Int -> Word8 -> ST s ()
stampCell alive species popRef i sid = do
  wasAlive <- readArray alive i
  writeArray alive i 1
  writeArray species i sid
  when (wasAlive == 0) (modifySTRef popRef (+ 1))

bytes :: [Word8] -> ByteArray
bytes xs = runST go
 where
  !(I# n#) = length xs
  go :: ST s ByteArray
  go = ST $ \s0 ->
    case newByteArray# n# s0 of
      (# s1, mba #) ->
        case write 0# xs mba s1 of
          s2 -> case unsafeFreezeByteArray# mba s2 of
            (# s3, ba #) -> (# s3, ByteArray ba #)
  write _ [] _ s = s
  write i# (W8# w : rest) mba s =
    write (i# +# 1#) rest mba (writeWord8Array# mba i# w s)

wrapCoord :: Int -> Int -> Int
wrapCoord c n = (c `mod` n + n) `mod` n

lcg01 :: Int -> (Int, Double)
lcg01 s =
  let
    s' = (1103515245 * s + 12345) `mod` 0x7fffffff
   in
    (s', fromIntegral s' / fromIntegral (0x7fffffff :: Int))

lcgRange :: Int -> Int -> (Int, Int)
lcgRange s n =
  let
    (s', v) = lcg01 s
   in
    (s', floor (v * fromIntegral n))

paletteBytes :: ByteArray
paletteBytes =
  packBytes
    [ w
    | i <- [0 .. 255]
    , (r, g, b) <- [speciesColor i]
    , w <- [fromIntegral r, fromIntegral g, fromIntegral b]
    ]

speciesColor :: Int -> (Int, Int, Int)
speciesColor 0 = (72, 72, 88)
speciesColor n
  | n == manualSpecies = hslRgb 280 0.45 0.68
  | n >= 1 && n <= 10 = hslRgb (200 + fromIntegral (n - 1) * 4) 0.62 0.58
  | n >= 11 && n <= 20 = hslRgb (95 + fromIntegral (n - 11) * 5) 0.58 0.52
  | n >= 21 && n <= 30 = hslRgb (12 + fromIntegral (n - 21) * 4) 0.72 0.55
  | otherwise = (72, 72, 88)

packBytes :: [Word8] -> ByteArray
packBytes = bytes

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
    clamp n = max 0 (min 255 (round (255 * (n + m))))
   in
    (clamp r1, clamp g1, clamp b1)
