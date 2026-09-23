{-# LANGUAGE OverloadedStrings #-}

-- | Pattern catalog for the life demo. Palette lives in 'Palette'.
--
-- Species @0@ soup; @1–24@ still lifes; @25–44@ oscillators; @45–59@ spaceships;
-- @60–69@ methuselah seeds; @70–79@ eaters; @80–89@ misc; @90@ manual;
-- @91–1023@ runtime discoveries.
module JShark.Example.Life.Patterns
  ( PatternSpec (..)
  , allPatterns
  , disturbPatterns
  , initialCatalogCells
  , initialPop
  , soupSeedPop
  , initialBoundX0
  , initialBoundY0
  , initialBoundX1
  , initialBoundY1
  , paletteBytes
  , speciesColor
  , glider
  , gliderOrientationCells
  , gliderSpeciesSid
  )
where

import Control.Monad (forM_, replicateM_, when)
import Data.Array.Byte (ByteArray)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Exts (fromList)
import GHC.ST (ST, runST)
import JShark.Example.Life.Palette (speciesColor)
import JShark.Example.Life.Types

data PatternSpec = PatternSpec
  { patId :: Int
  , patCount :: Int
  , patName :: Text
  , patCells :: [(Int, Int)]
  }

allPatterns :: [PatternSpec]
allPatterns =
  stillLifes ++ oscillators ++ spaceships ++ methuselahs ++ eaters ++ misc
 where
  stillLifes =
    [ pat 1 30 "Block" block
    , pat 2 30 "Beehive" beehive
    , pat 3 28 "Loaf" loaf
    , pat 4 28 "Boat" boat
    , pat 5 28 "Tub" tub
    , pat 6 24 "Pond" pond
    , pat 7 24 "Ship" ship
    , pat 8 20 "Long Boat" longBoat
    , pat 9 18 "Mango" mango
    , pat 10 18 "Hat" hat
    , pat 11 16 "Shillelagh" shillelagh
    , pat 12 16 "Dock" dock
    , pat 13 16 "Barge" barge
    , pat 14 14 "Long Snake" longSnake
    , pat 15 14 "Cis Hook" cisHook
    , pat 16 14 "Elevator" elevator
    , pat 17 12 "Paperclip" paperclip
    , pat 18 12 "Table On Table" tableOnTable
    , pat 19 12 "Integral Sign" integralSign
    , pat 20 12 "Hook" hook
    , pat 21 10 "Canoe" canoe
    , pat 22 10 "Aircraft Carrier" aircraftCarrier
    , pat 23 10 "Trans Barge" transBarge
    , pat 24 10 "Cis Fuse" cisFuse
    ]
  oscillators =
    [ pat 25 36 "Blinker" blinker
    , pat 26 32 "Toad" toad
    , pat 27 28 "Beacon" beacon
    , pat 28 8 "Pulsar" pulsar
    , pat 29 6 "Pentadecathlon" pentadecathlon
    , pat 30 6 "Queen Bee" queenBee
    , pat 31 22 "Figure Eight" figureEight
    , pat 32 18 "Sparkles" sparkles
    , pat 33 16 "Unix" unix
    , pat 34 16 "Tumbler" tumbler
    , pat 35 14 "Tripole" tripole
    , pat 36 12 "By Flops" byFlops
    , pat 37 10 "Mold" mold
    , pat 38 10 "Clock" clock
    , pat 39 8 "Quadpole" quadpole
    , pat 40 8 "Butterfly" butterfly
    , pat 41 8 "Traffic Circle" trafficCircle
    , pat 42 6 "Pentant" pentant
    , pat 43 6 "Crossroads" crossroads
    , pat 44 6 "Pinwheel" pinwheel
    ]
  spaceships =
    [ pat 45 44 "Glider" glider
    , pat 46 22 "LWSS" lwss
    , pat 47 14 "MWSS" mwss
    , pat 48 10 "HWSS" hwss
    , pat 49 36 "Glider Alt" gliderAlt
    , pat 50 18 "LWSS Alt" lwssAlt
    , pat 51 12 "Glider Perp" gliderPerp
    , pat 52 10 "LWSS Perp" lwssPerp
    , pat 53 8 "MWSS Alt" mwssAlt
    , pat 54 8 "Dart" dart
    , pat 55 8 "Crab" crabCanonical
    , pat 56 6 "Loafer" loaferSmall
    , pat 57 6 "Glider Up" gliderUp
    , pat 58 6 "Glider Down" gliderDown
    , pat 59 6 "Glider Left" gliderLeft
    ]
  methuselahs =
    [ pat 60 20 "R-Pentomino" rPentomino
    , pat 61 16 "Acorn" acorn
    , pat 62 14 "Diehard" diehard
    , pat 63 12 "Bunnies" bunnies
    , pat 64 10 "S-Diehard" sDiehard
    , pat 65 10 "B-Heptomino" bHeptomino
    , pat 66 8 "Pi-Heptomino" piHeptomino
    , pat 67 8 "R-Acorn" rAcorn
    , pat 68 6 "Switch Engine" switchEngine
    , pat 69 6 "Block On Table" blockOnTable
    ]
  eaters =
    [ pat 70 18 "Eater" eater
    , pat 71 14 "Eater 2" eater2
    , pat 72 12 "Eater 3" eater3
    , pat 73 12 "Block On Snake" blockOnSnake
    , pat 74 10 "Tub With Tail" tubWithTail
    , pat 75 10 "Long Hook With Tail" longHookWithTail
    , pat 76 8 "Snake Bridge" snakeBridge
    , pat 77 8 "Mirrored Eater" mirroredEater
    , pat 78 6 "Pre-Block" preBlock
    , pat 79 6 "Pre-Beehive" preBeehive
    ]
  misc =
    [ pat 80 16 "Traffic Light" trafficLight
    , pat 81 14 "Honey Farm" honeyFarm
    , pat 82 12 "Farm" farm
    , pat 83 12 "Long Boat Tie" longBoatTie
    , pat 84 10 "Cis Long Hook" cisLongHook
    , pat 85 10 "Trans Long Hook" transLongHook
    , pat 86 8 "Very Long Boat" veryLongBoat
    , pat 87 8 "Cis Boat" cisBoat
    , pat 88 6 "Trans Boat" transBoat
    , pat 89 6 "Cis Block" cisBlock
    ]

pat :: Int -> Int -> Text -> [(Int, Int)] -> PatternSpec
pat = PatternSpec

-- | Live cells of a picture, row-major; @O@ is alive.
art :: [String] -> [(Int, Int)]
art rows = [(x, y) | (y, row) <- zip [0 ..] rows, (x, 'O') <- zip [0 ..] row]

-- | Placement tools after Mouse / Glider / Eraser. HUD order: xWSS,
--   classic seeds, then eater.
disturbSids :: [Int]
disturbSids = [46, 47, 48, 60, 61, 62, 63, 68, 70]

disturbPatterns :: [PatternSpec]
disturbPatterns =
  mapMaybe (\sid -> find ((== sid) . patId) allPatterns) disturbSids

-- Still lifes ---------------------------------------------------------------

block, beehive, loaf, boat, tub, pond, ship, longBoat :: [(Int, Int)]
block = [(0, 0), (1, 0), (0, 1), (1, 1)]
beehive = [(1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (2, 2)]
loaf = [(1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (3, 2), (2, 3)]
boat = [(0, 0), (1, 0), (0, 1), (2, 1), (1, 2)]
tub = [(1, 0), (0, 1), (2, 1), (1, 2)]
pond = [(1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (2, 2), (2, 3)]
ship = [(0, 0), (1, 0), (0, 1), (2, 1), (1, 2), (2, 2)]
longBoat = [(0, 0), (1, 0), (0, 1), (3, 1), (1, 2), (2, 2), (3, 2)]

mango :: [(Int, Int)]
mango =
  [(1, 0), (2, 0), (3, 0), (0, 1), (4, 1), (0, 2), (4, 2), (1, 3), (2, 3), (3, 3)]

hat :: [(Int, Int)]
hat = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (3, 1), (0, 2), (3, 2)]

-- Oscillators ---------------------------------------------------------------

blinker, toad, beacon :: [(Int, Int)]
blinker = [(0, 0), (1, 0), (2, 0)]
toad = [(1, 0), (2, 0), (3, 0), (0, 1), (1, 1), (2, 1)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

pulsar :: [(Int, Int)]
pulsar =
  art
    [ "..OOO...OOO.."
    , "............."
    , "O....O.O....O"
    , "O....O.O....O"
    , "O....O.O....O"
    , "..OOO...OOO.."
    , "............."
    , "..OOO...OOO.."
    , "O....O.O....O"
    , "O....O.O....O"
    , "O....O.O....O"
    , "............."
    , "..OOO...OOO.."
    ]

pentadecathlon :: [(Int, Int)]
pentadecathlon =
  art
    [ ".OOOOOOOOO."
    , "O..O......O"
    , "O....O.O..O"
    , "O....O.O..O"
    , "...OOOOO..."
    ]

queenBee :: [(Int, Int)]
queenBee =
  art
    [ "OOOOOOO"
    , "O.O..OO"
    , "O.....O"
    , "OOOOOOO"
    ]

figureEight, sparkles, unix :: [(Int, Int)]
figureEight = [(1, 0), (2, 0), (0, 1), (3, 1), (0, 2), (3, 2), (1, 3), (2, 3)]
sparkles = [(1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (2, 2), (1, 3), (2, 3)]
unix = [(0, 0), (1, 0), (1, 1), (2, 1), (1, 2), (2, 2), (2, 3)]

tumbler :: [(Int, Int)]
tumbler =
  [(1, 0), (2, 0), (0, 1), (3, 1), (0, 2), (1, 2), (2, 2), (3, 2), (1, 3), (2, 3)]

-- Spaceships ----------------------------------------------------------------

glider :: [(Int, Int)]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

-- | SE, NE, NW, SW — index matches drag-quadrant aim in the Glider tool.
gliderOrientationCells :: [[(Int, Int)]]
gliderOrientationCells = [glider, gliderUp, gliderLeft, gliderDown]

gliderSpeciesSid :: Int
gliderSpeciesSid = 45

gliderAlt, lwss, lwssAlt :: [(Int, Int)]
gliderAlt = [(0, 0), (1, 0), (2, 0), (0, 1), (1, 2)]
lwss = [(1, 0), (4, 0), (0, 1), (0, 2), (4, 2), (0, 3), (1, 3), (2, 3), (3, 3)]
lwssAlt = [(0, 0), (3, 0), (4, 1), (0, 2), (4, 2), (0, 3), (1, 3), (2, 3), (3, 3)]

mwss :: [(Int, Int)]
mwss =
  art
    [ "..O.."
    , "O...."
    , "OOOOO"
    , "O...O"
    , ".OOO."
    ]

hwss :: [(Int, Int)]
hwss =
  art
    [ "...OO."
    , "O....."
    , "OOOOOO"
    , "O....O"
    , ".OOOO."
    ]

-- Extra still lifes ---------------------------------------------------------

shillelagh, dock, barge, longSnake, cisHook, elevator, paperclip :: [(Int, Int)]
shillelagh = [(0, 0), (1, 0), (2, 0), (0, 1), (1, 2)]
dock = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1)]
barge = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (1, 2)]
longSnake = [(0, 0), (0, 1), (0, 2), (0, 3), (1, 3)]
cisHook = [(0, 0), (0, 1), (1, 1), (2, 1), (2, 0)]
elevator = [(0, 0), (1, 0), (0, 1), (1, 1), (0, 2)]
paperclip = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 2), (3, 2), (3, 1)]

tableOnTable, integralSign, hook, canoe, aircraftCarrier :: [(Int, Int)]
tableOnTable = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 1)]
integralSign = [(0, 0), (1, 0), (2, 0), (0, 1), (0, 2), (1, 2), (2, 2)]
hook = [(0, 0), (0, 1), (1, 1), (2, 1), (2, 0)]
canoe = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (1, 2), (2, 2)]
aircraftCarrier = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (1, 2)]

transBarge, cisFuse :: [(Int, Int)]
transBarge = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (2, 2)]
cisFuse = [(0, 0), (1, 0), (2, 0), (3, 0), (0, 1)]

-- Extra oscillators ---------------------------------------------------------

tripole, byFlops, mold :: [(Int, Int)]
tripole = [(0, 0), (1, 0), (2, 0), (4, 0), (5, 0), (6, 0)]
byFlops = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
mold = [(1, 0), (2, 0), (0, 1), (3, 1), (0, 2), (3, 2), (1, 3), (2, 3)]

clock :: [(Int, Int)]
clock =
  [(2, 0), (5, 0), (1, 1), (0, 2), (1, 3), (2, 4), (3, 4), (4, 3), (5, 2), (4, 1)]

quadpole, butterfly, trafficCircle, pentant, crossroads :: [(Int, Int)]
quadpole = [(0, 0), (1, 0), (2, 0), (3, 0), (5, 0), (6, 0), (7, 0), (8, 0)]
butterfly = [(1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (2, 2)]
trafficCircle = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
pentant = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2), (0, 3), (2, 3)]
crossroads = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2), (0, 3), (1, 3), (2, 3)]

pinwheel :: [(Int, Int)]
pinwheel = [(1, 0), (2, 0), (0, 1), (2, 1), (0, 2), (1, 2)]

-- Extra spaceships ----------------------------------------------------------

gliderPerp, lwssPerp :: [(Int, Int)]
gliderPerp = [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
lwssPerp = [(0, 1), (0, 4), (1, 0), (2, 0), (2, 4), (3, 0), (3, 1), (3, 2), (3, 3)]

mwssAlt :: [(Int, Int)]
mwssAlt =
  [ (0, 2)
  , (1, 0)
  , (1, 1)
  , (1, 2)
  , (1, 3)
  , (1, 4)
  , (2, 0)
  , (2, 4)
  , (3, 1)
  , (3, 2)
  , (3, 3)
  ]

dart, crabCanonical, loaferSmall :: [(Int, Int)]
dart = [(0, 2), (1, 0), (1, 1), (1, 2), (2, 2), (2, 3), (3, 3)]
crabCanonical = [(0, 2), (1, 0), (1, 1), (1, 2), (2, 2), (2, 3), (3, 1), (3, 2)]
loaferSmall = [(0, 1), (1, 0), (1, 2), (2, 0), (2, 1), (2, 2)]

-- | North-east glider (canonical 'glider' rotated 90° CCW). The old
--   cells were an F-pentomino and exploded instead of translating.
gliderUp :: [(Int, Int)]
gliderUp = [(1, 0), (2, 0), (0, 1), (2, 1), (2, 2)]

-- | South-west glider (canonical 'glider' flipped horizontally). The old
--   cells were an R-pentomino.
gliderDown :: [(Int, Int)]
gliderDown = [(1, 0), (0, 1), (0, 2), (1, 2), (2, 2)]

-- | North-west glider. The previous cells were another SW phase of
--   'gliderDown', so Left and Down flew the same way.
gliderLeft :: [(Int, Int)]
gliderLeft = [(0, 0), (1, 0), (0, 1), (0, 2), (2, 1)]

-- Methuselah seeds ----------------------------------------------------------

rPentomino, acorn :: [(Int, Int)]
rPentomino = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 2)]
acorn = [(1, 0), (3, 1), (0, 2), (1, 2), (4, 2), (5, 2), (6, 2)]

-- | LifeWiki Diehard (7 cells, 8×3): 6bo$2o$bo3b2o!
diehard :: [(Int, Int)]
diehard =
  [(6, 0), (0, 1), (1, 1), (2, 1), (2, 2), (6, 2), (7, 2)]

-- | LifeWiki Bunnies (9 cells, 8×4): o5bo$2bo3bo$2bo2bobo$bobo!
bunnies, sDiehard, bHeptomino, piHeptomino, rAcorn, switchEngine :: [(Int, Int)]
bunnies = [(0, 0), (6, 0), (2, 1), (6, 1), (2, 2), (5, 2), (7, 2), (1, 3), (3, 3)]
sDiehard = [(0, 0), (1, 0), (2, 0), (0, 1), (1, 2), (2, 2), (3, 2)]
bHeptomino = [(0, 0), (1, 0), (0, 1), (1, 1), (2, 1), (0, 2), (1, 2)]
piHeptomino = [(0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
rAcorn = [(2, 0), (0, 1), (1, 1), (2, 1), (3, 1), (4, 1), (5, 1)]
switchEngine = [(0, 0), (1, 0), (0, 1), (1, 1), (2, 1), (3, 1)]

blockOnTable :: [(Int, Int)]
blockOnTable = [(0, 0), (1, 0), (0, 1), (1, 1), (0, 2), (1, 2), (2, 2)]

-- Eaters --------------------------------------------------------------------

eater, eater2, eater3, blockOnSnake, tubWithTail :: [(Int, Int)]
eater = [(0, 0), (1, 0), (0, 1), (0, 2), (1, 2), (2, 2), (2, 1)]
eater2 = [(0, 0), (1, 0), (2, 0), (0, 1), (0, 2), (1, 2), (2, 1)]
eater3 = [(0, 0), (1, 0), (0, 1), (0, 2), (1, 2), (2, 2), (3, 2), (3, 1)]
blockOnSnake = [(0, 0), (1, 0), (0, 1), (1, 1), (0, 2), (0, 3)]
tubWithTail = [(1, 0), (0, 1), (2, 1), (1, 2), (1, 3)]

longHookWithTail, snakeBridge, mirroredEater, preBlock :: [(Int, Int)]
longHookWithTail = [(0, 0), (0, 1), (1, 1), (2, 1), (2, 0), (2, 2)]
snakeBridge = [(0, 0), (1, 0), (0, 1), (1, 1), (2, 1), (3, 1)]
mirroredEater = [(2, 0), (1, 0), (2, 1), (2, 2), (1, 2), (0, 2), (0, 1)]
preBlock = [(0, 0), (1, 0), (0, 1)]

preBeehive :: [(Int, Int)]
preBeehive = [(1, 0), (2, 0), (0, 1), (3, 1)]

-- Misc ----------------------------------------------------------------------

trafficLight, honeyFarm, farm, longBoatTie, cisLongHook :: [(Int, Int)]
trafficLight = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
honeyFarm = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (0, 2), (1, 2), (2, 2)]
farm = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (0, 2), (1, 2)]
longBoatTie = [(0, 0), (1, 0), (0, 1), (3, 1), (1, 2), (2, 2), (3, 2)]
cisLongHook = [(0, 0), (0, 1), (1, 1), (2, 1), (2, 0), (2, 2)]

transLongHook, veryLongBoat, cisBoat, transBoat, cisBlock :: [(Int, Int)]
transLongHook = [(0, 0), (0, 1), (1, 1), (2, 1), (2, 0), (0, 2)]
veryLongBoat = [(0, 0), (1, 0), (0, 1), (4, 1), (1, 2), (2, 2), (3, 2), (4, 2)]
cisBoat = [(0, 0), (1, 0), (0, 1), (2, 1), (1, 2)]
transBoat = [(0, 0), (1, 0), (0, 1), (2, 1), (2, 2)]
cisBlock = [(0, 0), (1, 0), (0, 1), (2, 1)]

-- Host-built initial grid and flat RGB palette -----------------------------

initialCatalogCells :: [(Int, Word8)]
initialPop, soupSeedPop :: Int
initialBoundX0, initialBoundY0, initialBoundX1, initialBoundY1 :: Int
( initialCatalogCells
  , initialPop
  , soupSeedPop
  , initialBoundX0
  , initialBoundY0
  , initialBoundX1
  , initialBoundY1
  ) =
    buildInitialGrid

buildInitialGrid :: ([(Int, Word8)], Int, Int, Int, Int, Int, Int)
buildInitialGrid = runST $ do
  alive <- newGrid 0
  speciesGrid <- newGrid 0
  popRef <- newSTRef (0 :: Int)
  boundsRef <- newSTRef (gridW, gridH, -1, -1)
  rngRef <- newSTRef soupRngSeed
  let
    seedCell i = do
      rng <- readSTRef rngRef
      let
        (rng', v) = lcg01 rng
      writeSTRef rngRef rng'
      when (v < soupDensity) $ do
        writeArray alive i 1
        writeArray speciesGrid i 0
        modifySTRef popRef (+ 1)
        touchBounds boundsRef (i `mod` gridW) (i `div` gridW)
    stampOne p = do
      rng <- readSTRef rngRef
      let
        (rng1, ox) = lcgRange rng seedW
        (rng2, oy) = lcgRange rng1 seedH
      writeSTRef rngRef rng2
      forM_ (patCells p) $ \(dx, dy) -> do
        let
          x = seedOx + ox + dx
          y = seedOy + oy + dy
          i = y * gridW + x
        when (inGrid x y) $ do
          wasAlive <- readArray alive i
          writeArray alive i 1
          writeArray speciesGrid i (fromIntegral (patId p))
          when (wasAlive == 0) (modifySTRef popRef (+ 1))
          touchBounds boundsRef x y
  forM_ [seedOy .. seedOy + seedH - 1] $ \y ->
    forM_ [seedOx .. seedOx + seedW - 1] $ \x -> seedCell (y * gridW + x)
  soupPop <- readSTRef popRef
  forM_ allPatterns $ \p -> replicateM_ (patCount p) (stampOne p)
  pop <- readSTRef popRef
  (bx0, by0, bx1, by1) <- readSTRef boundsRef
  let
    bounds
      | bx1 < bx0 = (seedOx, seedOy, seedOx + seedW - 1, seedOy + seedH - 1)
      | otherwise = (bx0, by0, bx1, by1)
  catalog <- collectCatalogInBounds alive speciesGrid bounds
  case bounds of
    (bx0', by0', bx1', by1') ->
      pure (catalog, pop, soupPop, bx0', by0', bx1', by1')
 where
  collectCatalogInBounds aliveArr speciesGrid (bx0, by0, bx1, by1) =
    go by0 []
   where
    go y acc
      | y > by1 = pure (reverse acc)
      | otherwise = goRow bx0 y acc
    goRow x y acc
      | x > bx1 = go (y + 1) acc
      | otherwise = do
          let
            i = y * gridW + x
          a <- readArray aliveArr i
          if a == 1
            then do
              sp <- readArray speciesGrid i
              goRow (x + 1) y (if sp /= 0 then (i, sp) : acc else acc)
            else goRow (x + 1) y acc

newGrid :: Word8 -> ST s (STUArray s Int Word8)
newGrid v = newArray (0, gridN - 1) v

inGrid :: Int -> Int -> Bool
inGrid x y = x >= 0 && y >= 0 && x < gridW && y < gridH

touchBounds :: STRef s (Int, Int, Int, Int) -> Int -> Int -> ST s ()
touchBounds ref x y = modifySTRef ref $ \(x0, y0, x1, y1) ->
  (min x0 x, min y0 y, max x1 x, max y1 y)

lcg01 :: Int -> (Int, Double)
lcg01 s =
  let
    s' = (lcgMult * s + lcgInc) `mod` lcgModulus
   in
    (s', fromIntegral s' / fromIntegral lcgModulus)

lcgRange :: Int -> Int -> (Int, Int)
lcgRange s n =
  let
    (s', v) = lcg01 s
   in
    (s', floor (v * fromIntegral n))

paletteBytes :: ByteArray
paletteBytes =
  fromList
    [ w
    | i <- [0 .. discoverMax]
    , (r, g, b) <- [speciesColor i]
    , w <- [fromIntegral r, fromIntegral g, fromIntegral b]
    ]
