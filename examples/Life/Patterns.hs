{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Pattern catalog and species palette for the life demo.

Species @0@ soup; @1–24@ still lifes; @25–44@ oscillators; @45–59@ spaceships;
@60–69@ methuselah seeds; @70–79@ eaters; @80–89@ misc; @90@ manual;
@91–255@ runtime discoveries.
-}
module Patterns
  ( PatternSpec (..)
  , allPatterns
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
import Types
  ( gridH
  , gridN
  , gridW
  , manualSpecies
  , discoverMin
  , eaterMax
  , eaterMin
  , methuselahMax
  , methuselahMin
  , miscMax
  , miscMin
  , oscMax
  , oscMin
  , shipMax
  , shipMin
  , stillMax
  , stillMin
  , soupSpecies
  )

data PatternSpec = PatternSpec
  { patId :: Int
  , patCount :: Int
  , patCells :: [(Int, Int)]
  }

allPatterns :: [PatternSpec]
allPatterns =
  stillLifes ++ oscillators ++ spaceships ++ methuselahs ++ eaters ++ misc
 where
  stillLifes =
    [ pat 1 30 block
    , pat 2 30 beehive
    , pat 3 28 loaf
    , pat 4 28 boat
    , pat 5 28 tub
    , pat 6 24 pond
    , pat 7 24 ship
    , pat 8 20 longBoat
    , pat 9 18 mango
    , pat 10 18 hat
    , pat 11 16 shillelagh
    , pat 12 16 dock
    , pat 13 16 barge
    , pat 14 14 longSnake
    , pat 15 14 cisHook
    , pat 16 14 elevator
    , pat 17 12 paperclip
    , pat 18 12 tableOnTable
    , pat 19 12 integralSign
    , pat 20 12 hook
    , pat 21 10 canoe
    , pat 22 10 aircraftCarrier
    , pat 23 10 transBarge
    , pat 24 10 cisFuse
    ]
  oscillators =
    [ pat 25 36 blinker
    , pat 26 32 toad
    , pat 27 28 beacon
    , pat 28 8 pulsar
    , pat 29 6 pentadecathlon
    , pat 30 6 queenBee
    , pat 31 22 figureEight
    , pat 32 18 sparkles
    , pat 33 16 unix
    , pat 34 16 tumbler
    , pat 35 14 tripole
    , pat 36 12 byFlops
    , pat 37 10 mold
    , pat 38 10 clock
    , pat 39 8 quadpole
    , pat 40 8 butterfly
    , pat 41 8 trafficCircle
    , pat 42 6 pentant
    , pat 43 6 crossroads
    , pat 44 6 pinwheel
    ]
  spaceships =
    [ pat 45 44 glider
    , pat 46 22 lwss
    , pat 47 14 mwss
    , pat 48 10 hwss
    , pat 49 36 gliderAlt
    , pat 50 18 lwssAlt
    , pat 51 12 gliderPerp
    , pat 52 10 lwssPerp
    , pat 53 8 mwssAlt
    , pat 54 8 dart
    , pat 55 8 crabCanonical
    , pat 56 6 loaferSmall
    , pat 57 6 gliderUp
    , pat 58 6 gliderDown
    , pat 59 6 gliderLeft
    ]
  methuselahs =
    [ pat 60 20 rPentomino
    , pat 61 16 acorn
    , pat 62 14 diehard
    , pat 63 12 rabbits
    , pat 64 10 sDiehard
    , pat 65 10 bHeptomino
    , pat 66 8 piHeptomino
    , pat 67 8 rAcorn
    , pat 68 6 switchEngine
    , pat 69 6 blockOnTable
    ]
  eaters =
    [ pat 70 18 eater
    , pat 71 14 eater2
    , pat 72 12 eater3
    , pat 73 12 blockOnSnake
    , pat 74 10 tubWithTail
    , pat 75 10 longHookWithTail
    , pat 76 8 snakeBridge
    , pat 77 8 mirroredEater
    , pat 78 6 preBlock
    , pat 79 6 preBeehive
    ]
  misc =
    [ pat 80 16 trafficLight
    , pat 81 14 honeyFarm
    , pat 82 12 farm
    , pat 83 12 longBoatTie
    , pat 84 10 cisLongHook
    , pat 85 10 transLongHook
    , pat 86 8 veryLongBoat
    , pat 87 8 cisBoat
    , pat 88 6 transBoat
    , pat 89 6 cisBlock
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

-- Extra still lifes ---------------------------------------------------------

shillelagh :: [(Int, Int)]
shillelagh = [(0, 0), (1, 0), (2, 0), (0, 1), (1, 2)]

dock :: [(Int, Int)]
dock = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1)]

barge :: [(Int, Int)]
barge = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (1, 2)]

longSnake :: [(Int, Int)]
longSnake = [(0, 0), (0, 1), (0, 2), (0, 3), (1, 3)]

cisHook :: [(Int, Int)]
cisHook = [(0, 0), (0, 1), (1, 1), (2, 1), (2, 0)]

elevator :: [(Int, Int)]
elevator = [(0, 0), (1, 0), (0, 1), (1, 1), (0, 2)]

paperclip :: [(Int, Int)]
paperclip = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 2), (3, 2), (3, 1)]

tableOnTable :: [(Int, Int)]
tableOnTable = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 1)]

integralSign :: [(Int, Int)]
integralSign = [(0, 0), (1, 0), (2, 0), (0, 1), (0, 2), (1, 2), (2, 2)]

hook :: [(Int, Int)]
hook = [(0, 0), (0, 1), (1, 1), (2, 1), (2, 0)]

canoe :: [(Int, Int)]
canoe = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (1, 2), (2, 2)]

aircraftCarrier :: [(Int, Int)]
aircraftCarrier = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (1, 2)]

transBarge :: [(Int, Int)]
transBarge = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (2, 2)]

cisFuse :: [(Int, Int)]
cisFuse = [(0, 0), (1, 0), (2, 0), (3, 0), (0, 1)]

-- Extra oscillators ---------------------------------------------------------

tripole :: [(Int, Int)]
tripole = [(0, 0), (1, 0), (2, 0), (4, 0), (5, 0), (6, 0)]

byFlops :: [(Int, Int)]
byFlops = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (1, 2)]

mold :: [(Int, Int)]
mold = [(1, 0), (2, 0), (0, 1), (3, 1), (0, 2), (3, 2), (1, 3), (2, 3)]

clock :: [(Int, Int)]
clock = [(2, 0), (5, 0), (1, 1), (0, 2), (1, 3), (2, 4), (3, 4), (4, 3), (5, 2), (4, 1)]

quadpole :: [(Int, Int)]
quadpole = [(0, 0), (1, 0), (2, 0), (3, 0), (5, 0), (6, 0), (7, 0), (8, 0)]

butterfly :: [(Int, Int)]
butterfly = [(1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (2, 2)]

trafficCircle :: [(Int, Int)]
trafficCircle = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]

pentant :: [(Int, Int)]
pentant = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2), (0, 3), (2, 3)]

crossroads :: [(Int, Int)]
crossroads = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2), (0, 3), (1, 3), (2, 3)]

pinwheel :: [(Int, Int)]
pinwheel = [(1, 0), (2, 0), (0, 1), (2, 1), (0, 2), (1, 2)]

-- Extra spaceships ----------------------------------------------------------

gliderPerp :: [(Int, Int)]
gliderPerp = [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)]

lwssPerp :: [(Int, Int)]
lwssPerp = [(0, 1), (0, 4), (1, 0), (2, 0), (2, 4), (3, 0), (3, 1), (3, 2), (3, 3)]

mwssAlt :: [(Int, Int)]
mwssAlt = [(0, 2), (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (2, 0), (2, 4), (3, 1), (3, 2), (3, 3)]

dart :: [(Int, Int)]
dart = [(0, 2), (1, 0), (1, 1), (1, 2), (2, 2), (2, 3), (3, 3)]

crabCanonical :: [(Int, Int)]
crabCanonical = [(0, 2), (1, 0), (1, 1), (1, 2), (2, 2), (2, 3), (3, 1), (3, 2)]

loaferSmall :: [(Int, Int)]
loaferSmall = [(0, 1), (1, 0), (1, 2), (2, 0), (2, 1), (2, 2)]

gliderUp :: [(Int, Int)]
gliderUp = [(0, 2), (1, 0), (1, 1), (1, 2), (2, 1)]

gliderDown :: [(Int, Int)]
gliderDown = [(0, 1), (1, 0), (1, 1), (1, 2), (2, 0)]

gliderLeft :: [(Int, Int)]
gliderLeft = [(0, 0), (0, 1), (0, 2), (1, 2), (2, 1)]

-- Methuselah seeds ----------------------------------------------------------

rPentomino :: [(Int, Int)]
rPentomino = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 2)]

acorn :: [(Int, Int)]
acorn = [(1, 0), (3, 1), (0, 2), (1, 2), (4, 2), (5, 2), (6, 2)]

diehard :: [(Int, Int)]
diehard = [(6, 1), (0, 2), (1, 2), (2, 2), (2, 3), (6, 5), (7, 5)]

rabbits :: [(Int, Int)]
rabbits = [(2, 0), (0, 1), (1, 1), (2, 1), (3, 1), (4, 1), (0, 2), (1, 2), (2, 2), (3, 2), (4, 2), (5, 2)]

sDiehard :: [(Int, Int)]
sDiehard = [(0, 0), (1, 0), (2, 0), (0, 1), (1, 2), (2, 2), (3, 2)]

bHeptomino :: [(Int, Int)]
bHeptomino = [(0, 0), (1, 0), (0, 1), (1, 1), (2, 1), (0, 2), (1, 2)]

piHeptomino :: [(Int, Int)]
piHeptomino = [(0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (1, 2)]

rAcorn :: [(Int, Int)]
rAcorn = [(2, 0), (0, 1), (1, 1), (2, 1), (3, 1), (4, 1), (5, 1)]

switchEngine :: [(Int, Int)]
switchEngine = [(0, 0), (1, 0), (0, 1), (1, 1), (2, 1), (3, 1)]

blockOnTable :: [(Int, Int)]
blockOnTable = [(0, 0), (1, 0), (0, 1), (1, 1), (0, 2), (1, 2), (2, 2)]

-- Eaters --------------------------------------------------------------------

eater :: [(Int, Int)]
eater = [(0, 0), (1, 0), (0, 1), (0, 2), (1, 2), (2, 2), (2, 1)]

eater2 :: [(Int, Int)]
eater2 = [(0, 0), (1, 0), (2, 0), (0, 1), (0, 2), (1, 2), (2, 1)]

eater3 :: [(Int, Int)]
eater3 = [(0, 0), (1, 0), (0, 1), (0, 2), (1, 2), (2, 2), (3, 2), (3, 1)]

blockOnSnake :: [(Int, Int)]
blockOnSnake = [(0, 0), (1, 0), (0, 1), (1, 1), (0, 2), (0, 3)]

tubWithTail :: [(Int, Int)]
tubWithTail = [(1, 0), (0, 1), (2, 1), (1, 2), (1, 3)]

longHookWithTail :: [(Int, Int)]
longHookWithTail = [(0, 0), (0, 1), (1, 1), (2, 1), (2, 0), (2, 2)]

snakeBridge :: [(Int, Int)]
snakeBridge = [(0, 0), (1, 0), (0, 1), (1, 1), (2, 1), (3, 1)]

mirroredEater :: [(Int, Int)]
mirroredEater = [(2, 0), (1, 0), (2, 1), (2, 2), (1, 2), (0, 2), (0, 1)]

preBlock :: [(Int, Int)]
preBlock = [(0, 0), (1, 0), (0, 1)]

preBeehive :: [(Int, Int)]
preBeehive = [(1, 0), (2, 0), (0, 1), (3, 1)]

-- Misc ----------------------------------------------------------------------

trafficLight :: [(Int, Int)]
trafficLight = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]

honeyFarm :: [(Int, Int)]
honeyFarm = [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (0, 2), (1, 2), (2, 2)]

farm :: [(Int, Int)]
farm = [(1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (0, 2), (1, 2)]

longBoatTie :: [(Int, Int)]
longBoatTie = [(0, 0), (1, 0), (0, 1), (3, 1), (1, 2), (2, 2), (3, 2)]

cisLongHook :: [(Int, Int)]
cisLongHook = [(0, 0), (0, 1), (1, 1), (2, 1), (2, 0), (2, 2)]

transLongHook :: [(Int, Int)]
transLongHook = [(0, 0), (0, 1), (1, 1), (2, 1), (2, 0), (0, 2)]

veryLongBoat :: [(Int, Int)]
veryLongBoat = [(0, 0), (1, 0), (0, 1), (4, 1), (1, 2), (2, 2), (3, 2), (4, 2)]

cisBoat :: [(Int, Int)]
cisBoat = [(0, 0), (1, 0), (0, 1), (2, 1), (1, 2)]

transBoat :: [(Int, Int)]
transBoat = [(0, 0), (1, 0), (0, 1), (2, 1), (2, 2)]

cisBlock :: [(Int, Int)]
cisBlock = [(0, 0), (1, 0), (0, 1), (2, 1)]

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
speciesColor n
  | n == soupSpecies = (72, 72, 88)
  | n == manualSpecies = hslRgb 280 0.45 0.68
  | n >= stillMin && n <= stillMax =
      hslRgb (200 + fromIntegral (n - stillMin) * 2.5) 0.62 0.58
  | n >= oscMin && n <= oscMax =
      hslRgb (95 + fromIntegral (n - oscMin) * 3) 0.58 0.52
  | n >= shipMin && n <= shipMax =
      hslRgb (12 + fromIntegral (n - shipMin) * 4) 0.72 0.55
  | n >= methuselahMin && n <= methuselahMax =
      hslRgb (175 + fromIntegral (n - methuselahMin) * 4) 0.55 0.50
  | n >= eaterMin && n <= eaterMax =
      hslRgb (35 + fromIntegral (n - eaterMin) * 5) 0.60 0.52
  | n >= miscMin && n <= miscMax =
      hslRgb (52 + fromIntegral (n - miscMin) * 4) 0.58 0.54
  | n >= discoverMin =
      hslRgb (fromIntegral ((n * 137508) `mod` 360000) / 1000) 0.62 0.56
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
