{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( boardId
  , gridW
  , gridH
  , cellPx
  , gridN
  , canvasW
  , canvasH
  , canvasBg
  , seedOx
  , seedOy
  , seedW
  , seedH
  , ink
  , soupSpecies
  , stillMin
  , stillMax
  , oscMin
  , oscMax
  , shipMin
  , shipMax
  , methuselahMin
  , methuselahMax
  , eaterMin
  , eaterMax
  , miscMin
  , miscMax
  , manualSpecies
  , discoverMin
  , discoverMax
  , discoverEvery
  , indexRefreshMs
  , lifeIndexHostId
  , lifeTypesListId
  , lifeTooltipId
  , lifeTooltipSwatchId
  , lifeTooltipNameId
  , lifeToolsId
  , toggleToolSid
  , hoverRadius
  , zoomMin
  , zoomMax
  , zoomFactor
  , soupRngSeed
  , lcgMult
  , lcgInc
  , lcgModulus
  , soupDensity
  , LifeState (..)
  )
where

import Data.Array.Byte (ByteArray)
import Data.Text (Text)
import GHC.Generics (Generic)

boardId :: Text
boardId = "life-board"

-- | Simulation grid (world). The canvas is only the viewport.
gridW, gridH, cellPx :: Int
gridW = 1024
gridH = 768
cellPx = 3

gridN :: Int
gridN = gridW * gridH

-- | Viewport size in CSS/bitmap pixels (not the whole world).
canvasW, canvasH :: Double
canvasW = 768
canvasH = 576

-- | Canvas background (#0f172a).
canvasBg :: Text
canvasBg = "#0f172a"

-- | Initial soup and catalog stamps land in this central region.
seedW, seedH :: Int
seedW = 512
seedH = 384

seedOx, seedOy :: Int
seedOx = (gridW - seedW) `div` 2
seedOy = (gridH - seedH) `div` 2

ink :: Text
ink = "#e2e8f0"

soupSpecies :: Int
soupSpecies = 0

stillMin, stillMax :: Int
stillMin = 1
stillMax = 24

oscMin, oscMax :: Int
oscMin = 25
oscMax = 44

shipMin, shipMax :: Int
shipMin = 45
shipMax = 59

methuselahMin, methuselahMax :: Int
methuselahMin = 60
methuselahMax = 69

eaterMin, eaterMax :: Int
eaterMin = 70
eaterMax = 79

miscMin, miscMax :: Int
miscMin = 80
miscMax = 89

manualSpecies :: Int
manualSpecies = 90

discoverMin, discoverMax :: Int
discoverMin = 91
discoverMax = 255

discoverEvery :: Int
discoverEvery = 45

indexRefreshMs :: Int
indexRefreshMs = 2000

lifeIndexHostId :: Text
lifeIndexHostId = "life-index-host"

lifeTypesListId :: Text
lifeTypesListId = "life-types"

lifeTooltipId :: Text
lifeTooltipId = "life-tooltip"

lifeTooltipSwatchId :: Text
lifeTooltipSwatchId = "life-tooltip-swatch"

lifeTooltipNameId :: Text
lifeTooltipNameId = "life-tooltip-name"

lifeToolsId :: Text
lifeToolsId = "life-tools"

-- | HUD default: left-click flips a single cell (species 'manualSpecies').
toggleToolSid :: Int
toggleToolSid = 0

-- | Chebyshev cells around the cursor that still count as hovering a species.
hoverRadius :: Int
hoverRadius = 2

zoomMin, zoomMax, zoomFactor :: Double
zoomMin = 0.5
zoomMax = 6
zoomFactor = 1.25

-- | Shared with 'JShark.Api.seedSoupRegion' and 'Patterns.seedCell'.
soupRngSeed :: Int
soupRngSeed = 42

lcgMult, lcgInc, lcgModulus :: Int
lcgMult = 1103515245
lcgInc = 12345
lcgModulus = 0x7fffffff

soupDensity :: Double
soupDensity = 0.20

data LifeState = LifeState
  { gen :: Int
  , pop :: Int
  , paused :: Bool
  , alive :: ByteArray
  , species :: ByteArray
  , nextAlive :: ByteArray
  , nextSpecies :: ByteArray
  , palette :: ByteArray
  , boundX0 :: Int
  , boundY0 :: Int
  , boundX1 :: Int
  , boundY1 :: Int
  , nextDiscover :: Int
  , recentDiscover :: Text
  , discoverVisited :: ByteArray
  , discoverStackX :: ByteArray
  , discoverStackY :: ByteArray
  -- | Host schema only; runtime is a JS @Array@ of cell indices (@Number@).
  , liveList :: [Int]
  -- | Host schema only; runtime is a JS @Array@ of cell indices (@Number@).
  , nextLiveList :: [Int]
  , stepStamp :: ByteArray
  -- | Host schema only; runtime is a JS @Array@ of CSS color strings.
  , paletteCss :: [Text]
  }
  deriving Generic
