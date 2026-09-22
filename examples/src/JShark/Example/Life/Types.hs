{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module JShark.Example.Life.Types where

import Data.Array.Byte (ByteArray)
import Data.Text (Text)
import GHC.Generics (Generic)

boardId :: Text
boardId = "life-board"

-- | 2D overlay canvas used to render the world when WebGL is lost.
lifeBoard2dId :: Text
lifeBoard2dId = "life-board-2d"

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

-- | Grid-resolution RGBA atlas (one texel per cell). ~3 MiB; the sprite
-- scales it on the GPU so pan/zoom never repaints cells.
texW, texH :: Double
texW = fromIntegral gridW
texH = fromIntegral gridH

-- | Canvas background @#RRGGBB@ (drives 'canvasBgPixi').
canvasBgHex :: Int
canvasBgHex = 0x111111

-- | @PIXI.Application({ backgroundColor })@ ('canvasBgHex').
canvasBgPixi :: Double
canvasBgPixi = fromIntegral canvasBgHex

-- | Initial soup and catalog stamps land in this central region.
seedW, seedH :: Int
seedW = 512
seedH = 384

seedOx, seedOy :: Int
seedOx = (gridW - seedW) `div` 2
seedOy = (gridH - seedH) `div` 2

ink :: Text
ink = "#e8e8e8"

soupSpecies :: Int
soupSpecies = 0

methuselahMin, methuselahMax :: Int
methuselahMin = 60
methuselahMax = 69

manualSpecies :: Int
manualSpecies = 90

discoverMin, discoverMax :: Int
discoverMin = 91
discoverMax = 1023

-- | Inclusive palette / birth-count slots: soup through 'discoverMax'.
speciesCount :: Int
speciesCount = discoverMax + 1

discoverEvery :: Int
discoverEvery = 15

indexRefreshMs :: Int
indexRefreshMs = 2000

-- | Min interval between HUD textContent updates (ms).
hudRefreshMs :: Int
hudRefreshMs = 100

lifeIndexHostId, lifeIndexTotalId, lifeTypesListId, lifeTooltipId :: Text
lifeIndexHostId = "life-index-host"
lifeIndexTotalId = "life-index-total"
lifeTypesListId = "life-types"
lifeTooltipId = "life-tooltip"

lifeTooltipSwatchId, lifeTooltipNameId, lifeToolsId, lifeToolsCollapseId :: Text
lifeTooltipSwatchId = "life-tooltip-swatch"
lifeTooltipNameId = "life-tooltip-name"
lifeToolsId = "life-tools"
lifeToolsCollapseId = "life-tools-collapse"

lifePauseOverlayId, lifePauseLabelId, lifeDebugId, lifeDebugCollapseId :: Text
lifePauseOverlayId = "life-pause-overlay"
lifePauseLabelId = "life-pause-label"
lifeDebugId = "life-debug"
lifeDebugCollapseId = "life-debug-collapse"

lifeSettingsId, lifeSettingsCollapseId, lifeSettingsZoomId :: Text
lifeSettingsId = "life-settings"
lifeSettingsCollapseId = "life-settings-collapse"
lifeSettingsZoomId = "life-settings-zoom"

lifeSettingsZoomInId, lifeSettingsZoomOutId, lifeSettingsResetId :: Text
lifeSettingsZoomInId = "life-settings-zoom-in"
lifeSettingsZoomOutId = "life-settings-zoom-out"
lifeSettingsResetId = "life-settings-reset"

lifeSettingsPurgeId, lifeSettingsGridId, lifeSettingsTickId :: Text
lifeSettingsPurgeId = "life-settings-purge"
lifeSettingsGridId = "life-settings-grid"
lifeSettingsTickId = "life-settings-tick"

lifeSettingsTickValId :: Text
lifeSettingsTickValId = "life-settings-tick-val"

-- | Selectable simulation worlds. Default matches 'gridW' × 'gridH'.
gridSizePresets :: [(Int, Int)]
gridSizePresets =
  [ (256, 192)
  , (512, 384)
  , (1024, 768)
  ]

tickMinMs, tickMaxMs, tickStepMs, tickDefaultMs :: Int
tickMinMs = 0
tickMaxMs = 200
tickStepMs = 5
tickDefaultMs = 0

lifeEraserSizeId, lifeEraserRadiusId, lifeEraserRadiusValId :: Text
lifeEraserSizeId = "life-eraser-size"
lifeEraserRadiusId = "life-eraser-radius"
lifeEraserRadiusValId = "life-eraser-radius-val"

lifeEraserGhostId :: Text
lifeEraserGhostId = "life-eraser-ghost"

eraserDefaultRadius, eraserMinRadius, eraserMaxRadius :: Int
eraserDefaultRadius = 3
eraserMinRadius = 1
eraserMaxRadius = 12

lifeStatGenId, lifeStatCellsId, lifeStatFpsId, lifeStatZoomId :: Text
lifeStatGenId = "life-stat-gen"
lifeStatCellsId = "life-stat-cells"
lifeStatFpsId = "life-stat-fps"
lifeStatZoomId = "life-stat-zoom"

lifeStatRenderId :: Text
lifeStatRenderId = "life-stat-render"

-- | Left-click clears live cells only (never births).
eraserToolSid :: Int
eraserToolSid = -1

-- | Pan the viewport: left drag or right drag with inertia.
mouseToolSid :: Int
mouseToolSid = -2

-- | Click-drag to aim and stamp a glider.
gliderToolSid :: Int
gliderToolSid = -3

-- | Chebyshev cells around the cursor that still count as hovering a species.
hoverRadius :: Int
hoverRadius = 2

-- | Discrete zoom ladder from 50% to 600% (Photoshop-style rational steps).
zoomSteps :: [(Double, Text)]
zoomSteps =
  [ (0.5, "50")
  , (2 / 3, "66.6")
  , (0.75, "75")
  , (1, "100")
  , (1.25, "125")
  , (4 / 3, "133.33")
  , (1.5, "150")
  , (2, "200")
  , (3, "300")
  , (4, "400")
  , (6, "600")
  ]

zoomLevels :: [Double]
zoomLevels = map fst zoomSteps

-- | HUD labels aligned with 'zoomLevels'.
zoomLevelLabels :: [Text]
zoomLevelLabels = map snd zoomSteps

-- | Wheel zoom sensitivity: @exp(-deltaY * rate)@ per event (~1.5%/100px).
wheelZoomRate :: Double
wheelZoomRate = 0.003

-- | Shared with 'GridApi.seedSoupRegion' and 'Patterns.seedCell'.
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
  , rgbaPixels :: ByteArray
  , paletteRgba :: ByteArray
  , boundX0 :: Int
  , boundY0 :: Int
  , boundX1 :: Int
  , boundY1 :: Int
  , nextDiscover :: Int
  , recentDiscover :: Text
  , discoverVisited :: ByteArray
  , discoverStackX :: ByteArray
  , discoverStackY :: ByteArray
  , liveList :: [Int]
  -- ^ Host schema only; runtime is a JS @Array@ of cell indices (@Number@).
  , nextLiveList :: [Int]
  -- ^ Host schema only; runtime is a JS @Array@ of cell indices (@Number@).
  , stepStamp :: ByteArray
  , changedList :: [Int]
  -- ^ Host schema only; runtime is a JS @Array@ of changed cell indices.
  , nextChangedList :: [Int]
  , birthCounts :: ByteArray
  , birthTouched :: ByteArray
  , sceneDirty :: Bool
  , worldW :: Int
  , worldH :: Int
  , tickMs :: Double
  , engineLut :: ByteArray
  , engineGridA :: ByteArray
  , engineGridB :: ByteArray
  }
  deriving Generic
