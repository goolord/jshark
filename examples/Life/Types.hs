{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( boardId
  , lifeBoard2dId
  , gridW
  , gridH
  , cellPx
  , gridN
  , canvasW
  , canvasH
  , texW
  , texH
  , canvasBg
  , canvasBgPixi
  , canvasBgRgba
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
  , simBudgetMs
  , hudRefreshMs
  , lifeIndexHostId
  , lifeTypesListId
  , lifeTooltipId
  , lifeTooltipSwatchId
  , lifeTooltipNameId
  , lifeToolsId
  , lifeToolsCollapseId
  , lifePauseOverlayId
  , lifePauseLabelId
  , lifeDebugId
  , lifeDebugCollapseId
  , lifeSettingsId
  , lifeSettingsCollapseId
  , lifeSettingsZoomId
  , lifeSettingsZoomInId
  , lifeSettingsZoomOutId
  , lifeSettingsResetId
  , lifeSettingsGridId
  , lifeSettingsTickId
  , lifeSettingsTickValId
  , gridSizePresets
  , tickMinMs
  , tickMaxMs
  , tickStepMs
  , tickDefaultMs
  , lifeEraserSizeId
  , lifeEraserRadiusId
  , lifeEraserRadiusValId
  , lifeEraserGhostId
  , eraserDefaultRadius
  , eraserMinRadius
  , eraserMaxRadius
  , lifeStatGenId
  , lifeStatCellsId
  , lifeStatFpsId
  , lifeStatStatusId
  , lifeStatZoomId
  , lifeStatTickId
  , lifeStatEngineId
  , eraserToolSid
  , mouseToolSid
  , gliderToolSid
  , hoverRadius
  , zoomSteps
  , zoomLevels
  , zoomLevelLabels
  , soupRngSeed
  , lcgMult
  , lcgInc
  , lcgModulus
  , soupDensity
  , LifeState (..)
  )
where

import Data.Array.Byte (ByteArray)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Numeric (showHex)

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

-- | Canvas background @#RRGGBB@ (also drives 'canvasBg' / 'canvasBgPixi').
canvasBgHex :: Int
canvasBgHex = 0x0f172a

hexColorText :: Int -> Text
hexColorText n =
  let
    h = map toLower (showHex n "")
    padded = replicate (max 0 (6 - length h)) '0' ++ h
   in
    "#" <> T.pack padded

-- | CSS color for shell chrome ('canvasBgHex').
canvasBg :: Text
canvasBg = hexColorText canvasBgHex

-- | @PIXI.Application({ backgroundColor })@ ('canvasBgHex').
canvasBgPixi :: Double
canvasBgPixi = fromIntegral canvasBgHex

-- | Initial soup and catalog stamps land in this central region.
seedW, seedH :: Int
seedW = 512
seedH = 384

-- | Canvas background (#0f172a) as @0xAABBGGRR@ for RGBA buffers.
canvasBgRgba :: Int
canvasBgRgba = 0xFF22170F

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

-- | Max wall time per rAF spent stepping (ms).
simBudgetMs :: Int
simBudgetMs = 8

-- | Min interval between HUD textContent updates (ms).
hudRefreshMs :: Int
hudRefreshMs = 100

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

lifeToolsCollapseId :: Text
lifeToolsCollapseId = "life-tools-collapse"

lifePauseOverlayId :: Text
lifePauseOverlayId = "life-pause-overlay"

lifePauseLabelId :: Text
lifePauseLabelId = "life-pause-label"

lifeDebugId :: Text
lifeDebugId = "life-debug"

lifeDebugCollapseId :: Text
lifeDebugCollapseId = "life-debug-collapse"

lifeSettingsId :: Text
lifeSettingsId = "life-settings"

lifeSettingsCollapseId :: Text
lifeSettingsCollapseId = "life-settings-collapse"

lifeSettingsZoomId :: Text
lifeSettingsZoomId = "life-settings-zoom"

lifeSettingsZoomInId :: Text
lifeSettingsZoomInId = "life-settings-zoom-in"

lifeSettingsZoomOutId :: Text
lifeSettingsZoomOutId = "life-settings-zoom-out"

lifeSettingsResetId :: Text
lifeSettingsResetId = "life-settings-reset"

lifeSettingsGridId :: Text
lifeSettingsGridId = "life-settings-grid"

lifeSettingsTickId :: Text
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

lifeEraserSizeId :: Text
lifeEraserSizeId = "life-eraser-size"

lifeEraserRadiusId :: Text
lifeEraserRadiusId = "life-eraser-radius"

lifeEraserRadiusValId :: Text
lifeEraserRadiusValId = "life-eraser-radius-val"

lifeEraserGhostId :: Text
lifeEraserGhostId = "life-eraser-ghost"

eraserDefaultRadius, eraserMinRadius, eraserMaxRadius :: Int
eraserDefaultRadius = 3
eraserMinRadius = 1
eraserMaxRadius = 12

lifeStatGenId :: Text
lifeStatGenId = "life-stat-gen"

lifeStatCellsId :: Text
lifeStatCellsId = "life-stat-cells"

lifeStatFpsId :: Text
lifeStatFpsId = "life-stat-fps"

lifeStatStatusId :: Text
lifeStatStatusId = "life-stat-status"

lifeStatZoomId :: Text
lifeStatZoomId = "life-stat-zoom"

lifeStatTickId :: Text
lifeStatTickId = "life-stat-tick"

lifeStatEngineId :: Text
lifeStatEngineId = "life-stat-engine"

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
  }
  deriving Generic
