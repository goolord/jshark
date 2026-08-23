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
  , hoverRadius
  , LifeState (..)
  )
where

import Data.Array.Byte (ByteArray)
import Data.Text (Text)
import GHC.Generics (Generic)

boardId :: Text
boardId = "life-board"

gridW, gridH, cellPx :: Int
gridW = 256
gridH = 192
cellPx = 3

gridN :: Int
gridN = gridW * gridH

canvasW, canvasH :: Double
canvasW = fromIntegral (gridW * cellPx)
canvasH = fromIntegral (gridH * cellPx)

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

-- | Chebyshev cells around the cursor that still count as hovering a species.
hoverRadius :: Int
hoverRadius = 2

data LifeState = LifeState
  { gen :: Int
  , pop :: Int
  , paused :: Bool
  , alive :: ByteArray
  , species :: ByteArray
  , nextAlive :: ByteArray
  , nextSpecies :: ByteArray
  , palette :: ByteArray
  , nextDiscover :: Int
  , recentDiscover :: Text
  }
  deriving Generic
