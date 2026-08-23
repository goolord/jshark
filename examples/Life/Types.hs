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
  , manualSpecies
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

manualSpecies :: Int
manualSpecies = 31

data LifeState = LifeState
  { gen :: Int
  , pop :: Int
  , paused :: Bool
  , alive :: ByteArray
  , species :: ByteArray
  , nextAlive :: ByteArray
  , nextSpecies :: ByteArray
  , counts :: ByteArray
  , palette :: ByteArray
  , rows :: [Double]
  , cols :: [Double]
  }
  deriving Generic
