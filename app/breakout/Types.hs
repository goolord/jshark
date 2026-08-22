{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Host-side Breakout model. 'Generic' rows become JS objects via
-- 'JShark.Generic'. Layout matches the MDN canvas workshop grid;
-- 'Game' / 'Brick' / 'Ball' follow the Haskell breakout domain split
-- (state, bricks, ball) without Gloss physics or items.
module Types
  ( Ball(..)
  , Paddle(..)
  , Brick(..)
  , Game(..)
  , Phase(..)
  , canvasW
  , canvasH
  , ballR
  , paddleW
  , paddleH
  , paddleSpeed
  , paddleMaxX
  , brickW
  , brickH
  , brickPad
  , brickCount
  , startLives
  , startBall
  , startPaddle
  , layoutBricks
  , startGame
  , boardId
  , ballFill
  , ink
  , bannerFill
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

canvasW, canvasH, ballR, paddleW, paddleH, paddleSpeed, paddleMaxX :: Double
canvasW = 480
canvasH = 320
ballR = 10
paddleW = 75
paddleH = 10
paddleSpeed = 7
paddleMaxX = canvasW - paddleW

brickW, brickH, brickPad, brickOffL, brickOffT :: Double
brickW = 75
brickH = 20
brickPad = 10
brickOffL = 30
brickOffT = 30

brickColumnCount, brickRowCount :: Int
brickColumnCount = 5
brickRowCount = 3

brickCount :: Int
brickCount = brickColumnCount * brickRowCount

startLives :: Int
startLives = 3

boardId :: Text
boardId = "board"

ballFill, ink, bannerFill :: Text
ballFill = "#0369a1"
ink = "#0c4a6e"
bannerFill = "#0f172a"

data Phase = Play | Win | Lose
  deriving (Eq, Generic)

data Ball = Ball
  { x :: Double
  , y :: Double
  , dx :: Double
  , dy :: Double
  }
  deriving (Generic)

data Paddle = Paddle
  { px :: Double
  }
  deriving (Generic)

data Brick = Brick
  { bx :: Double
  , by :: Double
  , alive :: Bool
  , color :: Text
  }
  deriving (Generic)

data Game = Game
  { ball :: Ball
  , paddle :: Paddle
  , bricks :: [Brick]
  , score :: Int
  , lives :: Int
  , phase :: Phase
  , leftOn :: Bool
  , rightOn :: Bool
  }
  deriving (Generic)

startBall :: Ball
startBall = Ball (canvasW / 2) (canvasH - 30) 2 (-2)

startPaddle :: Paddle
startPaddle = Paddle (paddleMaxX / 2)

rowColors :: [Text]
rowColors = ["#0c4a6e", "#0369a1", "#0284c7", "#0ea5e9", "#38bdf8"]

rowColor :: Int -> Text
rowColor i = cycle rowColors !! i

mkBrick :: Int -> Int -> Brick
mkBrick row col =
  Brick
    { bx = fromIntegral col * (brickW + brickPad) + brickOffL
    , by = fromIntegral row * (brickH + brickPad) + brickOffT
    , alive = True
    , color = rowColor row
    }

layoutBricks :: [Brick]
layoutBricks =
  liftA2 mkBrick [0 .. brickRowCount - 1] [0 .. brickColumnCount - 1]

startGame :: Game
startGame =
  Game startBall startPaddle layoutBricks 0 startLives Play False False
