{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure Mandelbrot kernel shared by JS fallback and HVM2 WASM export.
module Kernels
  ( canvasW
  , canvasH
  , resolutionPresets
  , blockPx
  , maxIter
  , zoomRate
  , zoomReferenceMs
  , minScale
  , initialCenterRe
  , initialCenterIm
  , initialScale
  , hvm2Entries
  , mandelKernel
  , mandelEscapes
  , mandelAt
  , crAt
  , ciAt
  , mandelJsSource
  )
where

import JShark.Api
import JShark.Api.Types (ClosedExpr, Hvm2KernelEntry (..))

resolutionPresets :: [(Int, Int, String)]
resolutionPresets =
  [ (320, 240, "320×240")
  , (480, 360, "480×360")
  , (640, 480, "640×480")
  , (800, 600, "800×600")
  , (960, 720, "960×720")
  , (1280, 960, "1280×960")
  , (1600, 1200, "1600×1200")
  , (1920, 1080, "1920×1080")
  , (2560, 1440, "2560×1440")
  , (3840, 2160, "3840×2160")
  ]

canvasW, canvasH, blockPx, maxIter :: Int
canvasW = 640
canvasH = 480
blockPx = 2
maxIter = 256

zoomRate, initialCenterRe, initialCenterIm, initialScale :: Double
zoomRate = 0.988
-- Seahorse-valley minibrot; stays on structure to minScale.
initialCenterRe = -0.743643887037151
initialCenterIm = 0.13182590420533
initialScale = 2.4

-- | @zoomRate@ is applied per this many ms (~60 Hz reference).
zoomReferenceMs, minScale :: Double
zoomReferenceMs = 1000 / 60
minScale = 1e-7

escapeR2 :: Expr f 'Number
escapeR2 = number 4

hvm2Entries :: [Hvm2KernelEntry]
hvm2Entries = [Hvm2KernelEntry "mandel" mandelKernel]

mandelKernel :: ClosedExpr ('Function 'Number ('Function 'Number 'Number))
mandelKernel = lambda (\cr -> lambda (\ci -> mandelEscapes cr ci))

mandelEscapes :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
mandelEscapes cr ci =
  letRec
    ( \f ->
        lambda
          ( \n ->
              lambda
                ( \zr ->
                    lambda
                      ( \zi ->
                          if_
                            ( n
                                .>= number (fromIntegral maxIter)
                                .|| (zr * zr + zi * zi)
                                .>= escapeR2
                            )
                            n
                            ( apply
                                ( apply
                                    ( apply
                                        f
                                        (n + number 1)
                                    )
                                    (zr * zr - zi * zi + cr)
                                )
                                (number 2 * zr * zi + ci)
                            )
                      )
                )
          )
    )
    ( \f ->
        apply
          ( apply
              ( apply
                  f
                  (number 0)
              )
              (number 0)
          )
          (number 0)
    )

crAt ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
crAt px w centerRe scale =
  centerRe + (px - w / number 2) * scale / w

ciAt ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
ciAt py h centerIm scale =
  centerIm + (py - h / number 2) * scale / h

mandelAt ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
mandelAt px py w h centerRe centerIm scale =
  mandelEscapes (crAt px w centerRe scale) (ciAt py h centerIm scale)

-- | JS fallback body.
mandelJsSource :: String
mandelJsSource =
  "function(cr,ci){"
    ++ "let n=0,zr=0,zi=0;"
    ++ "while(n<"
    ++ show maxIter
    ++ "&&zr*zr+zi*zi<4){"
    ++ "const nzr=zr*zr-zi*zi+cr,nzi=2*zr*zi+ci;"
    ++ "zr=nzr;zi=nzi;n++;"
    ++ "}"
    ++ "return n;"
    ++ "}"
