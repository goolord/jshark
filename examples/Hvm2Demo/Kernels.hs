{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure Mandelbrot kernel shared by JS fallback and HVM2 WASM export.
module Kernels
  ( canvasW
  , canvasH
  , blockPx
  , maxIter
  , zoomRate
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
import JShark.Types (ClosedExpr, Hvm2KernelEntry (..))

canvasW, canvasH, blockPx, maxIter :: Int
canvasW = 320
canvasH = 240
blockPx = 2
maxIter = 64

zoomRate, initialCenterRe, initialCenterIm, initialScale :: Double
zoomRate = 0.988
initialCenterRe = -0.74546
initialCenterIm = 0.11303
initialScale = 2.4

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

-- | JS fallback body; keep in sync with 'examples/Hvm2Demo/shims.c'.
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
