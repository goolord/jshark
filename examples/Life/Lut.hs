{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | LUT stepping via native JS FFI (semantics: 'LutCore.stepRegionLUTPure').
module Lut
  ( bootLifeLut
  , createLifeLUT
  , stepRegionLUT
  , stepCell
  , countNeighbors
  )
where

import qualified Data.Text as T
import Grid (cellIdx, inBounds, setU8, u8Get)
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import LutBoot (lifeLutEnsureJs, lifeLutGlobalJs, lifeLutInstallJs)

bootLifeLut :: EffectSyntax f (f 'Unit)
bootLifeLut = do
  toSyntax_ $
    ffi
      (T.unpack lifeLutInstallJs)
      RecNil
  done

createLifeLUT :: EffectSyntax f (Expr f 'Uint8Array)
createLifeLUT =
  bindExpr $
    ffi
      ( "(function(){"
          <> T.unpack lifeLutEnsureJs
          <> "var api="
          <> T.unpack lifeLutGlobalJs
          <> ";"
          <> "return api&&api.createLifeLUT();"
          <> "})"
      )
      RecNil

countNeighbors ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
countNeighbors grid w h x y = do
  st <- hold newObject
  _ <- setProp st "n" (number 0)
  forRange_ (number (-1)) (number 2) $ \dy ->
    forRange_ (number (-1)) (number 2) $ \dx ->
      whenS (not_ (dx .== 0 .&& dy .== 0)) $ do
        let
          nx = x + dx
          ny = y + dy
        whenS (inBounds w h nx ny) $ do
          let
            idx = cellIdx w nx ny
          cur <- getProp st "n"
          whenS (bitAnd (u8Index grid idx) (number 1) .== 1) $
            setProp st "n" (cur + 1)
        done
  getProp st "n"

stepCell ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
stepCell gridA gridB w h x y = do
  let
    i = cellIdx w x y
  alive <- u8Get gridA i
  n <- countNeighbors gridA w h x y
  let
    live = bitAnd alive (number 1) .== 1
    next =
      if_
        (live .&& (n .== 2 .|| n .== 3))
        (number 1)
        (if_ (n .== 3) (number 1) (number 0))
  setU8 gridB i next
  done

stepRegionLUT ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
stepRegionLUT lut gridA gridB w h y0 y1 = do
  toSyntax_ $
    ffi
      ( "(function(L,a,b,w,h,y0,y1){"
          <> T.unpack lifeLutEnsureJs
          <> "var api="
          <> T.unpack lifeLutGlobalJs
          <> ";"
          <> "if(api&&api.stepRegionLUT)api.stepRegionLUT(L,a,b,w,h,y0,y1);"
          <> "})"
      )
      ( arg lut
          <: arg gridA
          <: arg gridB
          <: arg w
          <: arg h
          <: arg y0
          <: arg y1
          <: RecNil
      )
  done
