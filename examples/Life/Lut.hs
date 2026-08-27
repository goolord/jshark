{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | JShark LUT stepping (replaces LUTGenerator.js).
module Lut
  ( createLifeLUT
  , stepRegionLUT
  , stepCell
  , countNeighbors
  )
where

import Grid (cellIdx, inBounds, setU8, u8Get)
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Math as Math

createLifeLUT :: EffectSyntax f (Expr f 'Uint8Array)
createLifeLUT =
  bindExpr $
    ffi
      ( "(()=>{"
          <> "const LUT=new Uint8Array(65536);"
          <> "for(let key=0;key<65536;key++){"
          <> "const top=(key>>8)&255,cur=key&255;"
          <> "let out=0;"
          <> "for(let bit=0;bit<8;bit++){"
          <> "const alive=(cur>>bit)&1;"
          <> "const left=bit>0?(cur>>(bit-1))&1:0;"
          <> "const right=bit<7?(cur>>(bit+1))&1:0;"
          <> "const topL=bit>0?(top>>(bit-1))&1:0;"
          <> "const topC=(top>>bit)&1;"
          <> "const topR=bit<7?(top>>(bit+1))&1:0;"
          <> "const n=topL+topC+topR+left+right;"
          <> "const next=alive?n===2||n===3:n===3;"
          <> "if(next)out|=1<<bit;"
          <> "}"
          <> "LUT[key]=out;"
          <> "}"
          <> "return LUT;"
          <> "})"
      )
      RecNil

computeNextByte ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
computeNextByte top cur bot lt lc lb rt rc rb =
  go (number 0) (number 0)
 where
  go bit acc =
    if_
      (bit .>= 8)
      acc
      ( let
          sh = bit
          alive = bitAnd (shr cur sh) (number 1)
          left =
            if_ (bit .> 0) (bitAnd (shr cur (bit - 1)) (number 1)) lc
          right =
            if_ (bit .< 7) (bitAnd (shr cur (bit + 1)) (number 1)) rc
          topL =
            if_ (bit .> 0) (bitAnd (shr top (bit - 1)) (number 1)) lt
          topC = bitAnd (shr top sh) (number 1)
          topR =
            if_ (bit .< 7) (bitAnd (shr top (bit + 1)) (number 1)) rt
          botL =
            if_ (bit .> 0) (bitAnd (shr bot (bit - 1)) (number 1)) lb
          botC = bitAnd (shr bot sh) (number 1)
          botR =
            if_ (bit .< 7) (bitAnd (shr bot (bit + 1)) (number 1)) rb
          n = topL + topC + topR + left + right + botL + botC + botR
          born =
            if_ (alive .== 1) (n .== 2 .|| n .== 3) (n .== 3)
          acc' =
            if_ born (bitOr acc (shl (number 1) sh)) acc
         in
          go (bit + 1) acc'
      )

stepChunk ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
stepChunk lut top cur bot lt lc lb rt rc rb =
  let
    edge = bitOr (bitOr (bitOr lt lc) lb) (bitOr (bitOr rt rc) rb)
    combined = bitOr (bitOr top cur) (bitOr bot edge)
   in
    if_
      (combined .== 0)
      (number 0)
      ( if_
          ((bot .== 0) .&& (edge .== 0))
          (u8Index lut (shl top (number 8) + cur))
          (computeNextByte top cur bot lt lc lb rt rc rb)
      )

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

packRowByte ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
packRowByte grid rowOff w x0 = do
  st <- hold newObject
  _ <- setProp st "byte" (number 0)
  forRange_ (number 0) (number 8) $ \b -> do
    let
      x = x0 + b
    whenS (x .< w) $ do
      let
        sh = shl (number 1) b
      cur <- getProp st "byte"
      whenS (bitAnd (u8Index grid (rowOff + x)) (number 1) .== 1) $
        setProp st "byte" (bitOr cur sh)
    done
  getProp st "byte"

edgeBit ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
edgeBit grid rowOff w col =
  if_
    (col .< 0 .|| col .>= w)
    (number 0)
    ( if_
        (bitAnd (u8Index grid (rowOff + col)) (number 1) .== 1)
        (number 1)
        (number 0)
    )

unpackRowByte ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
unpackRowByte grid rowOff w x0 byte =
  forRange_ (number 0) (number 8) $ \b -> do
    let
      x = x0 + b
    whenS (x .< w) $ do
      let
        sh = shl (number 1) b
        v = if_ (bitAnd byte sh .== 0) (number 0) (number 1)
      setU8 grid (rowOff + x) v
    done

clearRowSimd ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
clearRowSimd grid off len = do
  toSyntax_ $
    ffi
      "(function(g,o,l){const s=globalThis.LifeSimd;if(s&&s.clearRow){s.clearRow(g,o,l);return;}g.fill(0,o,o+l);})"
      (arg grid <: arg off <: arg len <: RecNil)
  done

copyRowSimd ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
copyRowSimd src srcOff dst dstOff len = do
  toSyntax_ $
    ffi
      ( "(function(a,so,d,do_,l){const s=globalThis.LifeSimd;"
          <> "if(s&&s.copyRow){s.copyRow(a,so,d,do_,l);return;}"
          <> "d.set(a.subarray(so,so+l),do_);"
          <> "})"
      )
      (arg src <: arg srcOff <: arg dst <: arg dstOff <: arg len <: RecNil)
  done

trySimdStepRegionLUT ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Bool)
trySimdStepRegionLUT lut gridA gridB w h y0 y1 =
  bindExpr $
    ffi
      ( "(function(lut,a,b,w,h,y0,y1){"
          <> "const s=globalThis.LifeSimd;"
          <> "return!!(s&&s.stepRegionLUT&&s.stepRegionLUT(lut,a,b,w,h,y0,y1));"
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
  handled <- trySimdStepRegionLUT lut gridA gridB w h y0 y1
  whenS (not_ handled) $ stepRegionLUTScalar lut gridA gridB w h y0 y1
  done

stepRegionLUTScalar ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
stepRegionLUTScalar lut gridA gridB w h y0 y1 = do
  let
    yStart = Math.max (number 1) y0
    yStop = Math.min (h - number 1) y1
    bytes = Math.floor ((w + number 7) / number 8)
  forRange_ yStart yStop $ \y -> do
    let
      topOff = (y - number 1) * w
      curOff = y * w
      botOff = (y + number 1) * w
    clearRowSimd gridB curOff w
    forRange_ (number 0) bytes $ \xb -> do
      let
        x0 = xb * number 8
      whenS (x0 .< w) $ do
        let
          leftCol = x0 - number 1
          rightCol = x0 + number 8
          lt = edgeBit gridA topOff w leftCol
          lc = edgeBit gridA curOff w leftCol
          lb = edgeBit gridA botOff w leftCol
          rt = edgeBit gridA topOff w rightCol
          rc = edgeBit gridA curOff w rightCol
          rb = edgeBit gridA botOff w rightCol
        top <- packRowByte gridA topOff w x0
        cur <- packRowByte gridA curOff w x0
        bot <- packRowByte gridA botOff w x0
        let
          combined =
            bitOr
              (bitOr (bitOr top cur) bot)
              (bitOr (bitOr (bitOr lt lc) lb) (bitOr (bitOr rt rc) rb))
        whenS (combined .!= 0) $ do
          let
            nextByte = stepChunk lut top cur bot lt lc lb rt rc rb
          unpackRowByte gridB curOff w x0 nextByte
      done
    done
  whenS (y0 .== 0) (copyRowSimd gridA (number 0) gridB (number 0) w)
  whenS (y1 .>= h) $ do
    let
      botOff = (h - number 1) * w
    copyRowSimd gridA botOff gridB botOff w
  done
