{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed grid buffer and canvas helpers. Byte indexing and ImageData
--    live behind minimal 'ffi' — everything else stays in JShark.
module Grid
  ( ImageData
  , u8Get
  , u8Set
  , u8Fill
  , createImageData
  , putImageData
  , imageDataBytes
  , stepGrid
  , renderGrid
  , cellIdx
  , toroidal
  )
where

import JShark.Api
import qualified JShark.Canvas as Canvas
import JShark.Rec (Rec (..), (<:))

data ImageData

u8Get :: Expr f 'Uint8Array -> Expr f 'Number -> Effect f 'Number
u8Get buf i = ffi "((b, i) => b[i])" (arg buf <: arg i <: RecNil)

u8Set ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f 'Unit
u8Set buf i v =
  discard $
    ffi "((b, i, v) => { b[i] = v; })" (arg buf <: arg i <: arg v <: RecNil)

u8Fill :: Expr f 'Uint8Array -> Expr f 'Number -> Effect f 'Unit
u8Fill buf v =
  discard $ ffi "((b, v) => b.fill(v))" (arg buf <: arg v <: RecNil)

createImageData ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Effect f ('MutableObject ImageData))
createImageData ctx w h =
  hold $ callMethod ctx "createImageData" (arg w <: arg h <: RecNil)

putImageData ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f ('MutableObject ImageData)
  -> EffectSyntax f (f 'Unit)
putImageData ctx img = do
  toSyntax_
    $ discard
    $ callMethod
      ctx
      "putImageData"
      (arg img <: arg (number 0) <: arg (number 0) <: RecNil)
  done

imageDataBytes ::
  Expr f ('MutableObject ImageData) -> EffectSyntax f (Expr f 'Uint8Array)
imageDataBytes img = bindExpr $ ffi "((img) => img.data)" (arg img <: RecNil)

stepGrid ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f 'Number
stepGrid alive species nextAlive nextSpecies w h =
  ffi
    stepGridJs
    ( arg alive
        <: arg species
        <: arg nextAlive
        <: arg nextSpecies
        <: arg w
        <: arg h
        <: RecNil
    )

stepGridJs :: String
stepGridJs =
  "((alive, species, nextAlive, nextSpecies, w, h) => {"
    ++ "let pop = 0;"
    ++ "const counts = new Uint8Array(256);"
    ++ "const touched = [];"
    ++ "for (let y = 0; y < h; y++) {"
    ++ "for (let x = 0; x < w; x++) {"
    ++ "  let n = 0;"
    ++ "  let bestSp = 0;"
    ++ "  let bestCount = 0;"
    ++ "  touched.length = 0;"
    ++ "  for (let dy = -1; dy <= 1; dy++) {"
    ++ "    for (let dx = -1; dx <= 1; dx++) {"
    ++ "      if (dx === 0 && dy === 0) continue;"
    ++ "      const nx = (x + dx + w) % w;"
    ++ "      const ny = (y + dy + h) % h;"
    ++ "      const ni = ny * w + nx;"
    ++ "      if (alive[ni] !== 1) continue;"
    ++ "      n++;"
    ++ "      const sp = species[ni];"
    ++ "      if (counts[sp] === 0) touched.push(sp);"
    ++ "      counts[sp]++;"
    ++ "    }"
    ++ "  }"
    ++ "  for (const sp of touched) {"
    ++ "    const c = counts[sp];"
    ++ "    if (c > bestCount || (c === bestCount && sp < bestSp)) {"
    ++ "      bestCount = c;"
    ++ "      bestSp = sp;"
    ++ "    }"
    ++ "    counts[sp] = 0;"
    ++ "  }"
    ++ "  const i = y * w + x;"
    ++ "  const a = alive[i];"
    ++ "  const sp = species[i];"
    ++ "  if (a === 1) {"
    ++ "    if (n === 2 || n === 3) {"
    ++ "      nextAlive[i] = 1;"
    ++ "      nextSpecies[i] = sp;"
    ++ "      pop++;"
    ++ "    } else {"
    ++ "      nextAlive[i] = 0;"
    ++ "      nextSpecies[i] = 0;"
    ++ "    }"
    ++ "  } else if (n === 3) {"
    ++ "    nextAlive[i] = 1;"
    ++ "    nextSpecies[i] = bestSp;"
    ++ "    pop++;"
    ++ "  } else {"
    ++ "    nextAlive[i] = 0;"
    ++ "    nextSpecies[i] = 0;"
    ++ "  }"
    ++ "}"
    ++ "}"
    ++ "return pop;"
    ++ "})"

renderGrid ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f 'Unit
renderGrid pixels alive species pal w h px cw =
  discard $
    ffi
      ( "((pixels, alive, species, pal, w, h, px, cw) => {"
          <> "const data = pixels;"
          <> "for (let i = 0; i < data.length; i += 4) {"
          <> "data[i] = 15; data[i + 1] = 23; data[i + 2] = 42; data[i + 3] = 255;"
          <> "}"
          <> "for (let y = 0; y < h; y++) {"
          <> "for (let x = 0; x < w; x++) {"
          <> "const gi = y * w + x;"
          <> "if (alive[gi] !== 1) continue;"
          <> "const sp = species[gi];"
          <> "const base = sp * 3;"
          <> "const r = pal[base], g = pal[base + 1], b = pal[base + 2];"
          <> "const py = y * px, px0 = x * px;"
          <> "for (let dy = 0; dy < px; dy++) {"
          <> "const row = (py + dy) * cw;"
          <> "for (let dx = 0; dx < px; dx++) {"
          <> "const pix = (row + px0 + dx) * 4;"
          <> "data[pix] = r; data[pix + 1] = g; data[pix + 2] = b;"
          <> "}"
          <> "}"
          <> "}"
          <> "}"
          <> "})"
      )
      ( arg pixels
          <: arg alive
          <: arg species
          <: arg pal
          <: arg w
          <: arg h
          <: arg px
          <: arg cw
          <: RecNil
      )

cellIdx :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number -> Expr f 'Number
cellIdx w x y = y * w + x

-- | Toroidal wrap. Requires positive @w@ (grid width/height are fixed constants).
toroidal :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
toroidal w c = rem_ (c + w) w
