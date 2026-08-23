{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Runtime discovery of emergent life forms: connected components of soup
   cells are hashed, matched against the catalog, or minted with a fresh
   id, golden-angle hue, and procedural name.
-}
module Discover
  ( Registry
  , initRegistry
  , discoverLife
  )
where

import Catalog (knownCatalogJson)
import JShark.Api
import JShark.Rec (Rec (..), (<:))

data Registry

initRegistry ::
  EffectSyntax f (Effect f ('MutableObject Registry))
initRegistry = do
  hold $
    ffi
      ( "((catalogJson) => {"
          <> "const known = new Map(JSON.parse(catalogJson));"
          <> "return { known, seen: new Map(), names: new Map() };"
          <> "})"
      )
      (arg (string knownCatalogJson) <: RecNil)

discoverLife ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f ('MutableObject Registry)
  -> Expr f 'Number
  -> Expr f 'String
  -> Effect f ('MutableObject ())
discoverLife alive species palette registry nextId recent =
  ffi discoverJs
    ( arg alive
        <: arg species
        <: arg palette
        <: ArgEffect registry
        <: arg nextId
        <: arg recent
        <: RecNil
    )

discoverJs :: String
discoverJs =
  "((alive, species, palette, registry, nextId, recent) => {"
    ++ "const w = "
    ++ show gridW
    ++ ", h = "
    ++ show gridH
    ++ ";"
    ++ "const minCells = 4, maxCells = 72;"
    ++ "const visited = new Uint8Array(w * h);"
    ++ "let id = nextId;"
    ++ "let label = recent;"
    ++ "const prefixes = ['Nova','Mira','Axon','Zeph','Luma','Vex','Quin','Orb','Nex','Sol','Kael','Rune','Pyro','Cyan','Dusk'];"
    ++ "const suffixes = ['morph','form','life','cell','oid','ium','ula','bit','zen','pod','wave','spark','mote','plex','drift'];"
    ++ "const makeName = (n) => {"
    ++ "  const p = prefixes[n % prefixes.length];"
    ++ "  const s = suffixes[(n * 7) % suffixes.length];"
    ++ "  const t = Math.floor(n / prefixes.length);"
    ++ "  return p + s + (t ? String(t) : '');"
    ++ "};"
    ++ "const discoverColor = (n) => {"
    ++ "  const hue = (n * 137.508) % 360;"
    ++ "  const s = 0.62, l = 0.56;"
    ++ "  const c = (1 - Math.abs(2 * l - 1)) * s;"
    ++ "  const hp = hue / 60;"
    ++ "  const hpMod = hp - 2 * Math.floor(hp / 2);"
    ++ "  const x = c * (1 - Math.abs(hpMod - 1));"
    ++ "  let r1, g1, b1;"
    ++ "  if (hp < 1) { r1 = c; g1 = x; b1 = 0; }"
    ++ "  else if (hp < 2) { r1 = x; g1 = c; b1 = 0; }"
    ++ "  else if (hp < 3) { r1 = 0; g1 = c; b1 = x; }"
    ++ "  else if (hp < 4) { r1 = 0; g1 = x; b1 = c; }"
    ++ "  else if (hp < 5) { r1 = x; g1 = 0; b1 = c; }"
    ++ "  else { r1 = c; g1 = 0; b1 = x; }"
    ++ "  const m = l - c / 2;"
    ++ "  const clamp = (n) => Math.max(0, Math.min(255, Math.round(255 * (n + m))));"
    ++ "  return [clamp(r1), clamp(g1), clamp(b1)];"
    ++ "};"
    ++ "const shapeHash = (cells) => {"
    ++ "  let minX = 1e9, minY = 1e9;"
    ++ "  for (const i of cells) {"
    ++ "    const x = i % w, y = (i / w) | 0;"
    ++ "    if (x < minX) minX = x; if (y < minY) minY = y;"
    ++ "  }"
    ++ "  const pts = cells.map((i) => {"
    ++ "    const x = i % w, y = (i / w) | 0;"
    ++ "    return (x - minX) + ',' + (y - minY);"
    ++ "  }).sort();"
    ++ "  return pts.join(';');"
    ++ "};"
    ++ "const paint = (sid, rgb) => {"
    ++ "  const base = sid * 3;"
    ++ "  palette[base] = rgb[0]; palette[base + 1] = rgb[1]; palette[base + 2] = rgb[2];"
    ++ "};"
    ++ "const assign = (cells, sid) => { for (const i of cells) species[i] = sid; };"
    ++ "for (let y = 0; y < h; y++) {"
    ++ "  for (let x = 0; x < w; x++) {"
    ++ "    const i = y * w + x;"
    ++ "    if (!alive[i] || visited[i] || species[i] !== 0) continue;"
    ++ "    const stack = [[x, y]];"
    ++ "    const cells = [];"
    ++ "    while (stack.length) {"
    ++ "      const [cx, cy] = stack.pop();"
    ++ "      const ci = cy * w + cx;"
    ++ "      if (cx < 0 || cy < 0 || cx >= w || cy >= h) continue;"
    ++ "      if (visited[ci] || !alive[ci] || species[ci] !== 0) continue;"
    ++ "      visited[ci] = 1;"
    ++ "      cells.push(ci);"
    ++ "      stack.push([cx + 1, cy], [cx - 1, cy], [cx, cy + 1], [cx, cy - 1]);"
    ++ "    }"
    ++ "    if (cells.length < minCells || cells.length > maxCells) continue;"
    ++ "    const hash = shapeHash(cells);"
    ++ "    let sid;"
    ++ "    if (registry.known.has(hash)) sid = registry.known.get(hash);"
    ++ "    else if (registry.seen.has(hash)) sid = registry.seen.get(hash);"
    ++ "    else if (id <= "
    ++ show discoverMax
    ++ ") {"
    ++ "      sid = id;"
    ++ "      const rgb = discoverColor(sid);"
    ++ "      paint(sid, rgb);"
    ++ "      const name = makeName(sid);"
    ++ "      registry.seen.set(hash, sid);"
    ++ "      registry.names.set(sid, name);"
    ++ "      label = name;"
    ++ "      id++;"
    ++ "    } else continue;"
    ++ "    assign(cells, sid);"
    ++ "  }"
    ++ "}"
    ++ "return { nextId: id, recent: label };"
    ++ "})"
 where
  gridW = 256 :: Int
  gridH = 192 :: Int
  discoverMax = 255 :: Int
