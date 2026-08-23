{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Runtime discovery of emergent life forms: connected components of soup
--    cells are hashed, matched against the catalog, or minted with a fresh
--    id, golden-angle hue, and procedural name.
module Discover
  ( Registry
  , IndexTracker
  , initRegistry
  , initIndexTracker
  , initIndexContainer
  , initSeenSpecies
  , discoverLife
  , stepIndexTracker
  )
where

import Catalog (catalogNamesJson, knownCatalogJson)
import JShark.Api
import qualified JShark.Dom as Dom
import JShark.Rec (Rec (..), (<:))
import qualified JShark.Set as Set
import JShark.Types (Effect (Lift), Expr (Var))
import Types (indexRefreshMs, lifeIndexHostId, lifeTypesListId, manualSpecies, soupSpecies)

data Registry

data IndexTracker

initIndexTracker ::
  EffectSyntax f (Effect f ('MutableObject IndexTracker))
initIndexTracker =
  hold $
    ffi
      "(() => ({ lastMs: 0, lastFp: '', pending: false }))"
      RecNil

-- | Closed shadow root keeps index DOM mutations off the document tree so
--   document-level extension observers (e.g. video scanners) are not invoked.
--   Index styles are inlined here (duplicated from @life.css@) so the shadow
--   index can evolve independently of the page stylesheet.
initIndexContainer ::
  EffectSyntax f (Effect f ('MutableObject Dom.DomElement))
initIndexContainer =
  hold $
    ffi
      ( "((hostId, listId) => {"
          <> "const host = document.getElementById(hostId);"
          <> "if (!host) throw new Error('missing index host: ' + hostId);"
          <> "let root = host.shadowRoot;"
          <> "if (!root) {"
          <> "  root = host.attachShadow({ mode: 'closed' });"
          <> "  const style = document.createElement('style');"
          <> "  style.textContent = '"
          <> ".life-index-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(13rem,1fr));"
          <> "gap:0.35rem 2rem;font-size:0.85rem;color:#94a3b8;text-align:left}"
          <> ".index-row{display:grid;grid-template-columns:auto 1fr auto;gap:0.45rem;align-items:center}"
          <> ".index-row-dead{opacity:0.45}"
          <> ".index-row-dead .index-name,.index-row-dead .index-count{color:#64748b}"
          <> ".index-row-dead .swatch{filter:grayscale(1)}"
          <> ".index-name{overflow:hidden;text-overflow:ellipsis;white-space:nowrap;color:#cbd5e1}"
          <> ".index-count{font-variant-numeric:tabular-nums;color:#cbd5e1}"
          <> ".swatch{width:0.75rem;height:0.75rem;border-radius:1px;display:inline-block;flex-shrink:0}"
          <> "';"
          <> "  const grid = document.createElement('div');"
          <> "  grid.id = listId;"
          <> "  grid.className = 'life-index-grid';"
          <> "  root.appendChild(style);"
          <> "  root.appendChild(grid);"
          <> "}"
          <> "const el = root.getElementById(listId);"
          <> "if (!el) throw new Error('missing index grid: ' + listId);"
          <> "return el;"
          <> "})"
      )
      (arg (string lifeIndexHostId) <: arg (string lifeTypesListId) <: RecNil)

stepIndexTrackerJs :: String
stepIndexTrackerJs =
  "((alive, species, palette, registry, tracker, seen, container, now, refreshMs, soupSid, manualSid) => {"
    ++ "if (tracker.pending) return;"
    ++ "const lastMs = tracker.lastMs;"
    ++ "if (lastMs !== 0 && now - lastMs < refreshMs) return;"
    ++ "tracker.pending = true;"
    ++ "const escHtml = (s) => String(s)"
    ++ "  .replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;')"
    ++ "  .replace(/\"/g,'&quot;').replace(/'/g,'&#39;');"
    ++ "const displayName = (sid) => {"
    ++ "  const cache = registry.displayCache;"
    ++ "  if (cache.has(sid)) return cache.get(sid);"
    ++ "  const catalog = registry.catalogNames;"
    ++ "  if (catalog.has(sid)) return catalog.get(sid);"
    ++ "  const names = registry.names;"
    ++ "  if (names.has(sid)) return names.get(sid);"
    ++ "  if (sid === soupSid) return 'Soup';"
    ++ "  if (sid === manualSid) return 'Manual';"
    ++ "  return 'Type ' + sid;"
    ++ "};"
    ++ "const run = () => {"
    ++ "  try {"
    ++ "    const counts = new Map();"
    ++ "    for (let i = 0; i < alive.length; i++) {"
    ++ "      if (alive[i] !== 1) continue;"
    ++ "      const sid = species[i];"
    ++ "      seen.add(sid);"
    ++ "      counts.set(sid, (counts.get(sid) || 0) + 1);"
    ++ "    }"
    ++ "    const rows = [];"
    ++ "    for (const sid of seen) rows.push([sid, counts.get(sid) || 0]);"
    ++ "    rows.sort((a, b) => b[1] !== a[1] ? b[1] - a[1] : a[0] - b[0]);"
    ++ "    let fp = '';"
    ++ "    for (let i = 0; i < rows.length; i++) {"
    ++ "      const sid = rows[i][0], cnt = rows[i][1];"
    ++ "      fp += (i ? ',' : '') + sid + ':' + cnt + ':' + displayName(sid);"
    ++ "    }"
    ++ "    if (fp !== tracker.lastFp) {"
    ++ "      tracker.lastFp = fp;"
    ++ "      let html = '';"
    ++ "      for (let i = 0; i < rows.length; i++) {"
    ++ "        const sid = rows[i][0], cnt = rows[i][1];"
    ++ "        const nm = displayName(sid);"
    ++ "        const base = sid * 3;"
    ++ "        const rgb = `rgb(${palette[base]},${palette[base + 1]},${palette[base + 2]})`;"
    ++ "        const dead = cnt === 0 ? ' index-row-dead' : '';"
    ++ "        html += `<div class=\"index-row${dead}\">"
    ++ "<span class=\"swatch\" style=\"background:${rgb}\"></span>"
    ++ "<span class=\"index-name\">${escHtml(nm)}</span>"
    ++ "<span class=\"index-count\">${cnt}</span></div>`;"
    ++ "      }"
    ++ "      container.innerHTML = html;"
    ++ "    }"
    ++ "    tracker.lastMs = now;"
    ++ "  } finally {"
    ++ "    tracker.pending = false;"
    ++ "  }"
    ++ "};"
    ++ "if (typeof requestIdleCallback === 'function') requestIdleCallback(run, { timeout: refreshMs });"
    ++ "else run();"
    ++ "})"

initSeenSpecies :: EffectSyntax f (Effect f ('Set Number))
initSeenSpecies = do
  s <- toSyntax Set.new
  pure (Lift (Var s))

initRegistry ::
  EffectSyntax f (Effect f ('MutableObject Registry))
initRegistry =
  hold $
    ffi
      ( "((catalogJson, namesJson) => {"
          <> "const known = new Map(JSON.parse(catalogJson));"
          <> "const catalogNames = new Map(JSON.parse(namesJson));"
          <> "const displayCache = new Map(catalogNames);"
          <> "displayCache.set("
          <> show soupSpecies
          <> ", 'Soup');"
          <> "displayCache.set("
          <> show manualSpecies
          <> ", 'Manual');"
          <> "return {"
          <> " known, catalogNames,"
          <> " seen: new Map(), names: new Map(),"
          <> " takenNames: new Set(), displayCache"
          <> "};"
          <> "})"
      )
      (arg (string knownCatalogJson) <: arg (string catalogNamesJson) <: RecNil)

discoverLife ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f ('MutableObject Registry)
  -> Expr f 'Number
  -> Expr f 'String
  -> Effect f ('MutableObject ())
discoverLife alive species palette registry nextId recent =
  ffi
    discoverJs
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
    ++ "const mintedSids = [];"
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
    ++ "      mintedSids.push(sid);"
    ++ "      registry.seen.set(hash, sid);"
    ++ "      id++;"
    ++ "    } else continue;"
    ++ "    assign(cells, sid);"
    ++ "  }"
    ++ "}"
    ++ "return { nextId: id, mintedSids };"
    ++ "})"
 where
  gridW = 256 :: Int
  gridH = 192 :: Int
  discoverMax = 255 :: Int

stepIndexTracker ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f ('MutableObject Registry)
  -> Effect f ('MutableObject IndexTracker)
  -> Effect f ('Set Number)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
stepIndexTracker alive species palette registry tracker seen container now = do
  toSyntax_ $
    discard $
      ffi
        stepIndexTrackerJs
        ( arg alive
            <: arg species
            <: arg palette
            <: ArgEffect registry
            <: ArgEffect tracker
            <: ArgEffect seen
            <: ArgEffect container
            <: arg now
            <: arg (number (fromIntegral indexRefreshMs))
            <: arg (number (fromIntegral soupSpecies))
            <: arg (number (fromIntegral manualSpecies))
            <: RecNil
        )
  done
