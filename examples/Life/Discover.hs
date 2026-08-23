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
  , initSeenSpecies
  , discoverLife
  , stepIndexTracker
  )
where

import Catalog (catalogNamesJson, knownCatalogJson)
import Grid (u8Get)
import JShark.Api
import qualified JShark.Array as Array
import qualified JShark.Dom as Dom
import qualified JShark.Json as Json
import qualified JShark.Map as Map
import JShark.Rec (Rec (..), (<:))
import qualified JShark.Set as Set
import Names (nameOfSid)
import JShark.Types (Effect (Lift), Expr (Var))
import Types (gridN, indexRefreshMs)

data Registry

data IndexTracker

initIndexTracker ::
  EffectSyntax f (Effect f ('MutableObject IndexTracker))
initIndexTracker =
  hold $
    ffi
      "(() => ({ lastMs: 0, lastFp: '' }))"
      RecNil

initSeenSpecies :: EffectSyntax f (Effect f ('Set Number))
initSeenSpecies = do
  s <- toSyntax Set.new
  pure (Lift (Var s))

initRegistry ::
  EffectSyntax f (Effect f ('MutableObject Registry))
initRegistry =
  hold $
    fromSyntax $
      do
        knownArr <- bindExpr (Json.unsafeParse (string knownCatalogJson))
        known <- toSyntax (Map.fromEntries knownArr)
        namesArr <- bindExpr (Json.unsafeParse (string catalogNamesJson))
        catalogNames <- toSyntax (Map.fromEntries namesArr)
        seen <- toSyntax Map.new
        names <- toSyntax Map.new
        takenNames <- toSyntax Set.new
        displayCache <- toSyntax Map.new
        toSyntax $
          ffi
            ( "(({ known, catalogNames, seen, names, takenNames, displayCache }) =>"
                <> " ({ known, catalogNames, seen, names, takenNames, displayCache }))"
            )
            ( ArgEffect (Lift (Var known))
                <: ArgEffect (Lift (Var catalogNames))
                <: ArgEffect (Lift (Var seen))
                <: ArgEffect (Lift (Var names))
                <: ArgEffect (Lift (Var takenNames))
                <: ArgEffect (Lift (Var displayCache))
                <: RecNil
            )

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
  _ <-
    forRange_ (number 0) (number (fromIntegral gridN)) $ \i -> do
      a <- u8Get alive i
      whenS (a .== 1) $ do
        sid <- u8Get species i
        _ <- Set.insert seen sid
        done
  lastMs <- getProp tracker "lastMs"
  let refresh = number (fromIntegral indexRefreshMs)
  _ <-
    whenS (lastMs .== 0 .|| (now - lastMs) .>= refresh) $
      Map.withMap $ \current -> do
        _ <- setProp tracker "lastMs" now
        _ <-
          forRange_ (number 0) (number (fromIntegral gridN)) $ \i -> do
            a <- u8Get alive i
            whenS (a .== 1) $ do
              sid <- u8Get species i
              prev <- Map.lookup current sid
              let next = orElse prev (number 0) + 1
              _ <- Map.insert current sid next
              done
        rowsRef <- bindExpr (Array.fromEffects [])
        namesSym <- toSyntax Map.new
        let namesBySid = Lift (Var namesSym)
        _ <-
          Set.mapM_
            ( \sid -> do
                cnt <- Map.lookup current sid
                let n = orElse cnt (number 0)
                nm <- nameOfSid sid registry
                _ <- Map.insert namesBySid sid nm
                row <-
                  bindExpr $
                    ffi
                      "((a, b) => [a, b])"
                      (arg sid <: arg n <: RecNil)
                Array.push_ rowsRef row
            )
            seen
        _ <-
          bindExpr $
            Array.sort rowsRef $ \a b ->
              let_ (Array.index a (number 1)) $ \cntA ->
                let_ (Array.index b (number 1)) $ \cntB ->
                  if_
                    (cntB .== cntA)
                    (Array.index a (number 0) - Array.index b (number 0))
                    (cntB - cntA)
        parts <-
          bindExpr $
            Array.mapE rowsRef $ \row ->
              fromSyntax $ do
                sid <- pure (Array.index row (number 0))
                cnt <- pure (Array.index row (number 1))
                nm <- Map.lookup namesBySid sid
                toSyntax $
                  expr
                    ( toString sid
                        <> string ":"
                        <> toString cnt
                        <> string ":"
                        <> orElse nm (string "?")
                    )
        fp <- pure (Array.join parts (string ","))
        lastFp <- getProp tracker "lastFp"
        _ <-
          whenS (fp .!= lastFp) $ do
            _ <- Dom.replaceChildren container
            _ <-
              forRange_ (number 0) (Array.length rowsRef) $ \i -> do
                row <- pure (Array.index rowsRef i)
                sid <- pure (Array.index row (number 0))
                cnt <- pure (Array.index row (number 1))
                nm <- Map.lookup namesBySid sid
                let label = orElse nm (string "?")
                rowEl <- Dom.createElement (string "div")
                _ <- Dom.classAdd rowEl (string "index-row")
                -- cnt==0: seen before but extinct on the current board
                _ <- whenS (cnt .== 0) (Dom.classAdd rowEl (string "index-row-dead"))
                swatch <- Dom.createElement (string "span")
                _ <- Dom.classAdd swatch (string "swatch")
                let base = sid * number 3
                r <- u8Get palette base
                g <- u8Get palette (base + 1)
                b <- u8Get palette (base + 2)
                rgb <-
                  pure
                    ( string "rgb("
                        <> toString r
                        <> string ","
                        <> toString g
                        <> string ","
                        <> toString b
                        <> string ")"
                    )
                _ <- Dom.setStyleProperty swatch "background" rgb
                nameEl <- Dom.createElement (string "span")
                _ <- Dom.classAdd nameEl (string "index-name")
                _ <- Dom.setTextContent nameEl label
                countEl <- Dom.createElement (string "span")
                _ <- Dom.classAdd countEl (string "index-count")
                _ <- Dom.setTextContent countEl (toString cnt)
                _ <- Dom.appendChild rowEl swatch
                _ <- Dom.appendChild rowEl nameEl
                _ <- Dom.appendChild rowEl countEl
                Dom.appendChild container rowEl
            setProp tracker "lastFp" fp
        toSyntax noOp
  done
