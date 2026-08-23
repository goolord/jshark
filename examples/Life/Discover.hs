{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Runtime discovery of emergent life forms, plus the biomass index.
--    Isolation from extension observers is the blob iframe in 'Page'; the
--    index counts on the frame and paints on a timeout.
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
import GHC.Generics (Generic)
import Grid (cellIdx, setU8, u8Get)
import JShark.Api
import qualified JShark.Array as Array
import qualified JShark.Dom as Dom
import JShark.Generic (MutableObjectOf, newRecord)
import qualified JShark.Json as Json
import qualified JShark.Map as Map
import qualified JShark.Math as Math
import JShark.Object (field, frozen)
import qualified JShark.Set as Set
import qualified JShark.Timers as Timers
import JShark.Types (Effect (Lift))
import Names (lookupDisplayName)
import Types
  ( discoverMax
  , gridH
  , gridN
  , gridW
  , indexRefreshMs
  , lifeTypesListId
  , manualSpecies
  , soupSpecies
  )

data Registry

data IndexTracker

data DiscoverScratch = DiscoverScratch
  { nextId :: Double
  , stackLen :: Double
  , minX :: Double
  , minY :: Double
  , w :: Double
  , h :: Double
  , minCells :: Double
  , maxCells :: Double
  , maxSid :: Double
  }
  deriving Generic

data Pt

type instance Field Pt "x" = 'Number

type instance Field Pt "y" = 'Number

data SidCount

type instance Field SidCount "sid" = 'Number

type instance Field SidCount "cnt" = 'Number

initIndexTracker ::
  EffectSyntax f (Effect f ('MutableObject IndexTracker))
initIndexTracker = do
  t <- hold newObject
  _ <- setProp t "lastMs" (number 0)
  pure t

initIndexContainer ::
  EffectSyntax f (Effect f ('MutableObject Dom.DomElement))
initIndexContainer = do
  el <- Dom.lookupId (string lifeTypesListId)
  raw <- bindExpr el
  hold $
    optionCaseE
      (unsafeNullable raw)
      (throw_ (string "missing index grid: life-types"))
      (\hit -> expr hit)

initSeenSpecies :: EffectSyntax f (Effect f ('Set Number))
initSeenSpecies = hold Set.new

initRegistry ::
  EffectSyntax f (Effect f ('MutableObject Registry))
initRegistry = do
  knownPairs <- bindExpr $ Json.unsafeParse (string knownCatalogJson)
  namePairs <- bindExpr $ Json.unsafeParse (string catalogNamesJson)
  known <- hold $ Map.fromEntries knownPairs
  catalogNames <- hold $ Map.fromEntries namePairs
  displayCache <- hold $ Map.fromEntries namePairs
  _ <- Map.insert displayCache (number (fromIntegral soupSpecies)) (string "Soup")
  _ <- Map.insert displayCache (number (fromIntegral manualSpecies)) (string "Manual")
  seen <- hold Map.new
  names <- hold Map.new
  taken <- hold Set.new
  rec <- hold newObject
  knownE <- bindExpr known
  catalogE <- bindExpr catalogNames
  cacheE <- bindExpr displayCache
  seenE <- bindExpr seen
  namesE <- bindExpr names
  takenE <- bindExpr taken
  _ <- setProp rec "known" knownE
  _ <- setProp rec "catalogNames" catalogE
  _ <- setProp rec "seen" seenE
  _ <- setProp rec "names" namesE
  _ <- setProp rec "takenNames" takenE
  _ <- setProp rec "displayCache" cacheE
  pure rec

discoverLife ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f ('MutableObject Registry)
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number, Expr f ('Array 'Number))
discoverLife alive species palette registry nextId0 = do
  let
    w0 = number (fromIntegral gridW)
    h0 = number (fromIntegral gridH)
    n = number (fromIntegral gridN)
  visited <- bindExpr (newByteArray n)
  stackX <- bindExpr (newByteArray n)
  stackY <- bindExpr (newByteArray n)
  minted <- bindExpr $ Array.fromEffects []
  scratch <- hold (newRecord @DiscoverScratch)
  set @"nextId" scratch nextId0
  set @"stackLen" scratch 0
  set @"minX" scratch 0
  set @"minY" scratch 0
  set @"w" scratch w0
  set @"h" scratch h0
  set @"minCells" scratch 4
  set @"maxCells" scratch 72
  set @"maxSid" scratch (number (fromIntegral discoverMax))
  regE <- bindExpr registry
  _ <- setProp scratch "alive" alive
  _ <- setProp scratch "species" species
  _ <- setProp scratch "palette" palette
  _ <- setProp scratch "registry" regE
  _ <- setProp scratch "visited" visited
  _ <- setProp scratch "stackX" stackX
  _ <- setProp scratch "stackY" stackY
  _ <- setProp scratch "minted" minted
  forRange_ (number 0) h0 $ \y ->
    forRange_ (number 0) w0 $ \x -> do
      let
        i = cellIdx w0 x y
      a <- u8Get alive i
      vis <- u8Get visited i
      sp <- u8Get species i
      whenS (a .== 1 .&& vis .== 0 .&& sp .== 0) $
        floodComponent scratch i x y
  nid <- scratch.nextId
  pure (nid, minted)

floodComponent ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
floodComponent scratch i x y = do
  cells <- bindExpr $ Array.fromEffects []
  _ <- setProp scratch "cells" cells
  set @"stackLen" scratch 0
  visited <- getProp scratch "visited"
  setU8 visited i 1
  _ <- Array.push_ cells i
  _ <- pushNbrs scratch x y
  toSyntax_ $
    while_
      ( fromSyntax $ do
          sl <- scratch.stackLen
          toSyntax $ expr (sl .> 0)
      )
      ( fromSyntax $ do
          sl0 <- scratch.stackLen
          let
            sl = sl0 - 1
          set @"stackLen" scratch sl
          stackX <- getProp scratch "stackX"
          stackY <- getProp scratch "stackY"
          cx <- u8Get stackX sl
          cy <- u8Get stackY sl
          _ <- pushNbrs scratch cx cy
          done
      )
  minC <- scratch.minCells
  maxC <- scratch.maxCells
  whenS
    (Array.length cells .>= minC .&& Array.length cells .<= maxC)
    (resolveComponent scratch)

pushNbrs ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
pushNbrs scratch cx cy = do
  _ <- tryPush scratch (cx + 1) cy
  _ <- tryPush scratch (cx - 1) cy
  _ <- tryPush scratch cx (cy + 1)
  tryPush scratch cx (cy - 1)

tryPush ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
tryPush scratch nx ny = do
  w0 <- scratch.w
  h0 <- scratch.h
  whenS (nx .>= 0 .&& ny .>= 0 .&& nx .< w0 .&& ny .< h0) $ do
    let
      ni = cellIdx w0 nx ny
    visited <- getProp scratch "visited"
    alive <- getProp scratch "alive"
    species <- getProp scratch "species"
    vis <- u8Get visited ni
    a <- u8Get alive ni
    sp <- u8Get species ni
    whenS (vis .== 0 .&& a .== 1 .&& sp .== 0) $ do
      setU8 visited ni 1
      cells <- getProp scratch "cells"
      _ <- Array.push_ cells ni
      sl <- scratch.stackLen
      stackX <- getProp scratch "stackX"
      stackY <- getProp scratch "stackY"
      setU8 stackX sl nx
      setU8 stackY sl ny
      set @"stackLen" scratch (sl + 1)

resolveComponent ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> EffectSyntax f (f 'Unit)
resolveComponent scratch = do
  hash <- componentHash scratch
  registry <- getProp scratch "registry"
  knownM <- getProp (Lift registry) "known"
  knownHit <- Map.lookup (Lift knownM) hash
  toSyntax $
    optionCaseE
      knownHit
      ( fromSyntax $ do
          seenM <- getProp (Lift registry) "seen"
          seenHit <- Map.lookup (Lift seenM) hash
          toSyntax $
            optionCaseE
              seenHit
              (fromSyntax (maybeMint scratch hash))
              ( \sid ->
                  fromSyntax $ do
                    species <- getProp scratch "species"
                    cells <- getProp scratch "cells"
                    assignCells species cells sid
              )
      )
      ( \sid ->
          fromSyntax $ do
            species <- getProp scratch "species"
            cells <- getProp scratch "cells"
            assignCells species cells sid
      )

maybeMint ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
maybeMint scratch hash = do
  nid <- scratch.nextId
  maxSid0 <- scratch.maxSid
  whenS (nid .<= maxSid0) $ do
    let
      (r, g, b) = discoverRgb nid
      base = nid * number 3
    palette <- getProp scratch "palette"
    setU8 palette base r
    setU8 palette (base + 1) g
    setU8 palette (base + 2) b
    minted <- getProp scratch "minted"
    _ <- Array.push_ minted nid
    registry <- getProp scratch "registry"
    seenM <- getProp (Lift registry) "seen"
    _ <- Map.insert (Lift seenM) hash nid
    set @"nextId" scratch (nid + 1)
    species <- getProp scratch "species"
    cells <- getProp scratch "cells"
    assignCells species cells nid

assignCells ::
  Expr f 'Uint8Array
  -> Expr f ('Array 'Number)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
assignCells species cells sid =
  forRange_ (number 0) (Array.length cells) $ \k ->
    setU8 species (Array.index cells k) sid

-- | Same order as 'Catalog.shapeHash' (numeric @(x,y)@, then @"x,y"@ join).
--   That is what 'knownCatalogJson' stores — not JS lexicographic string sort.
componentHash ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> EffectSyntax f (Expr f 'String)
componentHash scratch = do
  cells <- getProp scratch "cells"
  w0 <- scratch.w
  set @"minX" scratch (number 1e9)
  set @"minY" scratch (number 1e9)
  forRange_ (number 0) (Array.length cells) $ \k -> do
    let
      i = Array.index cells k
      x = rem_ i w0
      y = Math.floor (i / w0)
    mx <- scratch.minX
    my <- scratch.minY
    whenS (x .< mx) (set @"minX" scratch x)
    whenS (y .< my) (set @"minY" scratch y)
  pts <- bindExpr $ Array.fromEffects []
  mx <- scratch.minX
  my <- scratch.minY
  forRange_ (number 0) (Array.length cells) $ \k -> do
    let
      i = Array.index cells k
      x = rem_ i w0 - mx
      y = Math.floor (i / w0) - my
    Array.push_ pts (pt x y)
  let
    sorted =
      Array.toSorted pts $ \a b ->
        let
          dx = a.x - b.x
         in
          if_ (dx .!= 0) dx (a.y - b.y)
    hash =
      Array.join
        (Array.map sorted $ \p -> toString p.x <> string "," <> toString p.y)
        (string ";")
  pure hash

pt :: Expr f 'Number -> Expr f 'Number -> Expr f ('Object Pt)
pt x y = frozen [field @"x" x, field @"y" y]

sidCount :: Expr f 'Number -> Expr f 'Number -> Expr f ('Object SidCount)
sidCount sid cnt = frozen [field @"sid" sid, field @"cnt" cnt]

discoverRgb ::
  Expr f 'Number -> (Expr f 'Number, Expr f 'Number, Expr f 'Number)
discoverRgb n =
  let
    hue = rem_ (n * number 137.508) (number 360)
    s = number 0.62
    l = number 0.56
    c = (number 1 - abs (number 2 * l - number 1)) * s
    hp = hue / number 60
    hpMod = hp - number 2 * Math.floor (hp / number 2)
    x = c * (number 1 - abs (hpMod - number 1))
    m = l - c / number 2
    r1 =
      if_
        (hp .< 1)
        c
        ( if_
            (hp .< 2)
            x
            (if_ (hp .< 3) 0 (if_ (hp .< 4) 0 (if_ (hp .< 5) x c)))
        )
    g1 =
      if_
        (hp .< 1)
        x
        ( if_
            (hp .< 2)
            c
            (if_ (hp .< 3) c (if_ (hp .< 4) x (if_ (hp .< 5) 0 0)))
        )
    b1 =
      if_
        (hp .< 1)
        0
        ( if_
            (hp .< 2)
            0
            (if_ (hp .< 3) x (if_ (hp .< 4) c (if_ (hp .< 5) c x)))
        )
    clamp t = Math.max 0 (Math.min 255 (Math.round (number 255 * (t + m))))
   in
    (clamp r1, clamp g1, clamp b1)

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
  lastMs <- getProp tracker "lastMs"
  let
    refresh = number (fromIntegral indexRefreshMs)
  whenS (lastMs .== 0 .|| now - lastMs .>= refresh) $ do
    _ <- setProp tracker "lastMs" now
    counts <- bindExpr (newByteArray (number 512))
    forRange_ (number 0) (u8Len alive) $ \i -> do
      a <- u8Get alive i
      whenS (a .== 1) $ do
        sid <- u8Get species i
        incCount counts sid
        Set.insert seen sid
    _ <-
      Timers.setTimeout
        ( \_ ->
            stmts $ paintIndex counts palette registry seen container
        )
        0
    done

-- | 16-bit count in a 512-byte buffer: @lo@ at @sid*2@, @hi@ at @sid*2+1@.
--   Grid is 49152 cells, so one species fits in 16 bits.
incCount :: Expr f 'Uint8Array -> Expr f 'Number -> EffectSyntax f (f 'Unit)
incCount counts sid = do
  let
    base = sid * number 2
  lo <- u8Get counts base
  ifS
    (lo .== 255)
    ( do
        setU8 counts base 0
        hi <- u8Get counts (base + 1)
        setU8 counts (base + 1) (hi + 1)
    )
    (setU8 counts base (lo + 1))

countOf :: Expr f 'Uint8Array -> Expr f 'Number -> Expr f 'Number
countOf counts sid =
  let
    base = sid * number 2
   in
    u8Index counts (base + 1) * number 256 + u8Index counts base

paintIndex ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f ('MutableObject Registry)
  -> Effect f ('Set Number)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
paintIndex counts palette registry seen container = do
  entries <- bindExpr $ Array.fromEffects []
  _ <-
    Set.mapM_
      ( \sid ->
          Array.push_ entries (sidCount sid (countOf counts sid))
      )
      seen
  let
    sorted =
      Array.toSorted entries $ \a b ->
        let
          d = b.cnt - a.cnt
         in
          if_ (d .!= 0) d (a.sid - b.sid)
  _ <- Dom.replaceChildren container
  forRange_ (number 0) (Array.length sorted) $ \idx -> do
    let
      e = Array.index sorted idx
    appendIndexRow container palette registry e.sid e.cnt

appendIndexRow ::
  Effect f ('MutableObject Dom.DomElement)
  -> Expr f 'Uint8Array
  -> Effect f ('MutableObject Registry)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
appendIndexRow container palette registry sid cnt = do
  row <- Dom.createElement (string "div")
  _ <-
    Dom.setAttribute
      row
      "class"
      ( if_
          (cnt .== 0)
          (string "index-row index-row-dead")
          (string "index-row")
      )
  swatch <- Dom.createElement (string "span")
  _ <- Dom.setAttribute swatch "class" (string "swatch")
  let
    base = sid * number 3
  r <- u8Get palette base
  g <- u8Get palette (base + 1)
  b <- u8Get palette (base + 2)
  _ <-
    Dom.setStyleProperty
      swatch
      "background"
      ( string "rgb("
          <> toString r
          <> string ","
          <> toString g
          <> string ","
          <> toString b
          <> string ")"
      )
  nameEl <- Dom.createElement (string "span")
  _ <- Dom.setAttribute nameEl "class" (string "index-name")
  nm <- lookupDisplayName sid registry
  _ <- Dom.setTextContent nameEl nm
  countEl <- Dom.createElement (string "span")
  _ <- Dom.setAttribute countEl "class" (string "index-count")
  _ <- Dom.setTextContent countEl (toString cnt)
  _ <- Dom.appendChild row swatch
  _ <- Dom.appendChild row nameEl
  _ <- Dom.appendChild row countEl
  Dom.appendChild container row
