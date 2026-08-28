{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Runtime discovery of emergent life forms, plus the biomass index.
--    The game runs in a plain (unsandboxed) iframe from 'Page'; the
--    index counts on the frame and paints on a timeout.
module Discover
  ( Registry
  , IndexTracker
  , initRegistry
  , initIndexTracker
  , initIndexContainer
  , initIndexTotal
  , initSeenSpecies
  , discoverLife
  , purgeEmergentDiscoveries
  , stepIndexTracker
  )
where

import Catalog
  ( buildKnownMap
  , buildNamesMap
  , catalogAdjectives
  , catalogNouns
  , catalogPrefixes
  , catalogSuffixes
  , catalogVerbsIng
  )
import DiscoverRuntime (classifyAndResolveEffect)
import GHC.Generics (Generic)
import Grid
  ( cellIdx
  , clampLiveBounds
  , packedIsAlive
  , setU8
  , syncPaletteRgbaSid
  , u8Get
  )
import JShark.Api
import JShark.Api.Generic (MutableObjectOf, newRecord)
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types (Effect (Lift))
import qualified JShark.Array as Array
import qualified JShark.Dom as Dom
import JShark.Lucid
  ( JsHtml
  , renderFragment
  , text_
  )
import qualified JShark.Map as Map
import qualified JShark.Math as Math
import JShark.Object (field, frozen)
import qualified JShark.Set as Set
import Lucid (class_, div_, span_)
import Names (lookupDisplayName, refreshTakenNames)
import Patterns (paletteBytes)
import Types
  ( LifeState
  , discoverMax
  , discoverMin
  , indexRefreshMs
  , lifeIndexTotalId
  , lifeTypesListId
  , manualSpecies
  , soupSpecies
  , speciesCount
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

data SidCount

type instance Field SidCount "sid" = 'Number

type instance Field SidCount "cnt" = 'Number

initIndexTracker ::
  EffectSyntax f (Effect f ('MutableObject IndexTracker))
initIndexTracker = do
  t <- hold newObject
  _ <- setProp t "lastMs" (number 0)
  _ <- setProp t "pending" false_
  counts <- bindExpr (newByteArray (number (fromIntegral (speciesCount * 2))))
  _ <- setProp t "counts" counts
  frag <- renderFragment indexRowTemplate
  templateE <- getProp frag "firstChild"
  _ <- setProp t "rowTemplate" templateE
  pure t

initIndexTotal ::
  EffectSyntax f (Effect f ('MutableObject Dom.DomElement))
initIndexTotal = do
  el <- Dom.lookupId (string lifeIndexTotalId)
  raw <- bindExpr el
  hold $
    optionCaseE
      (unsafeNullable raw)
      (throw_ (string "missing index total: life-index-total"))
      (\hit -> expr hit)

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
  prefixes <- bindExpr catalogPrefixes
  suffixes <- bindExpr catalogSuffixes
  nouns <- bindExpr catalogNouns
  adjectives <- bindExpr catalogAdjectives
  verbsIng <- bindExpr catalogVerbsIng
  known <- buildKnownMap
  catalogNamesMap <- buildNamesMap
  displayCache <- buildNamesMap
  _ <- Map.insert displayCache (number (fromIntegral soupSpecies)) (string "Soup")
  _ <-
    Map.insert displayCache (number (fromIntegral manualSpecies)) (string "Manual")
  seen <- hold Map.new
  pending <- hold Map.new
  names <- hold Map.new
  taken <- hold Set.new
  rec <- hold newObject
  knownE <- bindExpr known
  catalogE <- bindExpr catalogNamesMap
  cacheE <- bindExpr displayCache
  seenE <- bindExpr seen
  pendingE <- bindExpr pending
  namesE <- bindExpr names
  takenE <- bindExpr taken
  _ <- setProp rec "known" knownE
  _ <- setProp rec "catalogNames" catalogE
  _ <- setProp rec "seen" seenE
  _ <- setProp rec "pending" pendingE
  _ <- setProp rec "names" namesE
  _ <- setProp rec "takenNames" takenE
  _ <- setProp rec "displayCache" cacheE
  _ <- setProp rec "prefixes" prefixes
  _ <- setProp rec "suffixes" suffixes
  _ <- setProp rec "nouns" nouns
  _ <- setProp rec "adjectives" adjectives
  _ <- setProp rec "verbsIng" verbsIng
  pure rec

discoverLife ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f ('MutableObject Registry)
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number, Expr f ('Array 'Number))
discoverLife
  alive
  species
  palette
  registry
  visited
  stackX
  stackY
  w0
  h0
  x0
  y0
  x1
  y1
  nextId0 = do
    toSyntax_ (u8Fill visited (number 0))
    minted <- bindExpr $ Array.fromEffects []
    scratch <- hold (newRecord @DiscoverScratch)
    set @"nextId" scratch nextId0
    set @"stackLen" scratch 0
    set @"minX" scratch 0
    set @"minY" scratch 0
    set @"w" scratch w0
    set @"h" scratch h0
    set @"minCells" scratch 3
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
    evictCounts <-
      bindExpr (newByteArray (number (fromIntegral (speciesCount * 2))))
    _ <- setProp scratch "evictCounts" evictCounts
    _ <- setProp scratch "evictReady" false_
    -- Margin 1 so unlabeled seeds on the live-bounds halo still flood in.
    let
      (ix0, iy0, ixStop, iyStop) =
        clampLiveBounds w0 h0 x0 y0 x1 y1 (number 1)
    whenS (ixStop .> ix0 .&& iyStop .> iy0) $
      forRange2_ iy0 iyStop ix0 ixStop $ \y x -> do
        let
          i = cellIdx w0 x y
        vis <- u8Get visited i
        sp <- u8Get species i
        whenS (packedIsAlive alive i .&& vis .== 0 .&& sp .== 0) $
          floodComponent scratch i x y
    nid <- scratch.nextId
    pure (nid, minted)

purgeEmergentDiscoveries ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject ())
  -> Effect f ('MutableObject Registry)
  -> Effect f ('MutableObject IndexTracker)
  -> Effect f ('Set Number)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
purgeEmergentDiscoveries
  state
  viewport
  registry
  tracker
  seen
  container
  totalEl
  now = do
    worldW <- state.worldW
    worldH <- state.worldH
    alive <- state.alive
    species <- state.species
    nextSpecies <- state.nextSpecies
    pal <- state.palette
    paletteRgba <- state.paletteRgba
    purgeEmergentCells worldW worldH alive species nextSpecies
    purgeEmergentRegistry registry
    resetDiscoverPaletteSlots pal paletteRgba
    set @"nextDiscover" state (fromIntegral discoverMin)
    set @"recentDiscover" state (string "")
    set @"sceneDirty" state true_
    _ <- setProp viewport "renderPanValid" false_
    _ <- Set.clear seen
    _ <- setProp tracker "lastMs" (number 0)
    _ <- setProp tracker "pending" false_
    liveX0 <- state.boundX0
    liveY0 <- state.boundY0
    liveX1 <- state.boundX1
    liveY1 <- state.boundY1
    stepIndexTracker
      alive
      species
      pal
      registry
      tracker
      seen
      container
      totalEl
      now
      liveX0
      liveY0
      liveX1
      liveY1
      worldW
      worldH

purgeEmergentCells ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> EffectSyntax f (f 'Unit)
purgeEmergentCells worldW worldH alive species nextSpecies = do
  let
    soup = number (fromIntegral soupSpecies)
    keep = number (fromIntegral manualSpecies)
  forRange2_ (number 0) worldH (number 0) worldW $ \y x -> do
    let
      i = cellIdx worldW x y
    whenS (packedIsAlive alive i) $ do
      sid <- u8Get species i
      whenS (sid .!= soup .&& sid .!= keep) $ do
        setU8 species i soup
        setU8 nextSpecies i soup
    done

purgeEmergentRegistry ::
  Effect f ('MutableObject Registry) -> EffectSyntax f (f 'Unit)
purgeEmergentRegistry registry = do
  seenE <- getProp registry "seen"
  _ <- Map.clear (Lift seenE)
  pendingE <- getProp registry "pending"
  _ <- Map.clear (Lift pendingE)
  namesE <- getProp registry "names"
  _ <- Map.clear (Lift namesE)
  cacheE <- getProp registry "displayCache"
  let
    cache = Lift cacheE
  forRange_
    (number (fromIntegral discoverMin))
    (number (fromIntegral discoverMax + 1))
    $ \sid -> Map.delete cache sid
  refreshTakenNames registry

resetDiscoverPaletteSlots ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> EffectSyntax f (f 'Unit)
resetDiscoverPaletteSlots pal paletteRgba = do
  let
    defaults = uint8Array paletteBytes
  forRange_
    (number (fromIntegral discoverMin))
    (number (fromIntegral discoverMax + 1))
    $ \sid -> do
      let
        src = sid * number 3
        dst = sid * number 3
        r = u8Index defaults src
        g = u8Index defaults (src + number 1)
        b = u8Index defaults (src + number 2)
      setU8 pal dst r
      setU8 pal (dst + number 1) g
      setU8 pal (dst + number 2) b
      syncPaletteRgbaSid pal paletteRgba sid
      done

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
  let
    nCells = Array.length cells
  ifS
    (nCells .>= minC .&& nCells .<= maxC)
    ( do
        resolveComponent scratch
        species <- getProp scratch "species"
        sid0 <- u8Get species (Array.index cells (number 0))
        whenS (sid0 .== 0) (carveIsolated scratch)
    )
    (whenS (nCells .>= minC) (carveIsolated scratch))

pushNbrs ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
pushNbrs scratch cx cy = do
  _ <- tryPush scratch (cx + 1) cy
  _ <- tryPush scratch (cx - 1) cy
  _ <- tryPush scratch (cx + 1) (cy + 1)
  _ <- tryPush scratch (cx - 1) (cy + 1)
  _ <- tryPush scratch (cx + 1) (cy - 1)
  _ <- tryPush scratch (cx - 1) (cy - 1)
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
    sp <- u8Get species ni
    whenS (vis .== 0 .&& packedIsAlive alive ni .&& sp .== 0) $ do
      setU8 visited ni 1
      cells <- getProp scratch "cells"
      _ <- Array.push_ cells ni
      sl <- scratch.stackLen
      stackX <- getProp scratch "stackX"
      stackY <- getProp scratch "stackY"
      setU8 stackX sl nx
      setU8 stackY sl ny
      set @"stackLen" scratch (sl + 1)

carveIsolated ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> EffectSyntax f (f 'Unit)
carveIsolated scratch = do
  parent <- getProp scratch "cells"
  budget <- hold newObject
  _ <- setProp budget "left" (number 24)
  forRange_ (number 0) (Array.length parent) $ \k -> do
    left <- getProp budget "left"
    whenS (left .> 0) $ do
      let
        seed = Array.index parent k
      species <- getProp scratch "species"
      alive <- getProp scratch "alive"
      sid <- u8Get species seed
      nbrs <- countLiveNbrs scratch seed
      whenS (sid .== 0 .&& packedIsAlive alive seed .&& nbrs .< number 4) $ do
        _ <- setProp budget "left" (left - 1)
        isolateAndResolve scratch seed
    done

countLiveNbrs ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number)
countLiveNbrs scratch seed = do
  worldW <- scratch.w
  worldH <- scratch.h
  alive <- getProp scratch "alive"
  let
    ox = rem_ seed worldW
    oy = Math.floor (seed / worldW)
  tally <- hold newObject
  _ <- setProp tally "n" (number 0)
  addNbr tally alive worldW worldH (ox + 1) oy
  addNbr tally alive worldW worldH (ox - 1) oy
  addNbr tally alive worldW worldH ox (oy + 1)
  addNbr tally alive worldW worldH ox (oy - 1)
  addNbr tally alive worldW worldH (ox + 1) (oy + 1)
  addNbr tally alive worldW worldH (ox - 1) (oy + 1)
  addNbr tally alive worldW worldH (ox + 1) (oy - 1)
  addNbr tally alive worldW worldH (ox - 1) (oy - 1)
  getProp tally "n"

addNbr ::
  Effect f ('MutableObject a)
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
addNbr tally alive worldW worldH nx ny =
  whenS (nx .>= 0 .&& ny .>= 0 .&& nx .< worldW .&& ny .< worldH) $ do
    let
      ni = cellIdx worldW nx ny
    whenS (packedIsAlive alive ni) $ do
      cur <- getProp tally "n"
      setProp tally "n" (cur + 1)

isolateAndResolve ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
isolateAndResolve scratch seed = do
  worldW <- scratch.w
  let
    ox = rem_ seed worldW
    oy = Math.floor (seed / worldW)
    radius = number 6
    maxIso = number 48
  localCells <- bindExpr $ Array.fromEffects []
  localVis <- hold Set.new
  hitSt <- hold newObject
  _ <- setProp hitSt "wall" false_
  set @"stackLen" scratch 0
  _ <- Set.insert localVis seed
  _ <- Array.push_ localCells seed
  stackX <- getProp scratch "stackX"
  stackY <- getProp scratch "stackY"
  setU8 stackX (number 0) ox
  setU8 stackY (number 0) oy
  set @"stackLen" scratch 1
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
          sx <- getProp scratch "stackX"
          sy <- getProp scratch "stackY"
          cx <- u8Get sx sl
          cy <- u8Get sy sl
          pushIsoNbrs scratch localCells localVis hitSt ox oy radius cx cy
          done
      )
  wall <- getProp hitSt "wall"
  let
    nIso = Array.length localCells
  whenS (not_ wall .&& nIso .>= number 3 .&& nIso .<= maxIso) $ do
    _ <- setProp scratch "cells" localCells
    resolveComponent scratch

pushIsoNbrs ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f ('Array 'Number)
  -> Effect f ('Set Number)
  -> Effect f ('MutableObject a)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
pushIsoNbrs scratch localCells localVis hitSt ox oy radius cx cy = do
  _ <- tryIso scratch localCells localVis hitSt ox oy radius (cx + 1) cy
  _ <- tryIso scratch localCells localVis hitSt ox oy radius (cx - 1) cy
  _ <- tryIso scratch localCells localVis hitSt ox oy radius (cx + 1) (cy + 1)
  _ <- tryIso scratch localCells localVis hitSt ox oy radius (cx - 1) (cy + 1)
  _ <- tryIso scratch localCells localVis hitSt ox oy radius (cx + 1) (cy - 1)
  _ <- tryIso scratch localCells localVis hitSt ox oy radius (cx - 1) (cy - 1)
  _ <- tryIso scratch localCells localVis hitSt ox oy radius cx (cy + 1)
  tryIso scratch localCells localVis hitSt ox oy radius cx (cy - 1)

tryIso ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f ('Array 'Number)
  -> Effect f ('Set Number)
  -> Effect f ('MutableObject a)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
tryIso scratch localCells localVis hitSt ox oy radius nx ny = do
  worldW <- scratch.w
  worldH <- scratch.h
  whenS (nx .>= 0 .&& ny .>= 0 .&& nx .< worldW .&& ny .< worldH) $ do
    let
      ni = cellIdx worldW nx ny
      dist = Math.max (abs (nx - ox)) (abs (ny - oy))
    whenS
      (dist .> radius)
      (setProp hitSt "wall" true_)
    whenS (dist .<= radius) $ do
      seen <- Set.member localVis ni
      alive <- getProp scratch "alive"
      species <- getProp scratch "species"
      sp <- u8Get species ni
      whenS
        (not_ seen .&& packedIsAlive alive ni .&& sp .== 0)
        $ do
          _ <- Set.insert localVis ni
          _ <- Array.push_ localCells ni
          sl <- scratch.stackLen
          stackX <- getProp scratch "stackX"
          stackY <- getProp scratch "stackY"
          setU8 stackX sl nx
          setU8 stackY sl ny
          set @"stackLen" scratch (sl + 1)
    done

resolveComponent ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> EffectSyntax f (f 'Unit)
resolveComponent scratch = do
  w0 <- scratch.w
  cells <- getProp scratch "cells"
  nextId0 <- scratch.nextId
  maxSid0 <- scratch.maxSid
  res <- classifyAndResolveEffect scratch w0 cells nextId0 maxSid0
  action <- getProp res "action"
  sid <- getProp res "sid"
  ifS
    (action .== 1)
    ( do
        species <- getProp scratch "species"
        cells' <- getProp scratch "cells"
        assignCells species cells' sid
    )
    ( ifS
        (action .== 2)
        (mintFresh scratch sid)
        ( whenS (action .== 3) $ do
            victim <- evictLowestDiscover scratch
            hashes <- getProp res "hashes"
            key <- getProp res "key"
            adoptSlot scratch victim key hashes
        )
    )

scratchRegistry ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> EffectSyntax f (Effect f ('MutableObject Registry))
scratchRegistry scratch = do
  regE <- getProp scratch "registry"
  hold (expr regE)

assignCells ::
  Expr f 'Uint8Array
  -> Expr f ('Array 'Number)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
assignCells species cells sid =
  forRange_ (number 0) (Array.length cells) $ \k ->
    setU8 species (Array.index cells k) sid

mintFresh ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
mintFresh scratch sid = do
  paintDiscoverSlot scratch sid
  minted <- getProp scratch "minted"
  _ <- Array.push_ minted sid
  set @"nextId" scratch (sid + 1)
  species <- getProp scratch "species"
  cells' <- getProp scratch "cells"
  assignCells species cells' sid

adoptSlot ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'Number
  -> Expr f 'String
  -> Expr f ('Array 'String)
  -> EffectSyntax f (f 'Unit)
adoptSlot scratch sid key hashes = do
  registry <- scratchRegistry scratch
  seenE <- getProp registry "seen"
  pendingE <- getProp registry "pending"
  let
    seen = Lift seenE
    pending = Lift pendingE
  _ <- Map.insert seen key sid
  forRange_ (number 0) (Array.length hashes) $ \k -> do
    Map.insert seen (Array.index hashes k) sid
    done
  _ <- Map.delete pending key
  paintDiscoverSlot scratch sid
  minted <- getProp scratch "minted"
  _ <- Array.push_ minted sid
  species <- getProp scratch "species"
  cells' <- getProp scratch "cells"
  assignCells species cells' sid
  bumpEvictCount scratch sid (Array.length cells')

paintDiscoverSlot ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
paintDiscoverSlot scratch sid = do
  let
    pal0 = uint8Array paletteBytes
    src = sid * number 3
    r = u8Index pal0 src
    g = u8Index pal0 (src + number 1)
    b = u8Index pal0 (src + number 2)
  palette <- getProp scratch "palette"
  setU8 palette src r
  setU8 palette (src + number 1) g
  setU8 palette (src + number 2) b

evictLowestDiscover ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> EffectSyntax f (Expr f 'Number)
evictLowestDiscover scratch = do
  counts <- ensureEvictCounts scratch
  minted <- getProp scratch "minted"
  let
    minSid = number (fromIntegral discoverMin)
    lastSid = number (fromIntegral discoverMax)
  pick <- hold newObject
  _ <- setProp pick "sid" minSid
  _ <- setProp pick "pop" (number 1.0e12)
  forRange_ minSid (lastSid + number 1) $ \sid -> do
    skip <- mintedThisPass minted sid
    whenS (not_ skip) $ do
      let
        pop = countOf counts sid
      bestPop <- getProp pick "pop"
      whenS (pop .< bestPop) $ do
        _ <- setProp pick "sid" sid
        setProp pick "pop" pop
    done
  victim <- getProp pick "sid"
  evictDiscoverSid scratch victim
  zeroCount counts victim
  pure victim

ensureEvictCounts ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> EffectSyntax f (Expr f 'Uint8Array)
ensureEvictCounts scratch = do
  counts <- getProp scratch "evictCounts"
  ready <- getProp scratch "evictReady"
  whenS (not_ ready) $ do
    toSyntax_ (u8Fill counts (number 0))
    worldW <- scratch.w
    worldH <- scratch.h
    alive <- getProp scratch "alive"
    species <- getProp scratch "species"
    let
      minSid = number (fromIntegral discoverMin)
      lastSid = number (fromIntegral discoverMax)
    forRange2_ (number 0) worldH (number 0) worldW $ \y x -> do
      let
        i = cellIdx worldW x y
      whenS (packedIsAlive alive i) $ do
        sid <- u8Get species i
        whenS (sid .>= minSid .&& sid .<= lastSid) (incCount counts sid)
      done
    _ <- setProp scratch "evictReady" true_
    done
  pure counts

zeroCount :: Expr f 'Uint8Array -> Expr f 'Number -> EffectSyntax f (f 'Unit)
zeroCount counts sid = do
  let
    base = sid * number 2
  setU8 counts base (number 0)
  setU8 counts (base + 1) (number 0)

bumpEvictCount ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
bumpEvictCount scratch sid nAdd = do
  ready <- getProp scratch "evictReady"
  whenS ready $ do
    counts <- getProp scratch "evictCounts"
    forRange_ (number 0) nAdd $ \_ -> incCount counts sid

mintedThisPass ::
  Expr f ('Array 'Number) -> Expr f 'Number -> EffectSyntax f (Expr f 'Bool)
mintedThisPass minted sid = do
  st <- hold newObject
  _ <- setProp st "hit" false_
  forRange_ (number 0) (Array.length minted) $ \i -> do
    whenS (Array.index minted i .== sid) (setProp st "hit" true_)
    done
  getProp st "hit"

evictDiscoverSid ::
  Effect f (MutableObjectOf DiscoverScratch)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
evictDiscoverSid scratch victim = do
  worldW <- scratch.w
  worldH <- scratch.h
  alive <- getProp scratch "alive"
  species <- getProp scratch "species"
  let
    soup = number (fromIntegral soupSpecies)
  forRange2_ (number 0) worldH (number 0) worldW $ \y x -> do
    let
      i = cellIdx worldW x y
    whenS (packedIsAlive alive i) $ do
      sid <- u8Get species i
      whenS (sid .== victim) (setU8 species i soup)
    done
  registry <- scratchRegistry scratch
  seenE <- getProp registry "seen"
  namesE <- getProp registry "names"
  cacheE <- getProp registry "displayCache"
  let
    seen = Lift seenE
    names = Lift namesE
    cache = Lift cacheE
  dropKeys <- bindExpr $ Array.fromEffects []
  _ <-
    Map.mapM_
      ( \k v ->
          whenS (v .== victim) (Array.push_ dropKeys k)
      )
      seen
  forRange_ (number 0) (Array.length dropKeys) $ \i -> do
    Map.delete seen (Array.index dropKeys i)
    done
  _ <- Map.delete names victim
  Map.delete cache victim

sidCount :: Expr f 'Number -> Expr f 'Number -> Expr f ('Object SidCount)
sidCount sid cnt = frozen [field @"sid" sid, field @"cnt" cnt]

stepIndexTracker ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f ('MutableObject Registry)
  -> Effect f ('MutableObject IndexTracker)
  -> Effect f ('Set Number)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
stepIndexTracker
  alive
  species
  palette
  registry
  tracker
  seen
  container
  totalEl
  now
  x0
  y0
  x1
  y1
  w0
  h0 = do
    pending <- getProp tracker "pending"
    lastMs <- getProp tracker "lastMs"
    let
      refresh = number (fromIntegral indexRefreshMs)
      (ix0, iy0, ixStop, iyStop) =
        clampLiveBounds w0 h0 x0 y0 x1 y1 (number 0)
    whenS
      (not_ pending .&& (lastMs .== 0 .|| now - lastMs .>= refresh))
      ( do
          _ <- setProp tracker "lastMs" now
          _ <- setProp tracker "pending" true_
          counts <- getProp tracker "counts"
          toSyntax_ (u8Fill counts (number 0))
          whenS (ixStop .> ix0 .&& iyStop .> iy0) $
            forRange_ iy0 iyStop $ \y ->
              forRange_ ix0 ixStop $ \x -> do
                let
                  i = cellIdx w0 x y
                whenS (packedIsAlive alive i) $ do
                  sid <- u8Get species i
                  _ <- incCount counts sid
                  Set.insert seen sid
          paintIndex tracker counts palette registry seen container totalEl
          done
      )

-- | 16-bit count: @lo@ at @sid*2@, @hi@ at @sid*2+1@. Saturates at 65535.
incCount :: Expr f 'Uint8Array -> Expr f 'Number -> EffectSyntax f (f 'Unit)
incCount counts sid = do
  let
    base = sid * number 2
  lo <- u8Get counts base
  hi <- u8Get counts (base + 1)
  whenS (not_ (lo .== 255 .&& hi .== 255)) $
    ifS
      (lo .== 255)
      ( do
          setU8 counts base (number 0)
          setU8 counts (base + 1) (hi + 1)
      )
      (setU8 counts base (lo + 1))

countOf :: Expr f 'Uint8Array -> Expr f 'Number -> Expr f 'Number
countOf counts sid =
  let
    base = sid * number 2
   in
    u8Index counts (base + 1) * number 256 + u8Index counts base

-- | Offline Lucid template; live rows are 'cloneNode' copies patched via
-- 'setStyleProperty' / 'setTextContent'.
indexRowTemplate :: JsHtml f ()
indexRowTemplate =
  div_ [class_ "index-row"] $ do
    span_ [class_ "swatch"] mempty
    div_ [class_ "index-body"] $ do
      span_ [class_ "index-name"] (text_ "")
      div_ [class_ "index-bar-track"] $
        div_ [class_ "index-bar-fill"] mempty
    span_ [class_ "index-count"] (text_ "0")

cloneIndexRow ::
  Effect f u
  -> Expr f 'Uint8Array
  -> Effect f ('MutableObject Registry)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Effect f ('MutableObject Dom.DomElement))
cloneIndexRow template palette registry sid cnt maxCnt = do
  row <- hold $ callMethod template "cloneNode" (arg true_ <: RecNil)
  swatch <-
    hold $ callMethod row "querySelector" (arg (string ".swatch") <: RecNil)
  nameEl <-
    hold $ callMethod row "querySelector" (arg (string ".index-name") <: RecNil)
  barFill <-
    hold $ callMethod row "querySelector" (arg (string ".index-bar-fill") <: RecNil)
  countEl <-
    hold $ callMethod row "querySelector" (arg (string ".index-count") <: RecNil)
  let
    base = sid * number 3
  r <- u8Get palette base
  g <- u8Get palette (base + 1)
  b <- u8Get palette (base + 2)
  let
    rgb =
      string "rgb("
        <> toString r
        <> string ","
        <> toString g
        <> string ","
        <> toString b
        <> string ")"
    dead = cnt .== 0
    pct =
      if_ (maxCnt .> 0) (cnt / maxCnt * number 100) (number 0)
  nm <- lookupDisplayName sid registry
  _ <- Dom.setStyleProperty swatch "background" rgb
  _ <- Dom.setStyleProperty barFill "width" (toString pct <> string "%")
  _ <- Dom.setStyleProperty barFill "background" rgb
  _ <- Dom.setTextContent nameEl nm
  _ <- Dom.setTextContent countEl (toString cnt)
  toSyntax_ $
    callMethod
      row
      "classList.toggle"
      (arg (string "index-row-dead") <: arg dead <: RecNil)
  pure row

paintIndex ::
  Effect f ('MutableObject IndexTracker)
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Effect f ('MutableObject Registry)
  -> Effect f ('Set Number)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
paintIndex tracker counts palette registry seen container totalEl = do
  _ <-
    Set.mapM_
      ( \sid ->
          whenS (countOf counts sid .== 0) (Set.delete seen sid)
      )
      seen
  entries <- bindExpr $ Array.fromEffects []
  _ <-
    Set.mapM_
      ( \sid -> do
          let
            cnt = countOf counts sid
          whenS (cnt .> 0) (Array.push_ entries (sidCount sid cnt))
      )
      seen
  toSyntax_
    ( Array.sort entries $ \a b ->
        let
          d = b.cnt - a.cnt
         in
          if_ (d .!= 0) d (a.sid - b.sid)
    )
  let
    n = Array.length entries
    maxCnt =
      if_ (n .> 0) (Array.index entries (number 0)).cnt (number 1)
  _ <- setProp tracker "indexTotal" (number 0)
  forRange_ (number 0) n $ \idx -> do
    let
      e = Array.index entries idx
    cur <- getProp tracker "indexTotal"
    _ <- setProp tracker "indexTotal" (cur + e.cnt)
    done
  total <- getProp tracker "indexTotal"
  let
    label =
      toString total
        <> string " cells · "
        <> toString n
        <> string " types"
  _ <- Dom.setTextContent totalEl label
  templateE <- getProp tracker "rowTemplate"
  fragH <- hold $ ffi "document.createDocumentFragment" RecNil
  forRange_ (number 0) n $ \idx -> do
    let
      e = Array.index entries idx
    row <-
      cloneIndexRow (expr templateE) palette registry e.sid e.cnt maxCnt
    Dom.appendChild fragH row
  Dom.replaceChildrenFrom container fragH
  setProp tracker "pending" false_
