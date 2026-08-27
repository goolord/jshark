{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | JShark runtime for species shape keys and discovery (replaces Discover.js).
module DiscoverRuntime (classifyAndResolveEffect, collectPhaseKey) where

import Data.Text (Text)
import DiscoverCore (discoverRgb)
import Grid (setU8)
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types (Effect (Lift))
import qualified JShark.Array as Array
import qualified JShark.Map as Map
import qualified JShark.Math as Math
import qualified JShark.Set as Set
import Types (discoverMax, discoverMin)

classifyAndResolveEffect ::
  Effect f ('MutableObject scratch)
  -> Expr f 'Number
  -> Expr f ('Array 'Number)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Effect f ('MutableObject a))
classifyAndResolveEffect scratch w cells nextId0 maxSid0 = do
  registry <- getProp scratch "registry"
  rgbTable <- bindExpr discoverRgbTable
  coords <- extractCoords w cells
  (key, hashes) <- collectPhaseKey coords
  res <- hold newObject
  ifS
    (key .== string "")
    (fillDefault res)
    (fillResolve res registry key hashes nextId0 maxSid0 rgbTable)
  pure res

registryField ::
  Expr f u -> Text -> EffectSyntax f (Expr f v)
registryField reg name =
  bindExpr $
    ffi
      "(r,n)=>r[n]"
      (arg reg <: arg (string name) <: RecNil)

fillDefault ::
  Effect f ('MutableObject a) -> EffectSyntax f (f 'Unit)
fillDefault res = do
  _ <- setProp res "action" (number 0)
  _ <- setProp res "sid" (number 0)
  _ <- setProp res "key" (string "")
  done

coordPair :: Expr f 'Number -> Expr f 'Number -> Effect f ('Array 'Number)
coordPair x y =
  Array.fromEffects [expr x, expr y]

extractCoords ::
  Expr f 'Number
  -> Expr f ('Array 'Number)
  -> EffectSyntax f (Expr f ('Array ('Array 'Number)))
extractCoords w cells = do
  coords <- bindExpr $ Array.fromEffects []
  forRange_ (number 0) (Array.length cells) $ \k -> do
    let
      i = Array.index cells k
      x = rem_ i w
      y = Math.floor (i / w)
    row <- bindExpr $ coordPair x y
    _ <- Array.push_ coords row
    done
  pure coords

normCellsHash ::
  Expr f ('Array ('Array 'Number)) -> EffectSyntax f (Expr f 'String)
normCellsHash coords = do
  st <- hold newObject
  let
    n = Array.length coords
  ifS
    (n .== 0)
    (setProp st "out" (string ""))
    (normCellsHashBody coords st n)
  getProp st "out"

normCellsHashBody ::
  Expr f ('Array ('Array 'Number))
  -> Effect f ('MutableObject a)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
normCellsHashBody coords st n = do
  _ <- setProp st "minX" (number 1e9)
  _ <- setProp st "minY" (number 1e9)
  forRange_ (number 0) n $ \i -> do
    let
      pt = Array.index coords i
      x = Array.index pt 0
      y = Array.index pt 1
    minX <- getProp st "minX"
    minY <- getProp st "minY"
    whenS (x .< minX) (setProp st "minX" x)
    whenS (y .< minY) (setProp st "minY" y)
    done
  pts <- bindExpr $ Array.fromEffects []
  minX <- getProp st "minX"
  minY <- getProp st "minY"
  forRange_ (number 0) n $ \i -> do
    let
      pt = Array.index coords i
      x = Array.index pt 0
      y = Array.index pt 1
    row <- bindExpr $ coordPair (x - minX) (y - minY)
    _ <- Array.push_ pts row
    done
  toSyntax_
    ( Array.sort pts $ \a b ->
        let
          dx = Array.index a 0 - Array.index b 0
         in
          if_ (dx .!= 0) dx (Array.index a 1 - Array.index b 1)
    )
  let
    m = Array.length pts
    p0 = Array.index pts 0
    out0 =
      toString (Array.index p0 0)
        <> string ","
        <> toString (Array.index p0 1)
  _ <- setProp st "out" out0
  forRange_ (number 1) m $ \i -> do
    let
      pt = Array.index pts i
    cur <- getProp st "out"
    _ <-
      setProp st "out" $
        cur
          <> string ";"
          <> toString (Array.index pt 0)
          <> string ","
          <> toString (Array.index pt 1)
    done
  done

transformCoord ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f ('Array 'Number)
  -> Effect f ('Array 'Number)
transformCoord ri flp pt =
  coordPair
    ( if_
        (flp .== 0)
        ( if_
            (ri .== 0)
            (Array.index pt 0)
            ( if_
                (ri .== 1)
                (-Array.index pt 1)
                (if_ (ri .== 2) (-Array.index pt 0) (Array.index pt 1))
            )
        )
        ( let
            x =
              if_
                (ri .== 0)
                (Array.index pt 0)
                ( if_
                    (ri .== 1)
                    (-Array.index pt 1)
                    (if_ (ri .== 2) (-Array.index pt 0) (Array.index pt 1))
                )
           in
            -x
        )
    )
    ( if_
        (ri .== 0)
        (Array.index pt 1)
        ( if_
            (ri .== 1)
            (Array.index pt 0)
            (if_ (ri .== 2) (-Array.index pt 1) (-Array.index pt 0))
        )
    )

canonicalHashFromCoords ::
  Expr f ('Array ('Array 'Number)) -> EffectSyntax f (Expr f 'String)
canonicalHashFromCoords coords = do
  st <- hold newObject
  _ <- setProp st "best" (string "")
  _ <- setProp st "hasBest" false_
  forRange_ (number 0) (number 4) $ \ri ->
    forRange_ (number 0) (number 2) $ \flp -> do
      pts <- bindExpr $ Array.fromEffects []
      forRange_ (number 0) (Array.length coords) $ \i -> do
        let
          pt = Array.index coords i
        row <- bindExpr $ transformCoord ri flp pt
        _ <- Array.push_ pts row
        done
      h <- normCellsHash pts
      hasBest <- getProp st "hasBest"
      ifS
        (not_ hasBest)
        ( do
            _ <- setProp st "best" h
            setProp st "hasBest" true_
        )
        ( do
            best <- getProp st "best"
            whenS (h .< best) (setProp st "best" h)
        )
      done
  getProp st "best"

centroidCoords ::
  Expr f ('Array ('Array 'Number))
  -> EffectSyntax f (Expr f 'Number, Expr f 'Number)
centroidCoords coords = do
  let
    n = Array.length coords
  st <- hold newObject
  _ <- setProp st "sx" (number 0)
  _ <- setProp st "sy" (number 0)
  forRange_ (number 0) n $ \i -> do
    let
      pt = Array.index coords i
      x = Array.index pt 0
      y = Array.index pt 1
    sx <- getProp st "sx"
    sy <- getProp st "sy"
    _ <- setProp st "sx" (sx + x)
    _ <- setProp st "sy" (sy + y)
    done
  sx <- getProp st "sx"
  sy <- getProp st "sy"
  let
    inv = number 1 / n
  pure (sx * inv, sy * inv)

boundsCoords ::
  Expr f ('Array ('Array 'Number))
  -> EffectSyntax f (Expr f 'Number, Expr f 'Number, Expr f 'Number, Expr f 'Number)
boundsCoords coords = do
  st <- hold newObject
  _ <- setProp st "minX" (number 1e9)
  _ <- setProp st "minY" (number 1e9)
  _ <- setProp st "maxX" (number (-1e9))
  _ <- setProp st "maxY" (number (-1e9))
  forRange_ (number 0) (Array.length coords) $ \i -> do
    let
      pt = Array.index coords i
      x = Array.index pt 0
      y = Array.index pt 1
    minX <- getProp st "minX"
    minY <- getProp st "minY"
    maxX <- getProp st "maxX"
    maxY <- getProp st "maxY"
    whenS (x .< minX) (setProp st "minX" x)
    whenS (y .< minY) (setProp st "minY" y)
    whenS (x .> maxX) (setProp st "maxX" x)
    whenS (y .> maxY) (setProp st "maxY" y)
    done
  minX <- getProp st "minX"
  minY <- getProp st "minY"
  maxX <- getProp st "maxX"
  maxY <- getProp st "maxY"
  pure (minX, minY, maxX, maxY)

stampCoords ::
  Expr f ('Array ('Array 'Number))
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Uint8Array)
stampCoords coords ox oy gw gh = do
  grid <- bindExpr $ newByteArray (gw * gh)
  forRange_ (number 0) (Array.length coords) $ \i -> do
    let
      pt = Array.index coords i
      x = Array.index pt 0
      y = Array.index pt 1
      lx = x - ox
      ly = y - oy
      idx = ly * gw + lx
    setU8 grid idx (number 1)
    done
  pure grid

collectLiveLocal ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f ('Array ('Array 'Number)))
collectLiveLocal grid gw gh = do
  live <- bindExpr $ Array.fromEffects []
  forRange2_ (number 0) gh (number 0) gw $ \y x -> do
      let
        idx = y * gw + x
      whenS (u8Index grid idx .== 1) $ do
        row <- bindExpr $ coordPair x y
        _ <- Array.push_ live row
        done
  pure live

sandboxNbr ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
sandboxNbr grid gw gh x y dx dy =
  let
    nx = x + dx
    ny = y + dy
   in
    if_
      (dx .== 0 .&& dy .== 0)
      (number 0)
      ( if_
          (nx .>= 0 .&& ny .>= 0 .&& nx .< gw .&& ny .< gh)
          (if_ (u8Index grid (ny * gw + nx) .== 1) (number 1) (number 0))
          (number 0)
      )

sandboxStepGrid ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Uint8Array)
sandboxStepGrid grid gw gh = do
  out <- bindExpr $ newByteArray (gw * gh)
  forRange2_ (number 0) gh (number 0) gw $ \y x -> do
      let
        n =
          sandboxNbr grid gw gh x y (-1) (-1)
            + sandboxNbr grid gw gh x y 0 (-1)
            + sandboxNbr grid gw gh x y 1 (-1)
            + sandboxNbr grid gw gh x y (-1) 0
            + sandboxNbr grid gw gh x y 1 0
            + sandboxNbr grid gw gh x y (-1) 1
            + sandboxNbr grid gw gh x y 0 1
            + sandboxNbr grid gw gh x y 1 1
        idx = y * gw + x
        alive = u8Index grid idx .== 1
        next =
          if_ (n .== 3) (number 1) (if_ (alive .&& n .== 2) (number 1) (number 0))
      setU8 out idx next
  pure out

collectPhaseKey ::
  Expr f ('Array ('Array 'Number))
  -> EffectSyntax f (Expr f 'String, Expr f ('Array 'String))
collectPhaseKey coords = do
  st <- hold newObject
  let
    n = Array.length coords
  ifS
    (n .== 0)
    ( do
        _ <- setProp st "key" (string "")
        emptyHashes <- bindExpr $ Array.fromEffects []
        setProp st "hashes" emptyHashes
    )
    (collectPhaseKeyBody st coords)
  key <- getProp st "key"
  hashes <- getProp st "hashes"
  pure (key, hashes)

collectPhaseKeyBody ::
  Effect f ('MutableObject a)
  -> Expr f ('Array ('Array 'Number))
  -> EffectSyntax f (f 'Unit)
collectPhaseKeyBody outSt coords = do
  (minX, minY, maxX, maxY) <- boundsCoords coords
  let
    pad = number 2
    ox = minX - pad
    oy = minY - pad
    gw = maxX - minX + number 1 + pad * number 2
    gh = maxY - minY + number 1 + pad * number 2
  grid0 <- stampCoords coords ox oy gw gh
  (c0x, c0y) <- centroidCoords coords
  loopSt <- hold newObject
  _ <- setProp loopSt "grid" grid0
  _ <- setProp loopSt "step" (number 0)
  _ <- setProp loopSt "done" false_
  history <- hold Set.new
  hashes <- bindExpr $ Array.fromEffects []
  forRange_ (number 0) (number 32) $ \_ -> do
    doneFlag <- getProp loopSt "done"
    whenS (not_ doneFlag) $ do
      grid <- getProp loopSt "grid"
      live <- collectLiveLocal grid gw gh
      ifS
        (Array.length live .== 0)
        (setProp loopSt "done" true_)
        ( do
            exact <- normCellsHash live
            dup <- Set.member history exact
            ifS
              dup
              (setProp loopSt "done" true_)
              ( do
                  _ <- Set.insert history exact
                  _ <- Array.push_ hashes exact
                  step <- getProp loopSt "step"
                  whenS
                    (step .> 0)
                    ( do
                        absCoords <- bindExpr $ Array.fromEffects []
                        forRange_ (number 0) (Array.length live) $ \i -> do
                          let
                            pt = Array.index live i
                            x = Array.index pt 0
                            y = Array.index pt 1
                          row <- bindExpr $ coordPair (x + ox) (y + oy)
                          _ <- Array.push_ absCoords row
                          done
                        (cx, cy) <- centroidCoords absCoords
                        whenS
                          (abs (cx - c0x) + abs (cy - c0y) .> number 0.75)
                          (setProp loopSt "done" true_)
                    )
                  stillOpen <- getProp loopSt "done"
                  whenS
                    (not_ stillOpen)
                    ( do
                        nextGrid <- sandboxStepGrid grid gw gh
                        _ <- setProp loopSt "grid" nextGrid
                        step' <- getProp loopSt "step"
                        setProp loopSt "step" (step' + 1)
                    )
              )
        )
    done
  let
    hLen = Array.length hashes
  ifS
    (hLen .> 1)
    ( do
        toSyntax_
          ( Array.sort hashes $ \a b ->
              if_ (a .< b) (number (-1)) (if_ (a .> b) (number 1) (number 0))
          )
        key <- joinSorted hashes
        _ <- setProp outSt "key" key
        setProp outSt "hashes" hashes
    )
    ( do
        key <- canonicalHashFromCoords coords
        _ <- setProp outSt "key" key
        setProp outSt "hashes" hashes
    )

joinSorted :: Expr f ('Array 'String) -> EffectSyntax f (Expr f 'String)
joinSorted sorted = do
  st <- hold newObject
  let
    n = Array.length sorted
  ifS
    (n .== 0)
    (setProp st "out" (string ""))
    ( do
        let
          h0 = Array.index sorted 0
        _ <- setProp st "out" h0
        forRange_ (number 1) n $ \i -> do
          cur <- getProp st "out"
          let
            hi = Array.index sorted i
          _ <- setProp st "out" (cur <> string "|" <> hi)
          done
        done
    )
  getProp st "out"

discoverRgbTable :: Effect f ('Array ('Array 'Number))
discoverRgbTable =
  Array.fromEffects
    [ Array.fromEffects
        [ expr (number (fromIntegral r))
        , expr (number (fromIntegral g))
        , expr (number (fromIntegral b))
        ]
    | sid <- [0 .. 255]
    , let
        (r, g, b) =
          if sid >= discoverMin && sid <= discoverMax
            then discoverRgb sid
            else (0, 0, 0)
    ]

rgbForSid ::
  Expr f ('Array ('Array 'Number))
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number, Expr f 'Number, Expr f 'Number)
rgbForSid table sid = do
  let
    rgb = Array.index table sid
  pure (Array.index rgb 0, Array.index rgb 1, Array.index rgb 2)

registerAliases ::
  Effect f ('Map 'String 'Number)
  -> Expr f ('Array 'String)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
registerAliases seen hashes sid =
  forRange_ (number 0) (Array.length hashes) $ \k -> do
    let
      h = Array.index hashes k
    Map.insert seen h sid
    done

findSidByHashes ::
  Effect f ('Map 'String 'Number)
  -> Expr f ('Array 'String)
  -> EffectSyntax f (Expr f ('Option 'Number))
findSidByHashes seen hashes = do
  st <- hold newObject
  _ <- setProp st "found" false_
  _ <- setProp st "sid" (number 0)
  forRange_ (number 0) (Array.length hashes) $ \k -> do
    found <- getProp st "found"
    whenS (not_ found) $ do
      let
        h = Array.index hashes k
      m <- Map.lookup seen h
      whenSomeS m $ \sid -> do
        _ <- setProp st "found" true_
        setProp st "sid" sid
    done
  found <- getProp st "found"
  sid <- getProp st "sid"
  ifS found (setProp st "hit" (some sid)) (setProp st "hit" none)
  getProp st "hit"

markKnown ::
  Effect f ('MutableObject a)
  -> Effect f ('Map 'String 'Number)
  -> Expr f 'String
  -> Expr f ('Array 'String)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
markKnown res seen key hashes sid = do
  _ <- Map.insert seen key sid
  _ <- registerAliases seen hashes sid
  _ <- setProp res "action" (number 1)
  _ <- setProp res "sid" sid
  _ <- setProp res "key" key
  done

fillPending ::
  Effect f ('MutableObject a)
  -> Effect f ('Map 'String 'Number)
  -> Effect f ('Map 'String 'Number)
  -> Expr f ('Array ('Array 'Number))
  -> Expr f 'String
  -> Expr f ('Array 'String)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
fillPending res seen pending rgbTable key hashes nextId0 maxSid0 = do
  cntHit <- Map.lookup pending key
  let
    cnt0 = orElse cntHit (number 0)
    cnt = cnt0 + 1
  _ <- Map.insert pending key cnt
  ifS
    (cnt .< 2)
    ( do
        _ <- setProp res "action" (number 0)
        _ <- setProp res "sid" (number 0)
        _ <- setProp res "key" key
        done
    )
    ( ifS
        (nextId0 .> maxSid0)
        ( do
            _ <- setProp res "action" (number 0)
            _ <- setProp res "sid" (number 0)
            _ <- setProp res "key" key
            done
        )
        ( do
            (r, g, b) <- rgbForSid rgbTable nextId0
            _ <- Map.insert seen key nextId0
            _ <- registerAliases seen hashes nextId0
            _ <- Map.delete pending key
            _ <- setProp res "action" (number 2)
            _ <- setProp res "sid" nextId0
            _ <- setProp res "r" r
            _ <- setProp res "g" g
            _ <- setProp res "b" b
            _ <- setProp res "key" key
            done
        )
    )

fillResolve ::
  Effect f ('MutableObject a)
  -> Expr f u
  -> Expr f 'String
  -> Expr f ('Array 'String)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f ('Array ('Array 'Number))
  -> EffectSyntax f (f 'Unit)
fillResolve res registry key hashes nextId0 maxSid0 rgbTable = do
  known <- registryField registry "known"
  seen <- registryField registry "seen"
  pending <- registryField registry "pending"
  let
    knownM = Lift known
    seenM = Lift seen
    pendingM = Lift pending
  st <- hold newObject
  _ <- setProp st "resolved" false_
  knownHit <- Map.lookup knownM key
  whenSomeS knownHit $ \sid -> do
    markKnown res seenM key hashes sid
    setProp st "resolved" true_
  stillOpen <- getProp st "resolved"
  whenS (not_ stillOpen) $ do
    seenHit <- Map.lookup seenM key
    whenSomeS seenHit $ \sid -> do
      markKnown res seenM key hashes sid
      setProp st "resolved" true_
  stillOpen2 <- getProp st "resolved"
  whenS (not_ stillOpen2) $ do
    hashHit <- findSidByHashes seenM hashes
    whenSomeS hashHit $ \sid -> do
      _ <- Map.insert seenM key sid
      markKnown res seenM key hashes sid
      setProp st "resolved" true_
  stillOpen3 <- getProp st "resolved"
  whenS
    (not_ stillOpen3)
    (fillPending res seenM pendingM rgbTable key hashes nextId0 maxSid0)
