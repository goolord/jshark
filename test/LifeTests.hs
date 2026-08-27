{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LifeTests (lifeTests) where

import qualified Data.Text as T
import EngineFinish (finishStep, initEngineGrids)
import Grid (StepCtx (StepCtx), rebuildPackedCounts, setU8)
import JShark.Api
import JShark.Api.Generic (toObject)
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Array as Array
import JShark.Bun (evaluateEffectJSON)
import qualified JShark.Math as Math
import LifeTestSupport
  ( beehiveCoords
  , blinkerHorizontalCoords
  , blinkerLutStepJson
  , blinkerVerticalCoords
  , blockCoords
  , coordsMatch
  , gridPop
  , runProcessCellAt
  , runStepGridOnce
  , seedBeehive
  , seedBlinkerHorizontal
  , seedBlock
  , setAlive
  )
import qualified LifeTestSupport as LifeAssert
import qualified Lut
import LutCore (computeNextByte, lifeLutEntry)
import System.Directory (findExecutable)
import Test.Tasty
import Test.Tasty.HUnit
import Types (canvasH, canvasW, cellPx, zoomLevelLabels, zoomLevels)
import WorkerBridge (engineStepGeneration)

lifeTests :: TestTree
lifeTests =
  withResource (findExecutable "bun") (const (pure ())) $ \getBun ->
    testGroup
      "life conway"
      [ testCase "bun is on PATH" $ do
          m <- getBun
          case m of
            Nothing -> assertFailure "bun not found on PATH"
            Just _ -> pure ()
      , testGroup
          "core rules"
          [ lifeCase "underpopulation kills live cell" testUnderpopulation
          , lifeCase "survival with two neighbors" testSurvival
          , lifeCase "overpopulation kills live cell" testOverpopulation
          , lifeCase "reproduction on three neighbors" testReproduction
          ]
      , testGroup
          "stepGrid patterns"
          [ lifeCase "block is stable for three generations" testBlockStable
          , lifeCase "beehive is stable for three generations" testBeehiveStable
          , lifeCase "blinker oscillates with period two" testBlinkerPeriod2
          ]
      , testGroup
          "zoom ladder"
          [ testCase "zoomStep labels match levels" $
              length zoomLevels @?= length zoomLevelLabels
          , testCase "zoomLevels ascend from 50% to 600%" $ do
              case zoomLevels of
                (lo : _ : _) -> lo @?= 0.5
                _ -> assertFailure "zoomLevels too short"
              case reverse zoomLevels of
                (hi : _) -> hi @?= 6
                _ -> assertFailure "zoomLevels too short"
              assertBool
                "ascending"
                (and (zipWith (<) zoomLevels (drop 1 zoomLevels)))
          , lifeCase
              "gridFromPointer inverts centerPan at canvas center"
              testViewportGridCoord
          ]
      , testGroup
          "lut engine"
          [ lifeCase "LifeLUT.stepCell matches Conway rules" testLutStepCell
          , lifeCase "LifeLUT.stepRegionLUT matches stepCell on blinker" testLutRegion
          , lifeCase
              "LifeLUT.stepRegionLUT matches stepCell for glider on 8-cell seam"
              testLutGliderSeam
          , lifeCase "finishStep keeps block stable" testFinishStepBlock
          , lifeCase "finishStep rebuilds packed counts" testFinishStepPacked
          , lifeCase "engineStepGeneration keeps block stable" testEngineStepGeneration
          , lifeCase "initEngineGrids allocates LUT and grids" testEngineInit
          , lifeCase "stepRegionLUT row slices match full LUT step" testLutStepTile
          , testCase "LutCore computeNextByte matches table entry" $
              lifeLutEntry 0x0101 @?= computeNextByte 1 1 0 0 0 0 0 0 0
          , testCase "blinker LUT step keeps three live cells" $
              testLutCoreBlinker
          ]
      ]

lifeCase :: String -> (forall f. Effect f 'Unit) -> TestTree
lifeCase name eff = testCase name $ do
  got <- T.unpack <$> evaluateEffectJSON eff
  assertBool (name ++ " should complete") (got == "undefined" || got == "null")

testLutStepCell :: forall f. Effect f 'Unit
testLutStepCell = fromSyntax $ do
  alive <- bindExpr (newByteArray (number 25))
  next <- bindExpr (newByteArray (number 25))
  toSyntax_ (u8Fill alive (number 0))
  toSyntax_ (u8Fill next (number 0))
  _ <- setU8 alive (number 6) (number 1)
  _ <- setU8 alive (number 7) (number 1)
  _ <- setU8 alive (number 11) (number 1)
  _ <- Lut.stepCell alive next (number 5) (number 5) (number 2) (number 2)
  LifeAssert.assertEqual (number 1) (u8Index next (number 12))
  toSyntax_ (u8Fill alive (number 0))
  toSyntax_ (u8Fill next (number 0))
  _ <- setU8 alive (number 6) (number 1)
  _ <- Lut.stepCell alive next (number 5) (number 5) (number 1) (number 1)
  LifeAssert.assertEqual (number 0) (u8Index next (number 6))
  done

testLutRegion :: forall f. Effect f 'Unit
testLutRegion = fromSyntax $ do
  let
    w = number 8
    h = number 8
    n = w * h
  lut <- Lut.createLifeLUT
  a <- bindExpr (newByteArray n)
  b <- bindExpr (newByteArray n)
  c <- bindExpr (newByteArray n)
  _ <- setU8 a (number 18) (number 1)
  _ <- setU8 a (number 19) (number 1)
  _ <- setU8 a (number 20) (number 1)
  _ <- Lut.stepRegionLUT lut a b w h (number 0) h
  forRange_ (number 0) h $ \y ->
    forRange_ (number 0) w $ \x -> do
      _ <- Lut.stepCell a c w h x y
      let
        i = y * w + x
        bi = u8Index b i
        ci = u8Index c i
      whenS (bitAnd bi (number 1) .!= bitAnd ci (number 1)) $ do
        toSyntax_ $ ffi "(()=>{throw new Error('lut mismatch');})" RecNil
        done
      done
  done

testLutGliderSeam :: forall f. Effect f 'Unit
testLutGliderSeam = fromSyntax $ do
  let
    w = number 16
    h = number 16
    n = w * h
  lut <- Lut.createLifeLUT
  a <- bindExpr (newByteArray n)
  b <- bindExpr (newByteArray n)
  c <- bindExpr (newByteArray n)
  _ <- setU8 a (number (8 + 2 * 16)) (number 1)
  _ <- setU8 a (number (9 + 3 * 16)) (number 1)
  _ <- setU8 a (number (7 + 4 * 16)) (number 1)
  _ <- setU8 a (number (8 + 4 * 16)) (number 1)
  _ <- setU8 a (number (9 + 4 * 16)) (number 1)
  _ <- Lut.stepRegionLUT lut a b w h (number 0) h
  forRange_ (number 0) h $ \y ->
    forRange_ (number 0) w $ \x -> do
      _ <- Lut.stepCell a c w h x y
      let
        i = y * w + x
      whenS (bitAnd (u8Index b i) (number 1) .!= bitAnd (u8Index c i) (number 1)) $ do
        toSyntax_ $ ffi "(()=>{throw new Error('seam mismatch');})" RecNil
        done
      done
  st <- hold newObject
  dst0 <- bindExpr (newByteArray n)
  _ <- setProp st "src" b
  _ <- setProp st "dst" dst0
  forRange_ (number 0) (number 3) $ \_ -> do
    src <- getProp st "src"
    dst <- getProp st "dst"
    _ <- Lut.stepRegionLUT lut src dst w h (number 0) h
    _ <- setProp st "src" dst
    setProp st "dst" src
    done
  src <- getProp st "src"
  popN <- gridPop src w h
  LifeAssert.assertEqual (number 5) popN
  done

testFinishStepBlock :: forall f. Effect f 'Unit
testFinishStepBlock = fromSyntax $ do
  let
    w = number 8
    h = number 8
  alive <- bindExpr (newByteArray (w * h))
  species <- bindExpr (newByteArray (w * h))
  nextAlive <- bindExpr (newByteArray (w * h))
  nextSpecies <- bindExpr (newByteArray (w * h))
  (lut, gridA, gridB) <- initEngineGrids (w * h)
  seedBlock alive w h
  rebuildPackedCounts alive w h
  nextLiveList <- bindExpr $ Array.fromEffects []
  nextChangedList <- bindExpr $ Array.fromEffects []
  stepCtx <- hold (toObject (StepCtx 0 0 (-1) (-1) 0 0 0 0 0))
  _ <-
    finishStep
      alive
      species
      nextAlive
      nextSpecies
      gridA
      gridB
      lut
      w
      h
      (number 0)
      (number 0)
      (number 7)
      (number 7)
      nextLiveList
      nextChangedList
      stepCtx
  popN <- stepCtx.pop
  LifeAssert.assertEqual (number 4) popN
  _ <-
    finishStep
      alive
      species
      nextAlive
      nextSpecies
      gridA
      gridB
      lut
      w
      h
      (number 0)
      (number 0)
      (number 7)
      (number 7)
      nextLiveList
      nextChangedList
      stepCtx
  popN2 <- stepCtx.pop
  LifeAssert.assertEqual (number 4) popN2
  done

testFinishStepPacked :: forall f. Effect f 'Unit
testFinishStepPacked = fromSyntax $ do
  let
    w = number 8
    h = number 8
  alive <- bindExpr (newByteArray (w * h))
  species <- bindExpr (newByteArray (w * h))
  nextAlive <- bindExpr (newByteArray (w * h))
  nextSpecies <- bindExpr (newByteArray (w * h))
  (lut, gridA, gridB) <- initEngineGrids (w * h)
  _ <- setU8 alive (number 9) (number 1)
  _ <- setU8 alive (number 10) (number 1)
  _ <- setU8 alive (number 17) (number 1)
  _ <- setU8 alive (number 18) (number 1)
  rebuildPackedCounts alive w h
  nextLiveList <- bindExpr $ Array.fromEffects []
  nextChangedList <- bindExpr $ Array.fromEffects []
  stepCtx <- hold (toObject (StepCtx 0 0 (-1) (-1) 0 0 0 0 0))
  _ <-
    finishStep
      alive
      species
      nextAlive
      nextSpecies
      gridA
      gridB
      lut
      w
      h
      (number 0)
      (number 0)
      (number 7)
      (number 7)
      nextLiveList
      nextChangedList
      stepCtx
  popN <- stepCtx.pop
  LifeAssert.assertEqual (number 4) popN
  LifeAssert.assertEqual
    (number 1)
    (bitAnd (u8Index nextAlive (number 9)) (number 1))
  whenS (shr (u8Index nextAlive (number 9)) (number 1) .== 0) $ do
    toSyntax_ $ ffi "(()=>{throw new Error('packed count');})" RecNil
    done
  done

testEngineInit :: forall f. Effect f 'Unit
testEngineInit = fromSyntax $ do
  (_lut, gridA, gridB) <- initEngineGrids (number 64)
  lenA <- bindExpr $ ffi "(a=>a.length)" (arg gridA <: RecNil)
  lenB <- bindExpr $ ffi "(a=>a.length)" (arg gridB <: RecNil)
  LifeAssert.assertEqual (number 64) lenA
  LifeAssert.assertEqual (number 64) lenB
  done

testLutStepTile :: forall f. Effect f 'Unit
testLutStepTile = fromSyntax $ do
  let
    w = number 8
    h = number 8
    n = w * h
  lut <- Lut.createLifeLUT
  a <- bindExpr (newByteArray n)
  full <- bindExpr (newByteArray n)
  tile <- bindExpr (newByteArray n)
  _ <- setU8 a (number 18) (number 1)
  _ <- setU8 a (number 19) (number 1)
  _ <- setU8 a (number 20) (number 1)
  _ <- Lut.stepRegionLUT lut a full w h (number 0) h
  forRange_ (number 0) h $ \y0 -> do
    _ <- Lut.stepRegionLUT lut a tile w h y0 (y0 + number 1)
    forRange_ (number 0) w $ \x -> do
      let
        i = y0 * w + x
      whenS
        (bitAnd (u8Index full i) (number 1) .!= bitAnd (u8Index tile i) (number 1))
        $ do
          toSyntax_ $ ffi "(()=>{throw new Error('stepTile mismatch');})" RecNil
          done
    done
  done

-- | 7×7 miniature grid helpers shared by rule tests.
miniGrid ::
  ( Expr f 'Uint8Array
    -> Expr f 'Uint8Array
    -> Expr f 'Uint8Array
    -> Expr f 'Uint8Array
    -> Expr f 'Number
    -> EffectSyntax f b
  )
  -> EffectSyntax f b
miniGrid k = do
  let
    w = number 7
    h = number 7
  alive <- bindExpr (newByteArray (w * h))
  species <- bindExpr (newByteArray (w * h))
  nextAlive <- bindExpr (newByteArray (w * h))
  nextSpecies <- bindExpr (newByteArray (w * h))
  k alive species nextAlive nextSpecies w

testUnderpopulation :: forall f. Effect f 'Unit
testUnderpopulation = fromSyntax $ do
  miniGrid $ \alive species nextAlive nextSpecies w -> do
    toSyntax_ (u8Fill alive (number 0))
    setAlive alive w (number 3) (number 2)
    setAlive alive w (number 3) (number 3)
    rebuildPackedCounts alive w (number 7)
    next <-
      runProcessCellAt
        alive
        species
        nextAlive
        nextSpecies
        w
        (number 7)
        (number 3)
        (number 3)
    LifeAssert.assertEqual (number 0) next
    done

testSurvival :: forall f. Effect f 'Unit
testSurvival = fromSyntax $ do
  miniGrid $ \alive species nextAlive nextSpecies w -> do
    toSyntax_ (u8Fill alive (number 0))
    setAlive alive w (number 3) (number 2)
    setAlive alive w (number 4) (number 3)
    setAlive alive w (number 3) (number 3)
    rebuildPackedCounts alive w (number 7)
    next <-
      runProcessCellAt
        alive
        species
        nextAlive
        nextSpecies
        w
        (number 7)
        (number 3)
        (number 3)
    LifeAssert.assertEqual (number 1) next
    done

testOverpopulation :: forall f. Effect f 'Unit
testOverpopulation = fromSyntax $ do
  miniGrid $ \alive species nextAlive nextSpecies w -> do
    toSyntax_ (u8Fill alive (number 0))
    setAlive alive w (number 2) (number 2)
    setAlive alive w (number 3) (number 2)
    setAlive alive w (number 4) (number 2)
    setAlive alive w (number 2) (number 3)
    setAlive alive w (number 3) (number 3)
    setAlive alive w (number 4) (number 3)
    rebuildPackedCounts alive w (number 7)
    next <-
      runProcessCellAt
        alive
        species
        nextAlive
        nextSpecies
        w
        (number 7)
        (number 3)
        (number 3)
    LifeAssert.assertEqual (number 0) next
    done

testReproduction :: forall f. Effect f 'Unit
testReproduction = fromSyntax $ do
  miniGrid $ \alive species nextAlive nextSpecies w -> do
    toSyntax_ (u8Fill alive (number 0))
    setAlive alive w (number 3) (number 2)
    setAlive alive w (number 4) (number 3)
    setAlive alive w (number 2) (number 3)
    rebuildPackedCounts alive w (number 7)
    next <-
      runProcessCellAt
        alive
        species
        nextAlive
        nextSpecies
        w
        (number 7)
        (number 3)
        (number 3)
    LifeAssert.assertEqual (number 1) next
    done

patternGrid ::
  ( Expr f 'Uint8Array
    -> Expr f 'Number
    -> Expr f 'Number
    -> EffectSyntax f (f 'Unit)
  )
  -> EffectSyntax f (Expr f ('Array ('Array 'Number)))
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
patternGrid seed coords expectedPop = do
  let
    w = number 8
    h = number 8
  alive <- bindExpr (newByteArray (w * h))
  species <- bindExpr (newByteArray (w * h))
  nextAlive <- bindExpr (newByteArray (w * h))
  nextSpecies <- bindExpr (newByteArray (w * h))
  seed alive w h
  forRange_ (number 0) (number 3) $ \_ -> do
    _ <-
      runStepGridOnce
        alive
        species
        nextAlive
        nextSpecies
        w
        h
        (number 0)
        (number 0)
        (number 7)
        (number 7)
    toSyntax_ (u8Copy alive nextAlive)
    rebuildPackedCounts alive w h
    done
  cells <- coords
  coordsMatch alive w cells
  popN <- gridPop alive w h
  LifeAssert.assertEqual expectedPop popN
  done

testBlockStable :: forall f. Effect f 'Unit
testBlockStable = fromSyntax (patternGrid seedBlock blockCoords (number 4))

testBeehiveStable :: forall f. Effect f 'Unit
testBeehiveStable = fromSyntax (patternGrid seedBeehive beehiveCoords (number 6))

testBlinkerPeriod2 :: forall f. Effect f 'Unit
testBlinkerPeriod2 = fromSyntax $ do
  let
    w = number 8
    h = number 8
  alive <- bindExpr (newByteArray (w * h))
  species <- bindExpr (newByteArray (w * h))
  nextAlive <- bindExpr (newByteArray (w * h))
  nextSpecies <- bindExpr (newByteArray (w * h))
  seedBlinkerHorizontal alive w h
  _ <-
    runStepGridOnce
      alive
      species
      nextAlive
      nextSpecies
      w
      h
      (number 0)
      (number 0)
      (number 7)
      (number 7)
  vCoords <- blinkerVerticalCoords
  coordsMatch nextAlive w vCoords
  toSyntax_ (u8Copy alive nextAlive)
  rebuildPackedCounts alive w h
  _ <-
    runStepGridOnce
      alive
      species
      nextAlive
      nextSpecies
      w
      h
      (number 0)
      (number 0)
      (number 7)
      (number 7)
  hCoords <- blinkerHorizontalCoords
  coordsMatch nextAlive w hCoords
  done

testViewportGridCoord :: forall f. Effect f 'Unit
testViewportGridCoord = fromSyntax $ do
  let
    cw = number canvasW
    ch = number canvasH
    px = number (fromIntegral cellPx)
    cx = number 512
    cy = number 384
    panX = cw / number 2 - cx * px
    panY = ch / number 2 - cy * px
    zoom = number 1
    bufScale = number 1
    localX = cw / number 2
    localY = ch / number 2
    gx = Math.floor ((localX * bufScale - panX) / zoom / px)
    gy = Math.floor ((localY * bufScale - panY) / zoom / px)
  LifeAssert.assertEqual cx gx
  LifeAssert.assertEqual cy gy
  done

testEngineStepGeneration :: forall f. Effect f 'Unit
testEngineStepGeneration = fromSyntax $ do
  let
    w = number 8
    h = number 8
  alive <- bindExpr (newByteArray (w * h))
  species <- bindExpr (newByteArray (w * h))
  nextAlive <- bindExpr (newByteArray (w * h))
  nextSpecies <- bindExpr (newByteArray (w * h))
  (lut, gridA, gridB) <- initEngineGrids (w * h)
  seedBlock alive w h
  rebuildPackedCounts alive w h
  nextLiveList <- bindExpr $ Array.fromEffects []
  nextChangedList <- bindExpr $ Array.fromEffects []
  stepCtx <- hold (toObject (StepCtx 0 0 (-1) (-1) 0 0 0 0 0))
  engineOk <-
    engineStepGeneration
      alive
      species
      nextAlive
      nextSpecies
      gridA
      gridB
      lut
      w
      h
      (number 1)
      (number 1)
      (number 2)
      (number 2)
      nextLiveList
      nextChangedList
      stepCtx
  LifeAssert.assertEqual (number 1) (if_ engineOk (number 1) (number 0))
  popN <- stepCtx.pop
  LifeAssert.assertEqual (number 4) popN
  cells <- blockCoords
  coordsMatch nextAlive w cells
  done

testLutCoreBlinker :: IO ()
testLutCoreBlinker = do
  got <- evaluateEffectJSON blinkerLutStepJson
  let
    s = T.unpack (T.strip got)
    body = case s of
      ('"' : rest)
        | not (null rest)
        , last rest == '"' ->
            take (length rest - 1) rest
      _ -> s
    parsed = read body :: [Int]
    live = filter (/= 0) parsed
  assertEqual "blinker LUT step pop" 3 (length live)
