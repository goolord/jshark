{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LifeTests (lifeTests) where

import BunGate (bunGated, bunPathTestName)
import qualified Control.Exception as Ex
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import JShark (renderJS)
import JShark.Api
import JShark.Generic (MutableObjectOf, toObject)
import qualified JShark.Array as Array
import JShark.Bun (evaluateEffectJSON)
import JShark.Example.Life (mainJS)
import JShark.Example.Life.EngineFinish
  ( EngineGrids (..)
  , finishStep
  , initEngineGrids
  )
import JShark.Example.Life.Grid
  ( CellGrids (..)
  , StepCtx (StepCtx)
  , StepRegion (..)
  , rebuildPackedCounts
  , setU8
  )
import JShark.Example.Life.GridApi (paintGridCellsJs, seedLiveCells)
import JShark.Example.Life.LifeTestSupport
  ( beehiveCoords
  , blinkerHorizontalCoords
  , blinkerLutStepJson
  , blinkerVerticalCoords
  , blockCoords
  , coordsMatch
  , gridPop
  , newCellGrids
  , runProcessCellAt
  , runStepGridOnce
  , seedBeehive
  , seedBlinkerHorizontal
  , seedBlock
  , setAlive
  )
import qualified JShark.Example.Life.LifeTestSupport as LifeAssert
import qualified JShark.Example.Life.Lut as Lut
import JShark.Example.Life.LutCore (computeNextByte, lifeLutEntry)
import JShark.Example.Life.Types
import JShark.Internal
  ( effectfulAST
  , effectfulASTWith
  , minifiedStyle
  , optimizedEffectSize
  )
import qualified JShark.Math as Math
import Test.Support (assertJSContains, assertJSOmits)
import Test.Tasty
import Test.Tasty.HUnit

lifeTests :: TestTree
lifeTests =
  bunGated $ \getBun ->
    testGroup
      "life conway"
      [ testCase bunPathTestName $ do
          m <- getBun
          case m of
            Nothing -> assertFailure "bun not found on PATH"
            Just _ -> pure ()
      , testGroup
          "core rules"
          [ ruleCase "underpopulation kills live cell" [(3, 2), (3, 3)] 0
          , ruleCase "survival with two neighbors" [(3, 2), (4, 3), (3, 3)] 1
          , ruleCase
              "overpopulation kills live cell"
              [(2, 2), (3, 2), (4, 2), (2, 3), (3, 3), (4, 3)]
              0
          , ruleCase "reproduction on three neighbors" [(3, 2), (4, 3), (2, 3)] 1
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
          , lifeCase "finishStep picks majority birth species" testFinishStepBirthSpecies
          , lifeCase "initEngineGrids allocates LUT and grids" testEngineInit
          , lifeCase "stepRegionLUT row slices match full LUT step" testLutStepTile
          , testCase "LutCore computeNextByte matches table entry" $
              lifeLutEntry 0x0101 @?= computeNextByte 1 1 0 0 0 0 0 0 0
          , testCase "blinker LUT step keeps three live cells" $
              testLutCoreBlinker
          ]
      , testGroup
          "life compiler regressions"
          [ testCase "hoists checkedIndex once" $ do
              let
                js = TE.decodeUtf8 (renderJS (effectfulAST (fromSyntax mainJS)))
              T.count "jshark: index" js @?= 1
              T.count "const $checkedIndex =" js @?= 1
          , testCase "paintGridCells dirty-rect reset (GridApi port)" $ do
              assertJSContains "out.dirtyCx0=0;out.dirtyCy0=0" paintGridCellsJs
              assertJSContains "out.dirtyCx1=0;out.dirtyCy1=0" paintGridCellsJs
              assertJSOmits "out.dirtyCy1=0;out.dirtyCy1=0" paintGridCellsJs
          , testCase "optimized size guards IR inline regression" $ do
              let
                life = stmts mainJS
                irNodes = optimizedEffectSize life
              js <-
                Ex.evaluate $ TE.decodeUtf8 (renderJS (effectfulASTWith minifiedStyle life))
              -- Mutable array reads (u8Index, FixArrLen, …) are no longer
              -- moved/inlined across writes, so a handful stay as bindings.
              irNodes @?= 70675
              T.length js @?= 880643
          , testCase "seedLiveCells stamps sparse pairs into zeroed buffers" $
              renderJS
                ( effectfulAST
                    ( fromSyntax
                        ( do
                            a <- fmap var (toSyntax (newByteArray (number 3)))
                            s <- fmap var (toSyntax (newByteArray (number 3)))
                            toSyntax_ (seedLiveCells a s [(0, 1), (2, 5)])
                            toSyntax noOp
                        )
                    )
                )
                @?= "const n0 = (n => new Uint8Array(n))(3);\nconst n1 = (n => new Uint8Array(n))(3);\n((a,s,p)=>{for(let k=0;k<p.length;k++){const t=p[k];a[t[0]]=1;s[t[0]]=t[1];}})(n0, n1, [[0, 1], [2, 5]]);"
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
        toSyntax_ $ throw_ (string "lut mismatch")
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
        toSyntax_ $ throw_ (string "seam mismatch")
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

-- | The 8x8 engine fixture the @finishStep@ cases share: four cell grids,
-- the LUT and its scratch grids, empty output lists, and a fresh
-- 'StepCtx'. The region is the whole grid.
data FinishFixture f = FinishFixture
  { ffGrids :: EngineGrids f
  , ffRegion :: StepRegion f
  , ffLive :: Expr f ('Array 'Number)
  , ffChanged :: Expr f ('Array 'Number)
  , ffCtx :: Effect f (MutableObjectOf StepCtx)
  }

-- | The fixture's cell grids.
ffCells :: FinishFixture f -> CellGrids f
ffCells = egCells . ffGrids

newFinishFixture :: EffectSyntax f (FinishFixture f)
newFinishFixture = do
  let
    w = number 8
    h = number 8
  alive <- bindExpr (newByteArray (w * h))
  species <- bindExpr (newByteArray (w * h))
  nextAlive <- bindExpr (newByteArray (w * h))
  nextSpecies <- bindExpr (newByteArray (w * h))
  (lut, gridA, gridB) <- initEngineGrids (w * h)
  nextLiveList <- bindExpr $ Array.fromEffects []
  nextChangedList <- bindExpr $ Array.fromEffects []
  stepCtx <- hold (toObject (StepCtx 0 0 (-1) (-1) 0 0 0 0 0))
  pure
    FinishFixture
      { ffGrids =
          EngineGrids
            { egCells = CellGrids {..}
            , egGridA = gridA
            , egGridB = gridB
            , egLut = lut
            }
      , ffRegion =
          StepRegion
            { srW = w
            , srH = h
            , srX0 = number 0
            , srY0 = number 0
            , srX1 = number 7
            , srY1 = number 7
            }
      , ffLive = nextLiveList
      , ffChanged = nextChangedList
      , ffCtx = stepCtx
      }

-- | Rebuild the packed neighbour counts the engine reads, then run it over
-- the whole fixture grid.
runFinishStep :: FinishFixture f -> EffectSyntax f (Expr f 'Bool)
runFinishStep fx = do
  rebuildPackedCounts (ffCells fx).alive (srW (ffRegion fx)) (srH (ffRegion fx))
  finishStep (ffGrids fx) (ffRegion fx) (ffLive fx) (ffChanged fx) (ffCtx fx)

-- | Set the given cell indices live.
setAliveAt :: FinishFixture f -> [Double] -> EffectSyntax f ()
setAliveAt fx =
  mapM_ (\i -> setU8 (ffCells fx).alive (number i) (number 1))

testFinishStepBlock :: forall f. Effect f 'Unit
testFinishStepBlock = fromSyntax $ do
  fx <- newFinishFixture
  seedBlock (ffCells fx).alive (srW (ffRegion fx)) (srH (ffRegion fx))
  _ <- runFinishStep fx
  popN <- (ffCtx fx).pop
  LifeAssert.assertEqual (number 4) popN
  -- A block is still life: stepping it again must not change the count.
  _ <- runFinishStep fx
  popN2 <- (ffCtx fx).pop
  LifeAssert.assertEqual (number 4) popN2
  done

testFinishStepPacked :: forall f. Effect f 'Unit
testFinishStepPacked = fromSyntax $ do
  fx <- newFinishFixture
  let
    nextAlive = (ffCells fx).nextAlive
  setAliveAt fx [9, 10, 17, 18]
  _ <- runFinishStep fx
  popN <- (ffCtx fx).pop
  LifeAssert.assertEqual (number 4) popN
  LifeAssert.assertEqual
    (number 1)
    (bitAnd (u8Index nextAlive (number 9)) (number 1))
  whenS (shr (u8Index nextAlive (number 9)) (number 1) .== 0) $ do
    toSyntax_ $ throw_ (string "packed count")
    done
  done

testFinishStepBirthSpecies :: forall f. Effect f 'Unit
testFinishStepBirthSpecies = fromSyntax $ do
  fx <- newFinishFixture
  let
    cells = ffCells fx
  -- Three live neighbors above (4,4): species 1 twice, species 2 once.
  setAliveAt fx [27, 28, 29]
  mapM_
    (\(i, sp) -> setU8 cells.species (number i) (number sp))
    [(27, 1), (28, 1), (29, 2)]
  engineOk <- runFinishStep fx
  LifeAssert.assertEqual (number 1) (u8Index cells.nextSpecies (number 36))
  LifeAssert.assertEqual
    (number 1)
    (bitAnd (u8Index cells.nextAlive (number 36)) (number 1))
  whenS (not_ engineOk) $
    do
      toSyntax_ $ throw_ (string "finishStep failed")
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
          toSyntax_ $ throw_ (string "stepTile mismatch")
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

-- | One Conway rule: seed @live@ on an empty 7x7 grid, step the cell at
-- (3,3), and check whether it comes out alive. The four rules differ only
-- in those two values.
ruleCase :: String -> [(Double, Double)] -> Double -> TestTree
ruleCase name live expected = lifeCase name (ruleProgram live expected)

ruleProgram :: [(Double, Double)] -> Double -> (forall f. Effect f 'Unit)
ruleProgram live expected = fromSyntax $ do
  miniGrid $ \alive species nextAlive nextSpecies w -> do
    toSyntax_ (u8Fill alive (number 0))
    mapM_ (\(x, y) -> setAlive alive w (number x) (number y)) live
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
    LifeAssert.assertEqual (number expected) next
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
  (cells, region) <- newCellGrids (number 8) (number 8)
  let
    alive = cells.alive
    w = srW region
    h = srH region
  seed alive w h
  forRange_ (number 0) (number 3) $ \_ -> do
    _ <- runStepGridOnce cells region
    advanceGeneration cells region
  expected <- coords
  coordsMatch alive w expected
  popN <- gridPop alive w h
  LifeAssert.assertEqual expectedPop popN
  done

-- | Copy the stepped grid back over the live one and rebuild the packed
-- neighbour counts, so the next generation reads current data.
advanceGeneration ::
  CellGrids f -> StepRegion f -> EffectSyntax f (f 'Unit)
advanceGeneration cells region = do
  toSyntax_ (u8Copy cells.alive cells.nextAlive)
  rebuildPackedCounts cells.alive (srW region) (srH region)
  done

testBlockStable :: forall f. Effect f 'Unit
testBlockStable = fromSyntax (patternGrid seedBlock blockCoords (number 4))

testBeehiveStable :: forall f. Effect f 'Unit
testBeehiveStable = fromSyntax (patternGrid seedBeehive beehiveCoords (number 6))

testBlinkerPeriod2 :: forall f. Effect f 'Unit
testBlinkerPeriod2 = fromSyntax $ do
  (cells, region) <- newCellGrids (number 8) (number 8)
  let
    w = srW region
    nextAlive = cells.nextAlive
  seedBlinkerHorizontal cells.alive w (srH region)
  -- A horizontal blinker becomes vertical, then horizontal again.
  _ <- runStepGridOnce cells region
  coordsMatch nextAlive w =<< blinkerVerticalCoords
  _ <- advanceGeneration cells region
  _ <- runStepGridOnce cells region
  coordsMatch nextAlive w =<< blinkerHorizontalCoords
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
