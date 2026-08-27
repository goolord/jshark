{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LifeTests (lifeTests) where

import qualified Data.Text as T
import Grid (StepCtx (StepCtx), rebuildPackedCounts)
import JShark (effectfulProgram, renderJSCompact)
import JShark.Api
import JShark.Api.Generic (toObject)
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Array as Array
import JShark.Bun (evaluateEffectJSON)
import JShark.Bun.Internal (JSProgram (..), bunTimeoutMicroseconds, runProgram)
import qualified JShark.Math as Math
import LifeTestSupport
  ( beehiveCoords
  , blinkerHorizontalCoords
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
import System.Directory (findExecutable, getCurrentDirectory)
import System.FilePath ((</>))
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
          "js engine"
          [ lifeJsCase "LifeLUT.stepCell matches Conway rules" testJsStepCell
          , lifeJsCase "LifeLUT.stepRegionLUT matches stepCell on blinker" testJsLutRegion
          , lifeJsCase
              "LifeLUT.stepRegionLUT matches stepCell for glider on 8-cell seam"
              testJsLutGliderSeam
          , lifeJsCase "LifeEngine.step keeps block stable" testJsEngineBlock
          , lifeJsCase "LifeEngineSync.finishStep rebuilds packed counts" testJsFinishStep
          , lifeJsCase
              "LifeEngineSync.finishStep reuses _stepOut object"
              testJsFinishStepReuse
          , lifeJsCase "engineStepGeneration keeps block stable" testEngineStepGeneration
          , lifeJsCase "stepTile row slices match full LUT step" testJsStepTile
          , lifeJsCase "initWorkerEngine registers LifeEngine" testEngineInit
          ]
      ]

lifeCase :: String -> (forall f. Effect f 'Unit) -> TestTree
lifeCase name eff = testCase name $ do
  got <- T.unpack <$> evaluateEffectJSON eff
  assertBool (name ++ " should complete") (got == "undefined" || got == "null")

lifeJsCase :: String -> (forall f. Effect f 'Unit) -> TestTree
lifeJsCase name eff = testCase name $ do
  prelude <- loadLifeJsPrelude
  let
    js = T.unpack (renderJSCompact (effectfulProgram eff))
    prog =
      JSProgram
        { jsFlags = []
        , jsPrelude = prelude
        , jsExpression = js
        , jsEpilogue = ""
        }
  got <- T.unpack <$> runProgram bunTimeoutMicroseconds prog
  assertBool (name ++ " should complete") (got == "undefined" || got == "null")

loadLifeJsPrelude :: IO String
loadLifeJsPrelude = do
  root <- getCurrentDirectory
  lut <- readFile (root </> "examples/Life/js/LUTGenerator.js")
  main <- readFile (root </> "examples/Life/js/Main.js")
  pure (lut ++ "\n" ++ main ++ "\n")

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

testJsStepCell :: forall f. Effect f 'Unit
testJsStepCell = fromSyntax $ do
  alive <- bindExpr (newByteArray (number 25))
  next <- bindExpr (newByteArray (number 25))
  toSyntax_ $
    ffi
      ( "((a,b)=>{"
          <> "a.fill(0);b.fill(0);"
          <> "a[6]=1;a[7]=1;a[11]=1;"
          <> "LifeLUT.stepCell(a,b,5,5,2,2);"
          <> "if((b[12]&1)!==1)throw new Error('birth failed');"
          <> "a.fill(0);b.fill(0);"
          <> "a[6]=1;"
          <> "LifeLUT.stepCell(a,b,5,5,1,1);"
          <> "if((b[6]&1)!==0)throw new Error('underpop failed');"
          <> "})"
      )
      (arg alive <: arg next <: RecNil)
  done

testJsLutRegion :: forall f. Effect f 'Unit
testJsLutRegion = fromSyntax $ do
  toSyntax_ $
    ffi
      ( "(()=>{"
          <> "const w=8,h=8,n=w*h;"
          <> "const lut=LifeLUT.createLifeLUT();"
          <> "const a=new Uint8Array(n);"
          <> "const b=new Uint8Array(n);"
          <> "const c=new Uint8Array(n);"
          <> "a[18]=1;a[19]=1;a[20]=1;"
          <> "LifeLUT.stepRegionLUT(lut,a,b,w,h,0,h);"
          <> "for(let y=0;y<h;y++)for(let x=0;x<w;x++){"
          <> "LifeLUT.stepCell(a,c,w,h,x,y);"
          <> "const i=y*w+x;"
          <> "if((b[i]&1)!==(c[i]&1))throw new Error('lut mismatch at '+i);"
          <> "}"
          <> "})"
      )
      RecNil
  done

testJsLutGliderSeam :: forall f. Effect f 'Unit
testJsLutGliderSeam = fromSyntax $ do
  toSyntax_ $
    ffi
      ( "(()=>{"
          <> "const w=16,h=16,n=w*h;"
          <> "const lut=LifeLUT.createLifeLUT();"
          <> "const a=new Uint8Array(n);"
          <> "const b=new Uint8Array(n);"
          <> "const c=new Uint8Array(n);"
          <> "const cells=[[8,2],[9,3],[7,4],[8,4],[9,4]];"
          <> "for(const [x,y] of cells)a[y*w+x]=1;"
          <> "LifeLUT.stepRegionLUT(lut,a,b,w,h,0,h);"
          <> "for(let y=0;y<h;y++)for(let x=0;x<w;x++){"
          <> "LifeLUT.stepCell(a,c,w,h,x,y);"
          <> "const i=y*w+x;"
          <> "if((b[i]&1)!==(c[i]&1))throw new Error('seam mismatch at '+i);"
          <> "}"
          <> "let src=b,dst=new Uint8Array(n);"
          <> "for(let g=0;g<3;g++){"
          <> "LifeLUT.stepRegionLUT(lut,src,dst,w,h,0,h);"
          <> "const tmp=src;src=dst;dst=tmp;"
          <> "}"
          <> "let pop=0;for(let i=0;i<n;i++)if(src[i]&1)pop++;"
          <> "if(pop!==5)throw new Error('glider pop after 4 '+pop);"
          <> "})"
      )
      RecNil
  done

testJsEngineBlock :: forall f. Effect f 'Unit
testJsEngineBlock = fromSyntax $ do
  toSyntax_ $
    ffi
      ( "(()=>{"
          <> "const w=8,h=8,n=w*h;"
          <> "LifeEngine.init({width:w,height:h,workerCount:0});"
          <> "const g=LifeEngine.gridA;"
          <> "g[9]=1;g[10]=1;g[17]=1;g[18]=1;"
          <> "LifeEngine.step();"
          <> "let pop=0;"
          <> "for(let i=0;i<n;i++)if(g[i]&1)pop++;"
          <> "if(pop!==4)throw new Error('block pop '+pop);"
          <> "LifeEngine.step();"
          <> "pop=0;"
          <> "for(let i=0;i<n;i++)if(g[i]&1)pop++;"
          <> "if(pop!==4)throw new Error('block pop after 2 '+pop);"
          <> "})"
      )
      RecNil
  done

testJsFinishStep :: forall f. Effect f 'Unit
testJsFinishStep = fromSyntax $ do
  toSyntax_ $
    ffi
      ( "(()=>{"
          <> "const w=8,h=8,n=w*h;"
          <> "LifeEngine.init({width:w,height:h,workerCount:0});"
          <> "const alive=new Uint8Array(n);"
          <> "const species=new Uint8Array(n);"
          <> "const nextAlive=new Uint8Array(n);"
          <> "const nextSpecies=new Uint8Array(n);"
          <> "const live=[];"
          <> "const changed=[];"
          <> "alive[9]=1;alive[10]=1;alive[17]=1;alive[18]=1;"
          <> "LifeEngineSync.rebuildPackedCounts(alive,w,h);"
          <> "const r=LifeEngineSync.finishStep("
          <> "alive,species,nextAlive,nextSpecies,w,h,0,0,7,7,live,changed);"
          <> "if(!r||r.pop!==4)throw new Error('finishStep pop');"
          <> "if((nextAlive[9]&1)!==1)throw new Error('packed bit');"
          <> "if((nextAlive[9]>>1)===0)throw new Error('packed count');"
          <> "})"
      )
      RecNil
  done

testJsFinishStepReuse :: forall f. Effect f 'Unit
testJsFinishStepReuse = fromSyntax $ do
  toSyntax_ $
    ffi
      ( "(()=>{"
          <> "const w=8,h=8,n=w*h;"
          <> "LifeEngine.init({width:w,height:h,workerCount:0});"
          <> "const alive=new Uint8Array(n);"
          <> "const species=new Uint8Array(n);"
          <> "const nextAlive=new Uint8Array(n);"
          <> "const nextSpecies=new Uint8Array(n);"
          <> "const live=[];"
          <> "const changed=[];"
          <> "alive[9]=1;alive[10]=1;alive[17]=1;alive[18]=1;"
          <> "LifeEngineSync.rebuildPackedCounts(alive,w,h);"
          <> "const a=LifeEngineSync.finishStep("
          <> "alive,species,nextAlive,nextSpecies,w,h,0,0,7,7,live,changed);"
          <> "const b=LifeEngineSync.finishStep("
          <> "alive,species,nextAlive,nextSpecies,w,h,0,0,7,7,live,changed);"
          <> "if(!a||!b||a!==b)throw new Error('finishStep must reuse _stepOut');"
          <> "})"
      )
      RecNil
  done

testEngineInit :: forall f. Effect f 'Unit
testEngineInit = fromSyntax $ do
  toSyntax_ $
    ffi
      "(()=>{const E=globalThis.LifeEngine;if(!E)throw new Error('missing');E.init({width:8,height:8,workerCount:0});if(E.mode==='none')throw new Error('LifeEngine missing');})"
      RecNil
  done

testEngineStepGeneration :: forall f. Effect f 'Unit
testEngineStepGeneration = fromSyntax $ do
  toSyntax_ $
    ffi
      "(()=>{LifeEngine.init({width:8,height:8,workerCount:0});})"
      RecNil
  let
    w = number 8
    h = number 8
  alive <- bindExpr (newByteArray (w * h))
  species <- bindExpr (newByteArray (w * h))
  nextAlive <- bindExpr (newByteArray (w * h))
  nextSpecies <- bindExpr (newByteArray (w * h))
  seedBlock alive w h
  nextLiveList <- bindExpr $ Array.fromEffects []
  nextChangedList <- bindExpr $ Array.fromEffects []
  stepCtx <- hold (toObject (StepCtx 0 0 (-1) (-1) 0 0 0 0 0))
  engineOk <-
    engineStepGeneration
      alive
      species
      nextAlive
      nextSpecies
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

testJsStepTile :: forall f. Effect f 'Unit
testJsStepTile = fromSyntax $ do
  toSyntax_ $
    ffi
      ( "(()=>{"
          <> "const w=8,h=8,n=w*h;"
          <> "const lut=LifeLUT.createLifeLUT();"
          <> "const a=new Uint8Array(n);"
          <> "const full=new Uint8Array(n);"
          <> "const tile=new Uint8Array(n);"
          <> "a[18]=1;a[19]=1;a[20]=1;"
          <> "LifeLUT.stepRegionLUT(lut,a,full,w,h,0,h);"
          <> "for(let y0=0;y0<h;y0++){"
          <> "LifeLUT.stepRegionLUT(lut,a,tile,w,h,y0,y0+1);"
          <> "for(let x=0;x<w;x++){"
          <> "const i=y0*w+x;"
          <> "if((full[i]&1)!==(tile[i]&1))throw new Error('stepTile mismatch at '+i);"
          <> "}"
          <> "}"
          <> "})"
      )
      RecNil
  done
