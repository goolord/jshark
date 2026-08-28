{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Compiler microbenchmarks across JShark AST constructors and Life-shaped
-- paths. AST size, not loop bounds, drives compile time.
--
--   cabal bench jshark-compiler
--   cabal bench jshark-compiler -- jshark-compiler -p 'stages/lifeStep'
--   cabal bench jshark-compiler -- jshark-compiler -p 'codepaths/effect/bindRec'
module Main (main) where

import Data.Array.Byte (ByteArray (..))
import GHC.Exts
  ( Int (..)
  , newByteArray#
  , unsafeFreezeByteArray#
  , writeWord8Array#
  , (+#)
  )
import GHC.Generics (Generic)
import GHC.ST (ST (..), runST)
import GHC.Word (Word8 (..))
import Grid (RenderDirty (..))
import JShark (Expr (Literal))
import JShark.Api
import JShark.Api.Generic (newRecord, toObject)
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types (ClosedEffect, ClosedExpr)
import qualified JShark.Array as Array
import qualified JShark.Canvas as Canvas
import qualified JShark.Console as Console
import qualified JShark.Dom as Dom
import qualified JShark.Object as Object
import qualified JShark.Timers as Timers
import LifeTestSupport (runStepGridOnce, seedBlock)
import Stages
  ( codepathStages
  , codepathStagesPure
  , stageBenches
  , stageBenchesPure
  )
import Test.Tasty.Bench

data Cell = Cell
  { cx :: Double
  , cy :: Double
  , live :: Bool
  }
  deriving Generic

data LitRow

type instance Field LitRow "x" = 'Number

type instance Field LitRow "y" = 'Number

main :: IO ()
main =
  defaultMain
    [ bgroup
        "stages"
        [ stageBenchesPure "arith" arith
        , stageBenches "lifeKernel" lifeKernel
        , stageBenches "lifeStep" lifeStep
        , stageBenches "lifeMedium" lifeMedium
        , stageBenches "frameBinds256" (frameBinds 256)
        ]
    , bgroup "codepaths/pure" pureCodepaths
    , bgroup "codepaths/effect" effectCodepaths
    , bgroup "scale" scaleBenches
    ]

pureCodepaths :: [Benchmark]
pureCodepaths =
  [ codepathStagesPure "arith" arith
  , codepathStagesPure "lets" (letChain 32)
  , codepathStagesPure "letRec" letRecPure
  , codepathStagesPure "lambdas" lambdas
  , codepathStagesPure "ifs" ifs
  , codepathStagesPure "bigInt" bigIntExpr
  , codepathStagesPure "frozenEq" frozenEq
  , codepathStagesPure "optionCase" optionCasePure
  , codepathStagesPure "resultCase" resultCasePure
  , codepathStagesPure "uint8Array" uint8Lit
  , codepathStagesPure "arrayMap" arrayMap
  , codepathStagesPure "arrayReduce" arrayReduce
  , codepathStagesPure "arrayFilter" arrayFilter
  , codepathStagesPure "arrayToSorted" arrayToSorted
  ]

effectCodepaths :: [Benchmark]
effectCodepaths =
  [ codepathStages "binds" (bindChain 32)
  , codepathStages "forRange1" forRange1
  , codepathStages "forRange2" forRange2
  , codepathStages "forNest" (forNest 4)
  , codepathStages "neighbors" neighbors
  , codepathStages "u8Fill" u8FillProg
  , codepathStages "u8FillRegion" fillRegion
  , codepathStages "while" whileProg
  , codepathStages "objects" objects
  , codepathStages "records" records
  , codepathStages "arrayLit" arrayLitProg
  , codepathStages "deleteProp" deleteProg
  , codepathStages "callMethod" callMethodProg
  , codepathStages "tryCatch" tryCatchProg
  , codepathStages "catch" catchProg
  , codepathStages "throw" throwProg
  , codepathStages "optionCaseE" optionCaseEffect
  , codepathStages "resultCaseE" resultCaseEffect
  , codepathStages "stringCaseE" stringCaseEffect
  , codepathStages "bindRec" bindRecProg
  , codepathStages "foreverFrame" frameSmall
  , codepathStages "domBoot" domBoot
  , codepathStages "paintGridCells" paintGrid
  , codepathStages "lifeKernel" lifeKernel
  , codepathStages "lifeStep" lifeStep
  , codepathStages "lifeMedium" lifeMedium
  ]

scaleBenches :: [Benchmark]
scaleBenches =
  [ scaleStages "binds" bindChain [16, 64, 256, 1024]
  , scaleStages
      "lets"
      (\n -> fromSyntax (toSyntax_ (discard (expr (letChain n))) *> done))
      [16, 64, 256, 1024]
  , scaleStages "forNest" forNest [2, 4, 6, 8]
  , scaleStages "frameBinds" frameBinds [16, 64, 256, 1024]
  ]

scaleStages :: String -> (Int -> ClosedEffect 'Unit) -> [Int] -> Benchmark
scaleStages name mk ns =
  bgroup name [codepathStages (show n) (mk n) | n <- ns]

arith :: ClosedExpr 'Number
arith = number 1 + number 2 * number 3 - number 4 / number 5

letChain :: Int -> ClosedExpr 'Number
letChain n = go n (number 0)
 where
  go 0 acc = acc
  go k acc = let_ acc (\x -> go (k - 1) (x + number 1))

letRecPure :: ClosedExpr 'Number
letRecPure = letRec (\_ -> number 1 + number 2) (\n -> n)

lambdas :: ClosedExpr 'Number
lambdas =
  apply3
    ( lambda $ \a ->
        lambda $ \b ->
          lambda $ \c -> a * b + c
    )
    (number 2)
    (number 3)
    (number 4)

ifs :: ClosedExpr 'Number
ifs =
  if_
    (number 1 .> number 0)
    (if_ (number 2 .> number 1) (number 3) (number 4))
    (number 5)

bigIntExpr :: ClosedExpr 'BigInt
bigIntExpr = bigInt 42 + bigInt 9 * bigInt 3

frozenEq :: ClosedExpr 'Bool
frozenEq =
  structuralEq
    ( Object.frozen [Object.field @"x" (number 1), Object.field @"y" (number 2)] ::
        ClosedExpr ('Object LitRow)
    )
    ( Object.frozen [Object.field @"x" (number 1), Object.field @"y" (number 3)] ::
        ClosedExpr ('Object LitRow)
    )

optionCasePure :: ClosedExpr 'Number
optionCasePure =
  optionCase (some (number 1)) (number 0) (\x -> x + number 1)

resultCasePure :: ClosedExpr 'Number
resultCasePure =
  resultCase (ok (number 7)) (\e -> number 0 + e) (\a -> a + number 1)

uint8Lit :: ClosedExpr 'Uint8Array
uint8Lit = uint8Array (packBytes [1, 2, 3, 4, 5])

arrayMap :: ClosedExpr ('Array 'Number)
arrayMap =
  Array.map
    ( Literal
        (ValueArray [ValueNumber 1, ValueNumber 2, ValueNumber 3, ValueNumber 4])
    )
    (\x -> x * number 2 + number 1)

arrayReduce :: ClosedExpr 'Number
arrayReduce =
  Array.reduce
    (Literal (ValueArray [ValueNumber 1, ValueNumber 2, ValueNumber 3]))
    (number 0)
    (\acc x -> acc + x)

arrayFilter :: ClosedExpr ('Array 'Number)
arrayFilter =
  Array.filter
    ( Literal
        (ValueArray [ValueNumber 1, ValueNumber 2, ValueNumber 3, ValueNumber 4])
    )
    (\x -> x .> number 1)

arrayToSorted :: ClosedExpr ('Array 'Number)
arrayToSorted =
  Array.toSorted
    (Literal (ValueArray [ValueNumber 3, ValueNumber 1, ValueNumber 2]))
    (\a b -> if_ (a .< b) (number (-1)) (number 1))

bindChain :: Int -> ClosedEffect 'Unit
bindChain n = fromSyntax (go n)
 where
  go :: Int -> EffectSyntax f (f 'Unit)
  go 0 = done
  go k = do
    toSyntax_ (ffi "sink" (arg (number (fromIntegral k)) <: RecNil))
    go (k - 1)

forRange1 :: ClosedEffect 'Unit
forRange1 = fromSyntax $ do
  buf <- bindExpr (newByteArray (number 64))
  forRange_ (number 0) (number 8) $ \i ->
    toSyntax (u8Set buf i (number 1))
  done

forRange2 :: ClosedEffect 'Unit
forRange2 = fromSyntax $ do
  buf <- bindExpr (newByteArray (number 64))
  let
    w = number 8
  forRange_ (number 0) w $ \y ->
    forRange_ (number 0) w $ \x -> do
      let
        i = y * w + x
      toSyntax (u8Set buf i (u8Index buf i + number 1))
  done

-- | Nesting depth and inner loop bound both scale with @d@.
forNest :: Int -> ClosedEffect 'Unit
forNest d = fromSyntax (loops d (number (fromIntegral d)))
 where
  loops :: Int -> Expr f 'Number -> EffectSyntax f (f 'Unit)
  loops 0 _ = done
  loops k bound =
    forRange_ (number 0) bound $ \_ ->
      loops (k - 1) bound

neighbors :: ClosedEffect 'Unit
neighbors = fromSyntax $ do
  grid <- bindExpr (newByteArray (number 64))
  let
    w = number 8
    h = number 8
  forRange_ (number 0) h $ \y ->
    forRange_ (number 0) w $ \x ->
      forRange_ (number (-1)) (number 2) $ \dy ->
        forRange_ (number (-1)) (number 2) $ \dx ->
          whenS (not_ (dx .== 0 .&& dy .== 0)) $ do
            let
              nx = x + dx
              ny = y + dy
            whenS (nx .>= 0 .&& ny .>= 0 .&& nx .< w .&& ny .< h) $ do
              let
                ni = ny * w + nx
              toSyntax (u8Set grid ni (u8Index grid ni + number 2))
  done

u8FillProg :: ClosedEffect 'Unit
u8FillProg = fromSyntax $ do
  buf <- bindExpr (newByteArray (number 64))
  toSyntax_ (u8Fill buf (number 0))
  done

fillRegion :: ClosedEffect 'Unit
fillRegion = fromSyntax $ do
  buf <- bindExpr (newByteArray (number 64))
  toSyntax_
    ( u8FillRegion
        buf
        (number 8)
        (number 0)
        (number 0)
        (number 8)
        (number 8)
        (number 1)
    )
  done

whileProg :: ClosedEffect 'Unit
whileProg =
  fromSyntax
    (toSyntax_ (while_ (ffi "cond" RecNil) (ffi "step" RecNil)) *> done)

objects :: ClosedEffect 'Unit
objects = fromSyntax $ do
  o <- hold emptyObject
  setProp o "x" (number 1)
  setProp o "y" (number 2)
  _ <- getProp o "x"
  done

records :: ClosedEffect 'Unit
records = fromSyntax $ do
  cell <- hold (newRecord @Cell)
  set @"cx" cell (number 1)
  set @"cy" cell (number 2)
  set @"live" cell true_
  _ <- get @"cx" cell
  done

arrayLitProg :: ClosedEffect 'Unit
arrayLitProg = fromSyntax $ do
  arr <-
    bindExpr $ Array.fromEffects [expr (number 1), expr (number 2), expr (number 3)]
  toSyntax_ (ffi "sink" (arg (Array.length arr) <: RecNil))
  done

deleteProg :: ClosedEffect 'Unit
deleteProg = fromSyntax $ do
  o <- hold emptyObject
  setProp o "x" (number 1)
  toSyntax_ (Object.delete o (string "x"))
  done

callMethodProg :: ClosedEffect 'Unit
callMethodProg = fromSyntax $ do
  el <- Dom.lookupId (string "board")
  _ <- Dom.getAttribute el "id"
  done

tryCatchProg :: ClosedEffect 'Unit
tryCatchProg = fromSyntax $ do
  toSyntax_ $ try_ (ffi "risky" RecNil) (ffi "recover" RecNil)
  done

catchProg :: ClosedEffect 'Unit
catchProg = fromSyntax $ do
  toSyntax_ $ catch_ (ffi "risky" RecNil) (\_ -> ffi "recover" RecNil)
  done

throwProg :: ClosedEffect 'Unit
throwProg = fromSyntax $ toSyntax_ (throw_ (string "boom")) *> done

optionCaseEffect :: ClosedEffect 'Unit
optionCaseEffect =
  fromSyntax $
    toSyntax_ (optionCaseE (some (number 1)) noOp (\x -> discard (expr x)))
      *> done

resultCaseEffect :: ClosedEffect 'Unit
resultCaseEffect =
  fromSyntax $
    toSyntax_
      ( resultCaseE
          (ok (number 7))
          (\_ -> noOp)
          (\a -> discard (expr a))
      )
      *> done

stringCaseEffect :: ClosedEffect 'Unit
stringCaseEffect =
  fromSyntax $
    toSyntax_
      ( stringCaseE
          (string "mode")
          [ ("a", ffi "modeA" RecNil)
          , ("b", ffi "modeB" RecNil)
          ]
          (ffi "modeDefault" RecNil)
      )
      *> done

bindRecProg :: ClosedEffect 'Unit
bindRecProg =
  fromSyntax
    ( loop0
        (\_ -> Console.log (string "p") *> done)
        (\_ -> Console.log (string "w") *> done)
    )

frameSmall :: ClosedEffect 'Unit
frameSmall = fromSyntax $
  Timers.foreverFrame $ \_now -> do
    toSyntax_ (ffi "tick" RecNil)
    done

frameBinds :: Int -> ClosedEffect 'Unit
frameBinds n = fromSyntax $
  Timers.foreverFrame $
    \_now -> go n
 where
  go :: Int -> EffectSyntax f (f 'Unit)
  go 0 = done
  go k = do
    toSyntax_ (ffi "sink" (arg (number (fromIntegral k)) <: RecNil))
    go (k - 1)

domBoot :: ClosedEffect 'Unit
domBoot = fromSyntax $ do
  canvas <- Dom.lookupId (string "board")
  ctxOpt <- Canvas.getContext2d canvas
  whenSomeE ctxOpt $ \_ctx -> do
    _ <- Canvas.setCanvasWidth canvas (number 100)
    _ <- Canvas.setCanvasHeight canvas (number 80)
    done
  done

paintGrid :: ClosedEffect 'Unit
paintGrid = fromSyntax $ do
  pixels <- bindExpr (newByteArray (number (16 * 4)))
  alive <- bindExpr (newByteArray (number 64))
  species <- bindExpr (newByteArray (number 64))
  palette <- bindExpr (newByteArray (number (256 * 4)))
  liveList <- bindExpr $ Array.fromEffects [expr (number 0), expr (number 1)]
  changedList <- bindExpr $ Array.fromEffects [expr (number 2)]
  dirty <- hold (toObject (RenderDirty 0 0 0 0 False False))
  toSyntax_
    ( paintGridCells
        pixels
        (number 4)
        (number 4)
        palette
        alive
        species
        (number 8)
        (number 1)
        (number 0)
        (number 0)
        (number 0xff000000)
        liveList
        changedList
        true_
        (number 0)
        (number 8)
        (number 0)
        (number 8)
        dirty
    )
  done

lifeKernel :: ClosedEffect 'Unit
lifeKernel = fromSyntax $ do
  canvas <- Dom.lookupId (string "board")
  ctxOpt <- Canvas.getContext2d canvas
  whenSomeE ctxOpt $ \_ctx -> do
    _ <- Canvas.setCanvasWidth canvas (number 100)
    grid <- bindExpr (newByteArray (number 64))
    Timers.foreverFrame $ \_now -> do
      let
        w = number 8
        h = number 8
      forRange_ (number 0) h $ \y ->
        forRange_ (number 0) w $ \x ->
          forRange_ (number (-1)) (number 2) $ \dy ->
            forRange_ (number (-1)) (number 2) $ \dx ->
              whenS (not_ (dx .== 0 .&& dy .== 0)) $ do
                let
                  nx = x + dx
                  ny = y + dy
                whenS (nx .>= 0 .&& ny .>= 0 .&& nx .< w .&& ny .< h) $ do
                  let
                    ni = ny * w + nx
                  toSyntax (u8Set grid ni (u8Index grid ni + number 2))
      done
  done

-- | One full 'Grid.stepGrid' pass via Life test helpers.
lifeStep :: ClosedEffect 'Unit
lifeStep = fromSyntax $ do
  let
    w = number 8
    h = number 8
  alive <- bindExpr (newByteArray (w * h))
  species <- bindExpr (newByteArray (w * h))
  nextAlive <- bindExpr (newByteArray (w * h))
  nextSpecies <- bindExpr (newByteArray (w * h))
  seedBlock alive w h
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
  done

-- | DOM boot + rAF loop wrapping a Life step (Client-shaped, not full mainJS).
lifeMedium :: ClosedEffect 'Unit
lifeMedium = fromSyntax $ do
  canvas <- Dom.lookupId (string "board")
  ctxOpt <- Canvas.getContext2d canvas
  whenSomeE ctxOpt $ \_ctx -> do
    _ <- Canvas.setCanvasWidth canvas (number 100)
    let
      w = number 8
      h = number 8
    alive <- bindExpr (newByteArray (w * h))
    species <- bindExpr (newByteArray (w * h))
    nextAlive <- bindExpr (newByteArray (w * h))
    nextSpecies <- bindExpr (newByteArray (w * h))
    seedBlock alive w h
    Timers.foreverFrame $ \_now -> do
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
      done
  done

packBytes :: [Word8] -> ByteArray
packBytes xs = runST go
 where
  !(I# n#) = length xs
  go :: ST s ByteArray
  go = ST $ \s0 ->
    case newByteArray# n# s0 of
      (# s1, mba #) ->
        case write 0# xs mba s1 of
          s2 -> case unsafeFreezeByteArray# mba s2 of
            (# s3, ba #) -> (# s3, ByteArray ba #)
  write _ [] _ s = s
  write i# (W8# w : rest) mba s =
    write (i# +# 1#) rest mba (writeWord8Array# mba i# w s)
