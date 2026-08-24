{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Compiler microbenchmarks for the paths Life stresses.
--
-- AST size, not loop bounds, drives compile time: @forRange 0 1024@
-- is the same tree as @forRange 0 3@. Scale groups grow binders.
--
--   cabal bench jshark-compiler
--   cabal bench jshark-compiler -- jshark-compiler -p 'stages|scale'
module Main (main) where

import GHC.Generics (Generic)
import JShark (Expr (Literal), effectfulAST, optimizedEffectSize, renderJS)
import JShark.Api
import qualified JShark.Array as Array
import qualified JShark.Canvas as Canvas
import qualified JShark.Console as Console
import qualified JShark.Dom as Dom
import JShark.Generic (newRecord)
import JShark.Rec (Rec (..), (<:))
import qualified JShark.Timers as Timers
import JShark.Types (ClosedEffect, ClosedExpr)
import Stages (emit, nfClosed, stageBenches, stageBenchesPure)
import Test.Tasty.Bench

data Cell = Cell
  { cx :: Double
  , cy :: Double
  , live :: Bool
  }
  deriving (Generic)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "stages"
        [ stageBenchesPure "arith" arith
        , stageBenches "lifeKernel" lifeKernel
        , stageBenches "frameBinds256" (frameBinds 256)
        ]
    , bgroup "codepaths" codepathBenches
    , bgroup "scale" scaleBenches
    ]

codepathBenches :: [Benchmark]
codepathBenches =
  [ benchEmit "pure/arith" (expr arith)
  , benchEmit "pure/lets" (expr (letChain 32))
  , benchEmit "pure/lambdas" (expr lambdas)
  , benchEmit "pure/ifs" (expr ifs)
  , benchEmit "pure/arrayMap" (expr arrayMap)
  , benchEmit "effect/binds" (bindChain 32)
  , benchEmit "effect/forRange1" forRange1
  , benchEmit "effect/forRange2" forRange2
  , benchEmit "effect/neighbors" neighbors
  , benchEmit "effect/u8FillRegion" fillRegion
  , benchEmit "effect/while" whileProg
  , benchEmit "effect/objects" objects
  , benchEmit "effect/records" records
  , benchEmit "effect/bindRec" bindRecProg
  , benchEmit "effect/foreverFrame" frameSmall
  , benchEmit "effect/domBoot" domBoot
  , benchEmit "effect/lifeKernel" lifeKernel
  , bench "effect/lifeKernel/PageMode" $ nfClosed (\e -> renderJS (effectfulAST e)) lifeKernel
  ]

scaleBenches :: [Benchmark]
scaleBenches =
  [ scale "binds" bindChain [16, 64, 256, 1024]
  , scale "lets" (\n -> expr (letChain n)) [16, 64, 256, 1024]
  , scale "forNest" forNest [2, 4, 6, 8]
  , scale "frameBinds" frameBinds [16, 64, 256, 1024]
  , bgroup
      "frameBinds/optimize"
      [ bench (show n) $ nfClosed optimizedEffectSize (frameBinds n)
      | n <- [16, 64, 256, 1024]
      ]
  ]

benchEmit :: String -> ClosedEffect u -> Benchmark
benchEmit name prog = bench name $ nfClosed emit prog

scale :: String -> (Int -> ClosedEffect u) -> [Int] -> Benchmark
scale name mk ns =
  bgroup name [bench (show n) $ nfClosed emit (mk n) | n <- ns]

arith :: ClosedExpr 'Number
arith = number 1 + number 2 * number 3 - number 4 / number 5

letChain :: Int -> ClosedExpr 'Number
letChain n = go n (number 0)
 where
  go 0 acc = acc
  go k acc = let_ acc (\x -> go (k - 1) (x + number 1))

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

arrayMap :: ClosedExpr ('Array 'Number)
arrayMap =
  Array.map
    (Literal (ValueArray [ValueNumber 1, ValueNumber 2, ValueNumber 3, ValueNumber 4]))
    (\x -> x * number 2 + number 1)

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

forNest :: Int -> ClosedEffect 'Unit
forNest d = fromSyntax (loops d)
 where
  loops :: Int -> EffectSyntax f (f 'Unit)
  loops 0 = done
  loops k =
    forRange_ (number 0) (number 4) $ \_ ->
      loops (k - 1)

frameBinds :: Int -> ClosedEffect 'Unit
frameBinds n = fromSyntax $
  Timers.foreverFrame $ \_now -> go n
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

-- | Life-shaped kernel: rAF loop around a packed-neighbor grid step
-- plus a DOM boot, without the full example program.
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
