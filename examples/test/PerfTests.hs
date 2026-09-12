{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Size and alloc ceilings for the Life / probe workloads.
-- Node counts and JS lengths are deterministic; alloc ceilings carry
-- slack so a GC or RTS noise band does not flake CI. Lower a ceiling
-- when an intentional win lands so the next regression still trips.
module PerfTests (perfTests) where

import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import JShark (ClosedEffect, renderJS)
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Api.Types as Ty
import JShark.Example.Life (mainJS)
import JShark.Internal (effectfulAST, optimizedEffectSize)
import System.Mem (getAllocationCounter)
import Test.Tasty
import Test.Tasty.HUnit

-- Measured on GHC 9.14.1 / -O2 after Index-only Array.index + dropped
-- expandBounds walk (2026-08-27): rawNodes=62574, optNodes=95752.
maxLifeRawNodes :: Int
maxLifeRawNodes = 72000

maxLifeOptNodes :: Int
maxLifeOptNodes = 110000

-- Process-wide Life metrics run allocated ~7.01 GB. Cap optimize
-- alone at 8 GB so a 9 GB+ walk (strict-child / empty-map-merge
-- class) fails without chasing RTS jitter.
maxLifeOptAlloc :: Int64
maxLifeOptAlloc = 8000000000

maxProbe16Chars :: Int
maxProbe16Chars = 3301

maxProbe32Chars :: Int
maxProbe32Chars = 11005

maxProbe16Alloc :: Int64
maxProbe16Alloc = 10000000

maxProbe32Alloc :: Int64
maxProbe32Alloc = 70000000

-- Life output is deterministic; cap UTF-8 bytes and hoisted `$`-helpers so
-- an accidental helper explosion or inline regression trips here as well as
-- in the golden test.
maxLifeBytes :: Int
maxLifeBytes = 900000

maxLifeHelpers :: Int
maxLifeHelpers = 100

life :: ClosedEffect 'Ty.Unit
life = stmts mainJS

-- | Same nest as @bench/Probe.hs@. Keep the two in lockstep.
probeN :: Int -> ClosedEffect 'Ty.Unit
probeN depth = fromSyntax (build depth)

nestWhile :: Int -> Effect f 'Ty.Unit
nestWhile 0 = ffi "step" RecNil
nestWhile n = while_ (ffi "cond" RecNil) (nestWhile (n - 1))

build :: Int -> EffectSyntax f (f 'Ty.Unit)
build 0 = done
build d = do
  buf <- bindExpr (newByteArray (number (fromIntegral (d * 8))))
  toSyntax_ (ffi "touch" (arg buf <: RecNil))
  toSyntax_ (nestWhile d)
  build (d - 1)

allocated :: IO a -> IO (a, Int64)
allocated act = do
  before <- getAllocationCounter
  x <- act
  doneAlloc <- getAllocationCounter
  pure (x, before - doneAlloc)

assertCeiling :: (Ord a, Show a) => String -> a -> a -> Assertion
assertCeiling label got maxOk =
  assertBool
    (label ++ " " ++ show got ++ " > ceiling " ++ show maxOk)
    (got <= maxOk)

perfTests :: TestTree
perfTests =
  testGroup
    "perf"
    [ testCase "Life raw IR nodes" $ do
        n <- evaluate (optimizedEffectSize life)
        assertCeiling "rawNodes" n maxLifeRawNodes
    , testCase "Life optimize nodes and alloc" $ do
        (n, bytes) <-
          allocated $
            evaluate (optimizedEffectSize life)
        assertCeiling "optNodes" n maxLifeOptNodes
        assertCeiling "optAlloc" bytes maxLifeOptAlloc
    , probeCase 16 maxProbe16Chars maxProbe16Alloc
    , probeCase 32 maxProbe32Chars maxProbe32Alloc
    , testCase "Life output bytes and helper count" $ do
        let js = renderJS (effectfulAST life)
        assertCeiling "lifeBytes" (BS.length js) maxLifeBytes
        assertCeiling
          "lifeHelpers"
          (T.count "const $" (TE.decodeUtf8 js))
          maxLifeHelpers
    ]

probeCase :: Int -> Int -> Int64 -> TestTree
probeCase n maxChars maxAlloc =
  testCase ("probe " ++ show n ++ " JS size and alloc") $ do
    (chars, bytes) <-
      allocated $
        evaluate (BS.length (renderJS (effectfulAST (probeN n))))
    assertCeiling ("probe" ++ show n ++ "Chars") chars maxChars
    assertCeiling ("probe" ++ show n ++ "Alloc") bytes maxAlloc
