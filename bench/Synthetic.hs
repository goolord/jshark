{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JShark
import JShark.Api
import JShark.Rec (Rec (..), (<:))
import Stages (codepathStages, codepathStagesPure)
import Test.Tasty.Bench

-- A long chain of binds:
-- do
--   x1 <- 1
--   x2 <- 1
--   ...
--   xN <- 1
--   x1 + x2 + ... + xN
longChain :: Int -> ClosedEffect 'Number
longChain n = fromSyntax $ do
  vars <- mapM (\_ -> toSyntax (Lift (number 1))) [1 .. n]
  toSyntax $ Lift (sum (map Var vars))

bindChain :: Int -> ClosedEffect 'Unit
bindChain n = fromSyntax (go n)
 where
  go :: Int -> EffectSyntax f (f 'Unit)
  go 0 = done
  go k = do
    toSyntax_ (ffi "sink" (arg (number (fromIntegral k)) <: RecNil))
    go (k - 1)

letChain :: Int -> ClosedExpr 'Number
letChain n = go n (number 0)
 where
  go 0 acc = acc
  go k acc = let_ acc (\x -> go (k - 1) (x + number 1))

deepUseChain :: Int -> ClosedEffect 'Number
deepUseChain n = fromSyntax $ do
  vars <- mapM (\_ -> toSyntax (Lift (number 1))) [1 .. n]
  toSyntax $ Lift (sum (map Var vars))

scaleNs :: [Int]
scaleNs = [100, 200, 400, 800]

main :: IO ()
main =
  defaultMain
    [ bgroup
        "longChain/optimize"
        [ bench (show n) $ nf (\k -> optimizedEffectSize (longChain k)) n | n <- scaleNs]
    , bgroup
        "bindChain"
        [ codepathStages (show n) (bindChain n) | n <- scaleNs]
    , bgroup
        "letChain"
        [ codepathStagesPure (show n) (letChain n) | n <- scaleNs]
    , bgroup
        "deepUseChain"
        [ codepathStages (show n) (deepUseChain n) | n <- scaleNs]
    ]
