{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JS @Math@ constants and functions beyond Haskell 'Num'/'Floating'.
--
-- Import qualified; @floor@, @round@, and friends still clash with 'Prelude'.
module JShark.Math
  ( inc
  , dec
  , e
  , sqrt2
  , sqrt1_2
  , ln2
  , ln10
  , log2e
  , log10e
  , cbrt
  , log2
  , log10
  , floor
  , ceil
  , round
  , trunc
  , atan2
  , max
  , min
  , hypot
  , random
  )
where

import JShark.Api (ffi, lambda)
import JShark.Api.Rec (Rec (..))
import JShark.Api.Types
import Prelude hiding (atan2, floor, max, min, round)

inc :: Expr f ('Function 'Number 'Number)
inc = lambda (+ 1)

dec :: Expr f ('Function 'Number 'Number)
dec = lambda (\x -> x - 1)

-- | @Math.E@
e :: Expr f 'Number
e = 2.718281828459045

sqrt2 :: Expr f 'Number
sqrt2 = 1.4142135623730951

sqrt1_2 :: Expr f 'Number
sqrt1_2 = 0.7071067811865476

ln2 :: Expr f 'Number
ln2 = 0.6931471805599453

ln10 :: Expr f 'Number
ln10 = 2.302585092994046

log2e :: Expr f 'Number
log2e = 1.4426950408889634

log10e :: Expr f 'Number
log10e = 0.4342944819032518

cbrt
  , log2
  , log10
  , floor
  , ceil
  , round
  , trunc ::
    Expr f 'Number -> Expr f 'Number
cbrt = expr1 FixCbrt
log2 = expr1 FixLog2
log10 = expr1 FixLog10
floor = expr1 FixFloor
ceil = expr1 FixCeil
round = expr1 FixRound
trunc = expr1 FixTrunc

atan2, max, min, hypot :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
atan2 x y = expr2 FixAtan2 x y
max x y = expr2 FixMax x y
min x y = expr2 FixMin x y
hypot x y = expr2 FixHypot x y

-- | @Math.random()@. Not pure (yields a different value each call), so
-- it's an 'Effect'.
random :: Effect f 'Number
random = ffi "Math.random" RecNil
