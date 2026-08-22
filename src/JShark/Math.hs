{-# LANGUAGE
    DataKinds
  , GADTs
  , OverloadedStrings
#-}
-- | JS @Math@ names that are not Haskell 'Num'/'Fractional'/'Floating'.
-- Import qualified; remaining names still clash with 'Prelude' (@floor@, @round@, …).
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
  ) where

import JShark.Types
import JShark.Api (lambda, ffi)
import JShark.Rec (Rec(..))
import Prelude hiding (floor, round, max, min, atan2)

inc :: Expr f ('Function 'Number 'Number)
inc = lambda (+1)

dec :: Expr f ('Function 'Number 'Number)
dec = lambda (\x -> x - 1)

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

cbrt, log2, log10, floor, ceil, round, trunc
  :: Expr f 'Number -> Expr f 'Number
cbrt = MathUnary MathCbrt
log2 = MathUnary MathLog2
log10 = MathUnary MathLog10
floor = MathUnary MathFloor
ceil = MathUnary MathCeil
round = MathUnary MathRound
trunc = MathUnary MathTrunc

atan2, max, min, hypot :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
atan2 = MathBinary MathAtan2
max = MathBinary MathMax
min = MathBinary MathMin
hypot = MathBinary MathHypot

-- | @Math.random()@. Not pure (yields a different value each call), so
-- it's an 'Effect'.
random :: Effect f 'Number
random = ffi "Math.random" RecNil
