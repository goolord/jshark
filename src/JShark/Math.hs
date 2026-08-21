{-# LANGUAGE
    DataKinds
  , GADTs
  , OverloadedStrings
#-}
module JShark.Math
  ( inc
  , dec
  , e
  , pi
  , sqrt2
  , sqrt1_2
  , ln2
  , ln10
  , log2e
  , log10e
  , sin
  , cos
  , tan
  , asin
  , acos
  , atan
  , sqrt
  , cbrt
  , exp
  , log
  , log2
  , log10
  , floor
  , ceil
  , round
  , trunc
  , pow
  , atan2
  , max_
  , min_
  , hypot
  , random
  ) where

import JShark.Types
import JShark.Api
import JShark.Rec (Rec(..))
import Prelude hiding (pi, sin, cos, tan, asin, acos, atan, atan2, sqrt, exp, log, floor, round, max, min)

inc :: Expr f ('Function 'Number 'Number)
inc = lambda (+1)

dec :: Expr f ('Function 'Number 'Number)
dec = lambda (\x -> x - 1)

e :: Expr f 'Number
e = 2.718281828459045

pi :: Expr f 'Number
pi = 3.141592653589793

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

sin, cos, tan, asin, acos, atan, sqrt, cbrt, exp, log, log2, log10, floor, ceil, round, trunc
  :: Expr f 'Number -> Expr f 'Number
sin = mathUnary MathSin
cos = mathUnary MathCos
tan = mathUnary MathTan
asin = mathUnary MathAsin
acos = mathUnary MathAcos
atan = mathUnary MathAtan
sqrt = mathUnary MathSqrt
cbrt = mathUnary MathCbrt
exp = mathUnary MathExp
log = mathUnary MathLog
log2 = mathUnary MathLog2
log10 = mathUnary MathLog10
floor = mathUnary MathFloor
ceil = mathUnary MathCeil
round = mathUnary MathRound
trunc = mathUnary MathTrunc

pow, atan2, max_, min_, hypot :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
pow = mathBinary MathPow
atan2 = mathBinary MathAtan2
max_ = mathBinary MathMax
min_ = mathBinary MathMin
hypot = mathBinary MathHypot

-- | @Math.random()@. Not pure (yields a different value each call), so
-- it's an 'Effect'.
random :: Effect f 'Number
random = ffi "Math.random" RecNil
