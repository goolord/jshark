{-# language DataKinds #-}
{-# language GADTs #-}

module Math
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
  ) where

import Types
import Reboot
import Prelude hiding (pi)

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
