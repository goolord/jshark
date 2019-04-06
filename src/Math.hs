{-# language DataKinds #-}
{-# language GADTs #-}

module Math
  (
  ) where

import Types
import Reboot

succ :: Expr f ('Function 'Number 'Number)
succ = lambda (+1)

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
