{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language OverloadedStrings #-}

module Example where

import Reboot
import Types
import Topaz.Types
import Topaz.Rec ((<:))

mathy :: Expr f 'Number
mathy =
  let_ (Plus 5 6) $ \x ->
  let_ (Plus 7 x) $ \y ->
  Plus x y

mathy2 :: Expr f 'Number
mathy2 = negate (-1)

-- mathy :: Expr f 'Number
-- mathy = do
--   x <- let_ (Plus (number 5) (number 6))
--   y <- let_ (Plus (number 7) x)
--   pure (Plus x y)

stringy :: Effect f 'String
stringy =
  host $ \x ->
  expr (Concat x x)

loggy :: Effect f 'Unit
loggy = 
  host $ \n0 ->
  host $ \n1 ->
  consoleLog "foo" $ consoleLog "bar" $ consoleLog n0 $ consoleLog n1 noOp

lookupy :: Effect f 'Unit
lookupy =
  lookupId "foo" $ \foo ->
  lookupId "bar" $ \bar ->
  lookupSelector ".baz" $ \baz ->
  consoleLog foo $
  consoleLog bar $
  consoleLog baz noOp

ffiey :: Effect f 'Number
ffiey = exampleFFIFunc "bar" 2
-- ffiey :: Effect f (w :: Universe)
-- ffiey = apply Math.inc (exampleFFIFunc "bar" 2)

exampleFFIFunc :: Expr f 'String -> Expr f 'Number -> Effect f 'Number
exampleFFIFunc x y = ffi "foo" (x <: y <: RecNil)

