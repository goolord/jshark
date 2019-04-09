{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}

module Example where

import Reboot
import Topaz.Rec ((<:))
import Topaz.Types
import Types
import qualified Math

mathy :: Expr f 'Number
mathy =
  let_ (Plus 5 6) $ \x ->
  let_ (Plus 7 x) $ \y ->
  let_ (Plus x y) $ \z ->
  apply Math.inc z

mathy2 :: Expr f 'Number
mathy2 = negate (-1)

-- mathy :: Expr f 'Number
-- mathy = do
--   x <- let_ (Plus (number 5) (number 6))
--   y <- let_ (Plus (number 7) x)
--   pure (Plus x y)

classy :: Effect f 'Unit
classy =
  lookupId "id" $ \el ->
  classToggle el "foo"

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

exampleFFIFunc :: Expr f 'String -> Expr f 'Number -> Effect f 'Number
exampleFFIFunc x y = ffi "foo" (x <: y <: RecNil)

fory :: Effect f 'Unit
fory =
  lookupSelector ".foo" $ \foos ->
  forEach foos $ \x ->
  classToggle x "foo"
