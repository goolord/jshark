{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}

module JShark.Example where

import qualified JShark.Math as Math
import JShark
import JShark.Api
import JShark.Types

-- mathy :: EffectSyntax f (f 'Number)
-- mathy = do
  -- let x = 5 + 6
      -- y = 7 + x
      -- z = x + y
      -- w = let k = 6 + 7 in k + 5
  -- toSyntax $ expr $ apply Math.inc w

mathy :: Expr f 'Number
mathy =
  let_ (Plus 5 6) $ \x ->
  let_ (Plus 7 x) $ \y ->
  let_ (Plus x y) $ \z ->
  let_ (Plus (let_ (Plus 6 7) $ \k -> k) 5) $ \w ->
  apply Math.inc z

-- mathy2 :: (Expr f 'Number -> Expr f 'Number) -> Expr f 'Number
-- mathy2 = do
  -- x <- let_ (Plus 5 6)
  -- y <- let_ (Plus 7 x)
  -- z <- let_ (Plus x y)
  -- pure $ apply Math.inc z

-- mathy2 :: Expr f 'Number
-- mathy2 = negate (-1)

-- mathy :: (Expr f 'Number -> Expr f 'Number) -> Expr f 'Number
-- mathy = do
  -- x <- let_ (Plus (number 5) (number 6))
  -- y <- let_ (Plus (number 7) x)
  -- pure (Plus x y)

-- classy :: Effect f 'Unit
-- classy =
  -- lookupId "id" $ \el ->
  -- classToggle el "foo"

-- stringy :: Effect f 'String
-- stringy =
  -- host $ \x ->
  -- expr (Concat x x)

-- loggy :: Effect f 'Unit
-- loggy = 
  -- host $ \n0 ->
  -- host $ \n1 ->
  -- consoleLog "foo" $ consoleLog "bar" $ consoleLog n0 $ consoleLog n1 noOp

-- lookupy :: Effect f 'Unit
-- lookupy =
  -- lookupId "foo" $ \foo ->
  -- lookupId "bar" $ \bar ->
  -- lookupSelector ".baz" $ \baz ->
  -- consoleLog foo $
  -- consoleLog bar $
  -- consoleLog baz noOp

-- ffiey :: Effect f 'Number
-- ffiey = exampleFFIFunc "bar" 2

-- exampleFFIFunc :: Expr f 'String -> Expr f 'Number -> Effect f 'Number
-- exampleFFIFunc x y = ffi "foo" (x <: y <: RecNil)

-- fory :: Effect f 'Unit
-- fory =
  -- lookupSelector ".foo" $ \foos ->
  -- forEach foos $ \x ->
  -- consoleLog x $
  -- classToggle x "foo"

-- arrayey :: Expr f ('Array 'Number)
-- arrayey = Literal $ ValueArray [ValueNumber 100000000000000000000, ValueNumber 2, ValueNumber 3]
