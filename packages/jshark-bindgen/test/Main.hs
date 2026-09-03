module Main (main) where

import BindgenTests (bindgenTests)
import Test.Tasty

main :: IO ()
main = defaultMain bindgenTests
