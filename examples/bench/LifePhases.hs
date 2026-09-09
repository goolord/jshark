{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark (closedEffectNodes, effectfulAST, renderJSCompact)
import JShark.Api (stmts)
import JShark.Api.Types (ClosedEffect)
import qualified JShark.Api.Types as Ty
import JShark.Example.Life (mainJS)

life :: ClosedEffect Ty.Unit
life = stmts mainJS

main :: IO ()
main = do
  putStrLn $ "rawNodes," ++ show (closedEffectNodes life)
  t1 <- getMonotonicTime
  let
    doc = effectfulAST life
  t2 <- getMonotonicTime
  evaluate doc
  putStrLn $ "effectfulAST," ++ show (t2 - t1)
  let
    bytes = T.length (renderJSCompact doc)
  t3 <- getMonotonicTime
  evaluate bytes
  putStrLn $ show bytes ++ "," ++ show (t3 - t2)
