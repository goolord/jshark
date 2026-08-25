{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark (effectfulAST, optimizeEffect, renderJSCompact)
import JShark.Api (stmts)
import JShark.Types (ClosedEffect)
import Life (mainJS)
import qualified Data.Text as T
import qualified JShark.Types as Ty

life :: ClosedEffect Ty.Unit
life = stmts mainJS

main :: IO ()
main = do
  t0 <- getMonotonicTime
  let opt = optimizeEffect life
  t1 <- getMonotonicTime
  evaluate opt
  putStrLn $ "optimize," ++ show (t1 - t0)
  let doc = effectfulAST life
  t2 <- getMonotonicTime
  evaluate doc
  putStrLn $ "effectfulAST," ++ show (t2 - t1)
  let bytes = T.length (renderJSCompact doc)
  t3 <- getMonotonicTime
  evaluate bytes
  putStrLn $ show bytes ++ "," ++ show (t3 - t2)
