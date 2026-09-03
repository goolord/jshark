{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified Data.Text as T
import JShark
import JShark.Api
import JShark.Api.Rec (Rec (..))
import JShark.Lucid
import Lucid (button_, class_, div_, label_, li_, type_)
import System.CPUTime
import Text.Printf

-- A large Lucid template to benchmark
largeTemplate :: Int -> JsHtml f ()
largeTemplate n =
  div_ [class_ "container"] $
    mapM_
      (\i -> todoRow (string ("Todo " <> T.pack (show i))) (bool (i `mod` 2 == 0)))
      [1 .. n]

todoRow :: Expr f 'String -> Expr f 'Bool -> JsHtml f ()
todoRow title isDone = li_ $ do
  classWhen isDone "completed"
  div_ [class_ "view"] $ do
    voidWith_ "input" [class_ "toggle", type_ "checkbox"] (prop "checked" isDone)
    label_ (dynText title)
    button_ [class_ "destroy"] mempty

benchmarkTemplate :: Int -> ClosedEffect 'Unit
benchmarkTemplate n = stmts $ renderInto (ffi "document.body" RecNil) (largeTemplate n)

timeIt :: String -> IO a -> IO a
timeIt label action = do
  start <- getCPUTime
  res <- action
  end <- getCPUTime
  let
    diff = fromIntegral (end - start) / 1e9 :: Double
  printf "%s: %0.3f ms\n" label diff
  return res

main :: IO ()
main = do
  putStrLn "Running manual benchmarks..."

  let
    test n = do
      putStrLn $ "\nTesting n=" ++ show n
      timeIt "  optimized" $ do
        let
          !size = optimizedEffectSize (benchmarkTemplate n)
        printf "    size: %d\n" size

      timeIt "  unoptimized" $ do
        let
          !js = renderJSCompact (effectfulAST (benchmarkTemplate n))
        printf "    js length: %d\n" (T.length js)

  test 1
  test 5
  test 10
