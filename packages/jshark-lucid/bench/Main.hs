{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Manual timing of a large Lucid template through the optimizing
-- pipeline and the plain emitter.
module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import JShark
import JShark.Api
import JShark.Api.Rec (Rec (..))
import JShark.Internal (effectfulAST, optimizedEffectSize)
import JShark.Lucid
import Lucid (button_, class_, div_, label_, li_, type_)
import System.CPUTime
import Text.Printf

-- | @n@ TodoMVC rows rendered into the body.
largeTemplate :: Int -> ClosedEffect 'Unit
largeTemplate n =
  stmts . renderInto (ffi "document.body" RecNil) . div_ [class_ "container"] $
    forM_ [1 .. n] $ \i -> do
      let
        isDone = bool (even i)
      li_ $ do
        classWhen isDone "completed"
        div_ [class_ "view"] $ do
          voidWith_ "input" [class_ "toggle", type_ "checkbox"] $
            prop "checked" isDone
          label_ (dynText (string ("Todo " <> T.pack (show i))))
          button_ [class_ "destroy"] mempty

timeIt :: String -> IO () -> IO ()
timeIt label action = do
  start <- getCPUTime
  action
  end <- getCPUTime
  printf "%s: %0.3f ms\n" label (fromIntegral (end - start) / 1e9 :: Double)

main :: IO ()
main = do
  putStrLn "Running manual benchmarks..."
  forM_ [1, 5, 10] $ \n -> do
    putStrLn ("\nTesting n=" ++ show n)
    timeIt "  optimized" $
      printf "    size: %d\n" (optimizedEffectSize (largeTemplate n))
    timeIt "  unoptimized" $
      printf
        "    js length: %d\n"
        (BS.length (renderJS (effectfulAST (largeTemplate n))))
