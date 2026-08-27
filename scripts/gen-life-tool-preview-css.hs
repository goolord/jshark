{-# LANGUAGE OverloadedStrings #-}
module Main where

import Numeric (showFFloat)
import Patterns (disturbPatterns, gliderSpeciesSid, patId, speciesColor)
import Types (eraserToolSid, gliderToolSid, mouseToolSid)

rgb :: (Int, Int, Int) -> String
rgb (r, g, b) = "rgb(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

rule :: Int -> String
rule sid
  | sid == eraserToolSid =
      ".life-tool[data-tool=\""
        ++ show sid
        ++ "\"] .life-tool-cell.is-on { background: #e55; }"
  | sid == mouseToolSid =
      ".life-tool[data-tool=\""
        ++ show sid
        ++ "\"] .life-tool-cell.is-on { background: #aaa; }"
  | sid == gliderToolSid =
      let c = rgb (speciesColor gliderSpeciesSid)
       in ".life-tool[data-tool=\""
            ++ show sid
            ++ "\"] .life-tool-cell.is-on { background: "
            ++ c
            ++ "; }"
  | otherwise =
      let s = show sid
          c = rgb (speciesColor sid)
       in ".life-tool[data-tool=\""
            ++ s
            ++ "\"] .life-tool-cell.is-on { background: "
            ++ c
            ++ "; }"

main :: IO ()
main = do
  putStrLn "/* Generated — run scripts/gen-life-tool-preview-css.sh */"
  putStrLn ""
  putStrLn ".life-tool-preview {"
  putStrLn "  display: grid;"
  putStrLn "  grid-auto-rows: 3px;"
  putStrLn "  gap: 1px;"
  putStrLn "  justify-content: center;"
  putStrLn "}"
  putStrLn ""
  mapM_
    ( \n ->
        putStrLn $
          ".life-tool-preview[data-tw=\""
            ++ show n
            ++ "\"] { grid-template-columns: repeat("
            ++ show n
            ++ ", 3px); }"
    )
    [1 .. 12]
  putStrLn ""
  mapM_ (putStrLn . rule) (map patId disturbPatterns)
  mapM_ (putStrLn . rule) [eraserToolSid, mouseToolSid, gliderToolSid]
