import Data.List.Split
import System.Environment

-- The Last Word. 
-- https://code.google.com/codejam/contest/4304486/dashboard

appendOrPrepend :: String -> Char -> String
appendOrPrepend "" c = [c]
appendOrPrepend s c = if c >= (head s)
                      then [c] ++ s
                      else s ++ [c]

lastWord :: String -> String
lastWord w = foldl appendOrPrepend "" w

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let results = map lastWord . drop 1 $ lines content
      output = foldl (\out (c, v) -> out ++ "Case #" ++ show c ++ ": " ++ v ++ "\n") ""  $ zip [1..] results 
  writeFile (args !! 1) output
  
