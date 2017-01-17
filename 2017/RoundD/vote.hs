import Data.List.Split
import System.Environment

-- https://code.google.com/codejam/contest/5264486/dashboard

data Vote = Yay | Nay deriving (Eq)

isGoodVote :: [Vote] -> Bool
isGoodVote [] = True
isGoodVote s = result > 0.5
  where result = (fromIntegral good) / (fromIntegral $ length s)
        good   = length . filter (== Yay) $ s

numGoodVotes :: Int -> Int -> [Vote] -> Int
numGoodVotes 0 0 s = if (isGoodVote s) then 1 else 0
numGoodVotes n m s = if (isGoodVote s) then (yayVotes * n) + (nayNotes * m) else 0
  where yayVotes = if n > 0 then numGoodVotes (n-1) m $ s ++ [Yay] else 0
        nayNotes = if m > 0 then numGoodVotes n (m-1) $ s ++ [Nay] else 0

calculatePercentage :: (Fractional a) => Int -> Int -> a
calculatePercentage n m = (fromIntegral $ numGoodVotes n m []) / (fromIntegral $ factorial (n + m))
  where factorial 0 = 1
        factorial n = n * (factorial (n - 1))

stringToInt :: String -> Int
stringToInt s = read s :: Int

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let lof = map (splitOn " ") $ drop 1 $ lines content
      results = map (\p -> calculatePercentage (stringToInt $ p !! 0) (stringToInt $ p !! 1)) lof
      output = foldl (\out (c, v) -> out ++ "Case #" ++ show c ++ ": " ++ show v ++ "\n") ""  $ zip [1..] results 
  writeFile (args !! 1) output
  

      
