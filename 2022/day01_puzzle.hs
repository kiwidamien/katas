import System.IO
import Control.Monad
import Data.List


main = do
    contents <- readFile "day01_input.txt"
    let elfs = elfCalorieParse . split $ contents
      in do
      print . elfWithMaxCalories $ elfs
      print . topThreeElves $ elfs
    

-- Parsing (dear lord!)
-- Turns out this is basically the buildin `words` function
split :: String -> [String]
split [] = [""]
split (c:cs)
  | c=='\n' = "" : rest
  | otherwise = (c :head rest): tail rest
  where
    rest = split cs

elfCalorieParse :: [String] -> [[Int]]
elfCalorieParse my_lines
  | my_lines == [] = []
  | otherwise = (map readInt $ (takeWhile (/="") my_lines)): elfCalorieParse (drop 1 (dropWhile (/="") my_lines))


readInt :: String -> Int
readInt = read

-- Actual problem code
elfWithMaxCalories :: [[Int]] -> Int
elfWithMaxCalories elfs = maximum . map (sum) $ elfs

topThreeElves elfList = sum $ take 3 $ reverse $ sort $ map (sum) elfList
