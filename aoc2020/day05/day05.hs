module Day05 where 

import Data.List
binary :: [Int] -> Int 
binary bits = foldl (\result new -> 2*result + new) 0 bits

mapBit :: Char -> Int 
mapBit 'F' = 0
mapBit 'L' = 0
mapBit 'R' = 1
mapBit 'B' = 1

rowNum :: String -> Int 
rowNum s = binary $ map mapBit $ filter (\c -> elem c "FB") s

colNum :: String -> Int 
colNum s = binary $ map mapBit $ filter (\c -> elem c "LR") s 

seatID :: String -> Int 
seatID s = 8*row + col 
  where row = rowNum s 
        col = colNum s 

maxIDPart1 :: [String] -> Int 
maxIDPart1 lines = maximum $ map seatID lines

seatIDWithConsecutive :: [String] -> [Int]
seatIDWithConsecutive lines = map (\(_,s,_) -> s) $ filter (not . areConsecutive) zipped 
  where ids = sort $ map seatID lines
        zipped = zip3 ids (tail ids) (tail $ tail ids)
	areConsecutive (a,b,c) = (a+1 == b) && (b+1 == c)

main :: IO ()
main = do 
  input <- lines <$> readFile "input.txt"
  let part1 = maxIDPart1 input
  print ("The maximum seat ID is " ++ (show part1))
  let part2 = seatIDWithConsecutive input
  print ("Candidates for your seat are " ++ (show part2))
