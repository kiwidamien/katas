module Day03 (parse, howManyTreesPart1) where 

import Data.List


parse :: String -> [String]
parse = lines 


treeAtLocation :: Int -> Int -> [String] -> Bool
treeAtLocation row col lines = char == '#'
  where line = lines !! row
        modCol = (mod col (length line))
	char = line !! modCol

locations :: (Int, Int) -> Int -> [(Int, Int)]
locations (dy, dx) numRows = takeWhile (\(r,_) -> r < numRows) $ iterate inc (0,0)
  where inc (a,b) = (a+dy, b+dx)


howManyTreesPart1 :: [String] -> Int 
howManyTreesPart1 lines = length $ filter (\(r,c) -> treeAtLocation r c lines) $ [(r, 3*r)| r<-[0..(length lines - 1)]]


howManyTrees :: [String] -> (Int, Int) -> Int 
howManyTrees lines (dy, dx) = length $ filter (\(r,c) -> treeAtLocation r c lines) locs
  where locs = locations (dy, dx) (length lines)

treeCompPart2 :: [String] -> Int 
treeCompPart2 lines = foldl' (*) 1 $ map (\step -> howManyTrees lines step) steps
  where steps = [(1,1), (1, 3), (1, 5), (1, 7), (2, 1)]

main :: IO ()
main = do 
    input <- readFile "input.txt"
    let part1 = howManyTreesPart1 $ parse input
    print ("Part 1: " ++ (show part1) ++ " collisions")
    let part2 = treeCompPart2 $ parse input
    print("Part 2: " ++ (show part2) )

