import Data.List.Split

test="16,1,2,0,4,2,7,1,2,14"

parseLine :: String -> [Int]
parseLine s = map (read) $ splitOn "," s

linearCostToMoveTo :: [Int] -> Int  -> Int
linearCostToMoveTo locs desired = sum $ map (\x->abs (x-desired)) locs

growingCostToMoveTo :: [Int] -> Int -> Int
growingCostToMoveTo locs desired = sum $ map (\x -> triangle x desired) locs
  where triangle x desired = div (n*(n+1)) 2
            where n = abs (x-desired)


minCostToMoveTo :: ([Int] -> Int -> Int) -> [Int] -> Int
minCostToMoveTo costFunc locs = minimum $ map (costFunc locs) [low..high]
  where low = minimum locs
        high = maximum locs

part1 = do
  contents <- parseLine <$> readFile "inputs/day07.txt"
  let mc = minCostToMoveTo linearCostToMoveTo contents
  return mc


part2 = do
  contents <- parseLine <$> readFile "inputs/day07.txt"
  let mc = minCostToMoveTo growingCostToMoveTo  contents
  return mc
