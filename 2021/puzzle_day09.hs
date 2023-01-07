import Data.Char

s="2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

test = [
  [1,2,3,7,5,3,5,3],
  [3,2,2,1,5,7,8,3],
  [3,2,3,5,2,7,1,4]
  ] :: [[Int]]


getNeighbors :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getNeighbors (r, c) (h, w) = (
        filter (\x->(snd x>=0)&&(snd x<w)) $ 
        filter (\x-> ((fst x) >=0)&&((fst x) < h)) protoN 
  )
  where protoN = [(r-1, c), (r+1, c), (r, c+1), (r, c-1)]


isLowPoint :: (Int, Int) -> [[Int]] -> Bool
isLowPoint (r,c) grid = foldl (&&) True $ map (\loc -> val < (grid !! (fst loc) !! (snd loc)) )$ getNeighbors (r,c) (h,w) 
    where h = length grid
          w = length (grid!!0)
          val = (grid !! r) !! c


numLowPoints :: [[Int]] -> Int
numLowPoints grid = length $ filter (\x->x) $ map (\loc-> isLowPoint loc grid) locs
  where locs = [(r,c) | r <- [0..(h-1)], c <- [0..(w-1)]]
        h = length grid
        w = length $ grid!!0

getLowPoints :: [[Int]] -> [Int]
getLowPoints grid = map (\loc->(grid!!(fst loc))!!(snd loc)) $ filter (\loc -> isLowPoint loc grid) locs
  where locs = [(r,c) | r<-[0..(h-1)], c<-[0..(w-1)]]
        h = length grid
        w = length $ grid!!0 


getRiskValue :: [[Int]] -> Int
getRiskValue grid = sum $ map (+ 1) $ getLowPoints grid


parseGrid :: String -> [[Int]]
parseGrid s = map (\line->map readDigit line) $ lines s
  where readDigit digit = ord(digit) - ord('0')

part1 = do
  grid <- parseGrid <$> readFile "inputs/day09.txt"
  let totalRisk = getRiskValue grid
  return totalRisk
