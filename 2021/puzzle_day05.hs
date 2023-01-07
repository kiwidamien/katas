-- Given a set of points
-- (x1,y1), (x2,y2), find the set of points between them 
-- in a line with integer coords
-- e.g. (5,6) (5, 10) would give (5,6),(5,7),(5,8),(5,9),(5,10)
-- Assume either horizontal or vertical lines
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split
import Data.List


type Point = (Int, Int)

_genRange :: Int -> Int -> [Int]
_genRange a b = [low..high]
  where low = min a b
        high = max a b 


generatePoints_HV :: Point -> Point -> [Point]
generatePoints_HV (x1,y1) (x2,y2)
  | x1==x2 = map (\y -> (x1,y)) (_genRange y1 y2)
  | y1==y2 = map (\x-> (x,y1)) (_genRange x1 x2)
  | otherwise = []


generatePoints_HVD :: Point -> Point -> [Point]
generatePoints_HVD (x1,y1) (x2,y2) = map (\t->(step t n x1 x2, step t n y1 y2)) [0..n]
    where step t n a b = a + (div (b - a) (n))*t
          n = (max (abs (x1-x2)) (abs (y1-y2))) 


countIntersections :: [Point] -> Map Point Int
countIntersections pts = Map.fromList $ map (\x->(head x, length x)) $ groupBy (==) $ sort pts


numMultipleIntersections :: Map Point Int -> Int
numMultipleIntersections mymap = Map.size $ Map.filter (>1) mymap

test = [ 
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"]

parseLine :: String -> (Point, Point)
parseLine line = tupPack $ map parsePair $ map (\x->splitOn "," x) $ splitOn " -> " line 
    where parsePair (x:y:_) = (read x, read y)
          tupPack (x:y:_) = (x,y)


allPoints :: (Point->Point->[Point]) -> [String] -> [Point]
allPoints gnFunc lst = concat $ map (\x->gnFunc (fst x) (snd x)) $ map parseLine lst 


testCast_Pt1 = numMultipleIntersections $ countIntersections $ (
  allPoints generatePoints_HV test)


part1 = do
  ptLists <- allPoints generatePoints_HV <$> lines <$> readFile "inputs/day05.txt"
  let numIntersections = numMultipleIntersections $ countIntersections ptLists
  return numIntersections


part2 = do
  ptLists <- allPoints generatePoints_HVD <$> lines <$> readFile "inputs/day05.txt"
  let numIntersections = numMultipleIntersections $ countIntersections ptLists
  return numIntersections

