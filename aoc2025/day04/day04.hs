import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map


parseLine :: String -> [Int]
parseLine line = map snd $ filter (\(c, i) -> c=='@') $ zip line [1..]

parseGrid :: [String] -> [(Int, Int)]
parseGrid grid = concat $ map (\(line, line_no) -> [(x, line_no) | x <- parseLine line]) collection
    where collection = zip grid [1..]

countNeighbors :: [(Int, Int)] -> Map (Int, Int) Int
countNeighbors locs = Map.fromList [(loc, count' loc) | loc <- locs]
    where saved_locations = Set.fromList locs
          count' (x,y) = (length $ Set.intersection (Set.fromList [(x + deltaX, y + deltaY) | deltaX <- [-1,0,1], deltaY <- [-1,0,1]]) saved_locations) - 1


remove4OrLess:: Map (Int, Int) Int -> [(Int, Int)]
remove4OrLess counter = Map.keys $ Map.filter (\neighbor -> neighbor >= 4) counter

countAndRemove :: [(Int, Int)] -> [(Int, Int)]
countAndRemove locs = remove4OrLess $ countNeighbors locs


fixPoint :: [(Int, Int)] -> [(Int, Int)]
fixPoint locs = let 
    next = countAndRemove locs
    in if next == locs then locs else fixPoint next


part_one :: String -> IO ()
part_one filename = do
    grid <- parseGrid <$> lines <$> readFile filename
    let end_count = length $ remove4OrLess $ countNeighbors $ grid
    let removed = (length grid) - end_count
    print(removed)

part_two :: String -> IO ()
part_two filename = do
    grid <- parseGrid <$> lines <$> readFile filename
    let end_count = length $ fixPoint $ grid
    let removed = (length grid) - end_count
    print(removed)