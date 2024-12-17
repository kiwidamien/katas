import Data.Char (ord)
import Data.List (nub)
import qualified Data.Map as M 

type Loc = (Int, Int)
data Grid = Grid Int Int (M.Map Loc Int) deriving (Show, Eq)

charToInt :: Char -> Int
charToInt c = ord c - ord '0'

parse :: String -> Grid
parse s = Grid nRows nCols mymap
  where rows = lines s
        (nRows, nCols) = (length rows, length $ head rows)
        mymap = M.fromList $ concat $ map (\(r, line) -> [((r,col), charToInt c) | (col, c) <- zip [0..] line]) $ zip [0..] $ rows

getHeight :: Grid -> Loc -> Int 
getHeight (Grid _ _ m) loc = M.findWithDefault (-10) loc m 

nextStep :: Grid -> Loc -> [Loc]
nextStep g (y,x) = filteredPoss
    where currentHeight = getHeight g (y,x)
          possibilities = [(y+1, x), (y, x+1), (y-1, x), (y, x-1)]
          filteredPoss = filter (\loc -> let h = getHeight g loc in h==(currentHeight + 1)) possibilities

fixed :: Eq a => (a -> a) -> a -> a 
fixed f x 
  | x' == x = x
  | otherwise = fixed f x' 
  where x' = f x 

trails :: Grid -> Loc -> [[Loc]]
trails g start = map reverse $ fixed go [[start]]
  where go :: [[Loc]] -> [[Loc]] 
        go paths = concat $ map (\(e, rest) -> let (h, n) = (head e, nextStep g h) in if length n > 0 then map (\ns -> (ns:h:rest)) n else [e ++ rest]) $ map (splitAt 1) paths

numEndpoints :: Grid -> Loc -> Int 
numEndpoints g start = length $ nub $ map head $ filter (\loc -> (getHeight g $ head loc) == 9) $ map reverse $ trails g start 

numTrails :: Grid -> Loc -> Int 
numTrails g start = length $ nub $ filter (\loc -> (getHeight g $ head loc) == 9) $ map reverse $ trails g start 


findTrailheads :: Grid -> [Loc]
findTrailheads (Grid _ _ m) = [loc | (loc, v) <- M.toList m, v==0]

totalScore :: Grid -> Int 
totalScore g = sum $ map (numEndpoints g) $ findTrailheads g

totalRating :: Grid -> Int
totalRating g = sum $ map (numTrails g) $ findTrailheads g

part1 :: String -> IO ()
part1 filename = do 
    grid <- parse <$> readFile filename
    print(totalScore grid)

part2 :: String -> IO ()
part2 filename = do 
    grid <- parse <$> readFile filename
    print(totalRating grid)