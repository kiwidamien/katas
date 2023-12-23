module Day23 where

import qualified Data.Set as S 
import qualified Data.Array.IArray as A
import qualified Data.Map as M
import Data.List (sort, nub, groupBy)
import Debug.Trace


type Loc = (Int, Int)
type Path = [Loc]

data Tile = Forest | MustLeft | MustRight | MustDown | MustUp | Empty deriving (Show, Eq, Ord) 
data Direction = N | E | S | W deriving (Eq, Show)
type Grid = A.Array Loc Tile


findPathSimple :: Grid -> Loc -> Loc -> [S.Set Loc]
findPathSimple grid start end = go nextStep grid S.empty start end

findPathPart2 :: Grid -> Loc -> Loc -> [S.Set Loc]
findPathPart2 grid start end = go nextStepPart2 grid S.empty start end 

nextStep :: Grid -> Loc -> [Loc]
nextStep grid (y,x) = case (A.!) grid (y,x) of
    Forest -> error "Cannot be in a forest tile"
    MustUp -> [(y-1, x)]
    MustLeft -> [(y, x-1)]
    MustRight -> [(y, x+1)]
    MustDown -> [(y+1, x)]
    Empty -> nonForest
        where possibilities = [(y-1, x), (y, x+1), (y+1, x), (y, x-1)]
              inGrid = filter (A.inRange (A.bounds grid)) possibilities
              nonForest = filter (\loc -> (A.!) grid loc /= Forest) inGrid


nextStepPart2 :: Grid -> Loc -> [Loc]
nextStepPart2 grid (y,x) = nonForest
    where possibilities = [(y-1, x), (y, x+1), (y+1, x), (y, x-1)]
          inGrid = filter (A.inRange (A.bounds grid)) possibilities
          nonForest = filter (\loc -> (A.!) grid loc /= Forest) inGrid


go :: (Grid -> Loc -> [Loc]) -> Grid -> (S.Set Loc) -> Loc -> Loc -> [S.Set Loc]
go stepGenFn grid seen current end
  | current == end = [S.insert end seen]
  | otherwise = concat [go stepGenFn grid (S.insert current seen) next end | next <- possible]
    where possible = filter (\p -> S.notMember p seen) $ stepGenFn grid current 

findEndpts :: Grid -> (Loc, Loc)
findEndpts grid = (head emptyFirstRow, head emptyLastRow)
  where emptyFirstRow = [(0, x) | x <- [0..lX], (A.!) grid (0,x) == Empty]
        emptyLastRow = [(lY, x) | x <- [0..lX], (A.!) grid (lY,x) == Empty]
        (_, (lY, lX)) = A.bounds grid


parseTile :: Char -> Tile
parseTile '#' = Forest
parseTile '.' = Empty
parseTile '>' = MustRight
parseTile '<' = MustLeft 
parseTile 'v' = MustDown
parseTile '^' = MustUp


gridFromFile :: String -> IO Grid
gridFromFile filename = do 
    contents <- lines <$> readFile filename 
    let grid::Grid = let 
                       lY = (length $ contents) - 1
                       lX = (length $ head $ contents) - 1
                     in A.listArray ((0,0), (lY, lX)) $ map (parseTile) $ concat contents 
    return grid


part1 :: String -> IO ()
part1 filename = do 
    grid <- gridFromFile filename
    let (start, end) = findEndpts grid
    print (map (\x -> x-1) $ sort $ map length $ findPathSimple grid start end)

part2 :: String -> IO ()
part2 filename = do 
    grid <- gridFromFile filename
    let (start, end) = findEndpts grid
    print (choicePoints grid)
    print (map (\x -> x-1) $ sort $ map length $ findPathPart2 grid start end)


-- Part 2 stuff

type WeightedGrid = M.Map Loc (M.Map Loc Int)

nextStep' :: Grid -> Loc -> [Loc]
nextStep' grid (y,x) = nonForest
    where possibilities = [(y-1, x), (y, x+1), (y+1, x), (y, x-1)]
          inGrid = filter (A.inRange (A.bounds grid)) possibilities
          nonForest = filter (\loc -> (A.!) grid loc /= Forest) inGrid


choicePoints :: Grid -> [Loc]
choicePoints g = [(y,x) | y <- [0..lY], x <-[0..lX], (A.!) g (y,x) /= Forest, length (nextStepPart2 g (y,x)) /= 2]
  where (_, (lY, lX)) = A.bounds g


simplePath :: Grid -> Loc -> [((Loc,Loc), Int)]
simplePath grid iniLoc = map (\neigh -> go' [neigh, iniLoc] neigh) next
  where choices = choicePoints grid
        next = nextStep' grid iniLoc
        go' seen current
          | elem current choices = ((iniLoc, current), length (nub $ current:seen) - 1)
          | length next' == 1 = go' (current:seen) (head next')
          | otherwise = error "wtf?"
            where next' = filter (\p -> notElem p seen) $ nextStep' grid current


contract :: Grid -> WeightedGrid
contract grid = M.fromList miniMaps
  where choices = choicePoints grid
        paths = groupBy (\a b -> (fst (fst a)) == (fst (fst b))) $ sort $ concat $ map (simplePath grid) choices
        miniMaps = map (\gp -> let top = head gp in (fst $ fst top, buildMap $ map (\((a,b),c) -> (b,c)) gp)) paths
        buildMap tups = foldl (\m (loc, dist) -> M.insertWith max loc dist m) M.empty tups


dfs :: WeightedGrid -> Loc -> Loc -> [[Loc]]
dfs grid start end = helper grid [] start end
  where helper :: WeightedGrid -> [Loc] -> Loc -> Loc -> [[Loc]]
        helper grid seen current end
          | current == end = [reverse $ current:seen]
          | otherwise = concat [helper grid (current:seen) current' end| current' <- possible]
            where possible' = case M.lookup current grid of 
                                Nothing -> error "Cannot find this location"
                                Just edges -> M.keys edges
                  possible = [p | p <- possible', notElem p seen]

pathLength :: WeightedGrid -> [Loc] -> Int
pathLength grid path = helper grid (0) path
  where helper :: WeightedGrid -> Int -> [Loc] -> Int 
        helper _ acc [] = acc
        helper grid acc [p] = acc
        helper grid acc (src:dest:rest) = helper grid (acc + this_edge) (dest:rest)
          where this_edge = case M.lookup src grid of 
                              Nothing -> error ("src unknown: " ++ show src)
                              Just dMap -> case M.lookup dest dMap of 
                                Nothing -> error("dest unknwon" ++ show dest)
                                Just edge -> edge 

part2again :: String -> IO () 
part2again filename = do 
    grid <- gridFromFile filename
    let (start, end) = findEndpts grid
    let weightGrid = contract grid
    print (show weightGrid)
    let paths = dfs weightGrid start end
    let lengths = map (pathLength weightGrid) paths
    print $ sort lengths


myMap = (A.listArray ((0,0), (4,4)) $ map (parseTile) $ concat ["#.###", 
                                  "#...#",
                                  "#.#.#",
                                  "#...#",
                                  "###.#"])::Grid

wmap = contract myMap