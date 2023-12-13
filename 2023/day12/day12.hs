module Day12 where 

import Data.List (group)
import Data.Array.IArray as A

data Tile = Active | Inactive | Unknown deriving (Eq)

instance Show Tile where 
    show Active = "#"
    show Inactive = "."
    show Unknown = "?"

char2Tile :: Char -> Tile 
char2Tile '#' = Active
char2Tile '.' = Inactive
char2Tile '?' = Unknown 

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

commaSepNums :: String -> [Int]
commaSepNums s = map read $ split ',' s

countLengths :: [Tile] -> [Int]
countLengths tiles = map length $ filter (\lst -> (head lst) == Active) $ group tiles

line = "???.### 1,1,3"

{-
Not used anymore. Explicitly creates all 2^n strings, where n is the number of
    Unknown tiles in the input.

Replaced with partialActualize, which expands one Unknown tile at a time, and then
    prunes
-}
actualize :: [Tile] -> [[Tile]]
actualize [] = [[]]
actualize (h:hs)
  | h == Unknown = [Active:r | r <- rest] ++ [Inactive : r| r <- rest]
  | otherwise = [h : r | r <- rest]
    where rest = actualize hs 
 
parseLine :: String -> ([Tile], [Int])
parseLine line = (map char2Tile tiles, commaSepNums order)
  where tiles = head $ words line
        order = head $ drop 1 $ words line

numWays :: [Tile] -> [Int] -> Int 
numWays tiles groups = length $ filter (== groups) $ map countLengths $ actualize tiles 

groupOne :: Eq a => [a] -> ([a], [a])
groupOne [] = ([], [])
groupOne lst = (takeWhile (==h) lst, dropWhile (==h) lst)
  where h = head lst 

partialActualize :: [Tile] -> [Int] -> [[Tile]]
partialActualize [] [] = [[]]
partialActualize [] _ = [] 
partialActualize tiles [] = if any (==Active) tiles then [] else [replicate (length tiles) Inactive]
partialActualize tiles nums
  | tileType == Inactive = partialActualize rest nums 
  | (tileType == Active) && (length gp == head nums) = partialActualize ([Inactive] ++ (drop 1 rest)) (tail nums)
  | (tileType == Active) && (length gp > head nums) = []
  | (tileType == Active) && (length gp < head nums) = if startUnknown then (partialActualize (gp ++ [Active] ++ (drop 1 rest)) nums) else [] 
  | (tileType == Unknown) = (partialActualize ([Active] ++ (drop 1 gp) ++ rest) nums) ++ (partialActualize ([Inactive] ++ (drop 1 gp) ++ rest)nums)
    where (gp, rest) =  groupOne tiles
          tileType = head gp 
          startUnknown = (not (null rest)) && (head rest == Unknown)

{-
 - Inactive are separators only, so collapse consecutive occurances to aid memoization
 -}
collapse :: [Tile] -> [Tile]
collapse tiles = concat $ map (replaceUnk) $ group tiles
  where replaceUnk [] = []
        replaceUnk (t:ts) = if t == Inactive then [Inactive] else (t:ts) 

makesGroup :: Int -> [Tile] -> Bool 
makesGroup gpSize tiles = canBeBlock && canTerminate && (length tileGp == gpSize)
  where firstTiles = take (gpSize + 1) tiles 
        tileGp = take gpSize firstTiles
        canTerminate = (length firstTiles == gpSize) || (last firstTiles /= Active)
        canBeBlock = not $ any (==Inactive) tileGp

{-
Memoization technique (e.g. using the data structure) AND deciding how to label the subproblems
done by Alan Malloy in this video:
    https://www.youtube.com/watch?v=ltrI20JUAdU
Highly recommended 
-}
mPartialActualize :: [Tile] -> [Int] -> Int 
mPartialActualize tiles labels = subproblems A.! (0, 0)
  where newTiles = collapse $ tiles ++ [Inactive]
        h = length newTiles
        w = length labels
        subproblems :: A.Array (Int, Int) Int 
        subproblems = A.array ((0, 0), (h, w)) $ do
            nTilesProcessed <- [0..h]
            nGroupsProcessed <- [0..w]
            pure ((nTilesProcessed, nGroupsProcessed), subproblem nTilesProcessed nGroupsProcessed)
        subproblem :: Int -> Int -> Int
        subproblem nTile nGroups = case (drop nTile newTiles, drop nGroups labels) of 
            ([], []) -> 1
            ([], (_:_)) -> 0
            (tiles, []) -> if any (==Active) tiles then 0 else 1
            ((tile:ts), groups) -> case tile of 
                Inactive -> skip 
                Active -> use 
                Unknown -> skip + use 
                where skip = subproblems A.! (nTile + 1, nGroups)
                      use = case ts of 
                        [] -> 0
                        otherwise -> if makesGroup (head groups) (tile:ts) then subproblems A.! (nTile + (head groups) + 1, nGroups + 1) else 0


expand :: [Tile] -> [Tile]
expand tiles = tiles ++ [Unknown] ++ tiles ++ [Unknown] ++ tiles ++ [Unknown] ++ tiles ++ [Unknown] ++ tiles

part1 :: String -> IO ()
part1 filename = do
    contents <- lines <$> readFile filename 
    let tilesAndGroups = map parseLine contents
    let countWays = map (\(tiles, nums) -> length $ partialActualize tiles nums) tilesAndGroups
    print(sum $ countWays)


part1Fast :: String -> IO ()
part1Fast filename = do 
    contents <- lines <$> readFile filename 
    let tilesAndGroups = map parseLine contents
    let countWays = map (uncurry mPartialActualize) tilesAndGroups
    print(sum $ countWays)


part2 :: String -> IO ()
part2 filename = do
    contents <- lines <$> readFile filename 
    let tilesAndGroups = map parseLine contents
    let countWays = map (\(tiles, nums) -> mPartialActualize (expand tiles) (concat $ replicate 5 nums)) tilesAndGroups
    print(sum $ countWays)
