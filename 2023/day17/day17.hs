module Day17 where 

import Data.Char (digitToInt)
import qualified Data.Set as S 
import qualified Data.Map as M
import qualified Data.Array.IArray as A
import Data.List (sort)
--import qualified Data.Heap as H

type Grid = [[Int]]
type Loc = (Int, Int)
type WeightedLoc = (Int, Loc)
type WeightedGrid = A.Array Loc Int

data St = St { _stLoc:: Loc,  _stDir:: Direction, _stTimes:: Int} deriving (Eq, Ord, Show)
data Direction = N|E|S|W deriving (Eq, Ord, Bounded, Enum, Show)


parse :: String -> WeightedGrid
parse s = A.listArray ((0,0), (maxY, maxX)) $ concat grid
    where grid = map (map digitToInt) $ lines s
          maxY = length grid - 1
          maxX = (length $ head grid) - 1

_inc :: Int -> Direction -> Direction -> Int 
_inc n d dd = if d==dd then n+1 else 1

move :: St -> Direction -> St 
move (St (y,x) d n) N = St (y-1, x) N (_inc n d N)
move (St (y,x) d n) S = St (y+1,x ) S (_inc n d S)
move (St (y,x) d n) E = St (y, x+1) E (_inc n d E)
move (St (y,x) d n) W = St (y, x-1) W (_inc n d W)

stateGen :: WeightedGrid -> St -> [St]
stateGen g st = filter (\s -> let loc = _stLoc s in A.inRange (A.bounds g) loc) $ map (move st) $ dirList (_stDir st)
  where dirList N = [N,E,W]
        dirList S = [S,E,W]
        dirList E = [E,S,N]
        dirList W = [W,S,N]

update :: M.Map St Int -> St -> Int -> M.Map St Int 
update oldMap state candidateWeight = M.insertWith min state candidateWeight oldMap

bfs :: WeightedGrid -> Loc -> Loc -> (Int, Int) -> M.Map St Int 
bfs grid start end (minStraight, maxStraight) = go M.empty S.empty iniPriorityQ
  where iniPriorityQ = M.fromList [(St start d 0, 0) | d <- [N, E, S, W]]
        go :: M.Map St Int -> S.Set St -> M.Map St Int -> M.Map St Int 
        go finalized seen pQ 
          | length pQ == 0 = finalized
          | otherwise = go finalized' seen' pQ'
            where (minW, minState) = minimum (map (\(a,b) -> (b,a)) $ M.assocs pQ)
                  finalized' = M.insert minState minW finalized
                  seen' = S.insert minState seen
                  candidateStates = if (_stTimes minState) <= minStraight then filter (\s -> let loc = _stLoc s in A.inRange (A.bounds grid) loc) $ [move minState (_stDir minState)] else stateGen grid minState
                  newStates = filter (\s -> S.notMember s seen') $ filter (\s -> (_stTimes s) <= maxStraight) candidateStates
                  pQ' = foldl (\acc ns -> let newWeight = minW + (A.!) grid (_stLoc ns) in update acc ns newWeight) (M.delete minState pQ) newStates
        

shortestPath :: WeightedGrid -> Loc -> Loc -> (Int, Int) -> Int
shortestPath grid start end (minStraight, maxStraight) = minimum endPoint 
  where distMap = bfs grid start end (minStraight, maxStraight)
        endPoint = map snd $ filter (\(s, _) -> (end == (_stLoc s))) $ M.assocs distMap

crossPath :: WeightedGrid -> (Int, Int) -> Int 
crossPath grid (minStraight, maxStraight) = shortestPath grid start end (minStraight, maxStraight)
    where (start, end) = A.bounds grid


part1 :: String -> IO ()
part1 filename = do 
    contents <- readFile filename
    let grid = parse contents 
    let cost = crossPath grid (0, 3)
    print cost 

part2 :: String -> IO ()
part2 filename = do 
    contents <- readFile filename
    let grid = parse contents 
    let cost = crossPath grid (4, 10)
    print cost 

testArray :: WeightedGrid 
testArray = A.listArray ((0,0), (1,5)) [1,1,1,2,2,2,3,3,3,4,4,4]

main :: IO ()
main = part2 "input.txt"