module Day17 where 

import Data.Char (digitToInt)
import qualified Data.Set as S 
import qualified Data.Map as M
import Data.List (sort)


type Grid = [[Int]]
type Loc = (Int, Int)
type WeightedLoc = (Int, Loc)
type WeightedGrid = M.Map Loc Int

data St = St { _stLoc:: Loc,  _stDir:: Direction, _stTimes:: Int} deriving (Eq, Ord, Show)
data Direction = N|E|S|W deriving (Eq, Ord, Bounded, Enum, Show)


parse :: String -> Grid
parse = map (map digitToInt) . lines

stateInGrid :: St -> WeightedGrid -> Bool 
stateInGrid (St (y,x) _ _) g = (x >=0) && (y >=0) && (x <= mx) && (y <= my)
  where (my, mx) = maximum $ M.keys g 

weightedGrid :: Grid -> WeightedGrid
weightedGrid g = M.fromList [((r,c), g!!r!!c) | r <- [0..(h-1)], c <- [0..(w-1)]]
  where h = length g 
        w = length $ head g

neighbors :: Loc -> [Loc]
neighbors (y,x) = [(y+1, x), (y, x+1), (y-1, x), (y, x-1)]

move :: Loc -> Direction -> Loc 
move (y,x) d
  | d == N = (y-1, x)
  | d == S = (y+1, x)
  | d == E = (y, x+1)
  | d == W = (y, x-1)

whereToGo :: St -> [St]
whereToGo (St (y,x) d n) = filter prune $ map (\d' -> St (move (y,x) d') d' (inc d')) (poss d)
  where poss N = [N, E, W]
        poss E = [E, N, S]
        poss S = [S, E, W]
        poss W = [W, N, S]
        inc dir= if (d == dir) then n + 1 else 0
        prune (St _ _ n') = n' < 3

{-
BFS:
  - Priority Queue of states yet to visit, along with their best distances so far
  - Map of all finalized states to distances
-}
bfs :: WeightedGrid -> M.Map St Int -> [(Int, St)] -> M.Map St Int
bfs grid finalized [] = finalized 
bfs grid finalized ((w,s):xs) = bfs grid newFinalized newStack 
    where newFinalized = M.insert s w finalized
          alreadySeen = S.fromList $ M.keys finalized
          candidates = filter (\s' -> S.notMember s' alreadySeen) $ filter (\s' -> stateInGrid s' grid) $ whereToGo s
          withWeight = map (\s' -> (w + (M.findWithDefault (-1) (_stLoc s') grid), s')) candidates
          protoStack = withWeight ++ xs
          merged = foldl (\m (w', s') -> M.insertWith min s' w' m) M.empty protoStack
          newStack = map (\(s', w') -> (w', s')) $ M.toList merged  
          

-- path :: WeightedGrid -> Loc -> Loc -> Int
path grid start end = minimum $ map snd $ filter (\(s',_) -> (_stLoc s') == end) $ M.assocs search
  where iniStates = [St start d 0 | d <-[N,E,S,W]]
        iniNeighbor = S.toAscList $ S.fromList $ map (\s' -> (M.findWithDefault (-1) (_stLoc s') grid, s')) $ filter (\s' -> stateInGrid s' grid) $ concat $ map whereToGo iniStates
        search = bfs grid (M.fromList [(ini, 0) | ini <- iniStates]) iniNeighbor 
        

--simplePath :: WeightedGrid -> Int
simplePath grid = path grid (0,0) largest
  where largest = maximum $ M.keys grid


testGrid = weightedGrid [[1,1,1, 2, 2, 2], [3,3,3,4,4,4]]

part1 :: String -> IO()
part1 filename = do 
    contents <- readFile filename 
    let grid = weightedGrid $ parse $ contents
    print (simplePath grid)