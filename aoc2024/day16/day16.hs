import qualified Data.Set as S 
import qualified Data.List as L 
import qualified Data.Heap as H 


type Loc = (Int, Int)
type Grid = S.Set Loc 
type Visited = S.Set Loc
data Direction = N | E | S | W deriving (Show, Eq, Ord)
data State = State (Int, Int) Direction deriving (Show, Eq, Ord)

type Queue = H.Heap H.FstMinPolicy (Integer, State)


getDirection :: (Int, Int) -> Direction 
getDirection (-1, 0) = N 
getDirection (0, 1) = E 
getDirection (1, 0) = S 
getDirection (0, -1) = W 

rotateCW :: Direction -> Direction
rotateCW N = E 
rotateCW E = S 
rotateCW S = W 
rotateCW W = N 

rotateCCW :: Direction -> Direction 
rotateCCW = rotateCW . rotateCW . rotateCW 

rotateCost :: Direction -> Direction -> Int 
rotateCost a b
  | a == b = 0 
  | a == rotateCW b = 1000 
  | a == rotateCCW b = 1000 
  | otherwise = 2000 

forward :: State -> State
forward (State (y,x) d)
  | d == N = State (y-1, x) d
  | d == S = State (y+1, x) d
  | d == E = State (y, x-1) d
  | d == W = State (y, x+1) d


move :: Grid -> Loc -> [Loc]
move g (y, x) = [p | p <- poss, not (elem p g)]
  where poss = [(y+1, x), (y, x+1), (y-1,x), (y, x-1)]

possNextState :: Grid -> S.Set State -> State -> [(State, Integer)]
possNextState g visited (State loc d) = filter (\(s, _) -> not (S.member (getLoc s) g)) $ filter (\(s, _) -> not (S.member s visited)) nextStateAndCost
  where nextStateAndCost = [(forward (State loc d), 1), (State loc (rotateCW d), 1000), (State loc (rotateCW $ rotateCW d), 2000), (State loc (rotateCCW d), 1000)]
        getLoc (State location _) = location


bfs :: Grid -> State -> Loc -> Integer
bfs g start end = go (H.singleton (0, start)) (S.singleton start) end
  where go :: Queue -> S.Set State -> Loc -> Integer
        go queue visited end
          | 0 == H.size queue = (-1)
          | loc == end = nextCost
          | otherwise = go newQueue (S.insert (State loc d) visited) end
            where 
                  ((nextCost, State loc d), remHeap) = case H.view queue of 
                    Just (ns, rem) -> (ns, rem)
                    Nothing -> undefined
                  newStates = possNextState g visited (State loc d)
                  newCosts =  (map (\(s,c) -> (nextCost + c, s)) newStates)
                  newQueue = foldl (\acc item -> H.insert item acc) remHeap newCosts


dfs' :: Grid -> (Integer, [State]) -> Integer -> State -> Loc -> [(Integer, [State])]
dfs' g (costSoFar, v) costLimit (State start iniDir) end
  | start == end = [(costSoFar, v)]
  | (length nextMoves) == 0 = []
  | otherwise = concat $ [dfs' g (cost', newStart:v) costLimit newStart end | (cost', newStart) <- nextMoves]
  where nextMoves = [(costSoFar + c, l) | (l, c) <- possNextState g (S.fromList v) (State start iniDir), costSoFar + c <= costLimit]

findPaths' :: Grid -> Integer -> State -> Loc -> [(Integer, [State])]
findPaths' g limit iniState end = dfs' g (0, [iniState]) limit iniState end

getDistinctLocations :: [State] -> S.Set Loc 
getDistinctLocations path = S.fromList $ map getLoc path 
  where getLoc (State loc _) = loc

-- 
dfs :: Grid -> [Loc] -> Loc -> Loc -> [[Loc]]
dfs g v start end
  | start == end = [v]
  | (length nextMoves) == 0 = []
  | otherwise = concat $ [dfs g (nextMove:v) nextMove end | nextMove <- nextMoves]
  where nextMoves = [l | l<-move g start, not (elem l v)]


findPaths :: Loc -> Loc -> Grid -> [[Loc]]
findPaths start end g = dfs g [start] start end

scoreMove :: Direction -> Loc -> Loc -> (Int, Direction)
scoreMove d (ys, xs) (ye, xe) = ((rotateCost d d') + 1, d')
  where delta = (ye - ys, xe - xs)
        d' = getDirection delta

scorePath :: Direction -> [Loc] -> Int 
scorePath d [] = 0
scorePath d [p] = 0
scorePath d (p:q:rest) = let (moveCost, newD) = scoreMove d p q in moveCost + scorePath newD (q:rest)


parseGrid :: String -> Grid 
parseGrid s = S.fromList $ concat $ map (\(r, line) -> [(r,c)|(c, letter) <- zip [0..] line, letter=='#']) $ zip [0..] $ lines s

parseStart :: String -> (Loc, Direction)
parseStart s = (sloc, E)
  where sloc = head $ concat $ map (\(r, line) -> [(r,c)|(c, letter) <- zip [0..] line, letter=='S']) $ zip [0..] $ lines s

parseEnd :: String -> Loc 
parseEnd s = head $ concat $ map (\(r, line) -> [(r,c)|(c, letter) <- zip [0..] line, letter=='E']) $ zip [0..] $ lines s

part1 :: String -> IO () 
part1 filename = do 
    contents <- readFile filename
    let grid = parseGrid contents
    let (start, iniDir) = parseStart contents 
    let end = parseEnd contents
    let paths = findPaths start end grid
    let scores = L.sort $ map (\path -> scorePath iniDir path) paths
    print(length scores)
    print(head scores)

part1' :: String -> IO () 
part1' filename = do 
    contents <- readFile filename
    let grid = parseGrid contents
    let (start, iniDir) = parseStart contents 
    let end = parseEnd contents
    let scoreMin = bfs grid (State start iniDir) end 
    print(scoreMin)

part2 :: String -> IO () 
part2 filename = do 
    contents <- readFile filename
    let grid = parseGrid contents
    let (start, iniDir) = parseStart contents 
    let end = parseEnd contents
    let scoreMin = bfs grid (State start iniDir) end 
    print(scoreMin)
    let paths = findPaths' grid scoreMin (State start iniDir) end
    let distinctLocs = getDistinctLocations $ concat $ map snd paths 
    print(length paths)
    print(length distinctLocs)