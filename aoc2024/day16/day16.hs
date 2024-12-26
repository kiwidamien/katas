import qualified Data.Set as S 
import qualified Data.List as L 

type Loc = (Int, Int)
type Grid = S.Set Loc 
type Visited = S.Set Loc
data Direction = N | E | S | W deriving (Show, Eq)
data State = State (Int, Int) Direction deriving (Show, Eq)

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


move :: Grid -> Loc -> [Loc]
move g (y, x) = [p | p <- poss, not (elem p g)]
  where poss = [(y+1, x), (y, x+1), (y-1,x), (y, x-1)]

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

