import qualified Data.Map as M 
import qualified Data.Set as S 


type Loc = (Int, Int)
type Grid = S.Set Loc
data Direction = MUp | MDown | MLeft | MRight deriving (Show, Eq)


buildGrid :: Int -> Int -> [Loc] -> Grid
buildGrid nRows nCols locs = S.fromList (boundary ++ locs)
  where boundary = [(-1, c) | c <- [0..nCols]] ++ [(nRows, c)| c<-[0..nCols]] ++ [(r, -1)| r<-[0..nRows]] ++ [(r,nCols) | r<- [0..nRows]]

allowed :: Grid -> Loc -> Bool 
allowed grid loc = not (S.member loc grid)

neighbors :: Loc -> Grid -> [Loc]
neighbors loc grid = filter (allowed grid) $ [move loc d| d<- [MUp, MDown, MLeft, MRight]]
  where move (y, x) MUp = (y-1, x)
        move (y, x) MDown = (y+1, x)
        move (y, x) MRight = (y, x+1)
        move (y, x) MLeft = (y, x-1)

parse :: String -> Int -> Int -> Grid
parse contents size nFall = buildGrid size size locations
  where locations = map (\line -> let [x,y] = words line in (read x, read y)) $ map (map (\c -> if c == ',' then ' ' else c)) $ take nFall $ lines contents

-- not used
extend :: S.Set Loc -> Grid -> [Loc] -> [[Loc]]
extend visited g (end:rest) = [n:end:rest | n <- neighbors end g, not (S.member n visited)]

-- not used
bfs :: Grid -> Loc -> Loc -> [[Loc]]
bfs grid start target = go (S.singleton start) [[start]]
    where go :: S.Set Loc -> [[Loc]] -> [[Loc]]
          go visited paths 
             | S.member target visited = filter (\path -> head path == target) paths
             | otherwise = let extensions = concat $ map (\path -> extend visited grid path) paths in let newlyVisited = map head extensions in go (S.union visited (S.fromList newlyVisited)) extensions 

bfsDist :: Grid -> Loc -> Loc -> Int
bfsDist grid start target = go (S.singleton start) 0 [start]
    where e visited loc = [n | n <- neighbors loc grid, not (S.member n visited)]
          go :: S.Set Loc -> Int -> [Loc] -> Int
          go visited step frontier
            | S.member target visited = step
            | length frontier == 0 = (-1)
            | otherwise = let newFrontier = S.fromList $ concat $ map (\f -> e visited f) frontier in go (S.union visited newFrontier) (step+1) (S.toList newFrontier)


part1 :: String -> Int -> Int -> IO ()
part1 filename size numSteps = do 
    grid <- parse <$> readFile filename <*> pure (size + 1) <*> pure numSteps
    let numPaths = bfsDist grid (0, 0) (size,size)
    print(numPaths)

part1_example :: IO ()
part1_example = part1 "example.txt" 6 12

part1_real :: IO () 
part1_real = part1 "input.txt" 70 1024

-- part2 was just an annoying binary search that you can do easily in ghci