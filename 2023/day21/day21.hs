module Day21 where 

import qualified Data.Array.IArray as A
import qualified Data.Set as S 

data Tile = R | G deriving (Show, Eq)
data Direction = N | E | S | W deriving (Show, Eq, Ord, Bounded, Enum)

type Loc = (Int, Int)
type Grid = A.Array Loc Tile 

parse :: String -> (Grid, Loc)
parse contents = (A.listArray ((0,0), (lY, lX)) $ concat listOfLists, (startRow, startCol))
  where pFn '#' = R
        pFn '.' = G
        pFn 'S' = G 
        listOfLists = map (map pFn ) $ lines contents
        lY = length listOfLists - 1
        lX = (length $ head listOfLists) - 1
        (startRow, startLine) = head $ filter (\(_, line) -> elem 'S' line) $ zip [0..] (lines contents)
        startCol = fst $ head $ filter (\(_, c) -> c=='S') $ zip [0..] startLine

tileToChar :: Tile -> Char
tileToChar R = '#'
tileToChar G = '.'

moveOne :: Loc -> [Loc]
moveOne (y,x) = [(y-1, x), (y, x+1), (y+1, x), (y, x-1)]

move :: [Loc] -> Grid -> [Loc]
move locs g = S.toAscList $ S.fromList $ filter (\loc -> (A.!) g loc == G) $ filter (\loc -> A.inRange (A.bounds g) loc) $ go locs
    where go [] = []
          go (p:rest) = (moveOne p) ++ (go rest)

moveN :: [Loc] -> Grid -> Int -> [Loc]
moveN locs g n = last $ take (n+1) $ iterate (\current -> move current g) locs

printGarden :: Grid -> String
printGarden g = let (_, (lY, lX)) = A.bounds g in unlines $ [[ tileToChar ((A.!) g (r,c))| c <- [0..lX]] | r <- [0..lY]]

moveInfinite :: [Loc] -> Grid -> [Loc]
moveInfinite locs g = S.toAscList $ S.fromList $ filter (\loc -> (A.!) g (mloc loc) == G) $ go locs
    where go [] = []
          go (p:rest) = (moveOne p) ++ (go rest)
          mloc (y,x) = (mod y h, mod x w)
          (h, w) = let (_, (lY, lX)) = A.bounds g in (lY + 1, lX + 1)


moveNInfinite :: [Loc] -> Grid -> Int -> [Loc]
moveNInfinite locs g n = last $ take (n+1) $ iterate (\current -> moveInfinite current g) locs

part1 :: String -> IO ()
part1 filename = do 
    contents <- readFile filename 
    let (garden, startLoc) = parse contents 
    putStrLn $ printGarden garden
    print startLoc
    print $ length $ moveN [startLoc] garden 1
    print $ length $ moveN [startLoc] garden 2
    print $ length $ moveN [startLoc] garden 3
    print $ length $ moveN [startLoc] garden 4
    print $ length $ moveN [startLoc] garden 5
    print $ length $ moveN [startLoc] garden 6
    print $ length $ moveN [startLoc] garden 64


quad :: Grid -> Loc -> (Int -> Int)
quad g startLoc = (\n -> div (doubleA * n^2 + doubleB * n + doubleC) 2)
  where (h, w) = let (_, (lY, lX)) = A.bounds g in (lY + 1, lX + 1)
        y0 = length $ moveNInfinite [startLoc] g 65
        y1 = length $ moveNInfinite [startLoc] g (65 + w)
        y2 = length $ moveNInfinite [startLoc] g (65 + 2*w) 
        doubleA = y2  - 2*y1 + y0 
        doubleB = 4*y1  - y2  - 3*y0 
        doubleC = 2*y0

part2 :: String -> IO ()
part2 filename = do 
    contents <- readFile filename 
    let (garden, startLoc) = parse contents 
    let func = quad garden startLoc 
    let wanted = 26501365
    let nCycles = div (wanted - 65) 131
    putStrLn $ printGarden garden
    print $ func nCycles