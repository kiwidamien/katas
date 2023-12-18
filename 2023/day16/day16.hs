module Day16 where 

import Prelude hiding (Left, Right)
import qualified Data.Set as S 
import qualified Data.Array.IArray as A

data Direction = Up | Right | Down | Left deriving (Enum, Show, Bounded, Eq, Ord)
data Beam = Beam {bLoc::Loc, bDir:: Direction} deriving (Show, Eq, Ord)
type Loc = (Int, Int)
type Grid = A.Array Loc Optic

data Optic = Empty | HSplit | VSplit | MirrorNegSlope | MirrorPosSlope deriving (Eq, Show)

move :: Loc -> Direction -> Loc 
move (y, x) Up = (y-1, x)
move (y, x) Right = (y, x+1)
move (y, x) Down = (y+1, x)
move (y, x) Left = (y, x-1)

natural :: Beam -> Beam
natural (Beam loc d) = Beam (move loc d) d

advance :: Beam -> Optic -> [Beam]
advance b Empty = [natural b]
advance (Beam loc d) HSplit 
    | (d==Right) || (d==Left) = [natural (Beam loc d)]
    | (d==Up) || (d==Down) = [Beam (move loc Left) Left, Beam (move loc Right) Right]
advance (Beam loc d) VSplit 
    | (d==Up) || (d==Down) = [natural (Beam loc d)]
    | (d==Right) || (d==Left) = [Beam (move loc Up) Up, Beam (move loc Down) Down]
advance (Beam loc d) MirrorNegSlope
    | d == Down = [Beam (move loc Right) Right]
    | d == Right = [Beam (move loc Down) Down]
    | d == Up = [Beam (move loc Left) Left]
    | d == Left = [Beam (move loc Up) Up]
advance (Beam loc d) MirrorPosSlope 
    | d == Down = [Beam (move loc Left) Left]
    | d == Left = [Beam (move loc Down) Down]
    | d == Up = [Beam (move loc Right) Right]
    | d == Right = [Beam (move loc Up) Up]


prune :: Grid -> [Beam] -> [Beam]
prune g b = filter isIn b 
  where isIn (Beam (y,x) _) = (y >= my) && (x >=mx) && (x <= sx) && (y <= sy)
        ((my, mx), (sy, sx)) = A.bounds g

char2Optic :: Char -> Optic 
char2Optic '.' = Empty
char2Optic '-' = HSplit
char2Optic '|' = VSplit 
char2Optic '/' = MirrorPosSlope
char2Optic '\\' = MirrorNegSlope

parse :: String -> Grid 
parse s = A.listArray ((0,0), (maxH, maxW)) $ concat g
    where g = map (map char2Optic) $ lines s
          maxH = length g - 1
          maxW = (length $ head g) - 1

shine :: [Beam] -> Grid -> S.Set Beam
shine beams grid = go S.empty grid (S.fromList beams)
  where go :: (S.Set Beam) -> Grid -> (S.Set Beam) -> (S.Set Beam)
        go acc g toVisit 
          | length toVisit == 0 = acc 
          | otherwise = go newVisited g newToVisit
            where new = S.findMin toVisit
                  newOptic = g A.! (bLoc new)
                  newVisited = S.insert new acc 
                  newToVisit = S.difference (S.union toVisit (S.fromList $ prune g $ advance new newOptic)) newVisited


brightLocations :: S.Set Beam -> Int 
brightLocations beamSet = length $ S.map bLoc beamSet

possibleStartBeams :: Grid -> [Beam]
possibleStartBeams g = [Beam (0, c) Down | c <- [0..lx]] ++ 
                       [Beam (r, lx) Left| r <- [0..ly]] ++ 
                       [Beam (ly, c) Up | c <- [0..lx]] ++ 
                       [Beam (r, 0) Right | r <- [0..ly]]
  where (_, (h,w)) = A.bounds g
        ly = h - 1
        lx = w - 1


part1 :: String -> IO ()
part1 filename = do 
    contents <- readFile filename 
    let grid = parse contents 
    let beams = shine [Beam (0,0) Right] grid 
    print $ brightLocations beams

part2 :: String -> IO ()
part2 filename = do 
    contents <- readFile filename
    let grid = parse contents 
    let starts = possibleStartBeams grid 
    let numLocs = map (\start -> brightLocations $ shine [start] grid) starts
    print (maximum  numLocs)