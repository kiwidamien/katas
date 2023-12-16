module Day16 where 

import Prelude hiding (Left, Right)
import qualified Data.Set as S 

data Direction = Up | Right | Down | Left deriving (Enum, Show, Bounded, Eq, Ord)
data Beam = Beam {bLoc::Loc, bDir:: Direction} deriving (Show, Eq, Ord)
type Loc = (Int, Int)
type Grid = [[Optic]]

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
  where isIn (Beam (y,x) _) = (y >= 0) && (x >=0) && (x < sx) && (y < sy)
        sy = length g
        sx = length $ head g

char2Optic :: Char -> Optic 
char2Optic '.' = Empty
char2Optic '-' = HSplit
char2Optic '|' = VSplit 
char2Optic '/' = MirrorPosSlope
char2Optic '\\' = MirrorNegSlope

parse :: String -> Grid 
parse s = map (map char2Optic) $ lines s

shine :: [Beam] -> Grid -> S.Set Beam
shine beams grid = go S.empty grid (S.fromList beams)
  where go :: (S.Set Beam) -> Grid -> (S.Set Beam) -> (S.Set Beam)
        go acc g toVisit 
          | length toVisit == 0 = acc 
          | otherwise = go newVisited g newToVisit
            where new = S.findMin toVisit
                  newOptic = let (y,x) = bLoc new in g!!y!!x
                  newVisited = S.insert new acc 
                  newToVisit = S.difference (S.union toVisit (S.fromList $ prune g $ advance new newOptic)) newVisited


brightLocations :: S.Set Beam -> Int 
brightLocations beamSet = length $ S.map bLoc beamSet

possibleStartBeams :: Grid -> [Beam]
possibleStartBeams g = [Beam (0, c) Down | c <- [0..lx]] ++ 
                       [Beam (r, lx) Left| r <- [0..ly]] ++ 
                       [Beam (ly, c) Up | c <- [0..lx]] ++ 
                       [Beam (r, 0) Right | r <- [0..ly]]
  where ly = length g - 1
        lx = (length $ head g) - 1


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