module Day11 where 
import Data.List (transpose)


data Astro = Empty | BigEmpty | Galaxy deriving Eq
type Starfield = [[Astro]]
type Loc = (Int, Int)

expansionFactor :: Int 
expansionFactor = 1000000


char2Astro :: Char -> Astro
char2Astro '.' = Empty
char2Astro '#' = Galaxy

instance Show Astro where 
    show Empty = "."
    show Galaxy = "#"
    show BigEmpty = "E"

manhattan :: Loc -> Loc -> Int
manhattan (a,b) (c,d) = (abs (a-c)) + (abs (b-d))

expandRows :: Starfield -> Starfield
expandRows [] = []
expandRows [[]] = [[]]
expandRows (x:xs) = if empty x then x:x:(expandRows xs) else x:expandRows xs
  where empty = all (==Empty)

expandCols :: Starfield -> Starfield
expandCols = transpose . expandRows . transpose

expand :: Starfield -> Starfield
expand = expandCols . expandRows 

newExpandRows :: Starfield -> Starfield 
newExpandRows [] = []
newExpandRows [[]] = [[]]
newExpandRows (x:xs) = if empty x then bigEmptyRow : (newExpandRows xs) else x : newExpandRows xs 
  where empty = all (\x -> (x==Empty) || (x == BigEmpty)) 
        bigEmptyRow = replicate (length x) BigEmpty

newExpandCols :: Starfield -> Starfield
newExpandCols = transpose . newExpandRows . transpose

newExpand :: Starfield -> Starfield
newExpand = newExpandCols . newExpandRows

locateGalaxies :: Starfield -> [Loc]
locateGalaxies field = [(y,x)| y <- [0..(length field - 1)], x <- [0..(length (field!!y) - 1)], (field!!y!!x) == Galaxy]

galaxyPairs :: Starfield -> [(Loc, Loc)]
galaxyPairs field = [(g1, g2) | g1 <- galaxies, g2 <- galaxies, g1 < g2]
  where galaxies = locateGalaxies field

distances :: [(Loc, Loc)] -> [Int]
distances = map (uncurry manhattan) 

newDistances :: Starfield -> (Loc, Loc) -> Int 
newDistances field ((a,b), (c,d)) = go 0 (min a c) (max a c) (min b d) (max b d)
  where go acc sy ey sx ex
          | (sx == ex) && (sy == ey) = acc
          | (sx < ex) = if isBigEmpty then go (acc+expansionFactor) sy ey (sx+1) ex else go (acc+1) sy ey (sx+1) ex
          | (sy < ey) = if isBigEmpty then go (acc+expansionFactor) (sy + 1) ey sx ex else go (acc+1) (sy+1) ey sx ex 
            where isBigEmpty = (field!!sy!!sx) == BigEmpty


parseMap :: String -> Starfield
parseMap s = map (map char2Astro) $ lines s

part1 :: String -> IO ()
part1 filename = do 
    contents <- readFile filename 
    let origField = parseMap contents 
    let expandedField = expand origField
    let galaxyDistances = distances $ galaxyPairs expandedField
    print $ sum galaxyDistances

part2 :: String -> IO ()
part2 filename = do 
    contents <- readFile filename 
    let origField = parseMap contents 
    let expandedField = newExpand origField
    let galaxyDistances = map (newDistances expandedField) $ galaxyPairs expandedField
    print $ sum galaxyDistances