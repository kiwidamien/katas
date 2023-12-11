module Day10 where 

import qualified Data.Map as M 

data Pipe = NS | EW | NE | NW | SW | SE | Ground | Animal deriving (Eq)
type Loc = (Int, Int)


parseMap :: [String] -> M.Map Loc Pipe 
parseMap lines = M.fromList lstRead
    where numLines = length lines 
          numCols = length (head lines)
          lstRead = [((y,x), char2Pipe (lines!!y!!x)) | y <- [0..(numLines - 1)], x<-[0..(numCols - 1)]]

instance Show Pipe where
    show NS = "|"
    show EW = "-"
    show NE = "L"
    show NW = "J"
    show SW = "7"
    show SE = "F"
    show Ground = "."
    show Animal = "S"

char2Pipe :: Char -> Pipe
char2Pipe '|' = NS 
char2Pipe '-' = EW 
char2Pipe 'L' = NE
char2Pipe 'J' = NW 
char2Pipe 'F' = SE
char2Pipe '7' = SW 
char2Pipe '.' = Ground 
char2Pipe 'S' = Animal 
char2Pipe x = error ("Do not recognize " ++ [x] ++ " as valid")


directions :: Pipe -> (Loc, Loc)
directions NS = ((-1, 0), (1, 0))
directions EW = ((0, 1), (0, -1))
directions NE = ((-1, 0), (0, 1))
directions NW = ((-1, 0), (0, -1))
directions SE = ((1, 0), (0, 1))
directions SW = ((1, 0), (0, -1))

add :: Loc -> Loc -> Loc 
add (y, x) (dy, dx) = (y+dy, x+dx)


moveOne :: Loc -> Pipe -> [Loc] 
moveOne _ Ground = error "You are not in a pipe"
moveOne _ Animal = error "Hard to do"
moveOne loc p = let (d1, d2) = directions p in [add loc d1, add loc d2]


moveAlong :: [Loc] -> Pipe -> [Loc] 
moveAlong locs Ground = error ("You are not in a pipe" ++ (show locs))
moveAlong locs Animal = locs
moveAlong [] _     = error "Must have a starting location"
moveAlong locs p = let 
            current = head locs
            (d1,d2) = directions p
            next1 = add current d1
            next2 = add current d2
            prev = if length locs > 1 then head $ drop 1 locs else head locs
        in if (prev == next1) then next2:locs else next1:locs

grow :: M.Map Loc Pipe -> [Loc] -> [Loc]
grow chart (l:ls)
  | pipe == Animal = (l:ls)
  | otherwise = grow chart (moveAlong (l:ls) pipe) 
    where pipe = M.findWithDefault Ground l chart 

locateAnimal :: M.Map Loc Pipe -> Loc 
locateAnimal chart = fst $ head $ filter (\x -> snd x==Animal) $ M.assocs chart

locateStart :: M.Map Loc Pipe -> Loc
locateStart chart = head poss
  where (ay, ax) = locateAnimal chart
        noGnd = filter (\x -> M.findWithDefault Ground x chart /= Ground) [(ay-1, ax), (ay+1, ax), (ay, ax+1), (ay, ax-1)]
        poss = filter (\x -> elem (ay, ax) (moveOne x (M.findWithDefault Ground x chart))) noGnd


fuse :: [Pipe] -> Int 
fuse pipes = counter
  where noEW = filter (/= Ground) $ filter (/= EW) pipes
        counter = go 0 noEW
        go x [] = x 
        go x [a] = x + 1
        go x (a:b:xs)
          | (a == NE) && (b==SW) = go (x+1) xs
          | (a == SE) && (b==NW) = go (x+1) xs
          | ((a==NE) || (a==SE)) && ((b==NW)||(b==SW)) = go x xs
          | otherwise = go (x+1) (b:xs)
        

part1 :: String -> IO ()
part1 filename = do 
    contents <- lines <$> readFile filename 
    let chart = parseMap contents 
    let animalLoc = locateAnimal chart 
    let start = locateStart chart 
    let iniPath = [start, animalLoc]
    let path = drop 1 $ grow chart iniPath
    print(path)
    print(length path)

isInside :: M.Map Loc Pipe -> [Loc] -> Loc -> Bool 
isInside chart path (y,x)
  | elem (y,x) path = False
  | otherwise = mod nPipes 2 == 1
    where thisRow = [M.findWithDefault Ground (y,xx) chart | xx <- [0..x], elem (y,xx) path, (M.findWithDefault Ground (y,xx) chart) /= EW]
          nPipes = fuse thisRow


countInterior :: M.Map Loc Pipe -> [Loc] -> Int
countInterior chart path = length $ filter id $ map (isInside chart path) $ M.keys chart

part2 :: String -> IO ()
part2 filename = do
    contents <- lines <$> readFile filename 
    let chart = parseMap contents
    let animalLoc = locateAnimal chart
    let start = locateStart chart 
    let path = grow chart [start, animalLoc]
    print(countInterior chart path)
    
-- Test code
contents = [".....",".S-7.",".|.|.",".L-J.","....."]
chart = parseMap contents
path = grow chart [locateStart chart, locateAnimal chart]