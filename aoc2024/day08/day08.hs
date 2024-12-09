import qualified Data.Map as M 
import qualified Data.Set as S 

type Loc = (Int, Int)
data Grid = Grid Int Int (M.Map Char [Loc]) deriving Show

getDim :: Grid -> Loc 
getDim (Grid numRows numCols _) = (numRows, numCols)

getMap :: Grid -> M.Map Char [Loc]
getMap (Grid _ _ m) = m

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [x] = []
pairs (x:xs) = (map (\n -> (x, n)) xs) ++ pairs xs

getFreqPairs :: Grid -> [(Loc, Loc)]
getFreqPairs grid = concat $ map pairs $ M.elems $ getMap $ grid

getNextLocNoBounds :: (Loc, Loc) -> [Loc]
getNextLocNoBounds ((x1, y1), (x2, y2)) = [(x2 + x2 - x1, y2 + y2 - y1), (x1 - (x2 - x1), y1 - (y2 - y1))]


parse :: [String] -> Grid
parse s = Grid numRows numCols (M.fromListWith (++) antLoc)
  where numRows = length s 
        numCols = length $ head s 
        antLoc = concat $ map (\(r, line) -> [(a, [(r,c)]) | (c,a) <- zip [0..] line, a/='.']) $ zip [0..] s


inBounds :: Grid -> Loc -> Bool 
inBounds g (x, y)
  | x < 0 = False
  | y < 0 = False
  | x >= dimX = False 
  | y >= dimY = False
  | otherwise = True 
    where (dimY, dimX) = getDim g

genAll :: Grid -> (Loc, Loc) -> [Loc]
genAll g ((x1,y1), (x2,y2)) = (takeWhile (inBounds g) $ iterate (add (diffX, diffY)) (x2, y2)) ++ (takeWhile (inBounds g) $ iterate (\l -> subtract l (diffX, diffY)) (x1,y1)) 
  where diffX = (x2 - x1)
        diffY = (y2 - y1)
        add (a,b) (c,d) = (a+c, b+d)
        subtract (a,b) (c,d) = (a-c, b-d)
        
part1 :: String -> IO ()
part1 filename = do 
    grid <- parse <$> lines <$> readFile filename
    let pairs = getFreqPairs grid
    let antiNodeLocs = concat $ map getNextLocNoBounds pairs
    let inGrid = filter (\loc -> inBounds grid loc) antiNodeLocs
    print(length $ S.fromList inGrid)

part2 :: String -> IO ()
part2 filename = do 
    grid <- parse <$> lines <$> readFile filename
    let pairs = getFreqPairs grid
    let antiNodeLocs = concat $ map (genAll grid) pairs
    let inGrid = filter (\loc -> inBounds grid loc) antiNodeLocs
    print(length $ S.fromList inGrid)