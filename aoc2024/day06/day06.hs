import qualified Data.Map as M 
import qualified Data.Set as S


type Loc = (Int, Int)
data Direction = U | L | R | D deriving (Show, Eq)
data State = State Loc Direction deriving (Show, Eq)
data Layout = Layout Int Int (S.Set Loc) deriving Show
data Contents = Empty | OffGrid | Obstacle deriving (Show, Eq)

makeLayout :: [[Char]] -> Layout
makeLayout charGrid = Layout numRows numCols (S.fromList boxLoc)
  where numRows = length charGrid
        numCols = length (charGrid !! 0)
        boxLoc = concat $ map (\(rowNum, line) -> [(rowNum, colNum) | (colNum, char) <- zip [0..] line, char=='#']) $ zip [0..] charGrid

findStart :: [[Char]] -> State
findStart charGrid = State (r, c) (parseD point)
    where parseD :: Char -> Direction
          parseD '>' = R 
          parseD 'v' = D 
          parseD '<' = L 
          parseD '^' = U 
          (r, c, point) = head $ concat $ map (\(rowNum, line) -> [(rowNum, colNum, char) | (colNum, char) <- zip [0..] line, char `elem` "<>v^"]) $ zip [0..] charGrid

move :: Loc -> Direction -> Loc 
move (r,c) U = (r-1, c)
move (r,c) R = (r, c+1)
move (r,c) D = (r+1, c)
move (r,c) L = (r, c-1)

rotate :: Direction -> Direction
rotate U = R 
rotate R = D 
rotate D = L 
rotate L = U

peek :: Layout -> Loc -> Direction -> Contents
peek (Layout numRows numCols obstacleLoc) loc d
    | new_r < 0 = OffGrid
    | new_r >= numRows = OffGrid 
    | new_c < 0 = OffGrid 
    | new_c >= numCols = OffGrid 
    | elem (new_r, new_c) obstacleLoc = Obstacle
    | otherwise = Empty
    where (new_r, new_c) = move loc d


travel :: Layout -> State -> (State, S.Set Loc)
travel layout (State loc d) = go (S.singleton loc) layout loc d 
  where go visited layout loc d
            | nextLocPeek == OffGrid = (State loc d, visited)
            | nextLocPeek == Obstacle = (State loc (rotate d), visited)
            | otherwise = go (S.insert loc visited) layout nextLoc d
                where nextLocPeek = peek layout loc d 
                      nextLoc = move loc d 
                

getLoc :: State -> Loc 
getLoc (State loc _) = loc


rememberTravel :: Layout -> State -> (State, S.Set Loc)
rememberTravel layout iniState = go layout iniState (S.empty)
  where go layout iniState visited = if terminal then (iniState, S.insert (getLoc iniState) visited) else go layout nextState (S.union visited nextVisited)
            where (nextState, nextVisited) = travel layout iniState
                  terminal = nextState == iniState

part1 :: String -> IO ()
part1 filename = do
    contents <- lines <$> readFile filename 
    let iniState = findStart contents
    let grid = makeLayout contents
    let result = rememberTravel grid iniState 
    let answer = length $ snd $ result 
    print(answer)