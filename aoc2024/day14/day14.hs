type Loc = (Int, Int)
type GridDim = (Int, Int)
data RobotState = RobotState Loc (Int, Int) deriving (Show, Eq)
data Quadrant = NW | NE | SE | SW | NotApp deriving (Show, Eq)

updateSteps :: RobotState -> GridDim -> Int -> RobotState
updateSteps (RobotState (y,x) (vy,vx)) (nRows, nCols) steps = RobotState (y', x') (vy, vx)
  where y' = mod (y + vy * steps) nRows
        x' = mod (x + vx * steps) nCols


getLoc :: RobotState -> Loc
getLoc (RobotState loc _) = loc 


updateAllRobots :: [RobotState] -> GridDim -> Int -> [RobotState]
updateAllRobots robots g nSteps = map (\s -> updateSteps s g nSteps) robots

getQuadrant :: Loc -> GridDim -> Quadrant
getQuadrant (y, x) (nRows, nCols)
  | ((y < midY) && (x < midX)) = NW
  | ((y < midY) && (x > midX)) = NE
  | ((y > midY) && (x < midX)) = SW
  | ((y > midY) && (x > midX)) = SE
  | otherwise = NotApp
  where midY = div nRows 2
        midX = div nCols 2


safetyFactor :: [RobotState] -> GridDim -> Int 
safetyFactor robots gridDim = nNW * nNE * nSW * nSE
    where quadrants = filter (/= NotApp) $ map (\r -> getQuadrant (getLoc r) gridDim) robots
          nNW = length $ filter (==NW) quadrants
          nNE = length $ filter (==NE) quadrants
          nSW = length $ filter (==SW) quadrants
          nSE = length $ filter (==SE) quadrants

parseLine :: String -> RobotState 
parseLine s = RobotState (y,x) (vy,vx)
  where x = read $ takeWhile (/= ',') $ drop 1 $ dropWhile (/='=') s
        y = read $ takeWhile (/=' ') $ drop 1 $ dropWhile (/=',') s 
        vx = read $ takeWhile (/=',') $ drop 2 $ dropWhile (/='v') s 
        vy = read $ drop 1 $ dropWhile (/=',') $ dropWhile (/='v') s

parse :: String -> [RobotState]
parse s = map parseLine $ lines s


part1 :: String -> IO () 
part1 filename = do 
    iniState <- parse <$> readFile filename 
    let finalState = updateAllRobots iniState (103,101) 100
    print(finalState)
    print(safetyFactor finalState (103,101))