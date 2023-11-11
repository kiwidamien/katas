module Day12 where

data Action = N | S | E | W | L | R | F deriving (Show, Eq, Read)
data Direction = North | South | East | West deriving (Show, Eq)
data Cmd = Cmd Action Int deriving (Show, Eq)
type State = (Int, Int, Direction) 

parseCmd :: String -> Cmd
parseCmd s = Cmd a num
  where a :: Action = read ([head s])
        num :: Int = read (tail s)

turnRight :: Direction -> Direction
turnRight d = head $ drop 1 $ dropWhile (/= d) [North, East, South, West, North]


turnLeft :: Direction -> Direction
turnLeft = turnRight . turnRight . turnRight 

directionToDelta :: Direction -> (Int, Int)
directionToDelta North = (-1, 0)
directionToDelta South = (1, 0)
directionToDelta East = (0, 1)
directionToDelta West = (0, -1)


runSingleCmd :: State -> Cmd -> State
runSingleCmd (y, x, d) (Cmd F n) = let (dy, dx) = directionToDelta d in (y+n*dy, x+n*dx, d)
runSingleCmd s (Cmd _ 0)  = s
runSingleCmd (y, x, d) (Cmd R 90) = (y, x, turnRight d)
runSingleCmd (y, x, d) (Cmd R 180)= (y, x, turnRight $ turnRight d)
runSingleCmd (y, x, d) (Cmd R 270)= (y, x, turnLeft d)
runSingleCmd s (Cmd R d) = runSingleCmd s (Cmd R (mod d 360)) 
runSingleCmd (y, x, d) (Cmd L 90) = (y, x, turnLeft d)
runSingleCmd (y, x, d) (Cmd L 180)= (y, x, turnRight $ turnRight d)
runSingleCmd (y, x, d) (Cmd L 270)= (y, x, turnRight d)
runSingleCmd s (Cmd L d) = runSingleCmd s (Cmd L (mod d 360)) 
runSingleCmd (y, x, d) (Cmd N n) = (y-n, x, d)
runSingleCmd (y, x, d) (Cmd S n) = (y+n, x, d)
runSingleCmd (y, x, d) (Cmd E n) = (y, x+n, d)
runSingleCmd (y, x, d) (Cmd W n) = (y, x-n, d)


runCommands :: [Cmd] -> State
runCommands cmds = foldl runSingleCmd (0, 0, East) cmds

part1 :: [String] -> Int
part1 s = let (y, x, _) = runCommands $ map parseCmd s in abs(y) + abs(x)
