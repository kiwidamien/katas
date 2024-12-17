import Data.Set as S 

data Direction = N | E | S | W deriving (Show, Eq)
data State = State (Int, Int) Direction deriving (Show, Eq)


rotateCW :: Direction -> Direction
rotateCW N = E 
rotateCW E = S 
rotateCW S = W 
rotateCW W = N 

rotateCCW :: Direction -> Direction 
rotateCCW = rotateCW . rotateCW . rotateCW 

