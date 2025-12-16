data Direction = Null | GoLeft | GoRight | GoUp | GoDown | GoActivate deriving (Show, Eq)
data Numpad = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Activate deriving (Show, Eq)
data DirPad = DirUp | DirLeft | DirDown | DirRight | DirActivate deriving (Show, Eq)

type State = (DirPad, DirPad, Numpad)


-- +---+---+---+
-- | 7 | 8 | 9 |
-- +---+---+---+
-- | 4 | 5 | 6 |
-- +---+---+---+
-- | 1 | 2 | 3 |
-- +---+---+---+
--     | 0 | A |
--     +---+---+
numPadMoves :: Numpad -> [(Direction, Numpad)]
numPadMoves Zero = [(GoUp, Two), (GoRight, Activate)]
numPadMoves One = [(GoUp, Four), (GoRight, Two)]
numPadMoves Two = [(GoLeft, One), (GoUp, Five), (GoRight, Three), (GoDown, Zero)]
numPadMoves Three = [(GoLeft, Two), (GoUp, Six), (GoDown, Activate)]
numPadMoves Four = [(GoUp, Seven), (GoRight, Five), (GoDown, One)]
numPadMoves Five = [(GoUp, Eight), (GoRight, Six), (GoDown, Two), (GoLeft, Four)]
numPadMoves Six = [(GoUp, Nine), (GoDown, Three), (GoLeft, Five)]
numPadMoves Seven = [(GoRight, Eight), (GoDown, Four)]
numPadMoves Eight = [(GoLeft, Seven), (GoRight, Nine), (GoDown, Five)]
numPadMoves Nine = [(GoLeft, Eight), (GoDown, Six)]
numPadMoves Activate = [(GoLeft, Zero), (GoUp, Three)]

--     +---+---+
--     | ^ | A |
-- +---+---+---+
-- | < | v | > |
-- +---+---+---+
dirPadMoves :: DirPad -> [(Direction, DirPad)]
dirPadMoves DirUp = [(GoRight, DirActivate), (GoDown, DirDown)]
dirPadMoves DirDown = [(GoRight, DirRight), (GoLeft, DirLeft), (GoUp, DirUp)]
dirPadMoves DirLeft = [(GoRight, DirDown)]
dirPadMoves DirRight = [(GoLeft, DirDown), (GoUp, DirActivate)]
dirPadMoves DirActivate = [(GoLeft, DirUp), (GoDown, DirRight)]

iniState = (DirActivate, DirActivate, Activate)

internalUpdate :: Direction -> DirPad -> DirPad
internalUpdate d start = snd $ head $ filter (\(x,_) -> x == d) $ dirPadMoves start

dirToAction :: DirPad -> Direction
dirToAction DirUp = GoUp 
dirToAction DirDown = GoDown 
dirToAction DirLeft = GoLeft 
dirToAction DirRight = GoRight 
dirToAction DirActivate = GoActivate

dirPadChain :: DirPad -> Direction -> (DirPad, Direction)
dirPadChain pad GoActivate = (pad, dirToAction pad)
dirPadChain pad Null = (pad, Null)
dirPadChain pad d = (internalUpdate d pad, Null)

numPadProcess :: Numpad -> Direction -> (Numpad, [Numpad])
numPadProcess numPad Null = (numPad, [])
numPadProcess numPad GoActivate = (numPad, [numPad])
numPadProcess numPad d = (numPad', [])
  where numPad' = snd $ head $ filter (\(x,_) -> x == d) $ numPadMoves numPad

update :: Direction -> (State, [Numpad]) -> (State, [Numpad])
update direction ((padOne, padTwo, numPad), output) =  ((padOne', padTwo', numPad'), output ++ pressed')
    where (padOne', dir') = dirPadChain padOne direction 
          (padTwo', dir'') = dirPadChain padTwo dir' 
          (numPad', pressed') = numPadProcess numPad dir'' 

processCommands :: [Direction] -> (State, [Numpad])
processCommands directions = foldl (\ref d -> update d ref) (iniState, []) directions 


parseLine :: String -> [Direction]
parseLine s = map parseChar s
  where parseChar '>' = GoRight
        parseChar '<' = GoLeft 
        parseChar '^' = GoUp 
        parseChar 'v' = GoDown
        parseChar 'A' = GoActivate


seq1 = "<A^A>^^AvvvA"
seq3 = "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"
seq4 = "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"

npp :: (Numpad, [Numpad]) -> Direction -> (Numpad, [Numpad])
npp (n, o) d = let (x, y) = numPadProcess n d in (x, o ++ y)

twostep :: (DirPad, Numpad, [Numpad]) -> Direction -> (DirPad, Numpad, [Numpad])
twostep (d1, n, o) d = (d1', x, o ++ y)
  where (d1', newDirection) = dirPadChain d1 d 
        (x, y) = numPadProcess n newDirection