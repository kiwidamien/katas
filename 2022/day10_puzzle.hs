main = do
    contents <- readFile "day10_input.txt"
    let commands = map (parseCommand) $ parseFile $ contents
        sequence = batchUpdates commands []
      in do
      print(sum $ scoreStackSequence sequence)
      putStr(render ([1]++sequence))


type Cmd = [String]
type Stack = [Int]
type Transition = Stack -> Stack

noop :: Transition
noop [] = [1]
noop s = s ++ [last s]

addx :: Int -> Transition
addx value oldStack
  | oldStack == [] = [1, 1+value]
  | otherwise = oldStack ++ [endValue, endValue + value] 
    where endValue = last oldStack


parseCommand :: Cmd -> Transition
parseCommand (cmd:rest)
  | cmd=="noop" = noop
  | cmd=="addx" = addx (read (head rest)::Int)
  | otherwise = noop


parseFile :: String -> [[String]]
parseFile s = map (words) $ lines s


batchUpdates :: [Transition] -> Stack -> Stack
batchUpdates [] s = s
batchUpdates (cmd:other) s = batchUpdates other (cmd s)


range :: Int -> Int -> [a] -> [a]
range start step lst
   | length(lst) == 0 = []
   | otherwise = (head $ begin) : (range 0 step (drop step begin)) 
     where begin = drop start lst
 

_renderPixel :: Int -> Int -> Char
_renderPixel state stepNumber
  | abs(state - (stepNumber) `mod` 40) <= 1 = '#'
  | otherwise = '.'

_renderAllPixels :: Stack -> String
_renderAllPixels ourStack = map (\a -> _renderPixel (fst a) (snd a)) $ zip ourStack [0..]

_wrap :: String -> String
_wrap "" = ""
_wrap long = (take 40 long) ++ "\n" ++ (_wrap $ drop 40 long)

render :: Stack -> String
render my_stack = _wrap $ _renderAllPixels my_stack

--scoreStackSequence :: Stack -> [Int]

scoreStackSequence s = map (\a -> (*) (fst a) (snd a)) $ range 18 40 (zip [2..] s)


-- Testing work
exampleFile =  "noop\naddx 3\naddx -5"
commands = map (parseCommand) $ parseFile exampleFile
exampleStack = batchUpdates commands []