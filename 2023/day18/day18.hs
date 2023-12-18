module Day18 where 

import Numeric


data Direction = N | E | S | W deriving (Eq, Show, Ord)
data Instruction = Instruction {_iDir:: Direction, _iSteps:: Int, _iColor:: String} deriving (Eq, Show, Ord)

type Loc = (Int, Int)

parseDirection :: Char -> Direction 
parseDirection 'U' = N 
parseDirection 'D' = S
parseDirection 'L' = W 
parseDirection 'R' = E 
parseDirection '0' = E 
parseDirection '1' = S 
parseDirection '2' = W 
parseDirection '3' = N

parseLine1 :: String -> Instruction 
parseLine1 s = Instruction (parseDirection direction) distance color
  where parts = words s 
        direction = head $ parts!!0 
        distance = read (parts!!1)
        color = parts!!2

parseLine2 :: String -> Instruction 
parseLine2 s = Instruction dir steps "black"
  where parts = filter (`elem` "0123456789abcdef") $ last $ words s 
        steps = fst $ head $ readHex $ init parts 
        dir = parseDirection $ last parts


parse :: (String -> Instruction) -> String -> [Instruction]
parse parseFn = map parseFn . lines 

move :: Loc -> Int -> Direction -> Loc 
move (y,x) n N = (y - n, x)
move (y,x) n S = (y + n, x)
move (y,x) n W = (y, x - n)
move (y,x) n E = (y, x + n)

vertices :: [Instruction] -> [Loc]
vertices ins = scanl (\acc i' -> move acc (_iSteps i') (_iDir i')) (0,0) ins

areaShoelace :: [Loc] -> Int
areaShoelace vs = div (go vs) 2
    where 
        go [] = 0
        go [v] = 0
        go (v:w:rest) = (fst v) * (snd w) - (snd v) * (fst w) + go (w:rest)

interiorPts :: [Instruction] -> Int
interiorPts ins = area + 1 - (div bnd 2) 
  where bnd = sum $ map _iSteps ins
        area = abs $ areaShoelace $ vertices ins 

part1 :: String -> IO()
part1 filename = do 
    instructions <- parse parseLine1 <$> readFile filename
    let bndPoints = sum $ map _iSteps instructions
    let intPoints = interiorPts instructions 
    print (bndPoints + intPoints)

part2 :: String -> IO()
part2 filename = do 
    instructions <- parse parseLine2 <$> readFile filename
    let bndPoints = sum $ map _iSteps instructions
    let intPoints = interiorPts instructions 
    print (bndPoints + intPoints)