-- Track movement. If we have position (x,y)
-- Forward X --> (x + X, y)
-- etc
type Pos = (Int, Int)
type State = (Pos, Int)
type Instruction = (String, Int)


move :: Instruction -> Pos -> Pos
move (d, amt) (x,y)
  | d == "forward" = (x+amt, y)
  | d == "up" = (x, y-amt)
  | d == "down" = (x, y+amt)


moveSequence :: [(String, Int)] -> Pos
moveSequence instructions = foldl (\loc d -> move d loc) (0,0) instructions


moveWithAim :: Instruction -> State -> State
moveWithAim (d, amt) ((x,y), aim)
  | d == "forward" = ((x + amt, y + amt*aim), aim)
  | d == "up" = ((x,y), aim-amt)
  | d == "down" = ((x,y), aim+amt)


moveSequenceWithAim :: [Instruction] -> State
moveSequenceWithAim ins = foldl(\loc d -> moveWithAim d loc) ((0,0),0) ins


-- Common pieces
parseInput :: String -> [(String, Int)]
parseInput t = map (\(d:n) -> (d, read $ head n)) $ map words $ lines t

{-
forward 5
down 5
forward 8
up 3
down 8
forward 2
-}

part1 = do
  contents <- readFile "inputs/day02.txt"
  let finalLoc = moveSequence $ parseInput contents
  return finalLoc

part2 = do
  contents <- readFile "inputs/day02.txt"
  let (finalLoc, _) = moveSequenceWithAim $ parseInput contents
  return finalLoc


main = do
  (x,y) <- part1
  print ("Final location is " ++ (show (x,y)) ++ " or product of " ++ (show (x*y)))
  (xx, yy) <- part2
  print("When including the aim, we are at " ++ (show (xx, yy)))
  let product = xx * yy
  print(". The product of which is "++ (show product))
