data Move = Rock | Paper | Scissors deriving (Show, Eq)
data Outcome = Player1 | Player2 | Draw deriving (Show, Eq)

game :: (Move, Move) -> Outcome
game (p1,p2)
  | p1 == p2 = Draw
  | (p1, p2) `elem` p1Win = Player1
  | otherwise = Player2
    where p1Win = [(Rock, Scissors), (Scissors, Paper), (Paper, Rock)]

shapeScore :: Move -> Integer
shapeScore move
  | move == Rock = 1
  | move == Paper = 2
  | move == Scissors = 3

scoreOutcome :: Outcome -> Integer
scoreOutcome Player1 = 0
scoreOutcome Player2 = 6
scoreOutcome Draw = 3

translate :: Char -> Move
translate c 
  | c == 'A' = Rock
  | c == 'B' = Paper
  | c == 'C' = Scissors
  | c == 'X' = Rock
  | c == 'Y' = Paper
  | c == 'Z' = Scissors

scoreGame :: (Move, Move) -> Integer
scoreGame (elfMove, youMove) = (shapeScore youMove) + (scoreOutcome $ game (elfMove, youMove))

eval_prob1 moveList = map scoreGame $ map (\x -> (translate (fst x), translate (snd x))) moveList

translateLinePart2 :: [String] -> (Move, Move)
translateLinePart2 (elf:you:[]) = (elfMove, yourMove)
  where elfMove = translate $ head elf
        yourChar = head you
        yourOutcome yc
          | yc == 'X' = Player1
          | yc == 'Y' = Draw
          | yc == 'Z' = Player2
        actualOutcome = (yourOutcome yourChar)
        yourMove = head $ filter (\x -> actualOutcome == (game (elfMove, x))) [Rock, Paper, Scissors]

eval_prob2 moveList = map scoreGame $ map translateLinePart2 moveList

problem1 :: [[String]] -> Integer
problem1 contents = sum $ eval_prob1 parsed
  where parsed = map (\x-> (head (x!!0), head (x!!1))) contents


problem2 :: [[String]] -> Integer
problem2 contents = sum( eval_prob2 contents )


main = do
  contents <- map words <$> (lines <$> readFile "day02_input.txt")
  print("Problem 1 solution scores you " ++ (show (problem1 contents)) ++ " points")
  print("Problem 2 solution scores you " ++ (show (problem2 contents)) ++ " points")

