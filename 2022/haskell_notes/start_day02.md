# First note: Day 02

(Skipping day01 because it was relatively trivial)

Problem statement is [here](https://adventofcode.com/2022/day/2), but the barebones version is

- We have a text file, with input in the following shape
```
A X
B Y
C X
D Z
```

- For both parts, the first column are the Elf's turn. We have A --> Rock, B --> Scissors, C --> Paper
- For both parts, we score 1 point for Rock, 2 points for Paper, and 3 points for Scissors; in addition 6 points for winning, 3 for drawing, and none for losing
- The meaning of the second column is different in the two parts:
  - Part 1: Your move: X --> Rock, Y --> Paper, Z --> Scissors
  - Part 2: Your result: X --> play tp lose, Y --> draw, Z --> play to win
- Goal: Calculate the sum of the scores of all the games you play

Let's focus on the approach I took for problem1
```haskell
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

problem1 :: [[String]] -> Integer
problem1 contents = sum $ eval_prob1 parsed
  where parsed = map (\x-> (head (x!!0), head (x!!1))) contents


main = do
  inputMoves <- map words <$> (lines <$> readFile "day02_input.txt")
  print(show (problem1 inputMoves))
``` 

Basically:
* `lines` splits the text file into lines, so I have `["A X", "B Y", "C X", ...]`
* `map words` then splits on the space in each element, so I have a list of lists of _strings_: `[["A", "X"], ["B", "Y"], ....]]`
* Unfortunately, I had defined translate to work on characters. I thought about redefining to strings, but we are only using a single char to map to a move. Using `head s` gets the (single) char from the string, as 	`String` is a type alias for `[Char]`. In `problem1`, the `parsed` variable does this mapping.
  * In retrospect, I could have made this part of the `map words` so the contents would be `[[Char]]` instead of `[[String]]`.
* The `map (\x -> (translate (fst x), translate (snd x))) contents` transformates the `[[Char]]` into `[[Move]]`, suitable for mapping over with the result of `scoreGame`

By the time we get to `scoreGame`, I feel pretty happy with the code. It is a little more verbose than I would want, but it feels like the functions are at the right level (i.e. I can read the code and be thinking about RPS, instead of thinking about Strings vs Chars and Lists vs Tuples).

## LunarCoffee

It is clear that my solution isn't great Haskell. Let's look at another solution to see how to be a little more terse. This one is from [LunarCoffee](https://github.com/lunarcoffee/advent-of-code/blob/main/2022/day02.hs)

```haskell
import Data.Char (ord)

roundScoreMove :: (Int, Int) -> Int
roundScoreMove = (+) <$> (+ 1) . snd <*> resultPoints
  where
    resultPoints (a, b) = cycle [3, 6, 0] !! (b - a + 3)

roundScoreResult :: (Int, Int) -> Int
roundScoreResult = (+) <$> (* 3) . snd <*> movePoints
  where
    movePoints (a, b) = cycle [1 .. 3] !! (a + b + 2)

main :: IO ()
main = do
  rounds <- map parseRound . lines <$> getContents
  print $ sum $ map roundScoreMove rounds 
  print $ sum $ map roundScoreResult rounds 
  where
    parseRound [x, _, y] = (ord x - ord 'A', ord y - ord 'X')
```

This uses `getContents`, which reads from `stdin`. 
Don't use this code in `ghci`, instead redirect the input from a file.

`parseRound [x, _, y]` takes a line, and ignores the space.
We have 'A'-->0, 'B' --> 1, 'C' --> 2, 'X' --> 0, 'Y' --> 1, 'Z' --> 2.

So we have numerical mapping. We also have the "winning" map

| Play          | Beats         |
|---------------|---------------|
| Rock     (0)  | Scissors (1)  |
| Scissors (1)  | Paper    (2)  |
| Paper    (2)  | Rock     (0)  |

That is, `x` beats `y` if `x == ((y-1) mod 3)`, draw if equal, and loss otherwise.
That is

1. x-y == 0 mod 3 ==> draw (3 points)
2. x-y == -1 mod 3 == 2 mod 3 ==> x wins (0 points for y)
3. x-y == 1 mod 3 ==> y wins (6 points for y)

Instead of `b-a mod 3`, we get `cycle [3, 6, 0] !! (b-a + 3)``
