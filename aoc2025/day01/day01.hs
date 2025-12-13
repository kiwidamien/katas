moveDial :: Int -> Int -> Int
moveDial state amount = mod (state + amount) 100


parseLine :: [Char] -> Int
parseLine ('L':rest) = 0 - read rest::Int
parseLine ('R':rest) = (read rest::Int)


movesCountZero :: Int -> [Int] -> (Int, Int)
movesCountZero start moves = foldl prop (0, start) moves
  where prop :: (Int, Int) -> Int -> (Int, Int)
        prop (count, current) amount = 
            let 
                after = moveDial current amount 
                countDelta = if (after==0) then 1 else 0
            in (count + countDelta, after)


propWithZeroCounter :: (Int, Int) -> Int -> (Int, Int)
propWithZeroCounter (count, current) amount = (count + f current amount, moveDial current amount)
  where 
    f current amount
      | current == 0 = div (abs amount) 100
      | amount > 0 = div (current + amount) 100
      | amount < 0 = if current + amount > 0 then 0 else 1 + div (abs (current + amount)) 100
      | otherwise = 0


movesCountAnyClickZero :: Int -> [Int] -> (Int, Int)
movesCountAnyClickZero start moves = foldl propWithZeroCounter (0, start) moves

-- These functions are the "do it the manual way"
--

generateStates :: Int -> Int -> [Int]
generateStates state amount
  | amount == 0 = []
  | amount > 0 = let x = (mod (state + 1) 100) in x : generateStates x (amount - 1)
  | amount < 0 = let x = (mod (state - 1) 100) in x : generateStates x (amount + 1)

stateStream :: Int -> [Int] -> [[Int]]
stateStream start [] = []
stateStream start (m:rest) = let x = generateStates start m in x : stateStream (last x) rest


movesCountAnyClickZero' :: Int -> [Int] -> (Int, Int)
movesCountAnyClickZero' start moves = (length $ filter (\n -> n == 0) $ stream, last stream)
  where stream = concat $ stateStream start moves

same :: Int -> [Int] -> Bool
same start moves = (fst $ movesCountAnyClickZero start moves) == (fst $ movesCountAnyClickZero' start moves)

-- End of checks. These are the wrapper functions

part1 :: String -> IO ()
part1 filename = do
    output <- map parseLine <$> lines <$> readFile filename
    let final = movesCountZero 50 output
    print(final)

part2 :: String -> IO ()
part2 filename = do
    output <- map parseLine <$> lines <$> readFile filename
    let final = movesCountAnyClickZero 50 output
    print(final)