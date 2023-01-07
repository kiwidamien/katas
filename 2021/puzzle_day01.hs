-- Given a series of numbers, count how often the numbers increased
-- e.g. [199, 200, 201, 210, 208, 207]
-- Inc? [N/A, True, True, True, False, False]
-- Num of increases == 3
--
numIncreases :: (Ord a) => [a] -> Int
numIncreases lst = length $ filter (\x->x) $ map (\x-> (snd x)>(fst x)) $ neigh
   where neigh = zip lst (tail lst)

-- Now do the same, but with a sliding window of length 3 (technically average
-- but since I am only looking at the average I can also do sum)
-- Our array [199, 200, 201,210, 208, 207] becomes
-- [199 + 200 + 201 = 600,
--  200 + 201 + 210 = 611,
--  201 + 210 + 208 = 619,
--  210 + 208 + 207 = 625]
sumTriples :: (Num a, Ord a) => [a] -> [a]
sumTriples lst = zipWith3 (\x y z -> x+y+z) lst dropFirst dropFirstTwo
  where dropFirst = tail lst
        dropFirstTwo = tail dropFirst 


-- Sum for an arbitary window size
-- Have sumTriples X == sumArbWindow 3 X
sumArbWindow :: (Num a, Ord a) => Int -> [a] -> [a]
sumArbWindow winSize lst = zipWith (-) (drop winSize runningSum) (runningSum)
  where runningSum = scanl (+) 0 lst


part1 = do
  contents <- readFile "inputs/day01.txt"
  let nIncreases =  numIncreases $ map (\x->(read x)::Int) $ lines contents
  return nIncreases

part2 = do
  contents <- readFile "inputs/day01.txt"
  let depths =  map (\x->(read x)::Int) $ lines contents
  let avgIncreased = numIncreases $ sumTriples $ depths
  return avgIncreased

main = do
  a <- part1
  b <- part2
  print("The number of increases is " ++ (show a) ++ ", but once averaged over a sliding window (size 3) becomes " ++ (show b))
