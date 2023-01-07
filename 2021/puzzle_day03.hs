import Data.List

count :: Int -> [Int] -> Int
count n = length . filter (== n) 

mostCommonBit :: [Int] -> Int
mostCommonBit lst = if numOnes >= numZeros then 1 else 0
  where numOnes = count 1 lst
        numZeros = count 0 lst

mostCommonBitSeq :: [[Int]] -> [Int]
mostCommonBitSeq lst = map mostCommonBit $ transpose lst

leastCommonBitSeq :: [[Int]] -> [Int]
leastCommonBitSeq lst = map (\x -> 1-x) $ mostCommonBitSeq lst

bin2Dec :: [Int] -> Int
bin2Dec lst  = go 0 lst
  where go numSoFar [] = numSoFar
        go numSoFar (x:xs) = go (2*numSoFar + x) (xs)

gammaRate :: [[Int]] -> Int
gammaRate = bin2Dec . mostCommonBitSeq

epsilonRate :: [[Int]] -> Int
epsilonRate = bin2Dec . leastCommonBitSeq

powerRate :: [[Int]] -> Int
powerRate x = (gammaRate x) * (epsilonRate x)

-- Part 2
_oxygenFilter :: [[Int]] -> Int -> [[Int]]
_oxygenFilter mat n = filter (\x -> (x!!n)==(mcbs)!!n) mat
    where mcbs = mostCommonBitSeq mat

oxygenFilter :: [[Int]] -> [[Int]]
oxygenFilter mat = foldl _oxygenFilter mat [0..n]
    where n = (length $ (mat!!0)) - 1

co2Filter :: [[Int]] -> [[Int]]
co2Filter mat = foldl atLeastOne mat [0..n]
    where n = (length $ (mat!!0)) - 1
          atLeastOne m idx = if (length m)==1 then m else (_co2Filter m idx)
          _co2Filter m idx= filter (\x->(x!!idx)==(leastCommonBitSeq m)!!idx) m

lifeSupportRating :: [[Int]] -> Int
lifeSupportRating m = decOxy * decCO2
  where decOxy = bin2Dec (head $ oxygenFilter m)
        decCO2 = bin2Dec (head $ co2Filter m)


parseMatrix :: String -> [[Int]]
parseMatrix = map (\x -> map (\c -> if c=='1' then 1 else 0) x) . lines 

test = "00100\n\
\11110\n\
\10110\n\
\10111\n\
\10101\n\
\01111\n\
\00111\n\
\11100\n\
\10000\n\
\11001\n\
\00010\n\
\01010"
   
three = [[0,0,0,1,1,0,1,0], [1,0,1,1,1,1,0,0], [0,1,1,0,0,0,1,1]] :: [[Int]]

part1 = do
   contents <- readFile "inputs/day03.txt"
   let matrix = parseMatrix contents
   return (powerRate matrix)

part2 = do
   contents <- readFile "inputs/day03.txt"
   let matrix = parseMatrix contents
   return (lifeSupportRating matrix)


main = do
  p1 <- part1
  p2 <- part2
  putStrLn ("Part 1 has " ++ (show p1) ++ " (no aim) and part 2 has " ++ (show p2) ++ " (with aim)")

