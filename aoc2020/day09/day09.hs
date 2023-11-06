module Day09 where 

import qualified Data.Set as S
import Data.List

isValid :: [Int] -> Int -> Bool
isValid buffer target = or hasSum
  where bufferSet = S.fromList buffer
        hasSum = map (\x -> (elem (target - x) bufferSet) && (target /= 2*x)) buffer

windows :: [a] -> Int -> [[a]]
windows stream winSz = unfoldr headSlice stream
  where headSlice list
                  | length list < winSz = Nothing
	          | otherwise = Just (take (winSz) list, tail list)


findFirstInvalid :: [Int] -> Int -> Int 
findFirstInvalid stream windowSize = snd $ head $ dropWhile (\(b, t) -> isValid b t) mySeq
  where mySeq = zip (windows stream windowSize) (drop windowSize stream)

prefixSum :: [Int] -> [Int]
prefixSum = drop 1 . scanl (+) 0 

findContiguous :: [Int] -> Int -> [Int]
findContiguous stream target = take (upperIdx - lowerIdx) $ drop (lowerIdx+1) stream 
  where prefix = prefixSum stream
        canSum prefix index tgt = let 
	    s = (take index prefix)
         in (elem (prefix!!index - tgt) s)
	upperIdx = head $ filter (\i -> canSum prefix i target) [0..] 
	lowerIdx = head $ filter (\i -> prefix!!upperIdx - prefix!!i == target) [0..upperIdx]


sumMinMax :: [Int] -> Int
sumMinMax nums = (maximum nums) + (minimum nums)

part1 :: [Int] -> Int -> Int 
part1 = findFirstInvalid

part2 :: [Int] -> Int -> Int
part2 nums windowSz = sumMinMax $ findContiguous nums target
  where target = findFirstInvalid nums windowSz


process:: String -> Int -> IO ()
process fn ws = do 
  example_stream::[Int] <- map (read) <$> lines <$> readFile fn 
  let ex1_ans = part1 example_stream ws
  let ex2_ans = part2 example_stream ws
  print("First invalid num in " ++ fn ++ " is " ++ (show ex1_ans))
  print("Code for " ++ fn ++ " is " ++ (show ex2_ans))

main :: IO ()
main = do
  process "example.txt" 5
  process "input.txt" 25
  
