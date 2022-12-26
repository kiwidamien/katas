import Data.Char (ord)


reverseList :: [a] -> [a]
reverseList [] = []
reverseList myList = (reverseList $ tail myList) ++ [head myList]

digitToDec :: Char -> Int
digitToDec '=' = -2
digitToDec '-' = -1
digitToDec x = ord(x) - ord('0')


snafuToInt :: String -> Int
snafuToInt numString = sum $ map (convertTuple) digitAndPower
  where convertTuple (num, place) = (digitToDec num) * (5^place)
        digitAndPower = zip (reverseList numString) [0..] 


_powersOfFive :: Int -> [Int]
_powersOfFive num = reverseList $ takeWhile (\d-> (div num d) >0) $ map ((^) 5) [0..]

decToBase5 :: Int -> [Int]
decToBase5 num = go num (_powersOfFive num) []
  where go num p soFar
         | p == [] = soFar 
         | otherwise = go (num - divisor * greatPow) (tail p) (soFar ++ [divisor])
              where greatPow = head p
                    divisor = div num greatPow


intToSnafu :: Int -> String
intToSnafu num = map (\x->snafuDigit (fst x)) preDigit
    where snafuDigit x = cycle ['0', '1', '2', '=', '-'] !! (x+5)
          preDigit = if (snd $ head pd) == 1 then [(1, 0)] ++ pd else pd
          pd = take (length numAndCarry) $ scanr combine (0,0) numAndCarry
          combine (d, c) (_, oc) = if (d+oc)>=3 then (d+oc-5, c+1) else (d+oc, c)
          numAndCarry = map (\x -> if x>=3 then (x-5, 1::Int) else (x, 0::Int)) $ decToBase5 num	

main = do
  contents <- readFile "day25_input.txt"
  let result = intToSnafu <$> sum $ map (snafuToInt) $ lines $ contents
  return result
