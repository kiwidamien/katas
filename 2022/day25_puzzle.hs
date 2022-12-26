import Data.Char (ord)


reverseList :: [a] -> [a]
reverseList [] = []
reverseList myList = (reverseList $ tail myList) ++ [head myList]

digitToDec :: Char -> Int
digitToDec '=' = -2
digitToDec '-' = -1
digitToDec x = ord(x) - ord('0')


--base5ToSnafu :: String -> Int
--base5ToSnafu "" = 0
--base5ToSnafu b5 = lastDigit + 5 * (base5ToDec otherDigits)
--    where reversed = reverseList b5
--          lastDigit = digitToDec $ head reversed
--          otherDigits = reverseList $ tail reversed

base5ToSnafu :: String -> Int
base5ToSnafu numString = sum $ map (convertTuple) digitAndPower
  where convertTuple (num, place) = (digitToDec num) * (5^place)
        digitAndPower = zip (reverseList numString) [0..] 


_powersOfFive :: Int -> [Int]
_powersOfFive num = map ((^) 5 ) $ reverseList $ takeWhile (\pow-> (div num (5^pow)) > 0) [0..]


decToBase5 :: Int -> [Int]
decToBase5 num = go num (_powersOfFive num) []
  where go num p soFar
         | p == [] = soFar 
         | otherwise = go (num - divisor * greatPow) (tail p) (soFar ++ [divisor])
              where greatPow = head p
                    divisor = div num greatPow


