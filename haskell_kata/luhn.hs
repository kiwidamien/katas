module Luhn (isValid) where
import Data.Char

sortOfDouble :: Integer -> Integer
sortOfDouble n = (if dbl > 9 then dbl - 9 else dbl) where dbl = 2*n

transferDigits :: [Char] -> [Integer]
transferDigits [] = [0]
transferDigits (a:[]) = [read [a]::Integer]
transferDigits (a:b:xs) = [(read [a]::Integer), sortOfDouble (read [b]::Integer)] ++ transferDigits xs


isValid :: String -> Bool
isValid cc_num
  | len(cc_num) < 2 = False
  | otherwise = mod summed 10 == 0
     where summed = sum $ transferDigits reversed
           reversed = reverse $ filter isNumber cc_num

