module SumOfMultiples (sumOfMultiples) where
import qualified Data.Set as S


{-
    Comments after reading others solutions:
        - This is dangerous -- if someone tries to eval directly, it is an infinite computation
        - First version used a naive (takeWhile (< limit) $ multiples n), which is also infinite
            if n = 0. My first version then filtered for positive n, but this is dangerous as this logic is
            in another function
        - a list comprehension can get safety!
        e.g. multiplesUpToN base N = [c*base | c=[1..N], c*base < N]
-}
multiples :: Integer -> [Integer]
multiples n = m
  where m = n : zipWith (+) (m) (repeat n)


uniqueMultiplesLessThan :: [Integer] -> Integer -> [Integer]
uniqueMultiplesLessThan factors limit = S.toList $ S.fromList factorList
    where factorList = concat $ map (\n -> takeWhile (< limit) $ multiples n) (filter (> 0) factors)

{-
    Contrived problem in terms of a game setting. Description is

    Given factors and a limit, find the sum of all the unique multiples of 
    each factor less then the limit and sum them
    e..g sumOfMultiples [3, 5] 20
    Multiples of 3 less than 20: 3, 6, 9, 12, 15, 18
    Multiples of 5 less than 20: 5, 10, **15**
    Sum = 3+5+6+9+10+12+15+18 = 78
-}
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ uniqueMultiplesLessThan factors limit

{-
With the simplication mentioned above, the takeWhile and filter are not necessary
sumOfMultiples factors limit = sum $ S.toList $ S.fromList $ concat $ map (\f -> multiplesUpToN f limit) factors
- }