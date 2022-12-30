-- toDigits 1234 = [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits n = if n < 10 then [n] else (toDigits $ div n 10 ) ++ [(mod n 10)]


-- doubleEveryOther [1,5,3,2] = [1,10,3,4]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther numList = reverse $ zipWith (*) (cycle [1,2]) (reverse numList)

-- sumDigits
-- sumDigits [1, 13, 4] = 1 + 1 + 3 + 4 = 9
sumDigits :: [Integer] -> Integer
sumDigits digitList = sum $ map sum $ map toDigits digitList

validate :: Integer -> Bool
validate n = (mod check 10 == 0)
    where check = sumDigits $ doubleEveryOther $  toDigits n


example = 4012888888881881


-- Question 2
-- Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)

-- hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
-- Move 
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 start end tmp = [(start, end)]
hanoi n start end tmp = (hanoi (n-1) start tmp end) ++ 
                        [(start, end)] ++ 
                        (hanoi (n-1) tmp end start)
