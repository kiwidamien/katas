-- Rewrite 
-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
--   | even x = (x-2) * fun1 xs
--   | otherwise = fun1 xs
fun1 :: [Integer] -> Integer
fun1 numL = foldl (*) 1 $ map (\x -> x-2) $ filter even numL

--Rewrite
--fun2 :: Integer -> Integer
--fun2 1 = 0
--fun2 n | even n = n + fun2 (n `div` 2)
--       | otherwise = fun2 (3*n + 1)
collitz :: Integer -> Integer
collitz 1 = 0



