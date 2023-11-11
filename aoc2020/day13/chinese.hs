module Chinese where

type Cong = (Integer, Integer) 

{-
gcd :: Int -> Int -> Int
gcd 0 m = m
gcd n 0 = n
gcd n m = gcd m (mod n m)

lcm :: Int -> Int -> Int
lcm m n = div (m*n) (gcd n m)
-} 

inverse :: Integer -> Integer -> Integer
inverse a m = head $ filter (\x -> mod (a*x) m == 1) [0..m]

_findUV :: Integer -> Integer -> (Integer, Integer)
_findUV n m = (v, u) 
  where d = gcd n m
        m' = div m d
        n' = div n d
        v = inverse n' m'
        u = inverse m' n'


pairwiseSolve :: Cong -> Cong -> Cong
pairwiseSolve (a, m) (b, n) 
  | mod (a-b) (gcd n m) /= 0 = error "No solution!"
  | otherwise = (mod base the_lcm, the_lcm)
      where lambda = div (a-b) (gcd n m)
            (_, u) = _findUV n m
            base = a - m*u*lambda
            the_lcm = lcm n m

solve :: [Cong] -> Cong
solve [] = (0, 0)
solve (x:[]) = x
solve (x:y:rest) = let s1 = pairwiseSolve x y in solve (s1:rest)
