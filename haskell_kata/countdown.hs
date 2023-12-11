module Countdown where 

import Data.List 

{-
Countdown problem:

Given
  - a list of input integers
  - a integer target number
construct (if possible) an expression using the numbers (at most once) and the operations *,+,-,/ 
that evaluates to the target.

Restriction:
  - As evaluating, you may only have positive integers
-}

data Operator = Add | Sub | Mul | Divide deriving (Show, Eq)
data Atom = Literal Int | Calc Operator Atom Atom 

eval :: Atom -> Maybe Int 
eval (Literal x) = if x > 0 then Just x else Nothing
eval (Calc Add a b) = let 
                        eA = eval a 
                        eB = eval b
                      in case (eA, eB) of 
                        (Just aVal, Just bVal) -> if (aVal <= bVal) then Just (+) <*> (eval a) <*> (eval b) else Nothing
                        _ -> Just (+) <*> (eval a) <*> (eval b)
eval (Calc Sub a b) = let result = Just (-) <*> (eval a) <*> (eval b) in case result of 
    Nothing -> Nothing
    Just x -> if (x <= 0) then Nothing else Just x
eval (Calc Mul a b) = let 
                            eA = eval a
                            eB = eval b 
                      in case (eA, eB) of
                        (Just 1, _) -> Nothing 
                        (_, Just 1) -> Nothing  
                        (Just aVal, Just bVal) -> if (aVal <= bVal) then Just (*) <*> eA <*> eB else Nothing
                        otherwise -> Just (+) <*> eA <*> eB
eval (Calc Divide a b) = let dm = Just divMod <*> (eval a) <*> (eval b) in case dm of
    Nothing -> Nothing
    Just (d, 1) -> Nothing 
    Just (d, m) -> if m /= 0 then Nothing else Just d

perms :: [a] -> [[a]]
perms [] = [[]]
perms (a:as) = [(take n other) ++ [a] ++ (drop n other)| other <- subperm, n<-[0..(length other)]] ++ subperm
  where subperm = perms as

values :: Atom -> [Int]
values (Literal x) = [x]
values (Calc _ a b) = values a ++ values b

solution :: Atom -> [Int] -> Int -> Bool
solution expr ns target = (elem (values expr) (perms ns)) && (eval expr == Just target)

split :: [a] -> [([a], [a])]
split lst = [(take n lst, drop n lst)| n <- [1..(length lst - 1)]]

combine :: Atom -> Atom -> [Atom]
combine left right = [Calc o left right | o <- [Add, Sub, Mul, Divide]]

generate :: [Int] -> [Atom]
generate [] = []
generate [n] = [Literal n]
generate ns = [e | (ls, rs) <- split ns, l <- generate ls, r <- generate rs, e <- combine l r]

solve :: [Int] -> Int -> [Atom]
solve ns target = map fst $ filter (\(_, result) -> result == Just target) $ map (\a -> (a, eval a)) atomLst
  where atomLst = concat $ map generate $ perms ns

{-
solve works and produces all solutions, but is largely inefficient. 

Note that for N numbers, we have n! + (n-1)! + ... + 1! + 0! different results
Then for m numbers, we have 2^(m-1) places to insert operators. Grows quickly!

At 33:36 of https://learn.microsoft.com/en-us/shows/c9-lectures-erik-meijer-functional-programming-fundamentals/c9-lectures-dr-graham-hutton-functional-programming-fundamentals-chapter-11-of-13
Graham Hutton talks about how to fuse the results.

i.e. instead of generating all possibile expressions, and then eval'ing, how can we do eval (partial eval) to prune
     unproductive paths?
-}
type Result = (Atom, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Literal n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, ls <- results ls, rs <- results rs, res <- combine' ls rs]

combine' :: Result -> Result -> [Result]
combine' (latom, lvalue) (ratom, rvalue) = map (\(a,v) -> (a, fn v)) candidates
    where candidates = [(Calc o latom ratom, eval (Calc o latom ratom)) | o <- [Add, Sub, Mul, Divide], 
                                                eval (Calc o latom ratom) /= Nothing]
          fn (Just x) = x


solve' :: [Int] -> Int -> [Atom]
solve' ns target = map fst $ filter (\(_, result) -> result == target) atomLst
  where atomLst = concat $ map results $ perms ns

{-
This is for the expression class, as the printing of it is quite ugly. 
It is not required for the correctness of the program
-}

operations :: Atom -> [Operator]
operations (Literal x) = []
operations (Calc op a b) = [op] ++ operations a ++ operations b 

instance Show Atom where 
    show (Literal x) = show x
    show (Calc o a b) = wrapped
        where opStr = case o of 
                Add -> "+"
                Sub -> "-"
                Mul -> "*"
                Divide -> "/"
              basicCal = (show a) ++ " " ++ opStr ++ " " ++ (show b)
              wrapped = "(" ++ basicCal ++ ")"
              allPlus = all (== Add) $ operations (Calc o a b)
              allMult = all (== Mul) $ operations (Calc o a b)