module Day18basic where

import Data.Char

data SExp = Literal Int | SOp Op SExp SExp
data Op = Plus | Mult 


eval :: SExp -> Int
eval (Literal i) = i
eval (SOp Plus s1 s2) = (eval s1) + (eval s2)
eval (SOp Mult s1 s2) = (eval s1) * (eval s2) 

swallow :: String -> String
swallow s = dropWhile (==' ') s

parseInt :: String -> Maybe (Int, String)
parseInt s = let (num, rest) = (takeWhile (isDigit) start,dropWhile (isDigit) start)
    in case num of
        "" -> Nothing
	otherwise -> Just (read num, rest)
  where start = swallow s

parseOp :: String -> Maybe (Op, String)
parseOp s
  | length s == 0 = Nothing
  | f == '+' = Just (Plus, rest)
  | f == '*' = Just (Mult, rest)
  | otherwise = Nothing 
    where start = swallow s
          (f, rest) = (head s, tail s)

subparens :: String -> Maybe (String, String)
subparens "" = Nothing 
subparens s = if part1=="" then Nothing else if (head part1) /= '(' then Nothing else Just (part1, part2) 
  where start = swallow s
        (part1, part2) = let x = break (==')') $ reverse s in (reverse $ snd x, reverse $ fst x)


example :: String
example = " 1 + 2 * 3 + 4 * 5 + 6"
