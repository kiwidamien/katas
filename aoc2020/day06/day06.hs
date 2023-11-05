module Day06 where 

import Data.List
import Data.Char
import Data.Set (Set)
import qualified Data.Set as S  

type Responses = String

readGroups :: String -> [[String]]
readGroups s = go $ lines s
  where go [] = []
        go lst = let 
	    (f, s) = break (=="") lst
	  in (f : (go $ drop 1 s))

countResponses :: [Responses] -> Int 
countResponses = length . nub . concat

sumAllGroupResponses :: [[Responses]] -> Int 
sumAllGroupResponses gpResponses= sum $ map countResponses gpResponses

responsesFromEveryone :: [String] -> Set Char 
responsesFromEveryone responses = foldl (S.intersection) allLetters rs
  where rs = map S.fromList responses
        allLetters = S.fromList "abcdefghijklmnopqrstuvwxyz"

sumNumResponsesAllInGroupAffirm :: [[Responses]] -> Int 
sumNumResponsesAllInGroupAffirm gpResponses = sum $ map (length . responsesFromEveryone) gpResponses

example :: String 
example = "ab\nbc\n\na\nb\nc\n\nabc\nbc\n"


main :: IO () 
main = do 
    input <- readGroups <$> readFile "input.txt"
    let part1 = sumAllGroupResponses input
    print ("part 1: " ++ (show part1))
    let part2 = sumNumResponsesAllInGroupAffirm input
    print ("part 2: " ++ (show part2))

