module Day02 (parse, countPasswordsPart1) where

import Data.Bits 

data MinMaxPolicy = MinMaxPolicy {
  polMin:: Int,
  polMax:: Int,
  polLetter::Char,
  polPassword :: String
} deriving Show


parseLine :: String -> MinMaxPolicy
parseLine line = MinMaxPolicy {polMin=min, polMax=max, polLetter=head letter, polPassword=password}
  where [minMax, letter, password] = words line
        min = (read (takeWhile (/= '-') minMax))::Int 
        max = (read $ drop 1 (dropWhile (/= '-') $ minMax))::Int
        
parse :: String -> [MinMaxPolicy]
parse contents = map parseLine $ lines contents 


validPasswordPart1 :: MinMaxPolicy -> Bool
validPasswordPart1 (MinMaxPolicy { polMin, polMax, polLetter, polPassword }) = (numOcc >= polMin) && (numOcc <= polMax)
  where numOcc = length $ filter (== polLetter) polPassword

validPasswordPart2 :: MinMaxPolicy -> Bool
validPasswordPart2 (MinMaxPolicy {polMin, polMax, polLetter, polPassword}) = xor (pos1==polLetter) (pos2==polLetter)
  where pos1 = polPassword !! (polMin - 1)
        pos2 = polPassword !! (polMax - 1)


countPasswordsPart1 :: String -> Int 
countPasswordsPart1 contents = length $ filter id $ map validPasswordPart1 $ parse contents

countPasswordsPart2 :: String -> Int  
countPasswordsPart2 contents = length $ filter validPasswordPart2 $ parse contents 

main :: IO ()
main = do 
    input <- readFile "input.txt"
    let part1 = countPasswordsPart1 input
    print part1
    let part2 = countPasswordsPart2 input
    print part2
