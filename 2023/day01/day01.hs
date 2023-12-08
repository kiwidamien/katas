module Day01 where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as M

digitNames = [
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
    ]


getDigits :: String -> [Int]
getDigits "" = []
getDigits (c:cs)
  | elem c "123456789" = (ord c - ord '0'):(getDigits cs)
  | otherwise =  getDigits cs 

digitName :: String -> Maybe (Int, String)
digitName s 
  | possible == [] = Nothing
  | otherwise = Just (value, drop (length name) s)
  where possible = take 1 $ filter (\(n,v) -> n == (take (length n) s)) digitNames
        (name, value) = head possible


getDigitsAdvanced :: String -> [Int]
getDigitsAdvanced "" = []
getDigitsAdvanced (d:ds)
  | elem d "123456789" = (ord d - ord '0'):(getDigitsAdvanced ds)
  | otherwise = case (digitName (d:ds)) of
                    Just (value, rem) -> value:(getDigitsAdvanced ds)
                    Nothing -> getDigitsAdvanced ds

calibrationValue :: String -> Int
calibrationValue s = (head nums) * 10 + (last nums)
  where nums = getDigits s


advancedCalibrationValue :: String -> Int 
advancedCalibrationValue s = (head nums) * 10 + (last nums)
  where nums = getDigitsAdvanced s 


sumCalibrations :: (String -> Int) -> [String] -> Int
sumCalibrations func lines = sum $ map func lines


part1 :: String -> IO ()
part1 filename = do 
                    contents <- lines <$> readFile filename
                    let answer = sumCalibrations calibrationValue contents
                    print(answer)


part2 :: String -> IO ()
part2 filename = do 
                    contents <- lines <$> readFile filename
                    let answer = sumCalibrations advancedCalibrationValue contents
                    print(answer)