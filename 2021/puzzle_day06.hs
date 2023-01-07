{-
 - SEQUENCES!
 -
-}
import Data.List
import Data.List.Split

type FishState = [Int] -- FishState!!day are number of fish with timer 'day'

fishListToFishState :: [Int] -> FishState
fishListToFishState lstOfFish = map (count lstOfFish) [0 .. 8]
  where
    count lst n = length $ filter (== n) lst

fishEvolution :: FishState -> FishState
fishEvolution days = (old1thru6) ++ [day7 + day0, day8, day0]
  where
    day0 = head days
    old1thru6 = take 6 $ tail days
    day7 = days !! 7
    day8 = last days

evolveDays :: Int -> [Int] -> [Int]
evolveDays nDays iniState = foldl (\old _ -> fishEvolution old) iniState [1 .. nDays]

part1 = do
  contents <- splitOn "," <$> readFile "inputs/day06.txt"
  let iniFish = fishListToFishState $ map (\x -> (read x) :: Int) contents
  let numFish = sum $ evolveDays 80 iniFish
  return numFish

part2 = do
  contents <- splitOn "," <$> readFile "inputs/day06.txt"
  let iniFish = fishListToFishState $ map (\x -> (read x) :: Int) contents
  let numFish = sum $ evolveDays 256 iniFish
  return numFish
