module Day10 where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M


parse :: String -> [Int]
parse s = sort $ map (read) $ lines s

calcJumps :: [Int] -> [Int]
calcJumps nums = map (\(a,b) -> b - a) $ zip modNums (drop 1 modNums)
  where modNums = [0] ++ nums ++ [(maximum nums) + 3]

countJumps :: [Int] -> Map Int Int
countJumps lst = M.fromList $ map (\elem -> (head elem, length elem)) $ groupBy (==) $ sort lst

scoreJumps :: Map Int Int -> Int
scoreJumps m = (M.findWithDefault 0 1 m) * (M.findWithDefault 0 3 m)

part1 :: [Int] -> Int
part1 = scoreJumps . countJumps . calcJumps

incrementAdapterCounter :: Map Int Int -> Int -> Map Int Int
incrementAdapterCounter state newAdapter = newState
  where lookupOrZero s n = M.findWithDefault 0 n s
        newCount = sum $ map (lookupOrZero state) [newAdapter-1, newAdapter-2, newAdapter-3]
        newState = M.insert newAdapter newCount state


countWays :: [Int] -> Int 
countWays adapters = finalCount
  where start = M.fromList [(0, 1)]
        newState = foldl (\state i -> incrementAdapterCounter state i) start adapters
        finalCount = M.findWithDefault 0 (maximum adapters) newState

main :: IO ()
main = do 
  example <- parse <$> readFile "example.txt"
  input <- parse <$> readFile "input.txt"
  let part1_example = part1 example
  let part1_input = part1 input
  let part2_example = countWays example
  let part2_input = countWays input

  print("Weird score for example: " ++ (show part1_example))
  print("# ways to connect (example) " ++ (show part2_example))
  print("Weird score for input: " ++ (show part1_input))
  print("# ways to connect (input) " ++ (show part2_input))

