module Day06 where

import Data.List (transpose)

distances :: Int -> Int -> [Int]
distances duration bestDistance = filter (> bestDistance) $ map (\wait -> wait * (duration - wait)) [0..duration]

numWaysPart1 :: [Int] -> [Int] -> Int 
numWaysPart1 durations bestDistances = foldl (*) 1 race_results
    where race_results = map (\(duration, bestD) -> length $ distances duration bestD)$ zip durations bestDistances

numWaysComplex :: Int -> Int -> Int 
numWaysComplex duration bestDistance = tmax - tmin + 1
    where halfD :: Double = (fromIntegral duration) / 2
          delta2 = (halfD) ^ 2 - (fromIntegral bestDistance)
          delta = sqrt delta2
          tmin = ceiling (halfD - delta)
          tmax = floor (halfD + delta)

part1 :: String -> IO()
part1 filename = do 
    contents <- lines <$> readFile filename 
    let times::[[Int]] = map (read) <$> words <$> drop 1 <$> snd <$> break (==':') <$> contents 
    let ans = numWaysPart1 (times!!0) (times!!1)
    print(ans)

part2 :: String -> IO()
part2 filename = do 
    contents <- lines <$> readFile filename
    let times::[Int] = read <$> filter (/=' ') <$> drop 1 <$> snd <$> break (==':') <$> contents 
    let ans = numWaysComplex (times!!0) (times!!1)
    print(ans)