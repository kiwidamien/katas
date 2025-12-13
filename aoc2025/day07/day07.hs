import Data.List (nub)
import qualified Data.Map as Map

propagate :: [Int] -> String-> ([Int], Int)
propagate beamLoc line = let 
    (lstWithDups, count') = go 0 [] splitterLoc beamLoc
  in 
    (nub lstWithDups, count')
  where splitterLoc = [ x | (x, c) <- zip [1..] line, c=='^']
        go count lst _ [] = (lst, count)
        go count lst splitterLoc (b:bs) = if b `elem` splitterLoc then go (count + 1) ((b-1):(b+1):lst) splitterLoc bs else go count (b:lst) splitterLoc bs

propagateHistories :: [(Int, Int)] -> String -> [(Int, Int)]
propagateHistories beamAndCount line = let 
    lstWithDups = go [] splitterLoc beamAndCount
  in 
    Map.toList $ Map.fromListWith (+) lstWithDups
  where splitterLoc = [ x | (x, c) <- zip [1..] line, c=='^']
        go lst _ [] = lst
        go lst splitterLoc ((loc, count):bs) = if loc `elem` splitterLoc then go ((loc-1, count):(loc+1, count):lst) splitterLoc bs else go ((loc, count):lst) splitterLoc bs

initialPosition :: String -> Int
initialPosition lookForS = (length $ takeWhile (\c -> c /= 'S') lookForS) + 1

countSplits :: [Int] -> [String] -> Int
countSplits beams splits = go 0 beams splits
  where go count _ [] = count
        go count beams (s:ss) = let (newBeams, numSplits) = propagate beams s in go (count + numSplits) newBeams ss


partOne :: String -> Int
partOne contents = countSplits [initial] (lines contents)
  where initial = (length $ takeWhile (\c -> c /= 'S') $ head $ lines contents) + 1

partTwo :: String -> Int
partTwo contents = sum $ map snd $ foldl propagateHistories [(initial, 1)] $ lines contents
  where initial = initialPosition $ head $ lines contents


solOne :: String -> IO ()
solOne filename = do
    contents <- readFile filename
    let result = partOne contents
    print(result)

  
solTwo :: String -> IO ()
solTwo filename = do
    contents <- readFile filename
    let result = partTwo contents
    print(result)