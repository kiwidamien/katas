import qualified Data.Set as Set

example = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 3"
exampleLarger = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"

updateDir::Char -> [Int]
updateDir d
  | d=='D' = [1, 0]
  | d=='U' = [-1, 0]
  | d=='L' = [0, -1]
  | d=='R' = [0, 1]
  | otherwise = [-100, 100]


_sign x = if x>0 then 1 else if x<0 then -1 else 0


updateHead d loc = zipWith(+) loc (updateDir d)


deltaAdj startLoc endLoc = zipWith (-) startLoc endLoc


isAdjacent loc1 loc2 = (maximum $ map abs $ deltaAdj loc1 loc2) <= 1


updateSegment newPrevSegment oldSegment
  | isAdjacent newPrevSegment oldSegment = oldSegment 
  | otherwise = zipWith (+) oldSegment newDirection
    where newDirection = map _sign $ deltaAdj newPrevSegment oldSegment


newBridge :: Int -> [[Int]]
newBridge numSegments = take numSegments $ repeat [0,0]

propagate :: Char -> [[Int]] -> [[Int]]
propagate d oldState = scanl updateSegment newHead (tail oldState)
    where newHead = updateHead d $ head oldState

parseInputLines :: String -> String
parseInputLines text = foldl (++) [] $ map (repeatFunc) $ dirAmountList 
    where repeatFunc (dir:numRepeat:[]) = replicate (read numRepeat::Int) (head dir)
          dirAmountList = map words $ lines text


trackTail numSegments dirList = go (newBridge numSegments) dirList Set.empty
    where go currState currDir tailVisited 
             | currDir == [] = tailVisited
             | otherwise = go (propagate (head currDir) currState) (tail currDir) (Set.insert (last currState) tailVisited)

{-
 - This is the test case from advent of code
-}

testCase = do
  let exampleDirList = parseInputLines example
  let numVisited_part1 = length $ trackTail 2 exampleDirList
  let numVisited_part2 = length $ trackTail 10  $ parseInputLines exampleLarger
  let msg1 = "In part 1, the tail should visit 13 places, saw " ++ (show numVisited_part1) ++ " from our code"
  let msg2 = "In part 2, the tail should visit 36 places, saw " ++ (show numVisited_part2) ++ " from our code"
  putStrLn msg1
  putStrLn msg2
