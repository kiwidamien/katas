type Report = [Int]

example = unlines [
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9"]

parse :: String -> [Report]
parse buffer = (fmap. fmap) read $ map words $ lines buffer

difference :: Report -> Report
difference report = map (\(x,y) -> y - x) reportPairs
    where reportPairs = zip report (drop 1 report)

isMonotonicInc :: Report -> Bool 
isMonotonicInc r = all (\x -> x > 0) $ difference r 

isMonotonicDec :: Report -> Bool 
isMonotonicDec r = all (\x -> x < 0) $ difference r 

isMonotonic :: Report -> Bool
isMonotonic r = (isMonotonicInc r) || (isMonotonicDec r)

isAcceptableSize :: Report -> Bool
isAcceptableSize r = all (\x -> (abs(x) < 4) && (x /= 0)) $ difference r

isSafe :: Report -> Bool 
isSafe report = (isMonotonic report) && (isAcceptableSize report)

allSublistsWithDropOne :: Report -> [Report]
allSublistsWithDropOne report = map (\x -> (take x report) ++ (drop (x+1) report)) [0..(length report)]

isSafeWithDrop1 :: Report -> Bool 
isSafeWithDrop1 report = any isSafe $ allSublistsWithDropOne report

part1 :: String -> IO ()
part1 filename = do
    reports <- parse <$> readFile filename 
    print(length $ filter isSafe reports)

part2 :: String -> IO ()
part2 filename = do
    reports <- parse <$> readFile filename 
    print(length $ filter isSafeWithDrop1 reports)