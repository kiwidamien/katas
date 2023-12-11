module Day09 where

diff :: [Int] -> [Int]
diff nums = zipWith (-) (drop 1 nums) nums


build :: Int -> [Int] -> [Int]
build start mydiffs = scanl (+) start mydiffs

isAllZero :: [Int] -> Bool 
isAllZero nums = all (==0) nums


extrapolate :: [Int] -> [Int]
extrapolate nums
  | isAllZero nums = repeat 0
  | otherwise = build (head nums) (extrapolate $ diff nums)

nextInSeq :: [Int] -> Int 
nextInSeq nums = last $ take (length nums + 1) $ extrapolate nums

part1 :: String -> IO ()
part1 filename = do
    contents <- lines <$> readFile filename
    let nums::[[Int]] = map (map read . words) contents
    let nextVals = map nextInSeq nums
    print( sum nextVals)

part2 :: String -> IO ()
part2 filename = do
    contents <- lines <$> readFile filename
    let nums::[[Int]] = map (map read . words) contents
    let nextVals = map (nextInSeq . reverse) nums
    print(sum nextVals)