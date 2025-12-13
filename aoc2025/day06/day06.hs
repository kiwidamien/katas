import Data.List (transpose)

data Op = Mult | Add deriving (Eq, Show)

parseOperator :: String -> Op
parseOperator "+" = Add
parseOperator "*" = Mult


parsePartOne :: String -> [(Op, [Int])]
parsePartOne contents = map _parse $ transpose $ map words $ reverse $ lines contents
  where _parse (o:nums) = (parseOperator o, map read nums)


-- New parsing


consumeOpline :: String -> [(Op, Int)]
consumeOpline "" = []
consumeOpline (x:xs) = let 
    count = length $ takeWhile (\c -> c==' ') xs 
    remaining = drop (count + 1) (x:xs)
  in 
    (parseOperator [x], if remaining=="" then count + 1 else count):(consumeOpline remaining)


splitOnEmptyLines :: [String] -> [[String]]
splitOnEmptyLines [] = []
splitOnEmptyLines lstOfString = 
    let 
        topElem = takeWhile notAllSpace lstOfString
    in topElem:(splitOnEmptyLines $ drop ((length topElem) + 1) lstOfString)
  where
    notAllSpace s = any (\c -> c/=' ') s


consumeInt :: String -> Int
consumeInt contents = read $ filter (\c -> c/=' ') contents


consumeNumLine :: [String] -> [Int]
consumeNumLine = map consumeInt

-- End new parsing

parsePartTwo :: String -> [(Op, [Int])]
parsePartTwo contents = zip ops numbers
   where 
    preprocessed = reverse $ lines contents
    opLine = head preprocessed
    numbers = map consumeNumLine $ splitOnEmptyLines $ map reverse $ transpose $ tail preprocessed
    ops = map fst $ consumeOpline opLine


calculate :: (Op, [Int]) -> Int
calculate (Mult, xs) = product xs
calculate (Add, xs) = sum xs

partOne :: String -> Int
partOne contents = sum $ map calculate $ parsePartOne contents

partTwo :: String -> Int
partTwo contents = sum $ map calculate $ parsePartTwo contents


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


opLine = head <$> reverse <$> lines <$> readFile "example.txt"
numLines = tail <$> reverse <$> lines <$> readFile "example.txt"
