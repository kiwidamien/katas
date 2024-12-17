import Data.Char (ord)

charSeqToInt :: String -> Int
charSeqToInt s = foldl (\acc letter -> 10*acc + (ord letter - ord '0')) 0 s

blinkStone :: Int -> [Int]
blinkStone 0 = [1]
blinkStone n = let digitLen = length $ show n in if 
                (even digitLen) 
               then 
                [read $ take (div digitLen 2) (show n), charSeqToInt $ drop (div digitLen 2) (show n)]
               else [n*2024]


blinkStones :: [Int] -> [Int]
blinkStones stones = concat $ map blinkStone stones

part1 :: String -> IO ()
part1 filename = do 
                stones :: [Int] <- map (read) <$> words <$> readFile filename
                let result = length $ (iterate blinkStones stones)!!25
                print(result)

part2 :: String -> IO ()
part2 filename = do 
                stones :: [Int] <- map (read) <$> words <$> readFile filename
                let result = length $ (iterate blinkStones stones)!!75
                print(result)