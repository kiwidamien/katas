import Data.Char (ord)
import qualified Data.Map as M 
import Data.List (group, groupBy, sort)

type StoneCounter = M.Map Int Int 

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

blinkStoneCounter :: Int -> Int -> StoneCounter 
blinkStoneCounter stone times = M.fromList $ map (\x -> (head x, times * (length x))) $ group $ sort $ blinkStone stone

makeCounter :: [Int] -> StoneCounter
makeCounter stones = M.fromList $ map (\x -> (head x, length x)) $ group $ sort stones

nextCounter :: StoneCounter -> StoneCounter 
nextCounter stones_and_counts = M.unionsWith (+) $ map (\(stone, times) -> blinkStoneCounter stone times) $ M.toList stones_and_counts

part1 :: String -> IO ()
part1 filename = do 
                stones :: [Int] <- map (read) <$> words <$> readFile filename
                let result = length $ (iterate blinkStones stones)!!25
                print(result)

part2 :: String -> IO ()
part2 filename = do 
                stones :: [Int] <- map (read) <$> words <$> readFile filename
                let counter = makeCounter stones
                let result = sum $ M.elems $ (iterate nextCounter counter)!!75
                print(result)