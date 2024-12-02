import Data.List (sort)
import qualified Data.Map as M


parse buffer = map words $ lines buffer

parseInt :: String -> Int
parseInt = read

changeType :: [[String]] -> [[Int]]
changeType = ((fmap . fmap) parseInt)

transpose :: [[a]] -> [[a]]
transpose ([]:_) = [] 
transpose lol = (map head lol):(transpose (map tail lol))

twoLists :: String -> [[Int]] 
twoLists buffer = transpose $ changeType $ parse buffer

manhattan :: [[Int]] -> Int
manhattan xs = sum $ map (\x -> abs(x!!0 - x!!1)) $ transpose $ sort <$> xs

makeCounter :: [Int] -> M.Map Int Int
makeCounter xs = M.fromListWith (+) $ map (\x -> (x,1)) xs

similarity :: [Int] -> [Int] -> Int
similarity left right = M.foldrWithKey keyReducer 0 (makeCounter left)
  where rc = makeCounter right
        keyReducer n freqLeft acc = acc + n * freqLeft * (M.findWithDefault 0 n rc)

part1 :: String -> IO ()
part1 filename = do
		    myLists <- twoLists <$> readFile filename
		    let answer = manhattan myLists
		    print(answer)

part2 :: String -> IO ()
part2 filename = do
		    (left:right:_) <- twoLists <$> readFile filename
		    let answer = similarity left right
		    print(answer
