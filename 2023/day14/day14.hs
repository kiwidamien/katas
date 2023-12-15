module Day14 where 

import Data.List (transpose, sort)
import Data.Set (member)
import qualified Data.Set as S


data Direction = North | East | South | West deriving (Eq, Show, Bounded, Enum)
type Grid = [[Char]]

breakParts :: String -> [String]
breakParts "" = []
breakParts s = let (fst, snd) = break (=='#') s in  fst : (take 1 snd) : (breakParts $ drop 1 snd)

fallDown :: String -> String 
fallDown s = concat $ filter (/="") $ map (reverse . sort) $ breakParts s

tilt :: Direction -> Grid -> Grid 
tilt North g = transpose $ map fallDown $ transpose g
tilt East g = map reverse $ map fallDown $ map reverse g
tilt West g = map fallDown g
tilt South g = transpose $ map (\s -> concat $ filter (/="") $ map sort $ breakParts s) $ transpose g


score :: Grid -> Int 
score g = sum withPts
  where numRocks = reverse $ map (length . filter (=='O')) g 
        withPts = zipWith (*) [1..] numRocks

takeUntilDuplicate :: Ord a => [a] -> [a]
takeUntilDuplicate xs = foldr go (const []) xs S.empty
  where
    go x cont set
      | x `member` set = []
      | otherwise      = x : cont (S.insert x set)

gCycle :: Grid -> Grid 
gCycle startGrid = foldl (\g d -> tilt d g) startGrid directionOrder
  where directionOrder = [North, West, South, East]

scoreSequence :: Grid -> [Int]
scoreSequence g = map score $ iterate gCycle g

scoreAtN :: Grid -> Int -> Int 
scoreAtN grid n 
  | n < firstRepeat = score $ last $ take n inf 
  | n < firstRepeat + nextRepeat = score $ last $ take n inf 
  | otherwise = let m = mod (n - firstRepeat) nextRepeat in score $ inf!!(firstRepeat + m)
  where inf = iterate gCycle grid 
        firstRepeat = length $ takeUntilDuplicate inf
        nextRepeat = length $ takeUntilDuplicate $ drop firstRepeat inf

part1 :: String -> IO ()
part1 filename = do 
    contents <- lines <$> readFile filename 
    let tilted = tilt North contents
    print (score tilted)


part2 :: String -> IO ()
part2 filename = do 
    contents <- lines <$> readFile filename 
    print (scoreAtN contents 1000000000)
