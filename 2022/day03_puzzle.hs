import System.IO
import Control.Monad
import qualified Data.Set as Set
import Data.Char (ord)


main = do
    contents <- readFile "third_puzzle_input.txt"
    print(sumPartitionScores(words contents))
    print(findBadgeScoreTriples(words contents))


two_halves r = splitAt (length(r) `div` 2) r

-- find the element for part 1 (find the element duplicated in both halves)
uniqueElements p = head $ Set.elems (Set.intersection left right)
    where left = Set.fromList (fst p)
          right = Set.fromList (snd p)


scorePartition :: Char -> Int
scorePartition c
  | ord c < ord 'a' = ord c - ord 'A' + 1 + 26
  | otherwise = ord c - ord 'a' + 1


sumPartitionScores rugs = sum $ map (scorePartition . uniqueElements . two_halves) rugs

-- part 2: find the common element amongst three rows
--findBadge :: [String] -> Int
findCommon :: [String] -> String
findCommon rugsacks = Set.elems (foldr (Set.intersection) (r!!0) r) where r = map (Set.fromList) rugsacks

findBadgeScore :: [String] -> Int
findBadgeScore rugsacks = scorePartition $ head $ findCommon rugsacks

findBadgeScoreTriples n
  | n==[] = 0
  | otherwise = (findBadgeScore $ take 3 $ n) + (findBadgeScoreTriples $ drop 3 $ n)


-- example
example = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\njqHRFNqRjq\nvJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\njqHRFNqRjq"

rugsacks = words example
rugsack = head rugsacks
partitions = two_halves rugsack

