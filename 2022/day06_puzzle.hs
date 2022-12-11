import Data.Set (fromList)

main = do
  contents <- readFile "sixth_puzzle_input.txt"
  print(indexStartMsg_blocksize4 contents)
  print(indexStartMsg_blocksize14 contents)

example = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

allUnique :: String -> Bool
allUnique s = (length $ Data.Set.fromList(s))==length(s)

_counter :: String -> Int -> Int -> Int
_counter s idx blockSize
  | length(s) < blockSize = -1
  | (allUnique $ take blockSize s) = idx + blockSize
  | otherwise = _counter (tail s) (idx+1) blockSize


counter :: String -> Int -> Int
counter s blockSize = _counter s 0 blockSize

indexStartMsg_blocksize4 s = counter s 4
indexStartMsg_blocksize14 s = counter s 14