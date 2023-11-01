module Day03 (part1, part2, treeCount, Forest) where
type Forest = [String]

columns :: Forest -> Int
columns f = length (head f)

treeCount :: Forest -> Int -> Int
treeCount f mv = length $ filter (=='#') $ map (\(row, pos) -> let i = mod pos c in row!!i) $ zip f [0,mv..]
  where c = columns f

part1 = do
  forest <- lines <$> readFile "input.txt"
  return $ treeCount forest 3

part2 = do
  forest <- lines <$> readFile "input.txt"
  let everyOther = map snd $ filter (\(pos, line) -> (mod pos 2)==0) $ zip [0..] forest
  let ones = treeCount forest 1
  let three = treeCount forest 3
  let five = treeCount forest 5
  let seven = treeCount forest 7
  let otherCount = treeCount everyOther 1
  return $ ones*three*five*seven*otherCount
