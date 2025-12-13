index_in_list :: Eq a => a -> [a] -> Int
index_in_list _ [] = -100000
index_in_list elem (x:xs) = if x==elem then 1 else 1 + index_in_list elem xs

_highest::String -> Int -> String
_highest [] n = []
_highest s 0 = []
_highest s n = (largest:_highest (drop position s) (n-1)) 
  where front = take ((length s) - n + 1) s
        largest = maximum front
        position = index_in_list largest front

solutionOne :: [String] -> Int
solutionOne lstLines = sum $ map (\s -> read $ _highest s 2) lstLines

solutionTwo :: [String] -> Int
solutionTwo lstLines = sum $ map (\s -> read $ _highest s 12) lstLines

partOne :: String -> IO ()
partOne filename = do
    contents <- readFile filename
    let result = solutionOne $ lines contents
    print(result)

partTwo :: String -> IO ()
partTwo filename = do
    contents <- readFile filename
    let result = solutionTwo $ lines contents
    print(result)