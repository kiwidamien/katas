-- nth element should contain every nth element
-- i.e. first entry should contain entire list, second entry every second entry, etc
-- skips "hello!" = ["hello!", "el!', "l!', "l", "o", "!"]
-- skips [1,2,3,4] = [[1,2,3,4], [2,4], [3], [4]]
skips :: [a] -> [[a]]
skips lst = map (\x->go x lst) [1..(length lst)]
      where go n ll = map snd $ filter (\x -> (fst x)==n) $ zip (cycle [1..n]) ll


localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) = (if (a < b) && (b > c) then [b] else []) ++ localMaxima (b:c:xs) 
localMaxima _ = []

 
