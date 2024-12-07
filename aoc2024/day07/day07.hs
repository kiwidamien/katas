parseLine :: String -> (Int, [Int])
parseLine s = (read target, map read rest)
  where target = takeWhile (/=':') s 
        rest = words $ drop ((length target) + 1) s


myCombineTwoOps :: [Int] -> [Int]
myCombineTwoOps (x:xs) = go [x] xs
  where go acc [] = acc
        go acc (x:xs) = go ((map (\n -> n*x) acc) ++ (map (\n -> n+x) acc)) xs

orOp :: Int -> Int -> Int 
orOp x y = read $ (show x) ++ (show y)

myCombineThreeOps :: [Int] -> [Int]
myCombineThreeOps (x:xs) = go [x] xs
  where go acc [] = acc
        go acc (x:xs) = go ((map (\n -> n*x) acc) ++ (map (\n -> n+x) acc) ++ (map (\n -> orOp n x) acc)) xs


canHitTarget2Ops :: Int -> [Int] -> Bool 
canHitTarget2Ops t possibilities = elem t $ (myCombineTwoOps possibilities)

canHitTarget3Ops :: Int -> [Int] -> Bool 
canHitTarget3Ops t possibilities = elem t $ (myCombineThreeOps possibilities)


part1 :: String -> IO () 
part1 filename = do 
    listOfTargetsAndInputs <- map parseLine <$> lines <$> readFile filename
    let reachable = filter (\(t, inputs) -> canHitTarget2Ops t inputs) listOfTargetsAndInputs
    print(sum $ map fst reachable)

part2 :: String -> IO () 
part2 filename = do 
    listOfTargetsAndInputs <- map parseLine <$> lines <$> readFile filename
    let reachable = filter (\(t, inputs) -> canHitTarget3Ops t inputs) listOfTargetsAndInputs
    print(sum $ map fst reachable)