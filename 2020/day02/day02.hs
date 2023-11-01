countOcc :: (Eq a) => a -> [a] -> Int 
countOcc c xs = length $ filter (==c) xs


range :: String -> (Int, Int)
range rString = (minRange, maxRange)
  where (lower, upper) = break (=='-') rString
        minRange = read lower::Int 
        maxRange = read (tail upper)::Int 


parse :: String -> (String, Char, String)
parse line = (range, head chars, password)
  where (range:chars:password:rest) = words line

validatePassword :: Char -> (Int, Int) -> String -> Bool 
validatePassword check (min, max) passwd = (min <= count) && (count <= max)
    where count = countOcc check passwd 

validateLineViaCount :: String -> Bool
validateLineViaCount line = validatePassword symbol (range r) pass
  where (r, symbol, pass) = parse line

validateViaPosition :: String -> Bool 
validateViaPosition line = (countOcc symbol [pass!!(lower-1), pass!!(upper-1)])==1
  where (r, symbol, pass) = parse line
        (lower, upper) = range r


part1 :: IO Int
part1 = do 
    numValid <- length <$> filter (validateLineViaCount) <$> lines <$> readFile "input.txt"
    return $ numValid

part2 :: IO Int
part2 = do 
    numValid <- length <$> filter (validateViaPosition) <$> lines <$> readFile "input.txt"
    return $ numValid