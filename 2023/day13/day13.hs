module Day13 where 

import Data.List (splitAt, transpose)

type Grid = [[Char]]
type ReflectFunc = Grid -> Int -> Bool

reflectLineAbout :: Int -> String -> [(Char, Char)]
reflectLineAbout col line = let 
                (f,s) = splitAt col line
                minSize = min (length f) (length s)
            in zip (take minSize $ reverse f) (take minSize s)


hReflectImperfect :: Int -> Grid -> Int  -> Bool
hReflectImperfect nErr grid col = (sum $ map reflect grid) == nErr
    where
        reflect :: String -> Int
        reflect =  length . filter (uncurry (/=)) . (reflectLineAbout col)


vReflectImperfect :: Int -> Grid -> Int  -> Bool 
vReflectImperfect nErr grid row = hReflectImperfect nErr (transpose grid) row


hReflectPerfect :: Grid -> Int -> Bool
hReflectPerfect = hReflectImperfect 0

vReflectPerfect :: Grid -> Int -> Bool 
vReflectPerfect = vReflectImperfect 0


getHorzReflection :: ReflectFunc -> Grid -> Maybe Int 
getHorzReflection fn g
  | colNums == [] = Nothing 
  | otherwise = Just (head colNums)
  where numErr = map (fn g) [1..((length $ head g) - 1)]
        colNums = map snd $ filter (\(v,_) -> v) $ zip numErr [1..((length $ head g) - 1)]


getVertReflection :: ReflectFunc -> Grid -> Maybe Int 
getVertReflection fn g
  | rowNums == [] = Nothing
  | otherwise = Just (head rowNums)
    where isReflection = map (fn g) [1..(length g - 1)]
          rowNums = map snd $ filter (\(v,_) -> v) $ zip isReflection [1..(length g - 1)]


score :: (ReflectFunc, ReflectFunc) -> Grid -> Int 
score (fH, fV) g = case getHorzReflection fH g of 
    Just ans -> ans 
    Nothing -> case getVertReflection fV g of 
        Just ans -> 100*ans
        Nothing -> 0

parse :: String -> [Grid]
parse s = go $ lines s
    where theLines = lines s
          go [] = []
          go ls = let (first, rest) = break (=="") ls in first : go (drop 1 rest)


part1 :: String -> IO()
part1 filename = do 
    contents <- readFile filename
    let grids = parse contents 
    let scores = map (score (hReflectPerfect, vReflectPerfect)) grids 
    print (sum scores)

part2 :: String -> IO()
part2 filename = do 
    contents <- readFile filename
    let grids = parse contents 
    let scores = map (score (hReflectImperfect 1, vReflectImperfect 1)) grids 
    print (sum scores)