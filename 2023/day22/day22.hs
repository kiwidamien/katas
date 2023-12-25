module Day22 where 

import Data.List (sortBy, sort, groupBy)
import qualified Data.Map as M
import Debug.Trace 


type Loc = (Int, Int, Int)
type Block = (Loc, Loc)

_x :: Loc -> Int 
_x (x, _, _) = x 

_y :: Loc -> Int
_y (_, y, _) = y 

_z :: Loc -> Int 
_z (_, _, z) = z 

xRange :: Block -> (Int, Int)
xRange (l1, l2) = (min (_x l1) (_x l2), max (_x l1) (_x l2))

yRange :: Block -> (Int, Int)
yRange (l1, l2) = (min (_y l1) (_y l2), max (_y l1) (_y l2))

zRange :: Block -> (Int, Int)
zRange (l1, l2) = (min (_z l1) (_z l2), max (_z l1) (_z l2))

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = let (f', s') = break (==c) s in f': (splitOn c $ drop 1 s')

parseLine :: String -> Block
parseLine line = let (cnr1, _:cnr2) = break (=='~') line in (fn cnr1, fn cnr2)
  where fn s = let (a:b:c:_) = splitOn ',' s in (read a, read b, read c)

overlap :: (Int, Int) -> (Int, Int) -> Bool 
overlap (l1, h1) (l2, h2)
  | h1 < l2 = False
  | h2 < l1 = False
  | otherwise = True 

sortBlocks :: [Block] -> [Block]
sortBlocks = sortBy (\b' c -> let ((bzMin, _), (czMin, _))  = (zRange b', zRange c) in compare bzMin czMin)

moveDownN :: Int -> Block -> Block 
moveDownN n ((x1,y1,z1), (x2,y2,z2))  = ((x1,y1,z1-n), (x2, y2, z2-n))

moveDown :: Block -> Block 
moveDown = moveDownN 1

canMoveDown :: Block -> [Block] -> Int 
canMoveDown b blocks = ourZMin - ((maximum heights) + 1)
    where orderedBlocks = blocks
          ourZMin = let (bzMin, _) = zRange b in bzMin
          belowUs = filter (\b' -> let (bzMin, _) = zRange b' in bzMin < ourZMin) $ orderedBlocks
          ourYRange = yRange b 
          ourXRange = xRange b
          relevant = filter (\b' -> overlap (yRange b') ourYRange) $ filter (\b' -> overlap (xRange b') ourXRange) belowUs
          heights = [0] ++ (map (\b' -> let (_, bzMax) = zRange b' in bzMax) relevant)


moveDownFirst :: [Block] -> [Block] -> ([Block], [Block])
moveDownFirst frozen [] = (frozen, [])
moveDownFirst frozen (b:rest) = (newFrozenBlock:frozen, rest) 
  where newFrozenBlock = moveDownN (canMoveDown b frozen) b


converge :: [Block] -> [Block]
converge blocks = go [] orderedBlocks
  where orderedBlocks = sortBlocks blocks
        go frozen rest
          | rest == [] = frozen
          | otherwise = let (frozen', rest') = moveDownFirst frozen rest in go frozen' rest'


canRemove :: [Block] -> Block -> Bool 
canRemove blocks b = all (==0) $ map (\b' -> canMoveDown b' noB) noB 
    where noB = filter (/=b) blocks


howManyCanRemove :: [Block] -> Int
howManyCanRemove blocks = length $ filter (id) $ map (canRemoveFromCollapsed blocks) blocks

_supports :: [Block] -> Block -> [Block]
_supports collapsed b = directSupport 
  where (zmin, zmax) = zRange b
        relevant = filter (\b' -> let (zmin'', _) = zRange b' in zmin'' == zmax + 1) collapsed
        ourXRange = xRange b 
        ourYRange = yRange b 
        directSupport = filter (\b' -> (overlap ourXRange (xRange b') && (overlap ourYRange (yRange b')))) relevant
      
supports :: [Block] -> M.Map Block [Block]
supports collapsed = M.fromList $ map (\b' -> (b', _supports collapsed b')) collapsed

supportedBy :: M.Map Block [Block] -> M.Map Block [Block]
supportedBy supportMap = M.fromList tuples
  where tuples = map (\gp -> let (b', _) = head gp in (b', map snd gp)) $ groupBy (\a b -> fst a == fst b) $ sort $ concat $ map explode $ M.assocs supportMap

explode :: (a, [b]) -> [(a,b)]
explode (a, []) = []
explode (a, (b:bs)) = (a,b) : explode (a, bs)

canRemoveFromCollapsed :: [Block] -> Block -> Bool 
canRemoveFromCollapsed blocks b = trace (show relevant ++ " " ++ show b ++ " " ++ (show willCollapse)) (length willCollapse > 0) 
  where (zmin, zmax) = zRange b
        relevant =  filter (\b' -> let (zmin'', _) = zRange b' in zmin''==zmax+1) blocks
        ourXRange = xRange b 
        ourYRange = yRange b 
        willCollapse = map (\b' -> (overlap ourXRange (xRange b')) && (overlap ourYRange (yRange b'))) relevant


part1 :: String -> IO ()
part1 filename = do 
    contents <- lines <$> readFile filename 
    let blocks = map parseLine contents
    let steady = converge blocks 
    print steady
    let canSafelyRemove = howManyCanRemove steady 
    print canSafelyRemove

part2 :: String -> IO ()
part2 filename = do 
    contents <- lines <$> readFile filename 
    let blocks = map parseLine contents 
    let steady = converge blocks
    print steady


{-

shakeDown :: [Block] -> [Block]
shakeDown blocks = sortBlocks $ stationary ++ (map (\(amt, b) -> moveDownN amt b)) onlyMovingBlocks
  where movement = map (\b -> canMoveDown b blocks) blocks
        onlyMovingBlocks = filter (\(amt, _) -> amt > 0) $ zip movement blocks
        stationary = map snd $ filter (\(amt, _) -> amt == 0) $ zip movement blocks

converge :: [Block] -> [Block]
converge blocks = fst $ head $ dropWhile (\(a,b) -> (a/=b)) $ zip blockChain (drop 1 blockChain)
    where blockChain = iterate shakeDown blocks

canRemove :: [Block] -> Block -> Bool 
canRemove blocks b = noB == shakeDown noB 
    where noB = filter (/=b) blocks

howManyCanRemove :: [Block] -> Int
howManyCanRemove blocks = length $ filter (id) $ map (canRemove blocks) blocks

part1 :: String -> IO ()
part1 filename = do 
    contents <- lines <$> readFile filename 
    let blocks = map parseLine contents
    let steady = converge blocks 
    let canSafelyRemove = howManyCanRemove steady 
    print canSafelyRemove

part2 :: String -> IO ()
part2 filename = do 
    contents <- lines <$> readFile filename 
    let blocks = map parseLine contents 
    let ans = map (\b -> canMoveDown b blocks) blocks
    print ans


-}


ini = [
    "1,0,1~1,2,1","0,0,2~2,0,2","0,2,3~2,2,3","0,0,4~0,2,4","2,0,5~2,2,5","0,1,6~2,1,6","1,1,8~1,1,9"
  ]

exBlocks = map parseLine ini
collapsed = sortBlocks $ converge exBlocks
{-
main :: IO() 
main = part2 "input.txt"
-}