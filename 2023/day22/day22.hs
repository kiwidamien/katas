module Day22 where 

import Data.List 
import qualified Data.Map as M
import qualified Data.Set as S

type Pos = (Int, Int, Int)
type Block = (Pos, Pos)


-- Utility 

minHeight :: Block -> Int 
minHeight ((_,_,z1), (_,_,z2)) = min z1 z2

maxHeight :: Block -> Int
maxHeight ((_, _, z1), (_,_,z2)) = max z1 z2

moveDown :: Block -> Int -> Block 
moveDown ((x,y,z), (a,b,c)) n = ((x,y,z-n), (a,b,c-n))

xRange :: Block -> (Int, Int)
xRange ((x,_,_), (u,_,_)) = (min x u, max x u)

yRange :: Block -> (Int, Int)
yRange ((_,y,_), (_, w, _)) = (min y w, max y w)


overlap :: (Int, Int) -> (Int, Int) -> Bool 
overlap (a, a') (b, b') = let 
    low = min a a' 
    high = max a a'
    low' = min b b'
    high' = max b b' 
    in not ((high < low') || (low > high'))

overlap2d :: Block -> Block -> Bool 
overlap2d b1 b2 = (overlap (xRange b1) (xRange b2)) && (overlap (yRange b1) (yRange b2))

sortBlocks :: [Block] -> [Block]
sortBlocks blocks = sortBy (\a' b' -> compare (minHeight a') (minHeight b')) blocks


collapseBlock :: [Block] -> Block -> Block
collapseBlock pile block = moveDown block amount
  where lowPoint = minHeight block
        removed = filter (\b' -> maxHeight b' < lowPoint) $ filter (/=block) pile
        ourX = xRange block 
        ourY = yRange block
        blocking = sortBy (\a' b' -> compare (maxHeight b') (maxHeight a')) $ filter (\b' -> (overlap (xRange b') ourX) && (overlap (yRange b') ourY)) removed
        amount = if length blocking == 0 then (lowPoint - 1) else let b' = head blocking in (minHeight block) - (maxHeight b') - 1

collapsePile :: [Block] -> [Block]
collapsePile pile = sortBlocks $ foldl (\pile block -> (collapseBlock pile block):pile) [] sortedPile
    where sortedPile = sortBlocks pile

-- Part 1

firstSupportsSecond :: Block -> Block -> Bool
firstSupportsSecond first second = (((maxHeight first) + 1) == (minHeight second)) && (overlap2d first second) && (first /= second)


supports :: [Block] -> M.Map Block (S.Set Block)
supports pile = M.fromList $ map (\supporter -> (supporter, S.filter (firstSupportsSecond supporter) (S.fromList pile))) pile  

supportedBy :: [Block] -> M.Map Block (S.Set Block)
supportedBy pile = M.fromList $ map (\supportedBy -> if minHeight supportedBy == 1 then (supportedBy, S.singleton ((0,0,0), (0,0,0))) else (supportedBy, S.filter (\s -> firstSupportsSecond s supportedBy) (S.fromList pile))) pile

{-
A key block is any block that is a sole support of at least one block
-}
keyBlocks :: [Block] -> [Block]
keyBlocks pile = S.toAscList (S.difference areUniqueSupport (S.singleton ((0,0,0), (0,0,0))))
  where supp = supportedBy pile
        areUniqueSupport = S.unions $ filter (\suppSet -> length suppSet == 1) $ M.elems supp
            

part1 :: String -> IO ()
part1 filename = do 
    contents <- readFile filename 
    let pile = parse contents
    let stablePile = collapsePile pile 
    print( length $ stablePile)
    let cannotDestroy = keyBlocks stablePile
    print((length stablePile) - (length $ cannotDestroy))

-- Part 2            

buildCache :: [Block] -> M.Map Block (S.Set Block)
buildCache pile = go (M.empty) topToBottom 
  where topToBottom = reverse $ sortBlocks pile 
        s = supports pile
        sByIni = supportedBy pile
        go :: (M.Map Block (S.Set Block)) -> [Block] -> M.Map Block (S.Set Block)
        go cache [] =  cache
        go cache (block:rest) = case M.lookup block cache of 
            Just _ -> go cache rest
            Nothing -> go cache' rest
              where cache' = M.insert block (help sByIni cache (S.singleton block)) cache
        help :: (M.Map Block (S.Set Block)) -> (M.Map Block (S.Set Block))  -> (S.Set Block) -> (S.Set Block)
        help sBy cache removed  
          | length removed == 0 = S.empty
          | otherwise = ans
            where alreadyFallen = S.unions $ M.elems $ M.filterWithKey (\k _ -> S.member k removed) cache
                  withAdditional = S.union alreadyFallen removed
                  sByPostRemoval = M.map (\supportSet -> S.difference supportSet withAdditional) $ M.filterWithKey (\k _ -> S.notMember k withAdditional) sBy
                  willFall = S.fromList $ M.keys $ M.filter (\supportSet -> length supportSet == 0) sByPostRemoval
                  ans = S.union withAdditional (help sByPostRemoval cache (S.difference willFall withAdditional))


--Pt2 Answer should be 70727
chainReaction :: [Block] -> Int 
chainReaction pile = sum $ map (\blockCollapse -> length blockCollapse - 1) (M.elems cache)
  where cache = buildCache pile


chainReactionTest :: [Block] -> [Block] -> [Int]
chainReactionTest pile testSet = map (\blockCollapse -> length blockCollapse - 1) (map (\b -> M.findWithDefault S.empty b cache) testSet)
  where cache = buildCache pile 


part2 :: String -> IO ()
part2 filename = do 
    contents <- readFile filename 
    let pile = parse contents
    putStrLn "Parsed the pile" 
    let stablePile = collapsePile pile 
    putStrLn "Collapsed the pile"
    let chainReactions = chainReaction stablePile
    print (chainReactions)


-- Parsing
parse :: String -> [Block]
parse contents = map parseLine $ lines contents
  where parseLine line = let (begin,_:end) = break (=='~') line in (nums begin, nums end)
        nums s = let nStr::[Int] = map read $ words $ map (\c -> if c == ',' then ' ' else c) s in 
                     (nStr!!0, nStr!!1, nStr!!2)

-- Examples

exBlocksStr = [
    "1,0,1~1,2,1", 
    "0,0,2~2,0,2",
    "0,2,3~2,2,3",
    "0,0,4~0,2,4",
    "2,0,5~2,2,5",
    "0,1,6~2,1,6",
    "1,1,8~1,1,9"]

exBlocks :: [Block]
exBlocks = parse $ unlines exBlocksStr

steady :: [Block]
steady = collapsePile exBlocks

lowest :: Block
lowest = head steady
