import qualified Data.Map as M 
import qualified Data.Set as S 
import Data.List (sort, nub)


type Loc = (Int, Int)
type MemoPath = M.Map Loc Int
type Allowed = S.Set Loc 
type Config = (Loc, Loc, Allowed)

parse :: String -> Config
parse contents = ((sr,sc), (er,ec), S.fromList $ map (\(r,c,_) -> (r,c)) raw)
  where raw = concat $ map (\(r, line) -> [(r,c,char) | (c, char) <- zip [0..] line, char /= '#']) $ zip [0..] $ lines contents
        (sr,sc,_) = head $ filter (\(_,_,c) -> c=='S') raw 
        (er,ec,_) =  head $ filter (\(_,_,c) -> c=='E') raw 


move :: Loc -> [Loc]
move (y, x) = [(y-1, x), (y, x+1), (y+1, x), (y, x-1)]

twentyMoves :: Loc -> [Loc]
twentyMoves loc = go 20 [loc]
  where go 0 loc = loc 
        go n loc = go (n-1) (nub $ concat $ map move loc)

allowedMoves :: Allowed -> Loc -> [Loc]
allowedMoves allowed currLoc  = filter (\loc -> S.member loc allowed) poss
  where poss = move currLoc


noCheat :: Config -> MemoPath
noCheat (start, end, allowed) = go (M.fromList [(end, 0)]) end 0 (S.delete end allowed)
  where go :: MemoPath -> Loc -> Int -> Allowed -> MemoPath
        go acc curr currDist remain 
          | length remain == 0 = acc 
          | otherwise = go (M.insert nextMove (currDist+1) acc) nextMove (currDist + 1) (S.delete nextMove remain)
            where nextMove = head $ S.toList (S.intersection (S.fromList $ allowedMoves allowed curr) remain)

cheats :: Loc -> MemoPath -> [Int]
cheats mloc path = betterCheats
  where single (y, x) = [(y+1, x), (y-1, x), (y, x+1), (y, x-1)]
        twoStep (y, x) = map (\loc -> M.findWithDefault 10000 loc path) $ filter (\loc -> M.member loc path) $ [
          (y+2, x), (y+1, x-1), (y+1, x+1), (y, x-2), (y, x), (y, x+2), (y-1, x-1), (y-1, x+1), (y-2, x)]
        ordinary = (M.findWithDefault (-1) mloc path) - 2
        betterCheats = [ordinary - t | t <- twoStep mloc, t < ordinary]

twentyCheats :: Loc -> MemoPath -> [Int]
twentyCheats mloc path = betterCheats
  where 
        twoStep loc = map (\loc -> M.findWithDefault 10000 loc path) $ filter (\loc -> M.member loc path) $ twentyMoves loc
        ordinary = (M.findWithDefault (-1) mloc path)
        betterCheats = [ordinary - t | t <- twoStep mloc, t < ordinary]


singleCheat :: MemoPath -> [Int]
singleCheat memoPath = concat $ map (\loc -> cheats loc memoPath) (M.keys memoPath)

multiCheat :: MemoPath -> [Int]
multiCheat memoPath = concat $ map (\loc -> twentyCheats loc memoPath) (M.keys memoPath)


display :: MemoPath -> String 
display memoPath = unlines $ map getLine [0..15]
  where asc = M.toAscList memoPath 
        threeDigit :: Int -> String 
        threeDigit n = let s = (show n) in (take (3 - (length s)) $ repeat '0') ++ s
        getStr :: Loc -> String
        getStr loc = case M.lookup loc memoPath of 
          Nothing -> " # "
          Just n -> threeDigit n
        getLine lineNum = unwords $ map (\c -> getStr (lineNum, c)) [0..15]


part1 :: String -> IO ()
part1 filename = do
     config <- parse <$> readFile filename
     let memoized = noCheat config 
     putStr(display memoized)
     let oneCheat = singleCheat memoized
     print(sort $ oneCheat)
     let atLeast100 = filter (>= 100) oneCheat 
     print(length atLeast100)

part2 :: String -> IO ()
part2 filename = do
     config <- parse <$> readFile filename
     let memoized = noCheat config 
     let twentyCheats = multiCheat memoized
     print(sort $ twentyCheats)
     let atLeast100 = filter (>= 100) twentyCheats 
     print(length atLeast100)