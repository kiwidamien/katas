module Day24 where

import Data.List
import Data.Set (Set)
import qualified Data.Set as S


data Direction = E | SE | SW | W | NW | NE deriving (Eq, Show)
type State = Set (Int, Int, Int)


direction :: Direction -> (Int, Int, Int)
direction E = (1, 0, -1)
direction SE = (0, 1, -1)
direction SW = (-1, 1, 0)
direction W = (-1, 0, 1)
direction NW = (0, -1, 1)
direction NE = (1, -1, 0)

offset :: [Direction] -> (Int, Int, Int)
offset d = foldl (\(ax,ay,az) new -> let (q,r,s) = direction new in (ax+q, ay+r, az+s)) (0,0,0) d

parse :: String -> Maybe (Direction, String)
parse "" = Nothing
parse (f:rest) 
  | f == 'e' = Just (E, rest)
  | f == 'w' = Just (W, rest)
  | length rest == 0 = error "Invalid string"
  | (f=='s')&&(s=='e') = Just (SE, rrest)
  | (f=='s')&&(s=='w') = Just (SW, rrest)
  | (f=='n')&&(s=='e') = Just (NE, rrest)
  | (f=='n')&&(s=='w') = Just (NW, rrest)
    where s = head rest
          rrest = tail rest

parseLine :: String -> [Direction]
parseLine s = case parse s of 
    Nothing -> []
    Just (d, rest) -> d : (parseLine rest)

oddParity :: [[Direction]] -> [(Int, Int, Int)]
oddParity ds = map fst $ filter (\(_, c) -> mod c 2 == 1) $ map (\gp -> (head gp, length gp)) $ groupBy (==) $ sort $ map offset ds 

part1 :: [String] -> Int
part1 input = length $ oddParity $ map parseLine input

{- Cellular automata piece -}

initialState :: [String] -> State
initialState = S.fromList . oddParity . map parseLine 


neighbors :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbors (q,r,s) = map (\(oq,or,os) -> (q+oq, r+or, s+os)) $ map direction [E,SE,SW,W,NE,NW]

countBlackNeighbors :: State -> (Int, Int, Int) -> Int
countBlackNeighbors s tile = length $ S.intersection s (S.fromList $ neighbors tile)

candidates :: State -> State
candidates s = foldl (\acc tile -> S.union acc (S.fromList $ neighbors tile)) s s

shouldBeBlack :: State -> (Int, Int, Int) -> Bool
shouldBeBlack state tile = let numNeighbors = countBlackNeighbors state tile in 
    if S.member tile state then (numNeighbors==1)||(numNeighbors == 2) else
      (numNeighbors == 2)

updateState :: State -> State
updateState old = S.filter (shouldBeBlack old) $ candidates old

updateStateNTimes :: State -> Int ->State
updateStateNTimes state n = foldl (\acc _ -> updateState acc) state [1..n]

main :: String -> IO ()
main fname = do 
  input <- lines <$> readFile fname
  let numFlipped = part1 input
  print $ "Number of black tiles = " ++ (show numFlipped)
  let state = initialState input
  let finalState = updateStateNTimes state 100
  print $ "Number of black tiles after 100 days " ++ (show $ length finalState)
