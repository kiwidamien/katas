module Day11 where

import Data.List

type Grid = [[Char]]
type Loc = (Int, Int)

countNeighbors :: Grid -> Loc -> Int
countNeighbors g (r, c) = length $ filter (=='#') candidates 
  where candidates = [g !! tr !! tc | tr <- [r-1, r, r+1], tc <- [c-1,c,c+1], tr>=0, tr < (length g), tc >= 0, tc < (length (g!!tr)), (tr /= r) || (tc /= c)]


transFunc1 :: Char -> Int -> Char
transFunc1 '.' _ = '.'
transFunc1 'L' 0 = '#'
transFunc1 'L' _ = 'L'
transFunc1 '#' c = if (c >= 4) then 'L' else '#'
transFunc1 x _ = x


_cntGrid :: Grid -> [[(Char, Int)]]
_cntGrid g = map (\(row, rowNum) -> map (\(d, colNum) -> (d, countNeighbors g (rowNum, colNum))) $ zip row [0..]) $ zip g [0..]

update :: Grid -> (Char -> Int -> Char) -> Grid
update g tFn = map (\row -> map (\(char, count) -> transFunc1 char count) row) cnts
  where cnts = _cntGrid g


fix :: Grid -> Grid
fix g = fst $ head $ dropWhile (\(old, new) -> old /= new) $ zip lst (drop 1 lst) 
  where lst = iterate (\x -> update x transFunc1 ) g

countOccupied :: Grid -> Int
countOccupied = length . filter (=='#') . concat
