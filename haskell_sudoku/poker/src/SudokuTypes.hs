module SudokuTypes where
type Value = Char
type Row a = [a]
type Matrix a = [Row a]
type Grid = Matrix Value

blank :: Grid
blank = replicate 9 (replicate 9 '.')

{- Property: rows . rows = id -}
rows :: Matrix a -> [Row a]
rows = id

{- Property: cols . cols = id -}
cols :: Matrix a -> [Row a]
cols [] = []
cols m 
  | length (head m) == 0 = []
  | otherwise = (map head m) : (cols $ map tail m)

{- Property: blocks . blocks = id -}
blocks :: Matrix a -> [Row a]
blocks [] = []
blocks m = go m
  where go m = (_blockRow (take 3 m)) ++  (blocks (drop 3 m))


_blockRow :: [Row a] -> [Row a]
_blockRow [[],[],[]] = []
_blockRow rowBlock = (concat $ map (take 3) rowBlock) : _blockRow (map (drop 3) rowBlock)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs

valid :: Grid -> Bool
valid g = (all nodups (rows g)) && (all nodups (cols g)) && (all nodups (blocks g))

