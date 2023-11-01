module SimpleSolver where
import SudokuTypes (Grid, Matrix, Value, valid, Row, rows, cols, blocks, nodups)
import DefaultCases

{-
 - Solver code (lecture 2)
 -}

solve :: Grid -> [Grid]
solve = filter valid . explode . choices

 {-
  - choices: Replaces each '.' in the grid with the elements 1,...,9 in seperate grids
  -}
type Choices = [Value]

choices :: Grid -> Matrix Choices
choices g = map (map choice) g
  where choice v = if v == '.' then ['1'..'9'] else [v]


cartesianProduct:: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs:xss) = [y:ys| y<-xs, ys<- cartesianProduct xss]


explode :: Matrix [a] -> [Matrix a]
explode m = cartesianProduct colExplode
  where colExplode = map cartesianProduct m

--
badSolve :: Grid -> [Grid]
badSolve = filter valid . explode . choices

-- make more efficient
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy blocks . pruneBy cols . pruneBy rows
  where pruneBy f= f . map reduce . f  -- uses f . f = id


-- reduce ["1234", "1", "34", "3" ] = ["24", "1", "4". "3"]
-- i.e. if we have a single choice ("1", "3") then eliminate from other hoices
-- note not recursive (we see output has a single choice "4" but this does not 
-- eliminate the "4" from the first entry"
reduce :: Row Choices -> Row Choices
reduce collection = map filterChoice collection
  where singles = concat $ filter (\x -> (length x)==1) collection
        filterChoice c = if (length c)==1 then c else filter (\x -> not $ elem x singles) c


-- Easy grid now has ~10**24
solve2 :: Grid -> [Grid]
solve2 = filter valid . explode . prune . choices


-- repeatedly prune! See comment in prune
-- keep doing prune until we reach a fixed point
-- idea:
--   reduce ["1234", "1",  "34", "3"] = ["24", "1", "4", "3"]
--   reduce ["24", "1", "4", "3"] = ["2", "1", "4", "3"]
--   reduce ["2", "1", "4", "3"] = ["2", "1", "4", "3"] -- terminate, input == output
fix :: Eq a => (a -> a) -> a -> a
fix f initial
  | output == initial = initial
  | otherwise = fix f output
    where output = f initial

-- solves easy grids, but not higher
solve3 :: Grid -> [Grid]
solve3 = filter valid . explode . fix prune . choices

{-
 - More efficent solvers
 -}

{-
 - Concept 1: Void
 - A matrix is Void if it has no choices (i.e. one of the elemnts is the empty string
 - so we know there are no solutions
 -}
void :: Matrix Choices -> Bool
void m = any (any null) m

safe :: Matrix Choices -> Bool
safe m = all consistent (rows m) && all consistent (cols m) && all consistent (blocks m)

{-
 - Is there a duplicate single entry?
 - consistent ["1", "23", "15" ,"5"] = True   -- even though would be void after pruning
 - consistnet ["1", "2", "15", "1"] = False   -- Two singletons "1" in that row
 -}

single :: [a] -> Bool
single [_] = True
single _ = False

consistent :: Row Choices -> Bool
consistent = nodups . filter single

blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)

solve4 :: Grid -> [Grid]
solve4 = search . prune . choices

search :: Matrix Choices -> [Grid]
search m 
  | blocked m = []
  | all (all single) m = explode m
  | otherwise = [g | m' <- expandOne m, g <- search(prune m')]


-- expands the first non-trivial choice it sees
--
expandOne :: Matrix Choices -> [Matrix Choices] 
expandOne m = [rows1 ++ [before ++ ([expandable] : after)] ++rows2 | expandable <- cs]
  where
    (rows1, row:rows2) = break (any (not . single)) m
    (before, cs:after) = break (not . single) row

