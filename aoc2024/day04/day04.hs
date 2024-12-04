type Grid = [[Char]]
type Block = [[Char]]

getLetterAtLoc :: Grid -> (Int, Int) -> Char 
getLetterAtLoc g (row, col)
    | row >= (length g) = '.' 
    | row < 0 = '.' 
    | col < 0 = '.' 
    | col >= (length (g!!row)) = '.' 
    | otherwise = (g!!row)!!col 


getFourLetterWords :: (Int, Int) -> Grid -> [String]
getFourLetterWords (row, col) g = star
  where star = [map (\x -> getLetterAtLoc g (row, col+x)) [0..3],
                map (\x -> getLetterAtLoc g (row, col-x)) [0..3],
                map (\x -> getLetterAtLoc g (row+x, col+x)) [0..3],
                map (\x -> getLetterAtLoc g (row+x, col-x)) [0..3],
                map (\x -> getLetterAtLoc g (row-x, col+x)) [0..3],
                map (\x -> getLetterAtLoc g (row-x, col-x)) [0..3],
                map (\x -> getLetterAtLoc g (row+x, col)) [0..3],
                map (\x -> getLetterAtLoc g (row-x, col)) [0..3]]

countXmas :: Grid -> [Int]
countXmas grid = [length $ filter (\x -> x=="XMAS") $ getFourLetterWords (r,c) grid| r <- [0..numRows], c <- [0..numCols]]
  where numRows = length grid - 1
        numCols = length (grid!!0) - 1

get3x3 :: (Int, Int) -> Grid -> Block
get3x3 (row, col) g = cols 
  where rows = take 3 $ drop row g
        cols = map (take 3 . drop col) rows

isBlockXMAS :: Block -> Bool
isBlockXMAS block
    | (length block) /= 3 = False
    | (length (block!!0)) /= 3 = False 
    | otherwise = all (\s -> (s=="MAS") || (s=="SAM")) diagonals
  where diagonals = [[(block!!x)!!x|x<-[0..2]], [(block!!x)!!(2-x)|x<-[0..2]]]


part1 :: String -> IO ()
part1 filename = do 
    grid <- lines <$> readFile filename 
    let answer = sum $ countXmas grid
    print answer

part2 :: String -> IO ()
part2 filename = do 
    grid <- lines <$> readFile filename 
    let answer = length $ filter isBlockXMAS $ [get3x3 (r, c) grid | r <- [0..(length grid - 1)], c<-[0..(length (grid!!0) - 1)]]
    print answer