import Data.Char 

example = "2333133121414131402"

data BlockContents = Empty | FileId Int deriving (Show, Eq)
data FileBlock = FileBlock BlockContents Int Int deriving (Show, Eq)

toNum :: Char -> Int 
toNum c = ord c - ord '0'

explode :: String -> [Int]
explode s = go 0 s
  where go _ "" = []
        go currentNum (c:[]) = let cAsNum = toNum c in replicate cAsNum currentNum
        go currentNum (c:d:ds) = let (cAsNum, dAsNum) = (toNum c, toNum d) in (replicate cAsNum currentNum) ++ (replicate dAsNum (-1)) ++ (go (currentNum+1) ds)

blockLength :: FileBlock -> Int 
blockLength (FileBlock _ _ len) = len


explodeToBlocks :: String -> [FileBlock]
explodeToBlocks s = filter (\x -> blockLength x > 0) $ go 0 0 s 
  where go :: Int -> Int -> String -> [FileBlock]
        go _ _ "" = []
        go currentNum loc (c:[]) = let cAsNum = toNum c in [FileBlock (FileId currentNum) loc cAsNum]
        go currentNum loc (c:d:ds) = let (cAsNum, dAsNum) = (toNum c, toNum d) in (FileBlock (FileId currentNum) loc cAsNum):(FileBlock Empty (loc + cAsNum) dAsNum):(go (currentNum+1) (loc + cAsNum + dAsNum) ds)


defrag :: [Int] -> [Int]
defrag [] = []
defrag [x] = [x]
defrag (x:xs) = if (x/=(-1)) then x:(defrag xs) else defrag $ doSwap x xs
  where doSwap n ns = let h = dropWhile (==(-1)) $ reverse ns in take 1 h ++ (reverse $ drop 1 h)

intListChecksum :: [Int] -> Int 
intListChecksum nums = sum $ zipWith (*) [0..] $ map (\x -> if x > 0 then x else 0) nums


isEmpty :: FileBlock -> Bool 
isEmpty (FileBlock Empty _ _) = True
isEmpty _ = False 

startLoc :: FileBlock -> Int 
startLoc (FileBlock _ s _) = s 

getType :: FileBlock -> BlockContents 
getType (FileBlock t _ _) = t

mlast :: [FileBlock] -> Maybe FileBlock
mlast as = let nonEmpty = dropWhile isEmpty $ reverse as in if length nonEmpty > 0 then (Just (head nonEmpty)) else Nothing        

merge :: [FileBlock] -> [FileBlock]
merge [] = []
merge [x] = [x]
merge (x:y:rest) = if (typeMatch x y) then merge ((combine x y):rest) else x:(merge (y:rest))
       where typeMatch (FileBlock Empty _ _) (FileBlock Empty _ _) = True
             typeMatch (FileBlock (FileId x) _ _) (FileBlock (FileId y) _ _) = (x == y)
             typeMatch _ _ = False
             combine (FileBlock x sx lx) (FileBlock _ _ ly) = FileBlock x sx (lx + ly)


_swap :: FileBlock -> FileBlock -> [FileBlock] -> [FileBlock]
_swap begin end bs
  | b_len > e_len = merge $ (FileBlock (getType end) b_s e_len):(FileBlock (getType begin) (b_s+e_len) (b_len - e_len)):(before ++ [FileBlock (getType begin) e_s e_len] ++ after)
  | b_len == e_len = merge $ (FileBlock (getType end) b_s e_len):(before ++ [FileBlock (getType begin) e_s e_len] ++ after)
  | b_len < e_len = merge $ (FileBlock (getType end) b_s b_len):(before ++ [FileBlock (getType end) e_s (e_len-b_len), FileBlock (getType begin) (e_s + b_len) b_len] ++ after)
  where (b_len, e_len) = (blockLength begin, blockLength end)
        (b_s, e_s) = (startLoc begin, startLoc end)
        after = takeWhile (\x -> (startLoc x) > (startLoc end)) $ reverse $ merge bs
        before = takeWhile (\x -> (startLoc x) < (startLoc end)) bs


swapHeadLastNonEmpty :: [FileBlock] -> [FileBlock]
swapHeadLastNonEmpty [] = []
swapHeadLastNonEmpty [b] = [b]
swapHeadLastNonEmpty (b:bs) = case mlast bs of 
                                Nothing -> merge (b:bs)
                                Just ls -> _swap b ls bs

defragBlocks :: [FileBlock] -> [FileBlock]
defragBlocks [] = []
defragBlocks [x] = [x]
defragBlocks (x:xs) = if (isEmpty x) then defragBlocks $ swapHeadLastNonEmpty (x:xs) else x:(defragBlocks xs)

blockChecksum :: FileBlock -> Int 
blockChecksum (FileBlock Empty _ _) = 0
blockChecksum (FileBlock (FileId id) start len) = id * (div ((2*start + len - 1)*len) 2)

checksum :: [FileBlock] -> Int 
checksum blocks = sum $ map blockChecksum blocks

part1 :: String -> IO ()
part1 filename = do 
          contents <- readFile filename 
          let ans = checksum $ defragBlocks $ explodeToBlocks contents
          print(ans)