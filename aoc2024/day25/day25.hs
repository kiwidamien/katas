import Data.List (transpose)

data Object = Key [Int] | Lock [Int] | Broken deriving (Show, Eq)


rawLock = ".....\n#....\n#.#.#\n#.#.#\n#.#.#\n###.#\n#####"

isLock :: [String] -> Bool
isLock (l:_) = all (\c -> c=='.') l

isKey :: [String] -> Bool 
isKey (l:_) = all (\c -> c=='#') l

getCounts :: Object -> [Int]
getCounts (Key l) = l 
getCounts (Lock l) = l 

parseObject :: [String] -> Object
parseObject lines
  | isKey lines = Key counts
  | isLock lines = Lock counts 
  | otherwise = Broken
  where countHash line = foldl (\acc c -> let n = if c == '#' then 1 else 0 in acc + n) (-1) line
        counts = map countHash $ transpose lines

-- Insist the key goes first to avoid double counting
noOverlap :: Object -> Object -> Bool 
noOverlap (Key k) (Lock l) = all (\c -> c < 6) $ zipWith (+) k l
noOverlap _ _ = False

split :: String -> [[String]]
split contents = go [] $ lines contents 
  where go acc [] = acc 
        go acc ls = let (chunk, rest) = (takeWhile (/="") ls, drop 1 $ dropWhile (/="") ls) in go (chunk:acc) rest

parse :: String -> [Object]
parse s = map parseObject $ split s

part1 :: String -> IO ()
part1 filename = do 
    collection <- parse <$> readFile filename 
    let fits = [(x,y) | x<-collection, y<-collection, noOverlap x y]
    print(length collection)
    print(length fits)