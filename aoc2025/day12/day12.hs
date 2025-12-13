data SolTypes = Trivial | Impossible | NeedMoreWork deriving (Show, Eq)

data Gift = Gift Int Int [[Int]] deriving (Show, Eq)
data Floor = Floor Int Int deriving (Show)


floorArea :: Floor -> Int
floorArea (Floor x y) = x*y

giftBoxSpace :: Gift -> Int
giftBoxSpace (Gift x y _) = x*y

giftOccupiedSpace :: Gift -> Int
giftOccupiedSpace (Gift _ _ grid) = sum $ map sum grid


trivialPuzzleSolution :: Floor -> [Gift] -> [Int] -> SolTypes
trivialPuzzleSolution floor gifts counts
    | totalGiftBoxSpace <= floorSpace = Trivial
    | totalOccupiedSpace > floorSpace = Impossible
    | otherwise = NeedMoreWork
      where floorSpace = floorArea floor
            totalGiftBoxSpace = sum $ map (\(g, count) -> count * (giftBoxSpace g)) $ zip gifts counts
            totalOccupiedSpace = sum $ map (\(g, count) -> count * (giftOccupiedSpace g)) $ zip gifts counts 


makeGift :: [[Int]] -> Gift
makeGift [] = Gift 0 0 []
makeGift (first:rest) = Gift (length (first:rest)) (length first) (first:rest)


splitOnEmpty :: String -> [[String]]
splitOnEmpty contents = go ll
    where ll = lines contents
          go :: [String] -> [[String]]
          go [] = []
          go stuff = let 
            top = (takeWhile (\c -> c /= "") stuff)
            in top : (go $ drop ((length top) + 1) stuff)


parseGift :: [String] -> Gift
parseGift ll = let 
    grid = [[if c == '#' then 1 else 0| c <- line] | line <- drop 1 ll]
    in makeGift grid


parseGifts :: [[String]] -> [Gift]
parseGifts = map parseGift


parseGiftFile :: String -> [Gift]
parseGiftFile contents = parseGifts $ splitOnEmpty contents


parseProblem :: String -> (Floor, [Int])
parseProblem line = let 
    firstNum = takeWhile (\c -> c /= 'x') line
    secondNum = takeWhile (\c -> c /= ':') $ drop ((length firstNum)+1) line
    numbers = map read $ words $ drop 1 $ dropWhile (\c -> c /= ':') line
    in (Floor (read firstNum) (read secondNum), numbers)


solveProblems :: [(Floor, [Int])] -> [Gift] -> [SolTypes]
solveProblems [] _ = []
solveProblems ((f,c):rest) gifts = (trivialPuzzleSolution f gifts c):(solveProblems rest gifts)

partOne :: String -> String -> [SolTypes]
partOne problemContents giftContents = let
    gifts = parseGiftFile giftContents
    problems = map parseProblem $ lines problemContents
    in solveProblems problems gifts

solOne :: String -> IO ()
solOne fileroot = do
    problem <- readFile (fileroot ++ ".floor")
    gift <- readFile (fileroot ++ ".pieces")
    let result = partOne problem gift
    let numTrivial = length $ filter (== Trivial) result
    let numNeedWork = length $ filter (== NeedMoreWork) result
    print(numTrivial)
    print(numNeedWork)