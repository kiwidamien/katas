import qualified Data.Set as S 


mgcd :: Int -> Int -> Int 
mgcd 0 a = a
mgcd a 0 = a
mgcd 1 a = 1
mgcd a b = if a > b then mgcd b (mod a b) else mgcd b a

-- Solves a * x + b * y = gcd(x,y) for a and b
euclidExtended :: Int -> Int -> (Int, Int)
euclidExtended a b = if a > b then go a 1 0 b 0 1 else go b 0 1 a 1 0
  where go r s t 0 _ _ = (s, t)
        go r0 s0 t0 r1 s1 t1 = go r1 s1 t1 r2 s2 t2
          where q = div r0 r1
                r2 = r0 - q * r1
                s2 = s0 - q * s1
                t2 = t0 - q * t1
    
_oneDiophantine :: Int -> Int -> Int -> Maybe ((Int, Int), (Int, Int))
_oneDiophantine x1 x2 tx 
  | (mod tx gx) /= 0 = Nothing 
  | otherwise = let c = div tx gx in 
                  let (s,t) = euclidExtended x1 x2 in Just ((c*s, c*t), (div x2 gx, -div x1 gx))
  where gx = gcd x1 x2

qceil :: Int -> Int -> Int
qceil a b = case mod a b of 
              0 -> div (abs a) (abs b)
              otherwise -> (div (abs a) (abs b)) + 1

oneDiophantine :: Int -> Int -> Int -> Maybe ((Int, Int), (Int, Int))
oneDiophantine x1 x2 tx = case _oneDiophantine x1 x2 tx of 
                            Nothing -> Nothing
                            Just ((s0, t0), (dx1, dx2)) -> if 
                                                            ((s0 >= 0) && (t0 >= 0)) 
                                                           then 
                                                            Just ((s0, t0), (dx1, dx2)) 
                                                           else if 
                                                            (s0 < 0) 
                                                           then 
                                                            let k = (qceil s0 dx1) in Just ((s0 + k*dx1, t0 + k*dx2), (dx1, dx2))
                                                           else 
                                                            let k = (qceil t0 dx2) in Just ((s0 - k*dx1, t0 - k*dx2), (dx1, dx2))

solsInLimits :: Int -> Int -> Int -> [(Int, Int)]
solsInLimits x1 x2 tx = case oneDiophantine x1 x2 tx of 
                          Nothing -> []
                          Just ((s,t), (ds, dt)) -> filter inLimits $ map (\k -> (s + k*ds, t + k*dt)) [-100..100]
  where inLimits (x,y) = if ((x < 0)||(x>100)||(y<0)||(y>100)) then False else True 

solsInLimits' :: Int -> Int -> Int -> [(Int, Int)]
solsInLimits' x1 x2 tx = case oneDiophantine x1 x2 tx of 
                          Nothing -> []
                          Just ((s,t), (ds, dt)) ->  (takeWhile inLimits $ map (\k -> (s + k*ds, t + k*dt)) [0..]) ++ (takeWhile inLimits $ map (\k -> (s - k*ds, t - k*dt)) [1..])
  where inLimits (x,y) = if ((x < 0)||(y<0)) then False else True


diophantine :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
diophantine (x1, y1) (x2, y2) (tx, ty) = common
  where solX = solsInLimits x1 x2 tx
        solY = solsInLimits y1 y2 ty
        common = S.toList $ S.intersection (S.fromList solX) (S.fromList solY)

diophantine' :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
diophantine' (x1, y1) (x2, y2) (tx, ty) = common
  where solX = solsInLimits' x1 x2 tx
        solY = solsInLimits' y1 y2 ty
        common = S.toList $ S.intersection (S.fromList solX) (S.fromList solY)

minCostPresses :: [(Int, Int)] -> Int 
minCostPresses [] = 0 
minCostPresses presses = minimum $ map (\(a,b) -> 3*a + b) presses 

-- parsing
buttonParse :: String -> (Int, Int)
buttonParse line = (x, y)
  where x = read $ takeWhile (/=',') $ drop 1 $ dropWhile (/='+') line
        y = read $ drop 2 $ dropWhile (/='Y') line

prizeParse :: String -> (Int, Int)
prizeParse line = (x, y)
  where x = read $ takeWhile (/=',') $ drop 1 $ dropWhile (/='=') line
        y = read $ drop 2 $ dropWhile (/='Y') line

splitOnDouble :: String -> [[String]]
splitOnDouble s = go $ lines s
  where go [] = []
        go r = (takeWhile (\s -> length s > 0) r):(go $ drop 1 $ (dropWhile (\s -> length s > 0) r))

parseSingle :: [String] -> ((Int, Int), (Int, Int), (Int, Int))
parseSingle s = let (a:b:c:_) = (map (\(f,line) -> f line) $ zip [buttonParse, buttonParse, prizeParse] s) in (a,b,c)

parse :: String -> [((Int, Int), (Int, Int), (Int, Int))]
parse s = map parseSingle $ splitOnDouble s

-- solutions

part1 :: String -> IO ()
part1 filename = do 
    contents <- parse <$> readFile filename
    let solutions = map (\(p1,p2,t) -> diophantine p1 p2 t) contents
    let costs = map minCostPresses solutions
    print(sum $ costs)