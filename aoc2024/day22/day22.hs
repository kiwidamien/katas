import Data.Bits
import Data.List (nub, foldl', scanl')
import qualified Data.Map as M 

type Key = (Int, Int, Int, Int)

mix :: Int -> Int -> Int 
mix = xor 

prune :: Int -> Int 
prune num = mod num 16777216

method1 :: Int -> Int 
method1 num = prune $ mix (shiftL num 6) num

method2 :: Int -> Int 
method2 num = mix (shiftR num 5) num

method3 :: Int -> Int 
method3 num = prune $ mix (shiftL num 11) num 


zip4 :: [a] -> [a] -> [a] -> [a] -> [(a,a,a,a)]
zip4 [] _ _ _ = []
zip4 _ [] _ _ = []
zip4 _ _ [] _ = []
zip4 _ _ _ [] = []
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d):(zip4 as bs cs ds)

numPrefixes :: Int -> Int -> Int 
numPrefixes secret nTimes = length $ nub $ fourChain
  where fourChain = let t = evolve' secret nTimes in zip4 t (drop 1 t) (drop 2 t) (drop 3 t)

evolve :: Int -> Int -> Int 
evolve secret nTimes= foldl (\secret f -> f secret) secret $ replicate nTimes (method3.method2.method1) 

evolve' :: Int -> Int -> [Int]
evolve' secret nTimes= map (\n -> mod n 10) $ scanl' (\secret f -> f secret) secret $ replicate nTimes (method3.method2.method1) 

diff :: [Int] -> [Int]
diff a = map (\(x,y) -> (y-x)) $ zip a (tail a)

buildPrefix :: [Int] -> M.Map Key Int
buildPrefix bananas = M.fromList $ reverse contents
  where d = diff bananas
        keys = zip4 d (tail d) (drop 2 d) (drop 3 d)
        contents = zip keys (drop 4 bananas)

allSecrets :: [Int] -> M.Map Key Int 
allSecrets secrets = foldl' (\acc newMap -> merge acc newMap) M.empty individualMaps
  where individualMaps = map (\secret -> buildPrefix $ evolve' secret 2000) secrets
        merge :: M.Map Key Int -> M.Map Key Int -> M.Map Key Int 
        merge map1 map2 = M.unionWith (\x y -> x + y) map1 map2

-- take 5 $ let d = diff $ evolve' 123 2000 in let keys = zip4 d (tail d) (drop 2 d) (drop 3 d) in keys

part1 :: String -> IO ()
part1 filename = do 
    secrets <- lines <$> readFile filename 
    let lastSecret = map (\s -> evolve s 2000) $ map read secrets
    print(sum $ lastSecret)

part2 :: String -> IO () 
part2 filename = do 
    secrets <- map read <$> lines <$> readFile filename 
    let prefixSum = M.elems $ allSecrets secrets
    print(maximum prefixSum)