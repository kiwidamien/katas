import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Heap as H
import Data.List (sort)
import Debug.Trace


commaSplit :: String -> [String]
commaSplit "" = []
commaSplit s = let 
    next = takeWhile (\c -> c /=',') s
    in next:(commaSplit $ drop ((length next)+1) s)


parse :: String -> [[Int]]
parse contents = map (\line -> map read $ commaSplit line) $ lines contents


pairwiseDistance :: [[Int]] -> [(Int, (Int, Int), ([Int], [Int]))]
pairwiseDistance vecList =  [(f v1 v2, (a, b), (v1, v2)) | (a, v1) <- zip [0..] vecList, (b, v2) <- zip [0..] vecList, a < b]
  where f vx vy = sum $ map (\(e1, e2) -> (e1 - e2) * (e1 - e2)) $ zip vx vy

initialize :: [a] -> [S.Set Int]
initialize lst = [S.fromList [n] | (_, n) <- zip lst [0..]]


merge :: [(Int, (Int, Int), ([Int], [Int]))] -> [S.Set Int] -> ([(Int, (Int, Int), ([Int], [Int]))], [S.Set Int])
merge [] clusters = ([], clusters)
merge pairwise clusters = (pairwise', clusters')
  where (_, (i1, i2), _) = head pairwise
        s1 = head $ [c | c <- clusters, S.member i1 c]
        s2 = head $ [c | c <- clusters, S.member i2 c]
        newCluster = S.union s1 s2
        clusters' = newCluster : [c | c <- clusters, not $ S.isSubsetOf c newCluster]
        pairwise' = filter (\(_, (a,b), (_, _)) -> (S.notMember a newCluster) || (S.notMember b newCluster)) pairwise

doNMerges :: Int -> [[Int]] -> [S.Set Int]
doNMerges times vecList = go times pairs initialClusters
  where initialClusters = initialize vecList
        pairs = sort $ pairwiseDistance vecList
        go 0 p c = c
        go n p c = let (p', c') = merge p c in go (n-1) p' c'

connectionOrder :: [[Int]] -> [(Int, Int)]
connectionOrder vecList = go pairs initialClusters
  where initialClusters = initialize vecList
        pairs = sort $ pairwiseDistance vecList
        go :: [(Int, (Int, Int), ([Int], [Int]))] -> [S.Set Int] -> [(Int, Int)]
        go [] _ = []
        go ((_, (i1, i2), _):rst) clusters
          | length clusters > 1 =  (i1, i2):(go rst $ connect i1 i2 clusters)
          | otherwise = []

finalConnection :: [[Int]] -> ([Int], [Int])
finalConnection vecList = (vecList !! i1, vecList !! i2)
  where (i1, i2) = last $ connectionOrder vecList

connect :: Int -> Int -> [S.Set Int] -> [S.Set Int]
connect i1 i2 clusters = clusters'
  where s1 = head $ [c | c <- clusters, S.member i1 c]
        s2 = head $ [c | c <- clusters, S.member i2 c]
        newCluster = S.union s1 s2 
        clusters' = newCluster : [c | c <- clusters, not $ S.isSubsetOf c newCluster]


simpleMerge :: Int  -> [(Int, (Int, Int), ([Int], [Int]))] -> [S.Set Int] -> [S.Set Int]
simpleMerge times pairs clusters = foldl (\acc (i1, i2) -> connect i1 i2 acc) clusters connections
  where connections = map (\(_, p, _) -> p) $ take times $ sort pairs


solOne :: Int -> String -> [Int]
solOne times contents = reverse $ sort $ map length $ finalClusters
  where vec = parse contents
        initialClusters = initialize vec
        pairs = sort $ pairwiseDistance vec
        finalClusters = simpleMerge times pairs initialClusters

partOne' :: Int -> String -> Int
partOne' times contents = product $ take 3 $ solOne times contents


partOne :: String -> IO ()
partOne filename = do
  contents <- readFile filename
  let result = partOne' 1000 contents
  print(result)

partTwo :: String -> IO ()
partTwo filename = do
  contents <- readFile filename
  let result = finalConnection $ parse contents
  print(result)