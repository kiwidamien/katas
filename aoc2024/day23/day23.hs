import qualified Data.Map as M 
import qualified Data.Set as S 
import Data.List (sort, nub) 

type Computer = String
type Network = M.Map Computer (S.Set Computer)
type Triangle = (Computer, Computer, Computer)
type Clique = S.Set Computer

parseLine::String -> (Computer, Computer)
parseLine s = (takeWhile (/='-') s, drop 1 $ dropWhile (/='-') s)

parse :: String -> Network 
parse s = M.fromListWith (S.union) $ concat $ map (\line -> let (x,y) = parseLine line in [(x, S.singleton y), (y,S.singleton x)]) $ lines s

neighbors :: Computer -> Network -> S.Set Computer 
neighbors c network = M.findWithDefault S.empty c network 


findTrianglesRoot :: Network -> Computer -> S.Set (Computer, Computer, Computer)
findTrianglesRoot network root = case M.lookup root network of 
    Nothing -> S.empty
    Just n -> S.fromList $ map (\(a,b,c) -> threeTup $ sort [a,b,c]) $ concat $ map (\c -> [(root, c, n') | n' <- S.toList $ neighbors c network, S.member root (neighbors n' network)]) $ S.toList n 
    where threeTup [a,b,c] = (a,b,c)


purge :: Network -> Computer -> Network
purge network computer = M.map (\old -> S.delete computer old) $ M.delete computer network


findTriangleWithTComp :: Network -> S.Set Triangle
findTriangleWithTComp network = go' S.empty network nodes
  where nodes = filter (\(n:_) -> n=='t') $ M.keys network
        go' :: S.Set Triangle -> Network -> [Computer] -> S.Set Triangle
        go' acc _ [] = acc 
        go' acc net (c:cs) = let net' = purge net c in go' (S.union acc (findTrianglesRoot net c)) net' cs


findTriangle :: Network -> S.Set Triangle 
findTriangle network = go' S.empty network nodes
  where nodes = M.keys network
        go' :: S.Set Triangle -> Network -> [Computer] -> S.Set Triangle
        go' acc _ [] = acc 
        go' acc net (c:cs) = let net' = purge net c in go' (S.union acc (findTrianglesRoot net c)) net' cs

    
part1 :: String -> IO ()
part1 filename = do 
    network <- parse <$> readFile filename
    let trianglesWithT = findTriangleWithTComp network 
    print(trianglesWithT)
    print(length $ trianglesWithT)


addToClique :: Network -> Clique -> [Clique]
addToClique network clique = S.toList $ S.map (\c -> S.insert c clique) $ S.foldl' (\acc computer -> S.intersection acc (neighbors computer network)) (S.fromList $ M.keys network) clique

bootstrapTriangles :: Network -> [Clique]
bootstrapTriangles network = map (\(a,b,c) -> S.fromList [a,b,c]) $ S.toList $ findTriangle network

genCliques :: Network -> [Clique] -> [Clique]
genCliques network currOrder =  nub $ concat $ map (addToClique network) currOrder

biggestClique :: Network -> [Clique]
biggestClique network = head $ reverse $ takeWhile (\x -> length x > 0) $ iterate (genCliques network) $ bootstrapTriangles network

part2 :: String -> IO ()
part2 filename = do 
    network <- parse <$> readFile filename 
    let biggest = nub $ map (S.toAscList) $ biggestClique network
    let password = map (tail . foldl (\acc n -> acc ++ "," ++ n) "") biggest
    print(biggest)
    print(password)
    print(length biggest)
    print(length $ head biggest)