import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Debug.Trace (trace)

data Node = Node String [String] deriving (Show, Eq)
type NodeMap = M.Map String Node
data Path = Path {
    nodes :: [Node],
    hasDac :: Bool,
    hasFft :: Bool
} deriving (Show)

parseLine :: String -> Node
parseLine n = let
    label = takeWhile (\c -> c /= ':') n
    children = words $ drop ((length label) + 1) n
    in Node label children

label :: Node -> String
label (Node lbl _) = lbl

children :: Node -> [String]
children (Node _ c) = c 

parse :: String -> NodeMap
parse contents = let
    allNodes = map parseLine $ lines contents
    in M.fromList ([(label n, n) | n <- allNodes] ++ [("out", Node "out" [])])

dfs :: NodeMap -> String -> [[Node]]
dfs nodeMap startLabel = go [[start]] []
  where error = Node "error" []
        missing lbl = Node ("error-"++lbl) []
        start = M.findWithDefault error startLabel nodeMap
        go [] completed = completed
        go ((curr:rest):other_paths) completed
          | label curr == "out" = go (other_paths) ((curr:rest):completed)
          | otherwise = let 
              spawnedPaths = [(x:curr:rest)| x <- map (\c -> M.findWithDefault (missing c) c nodeMap)$ children curr]
              in go (spawnedPaths ++ other_paths) completed


solOne :: String -> Int
solOne contents = length $ dfs (parse contents) "you"


solTwo :: String -> Int
solTwo contents = length $ filter (contains "fft") $ filter (contains "dac") $ dfs (parse contents) "svr"
  where contains lbl path = any (\n -> lbl == (label n)) path


solTwo' :: String -> Int
solTwo' contents = length $ dfsWithPruning (parse contents) "svr"

topoSort :: NodeMap -> [String]
topoSort nm = go (M.elems nm)
  where go :: [Node] -> [String]
        go [] = []
        go nodes = let 
            labels = S.fromList $ map label nodes
            allDest = S.fromList $ concat $ map children nodes
            notDest = S.toList (S.difference labels allDest)
            in notDest ++ (go [n | n <- nodes, S.member (label n) allDest])


dfs' :: String -> M.Map String [String] -> S.Set String
dfs' start g = go S.empty [start]
  where
    go visited [] = visited
    go visited (x:xs)
      | S.member x visited = go visited xs
      | otherwise =
          let next = M.findWithDefault [] x g
          in go (S.insert x visited) (next ++ xs)

-- computeReachableFrom :: NodeMap -> String -> S.Set String
-- computeReachableFrom nodeMap lbl = let topoOrder = topoSort nodeMap in S.fromList $ takeWhile (\x -> x /= lbl) topoOrder

computeReachableFrom :: NodeMap -> String -> S.Set String
computeReachableFrom nm target =
    let revAdj = reverseEdges nm
    in dfs' target revAdj

reverseEdges :: NodeMap -> M.Map String [String]
reverseEdges nm =
    M.fromListWith (++) 
       [ (c, [label n]) | n <- M.elems nm, c <- children n ]


dfsWithPruning :: NodeMap -> String -> [Path]
dfsWithPruning nodeMap startLabel = go (Q.singleton start) [] 
  where error = Node "error" []
        missing lbl = Node ("error-"++lbl) []
        start = Path [M.findWithDefault error startLabel nodeMap] False False
        reachableDac = computeReachableFrom nodeMap "dac"
        reachableFft = computeReachableFrom nodeMap "fft"
        pathCanReachDac path' = (hasDac path') || (S.member (label $ head $ nodes path') reachableDac)
        pathCanReachFft path' = (hasFft path')  || (S.member (label $ head $ nodes path') reachableFft)
        go Q.Empty completed = completed
        go (path Q.:<| other_paths) completed
            | label (head $ nodes path) == "out" =
                go other_paths (path : completed)

            | otherwise =
                let currentNodes = nodes path
                    lastNode = head currentNodes
                    spawnedPaths =
                            [ Path (x:currentNodes)
                                (hasDac path || label x == "dac")
                                (hasFft path || label x == "fft")
                            | x <- map (\c -> M.findWithDefault (missing c) c nodeMap)
                                    (children lastNode)
                            ]
                    filteredPaths =
                            [ p | p <- spawnedPaths
                                , pathCanReachDac p
                                , pathCanReachFft p
                            ]
                in go (other_paths <> Q.fromList filteredPaths) completed


partOne :: String -> IO ()
partOne filename = do
    contents <- readFile filename
    let result = solOne contents
    print(result)

partTwo :: String -> IO ()
partTwo filename = do
    contents <- readFile filename
    let result = solTwo' contents
    print(result)


d = (\x y -> dfsWithPruning y x)
nm = parse <$> readFile "example2.txt"