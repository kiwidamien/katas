import qualified Data.Map as M 
import Parse 

data Trie = Trie Bool (M.Map Char Trie) deriving Show


insertToken :: Token -> Trie -> Trie
insertToken "" (Trie _ m) = Trie True m
insertToken (p:ps) (Trie b m) = case M.lookup p m of 
    Nothing -> Trie b (M.insert p (insertToken ps (Trie False M.empty)) m)
    Just (Trie b' m') -> Trie b (M.adjust (\old -> insertToken ps old) p m)

isToken :: String -> Trie -> Bool 
isToken "" root = True
isToken (s:ss) (Trie _ m) = case M.lookup s m of 
    Nothing -> False 
    Just t -> isToken ss t

consumeToken' :: String -> Trie -> [(Int, String)]
consumeToken' "" _ = []
consumeToken' (s:ss) (Trie b m) = case M.lookup s m of
    Nothing -> [(0, s:ss)]
    Just (Trie b' m') -> if b' then (1, ss):(consumeToken' ss (Trie b' m')) else consumeToken' ss (Trie b' m')

consumeToken :: String -> Trie -> [String]
consumeToken s t = map snd $ filter (\(x, _) -> x > 0) $ consumeToken' s t 


consumeAll :: String -> Trie -> Int
consumeAll s t = let ss = consumeToken s t in (length $ filter zeroLength ss) + (sum $ map (\p -> consumeAll p t) ss)
  where zeroLength :: [a] -> Bool
        zeroLength lst = (length lst)==0

getMap :: Trie -> M.Map Char Trie 
getMap (Trie _ m) = m 

tokens = parseTokens "r, wr, b, g, bwu, rb, gb, br"

root = foldr insertToken (Trie False M.empty) tokens


matchStart :: String -> Token -> Bool 
matchStart s token = start == token 
  where start = take (length token) s

justCheck :: String -> [Token] -> Bool 
justCheck "" _ = True
justCheck s tokens = or (map (\t -> justCheck (drop (length t) s) tokens) nextToken)
  where nextToken = filter (matchStart s) tokens


-- 

consume :: String -> [Token] -> [(Token, String)]
consume s tokens = map (\t -> (t, drop (length t) s)) $ filter (matchStart s) tokens

consume' :: ([Token], String) -> [Token] -> [([Token], String)]
consume' (t_so_far, s) tokens = map (\t -> (t_so_far ++ [t], drop (length t) s)) $ filter (matchStart s) tokens

consume'' :: [([Token], String)] -> [Token] -> [([Token], String)]
consume'' poss_ans tokens = concat $ map (\ans -> consume' ans tokens) poss_ans 

_dfs :: [([Token], String)] -> [Token] -> [([Token], String)]
_dfs [] tokens = []
_dfs poss_ans tokens = let nextStep = consume'' poss_ans tokens in 
    (filter (\(ts, ss) -> ss=="") nextStep) ++ (_dfs (filter (\(_, ss) -> ss/="") nextStep) tokens)

dfs :: String -> [Token] -> [[Token]]
dfs message tokens = map fst $ _dfs [([], message)] tokens

--


part1 :: String -> IO ()
part1 filename = do 
    contents <- readFile filename
    let (tokens, patterns) = parse contents 
    let root = foldr insertToken (Trie False M.empty) tokens
    let numPossible = map (\p -> consumeAll p root) patterns
    let possible = filter (\x -> x > 0) numPossible
    print(length $ possible)