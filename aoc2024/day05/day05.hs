import qualified Data.Map as M 
import qualified Data.Set as S 

data Rule = Rule Int Int deriving (Show, Eq)
type RuleBook = M.Map Int [Int]
type Pages = [Int]

parseRule :: String -> Rule 
parseRule s= Rule (read first) (read second)
  where first = takeWhile (/= '|') s
        second = drop 1 $ dropWhile (/= '|') s

ruleBook :: [Rule] -> RuleBook 
ruleBook rules = M.fromListWith (++) tups 
  where extractRule rule = case rule of 
            Rule a b -> (a,[b])
        tups = map extractRule rules

disallowedRuleBook :: [Rule] -> RuleBook
disallowedRuleBook rules = M.fromListWith (++) tups 
  where extractRule rule = case rule of 
            Rule a b -> (b, [a])
        tups = map extractRule rules

obeyAllRules :: Pages -> RuleBook -> Bool 
obeyAllRules pages rulebook = go pages rulebook (S.empty)
  where go :: Pages -> RuleBook -> S.Set Int  -> Bool
        go [] _ _ = True
        go (p:ps) rulebook notAllowed = if (elem p notAllowed) then False else case M.lookup p rulebook of 
            Nothing -> go ps rulebook notAllowed
            Just moreDis -> go ps rulebook (S.union notAllowed (S.fromList moreDis))

obeyAllRulesArray :: [Pages] -> RuleBook -> [Pages] 
obeyAllRulesArray pages disallowed = filter (\p -> obeyAllRules p disallowed) pages

middle :: Pages -> Int 
middle ps = let index = div (length ps) 2 in ps!!index

commaSep :: String -> Pages 
commaSep "" = []
commaSep s = (read $ takeWhile (/=',') s):(commaSep $ drop 1 $ dropWhile (/=',') s)

topoSort :: Pages -> RuleBook -> Pages 
topoSort pages rules = go [] rules pages
  where go :: Pages -> RuleBook -> Pages -> Pages
        go sorted _ [] = sorted
        go [] rules (p:ps) = go [p] rules ps
        go sorted rules (p:ps) = undefined
          


ex = lines <$> readFile "example.txt"
rules = map parseRule <$> takeWhile (\x -> length x > 0) <$> ex
dis = disallowedRuleBook <$> rules
rb = ruleBook <$> rules
pages = map commaSep <$> drop 1 <$> dropWhile (\x -> length x > 0) <$> ex
p = head <$> pages

part1 :: String -> IO ()
part1 filename = do 
    contents <- lines <$> readFile filename
    let rules =  map parseRule $ takeWhile (\x -> length x > 0) $ contents
    let dis = disallowedRuleBook $ rules
    let pages = map commaSep $ drop 1 $ dropWhile (\x -> length x > 0) $ contents
    let working = obeyAllRulesArray pages dis
    print(sum $ map middle working)