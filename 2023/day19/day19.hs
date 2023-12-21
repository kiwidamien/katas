module Day19 where 

import qualified Data.Map as M
import Debug.Trace

type Label = String 


data Op = OGT | OLT deriving (Show, Eq)
data Part = Part {_x:: Integer, _m:: Integer, _a:: Integer, _s:: Integer} deriving (Show, Eq)
data Action = Accept | Reject | Redirect Label deriving (Show, Eq)
data Condition = Condition PartFn Action | Literal Action deriving (Show)

data PartRange = PartRange {_xx :: (Integer, Integer), _mm :: (Integer, Integer), _aa:: (Integer, Integer), _ss:: (Integer, Integer)} deriving (Eq, Show)
data PartFn = PartFn {_varName::Char, _opName:: Op, _val:: Integer} deriving (Show, Eq)

type Rule =  [Condition]
type Rulebook = M.Map Label Rule

splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c s = let (f',s') = break (==c) s in f':(splitOn c $ drop 1 s')

scorePart :: Part -> Integer 
scorePart p = sum $ map (\fn -> fn p) [_x, _m, _a, _s]


parsePartFn :: String -> PartFn 
parsePartFn (var:op:num) = PartFn var (if op=='>' then OGT else OLT) (read num)

extractFunc :: PartFn -> (Part -> Bool)
extractFunc (PartFn var op val) = \p -> (cmpOp) (pFunc p) val 
  where pFunc = case var of 
            'x' -> _x
            'm' -> _m
            'a' -> _a
            's' -> _s
        cmpOp = if op == OGT then (>) else (<)

extractVarPart :: Char -> PartRange -> (Integer, Integer)
extractVarPart c
  | c == 'x' = _xx 
  | c == 'm' = _mm 
  | c == 'a' = _aa 
  | c == 's' = _ss  


updatePartRange :: PartRange -> Char -> (Integer, Integer) -> PartRange
updatePartRange pr var val
  | var == 'x' = pr {_xx = val}
  | var == 'm' = pr {_mm = val}
  | var == 'a' = pr {_aa = val}
  | var == 's' = pr {_ss = val}


parseFn :: String -> (Part -> Bool)
parseFn = extractFunc . parsePartFn


parseCond :: String -> Condition
parseCond condStr 
  | condStr == "A" = Literal Accept
  | condStr == "R" = Literal Reject 
  | notElem ':' condStr = Literal (Redirect condStr)
  | otherwise = Condition (parsePartFn preFn) a
    where parseAction "A" = Accept
          parseAction "R" = Reject
          parseAction s = Redirect s
          (preFn, a) = let (f', s') = break (==':') condStr in (f', parseAction $ drop 1 s')


parseRule :: String -> (Label, Rule)
parseRule s = (label, map parseCond preconds)
  where (label, process) = let (f,s') = break (=='{') s in (f, init $ drop 1 s')
        preconds = splitOn ',' process 


parsePart :: String -> Part
parsePart s = Part (pieces!!0) (pieces!!1) (pieces!!2) (pieces!!3)
    where pieces::[Integer] = map (\frag -> let (_, d) = break (=='=') frag in (read $ drop 1 d)) $ splitOn ',' (init $ drop 1 s)


parseFile :: String -> (Rulebook, [Part])
parseFile contents = (M.fromList $ map parseRule preRules, map parsePart preParts)
    where (preRules, preParts) = let (ff,ss) = break (=="") (lines contents) in (ff, drop 1 ss)


evalCond :: Condition -> Part -> Maybe Action 
evalCond (Literal a) _ = Just a 
evalCond (Condition fn a) p = if ((extractFunc fn) p) then Just a else Nothing

evalRule :: Rule -> Part -> Action 
evalRule [] _ = error "Should not get an unresolved rule"
evalRule (c:cs) p = case evalCond c p of 
    Just a -> a
    Nothing -> evalRule cs p 

resolve :: Rulebook -> Part -> Action
resolve book part = go "in" book part
    where go :: Label -> Rulebook -> Part -> Action
          go label book part = case M.lookup label book of 
            Just r -> case (evalRule r part) of
                Accept -> Accept
                Reject -> Reject
                Redirect l' -> go l' book part 
            Nothing -> error ("No such rule as " ++ label)


part1 :: String -> IO ()
part1 filename = do 
    (rulebook, parts) <- parseFile <$> readFile filename 
    let accepted = filter (\p -> (resolve rulebook p) == Accept) parts
    let scores = map scorePart accepted
    --prInteger scores
    print $ sum $ scores


countPartsInRange :: PartRange -> Integer 
countPartsInRange (PartRange (xx0, xx1) (mm0, mm1) (aa0, aa1) (ss0, ss1)) = (xx1 - xx0 + 1) * (mm1 - mm0 + 1) * (aa1 - aa0 + 1) * (ss1 - ss0 + 1)

splitInterval :: (Integer, Integer) -> Op -> Integer -> (Maybe (Integer, Integer), Maybe (Integer, Integer))
splitInterval (a,b) OLT val
    | (a<val) && (val <= b) = (Just (a, val-1), Just (val, b))
    | (val<=a) = (Nothing, Just (a,b))
    | otherwise = (Just (a,b), Nothing)
splitInterval (a,b) OGT val 
    | (a<=val) && (val < b) = (Just (val+1, b), Just (a, val))
    | (val < a) = (Just (a,b), Nothing)
    | otherwise = (Nothing, Just (a,b))


evalFnOnRange :: PartFn -> PartRange -> (Maybe PartRange, Maybe PartRange)
evalFnOnRange (PartFn var op val) pr = (truePart, falsePart)
  where iniInterval = extractVarPart var pr
        (truePath, falsePath) = splitInterval iniInterval op val
        truePart = case truePath of 
            Just ti -> if (snd ti < fst ti) then Nothing else Just (updatePartRange pr var ti)
            Nothing -> Nothing 
        falsePart = case falsePath of
            Just fi -> if (snd fi < fst fi) then Nothing else Just (updatePartRange pr var fi)
            Nothing -> Nothing

evalCondOnRange :: Condition -> PartRange -> [(PartRange, Maybe Action)]
evalCondOnRange (Literal a) pr = [(pr, Just a)]
evalCondOnRange (Condition pF a) pr = case evalFnOnRange pF pr of 
    (Nothing, Nothing) -> [(pr, Nothing)]
    (Nothing, Just fp) -> [(fp, Nothing)]
    (Just tp, Nothing) -> [(tp, Just a)]
    (Just tp, Just fp) -> [(tp, Just a), (fp, Nothing)]



evalRuleOnRange :: Rule -> [PartRange] -> [(PartRange, Action)]
evalRuleOnRange [] _ = error "Should not get an unresolved rule"
evalRuleOnRange (c:cs) prs = getTerminal applyCond cs
    where applyCond = concat $ map (evalCondOnRange c) prs
          getTerminal :: [(PartRange, Maybe Action)] -> Rule -> [(PartRange, Action)]
          getTerminal [] _  = []
          getTerminal ((pr', Nothing):r) fallbackRules = (evalRuleOnRange fallbackRules [pr']) ++ (getTerminal r fallbackRules)
          getTerminal ((pr', Just a): r) fallbackRules = (pr', a): (getTerminal r fallbackRules)

          
resolveOnRange :: Rulebook -> PartRange -> [(PartRange, Action)]
resolveOnRange book part = go "in" book [part]
    where go :: Label -> Rulebook -> [PartRange] -> [(PartRange, Action)]
          go label book part = case M.lookup label book of 
            Just r -> concat $ map (\(pr', a) -> resolve' pr' a) (evalRuleOnRange r part)
            Nothing -> error ("No such rule as " ++ label)
          resolve' pr' (Redirect l') = go l' book [pr']
          resolve' pr' action' = [(pr', action')]


part2 :: String -> IO ()
part2 filename = do 
    (rulebook, _) <- parseFile <$> readFile filename 
    let iniParts = PartRange (1,4000) (1,4000) (1,4000) (1,4000)
    let resolved = resolveOnRange rulebook iniParts
    let accepted = map fst $ filter (\(_, result) -> result == Accept) resolved
    print $ sum $ map countPartsInRange accepted
