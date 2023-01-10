import Data.Map (Map)
import qualified Data.Map as Map

import Data.List

state = "NNCB"
transitions = [
    ("CH" , "B"),
    ("HH" , "N"),
    ("CB" , "H"),
    ("NH" , "C"),
    ("HB" , "C"),
    ("HC" , "B"),
    ("HN" , "C"),
    ("NN" , "C"),
    ("BH" , "H"),
    ("NC" , "B"),
    ("NB" , "B"),
    ("BN" , "B"),
    ("BB" , "N"),
    ("BC" , "B"),
    ("CC" , "N"),
    ("CN" , "C")
  ]

-- Naive attempt, generates entire string
myFunc :: [(String, String)] -> String -> String
myFunc t str = expand $ lookup str t
  where expand (Nothing) = str
        expand (Just c) = [head str] ++ c


evolve :: [(String, String)] -> String -> String
evolve _ "" = ""
evolve _ (c:[]) = [c] 
evolve t (a:b:rest) = (myFunc t [a,b]) ++ (evolve t (b:rest))
 

evolveMany :: [(String, String)] -> Int -> String -> String
evolveMany trans n iniState = foldl (\x _ ->evolve trans x) iniState [1..n]


mostCommonMinusLeastCommon :: [(String, String)] -> Int -> String -> Int
mostCommonMinusLeastCommon trans n iniState = mostCommon - leastCommon 
  where finalState = evolveMany trans n iniState
        freq = map (length) $ groupBy (==) $ sort finalState
        mostCommon = maximum freq
        leastCommon = minimum freq

-- bigram attempt, only track the bigrams in the transition sequence, and their frequeny

--stateToBigram :: [(String, String)] -> String -> Map String Int
type BigramState = [(String, Int)]


stateToBigram :: String -> BigramState
stateToBigram state = map (\x -> (head x, length x)) $ groupBy (==) $ sort $ bigrams
  where bigrams = zipWith (\x y -> [x,y]) state (tail state)


collect :: BigramState -> [(Char, Int)]
collect bigramState = genericCollect $ concat $ map split bigramState
  where split alpha = (,) <$> (fst alpha) <*> [snd alpha]

genericCollect :: (Eq a, Ord a) => [(a, Int)] -> [(a, Int)]
genericCollect keyCntWithRepeats = map (keyAndSum) $ groupBy (\x y->(fst x)==(fst y)) $ sort keyCntWithRepeats
  where keyAndSum lstKeyCnt = (fst $ head lstKeyCnt, sum $ map snd lstKeyCnt)


iniState = stateToBigram state
evolveBigram trans bigrams = undefined

_evolveChanges :: [(String, String)] -> (String, Int) -> BigramState 
_evolveChanges trans (bigram, occ) = 
  let finished = lookup bigram trans
  in case finished of 
     (Just letter) -> [([head bigram] ++ letter, occ), (letter ++ (tail bigram), occ)] 
     _ -> [(bigram, occ)]


evolveChanges :: [(String, String)] -> BigramState -> BigramState
evolveChanges trans biGramCounts = genericCollect $ expandedPairCount
  where expandedPairCount = concat $ map (_evolveChanges trans) biGramCounts 


evolveNDays :: [(String, String)] -> Int -> BigramState -> BigramState
evolveNDays trans n iniState = foldl' (\x _ -> evolveChanges trans x) iniState [1..n]


-- every letter is double counted EXCEPT the first and last character
letterFreqNDays :: [(String,String)] -> Int -> String -> [(Char, Int)]
letterFreqNDays trans n state = sortBy (\x y -> compare (snd x) (snd y)) $ genericCollect $ doubleCnt ++ correctEnds
  where preprocess = collect $ evolveNDays trans n $ stateToBigram state
        doubleCnt = map(\x->(fst x, div (snd x) 2)) preprocess
        correctEnds = [(head state, 1), (last state, 1)] 


diffMostCommonLeastCommon :: [(String, String)] -> Int -> String -> Int
diffMostCommonLeastCommon trans n state = most - least
  where  ordered = letterFreqNDays trans n state
         most = snd (last ordered)
         least = snd (head ordered)

{-
-- Example on updating Maps to refer to
-- countChar :: Char -> M.Map Char Int -> M.Map Char Int
-- countChar c oldMap = newMap
--    where
--       newMap = M.insertWith (+) c 1 oldMap

countChar :: Char -> Map Char Int -> Map Char Int
countChar c oldMap = newMap
   where
   newMap = Map.insertWith (+) c 1 oldMap

countString :: String -> Map Char Int
countString ""     = Map.empty
countString (c:cs) = countChar c (countString cs)
-}

-- Parsing information

part1 = do
  contents <- lines <$> readFile "inputs/day14.txt"
  let iniState = head contents
  let transText = (filter (/="") (tail contents))
  let transitions = map (\arrowString -> (head $ words arrowString, last $ words arrowString)) transText 
  let numDiff = mostCommonMinusLeastCommon transitions 10 iniState 
  return numDiff

part1_other = do
  contents <- lines <$> readFile "inputs/day14.txt"
  let iniState = head contents
  let transText = (filter (/="") (tail contents))
  let transitions = map (\arrowString -> (head $ words arrowString, last $ words arrowString)) transText 
  let numDiff = diffMostCommonLeastCommon transitions 10 iniState 
  return numDiff


part2 = do
  contents <- lines <$> readFile "inputs/day14.txt"
  let iniState = head contents
  let transText = (filter (/="") (tail contents))
  let transitions = map (\arrowString -> (head $ words arrowString, last $ words arrowString)) transText 
  let numDiff = mostCommonMinusLeastCommon transitions 40 iniState 
  return numDiff


part2_other = do
  contents <- lines <$> readFile "inputs/day14.txt"
  let iniState = head contents
  let transText = (filter (/="") (tail contents))
  let transitions = map (\arrowString -> (head $ words arrowString, last $ words arrowString)) transText 
  let numDiff = diffMostCommonLeastCommon transitions 40 iniState 
  return numDiff
