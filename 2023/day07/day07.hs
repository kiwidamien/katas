module Day07 where 

import qualified Data.Map as M
import Data.List (sortBy)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | A deriving (Show, Eq, Ord, Enum, Bounded)
data HandType = HighCard | OnePair | TwoPair | ThreeOfKind | FullHouse | FourOfKind | FiveOfKind deriving (Show, Eq, Ord)


type Card = Rank 
type Hand = [Rank]

readRank :: Char -> Rank 
readRank r 
  | r == '2' = Two
  | r == '3' = Three
  | r == '4' = Four 
  | r == '5' = Five 
  | r == '6' = Six
  | r == '7' = Seven 
  | r == '8' = Eight 
  | r == '9' = Nine
  | r == 'T' = Ten 
  | r == 'J' = J 
  | r == 'Q' = Q 
  | r == 'K' = K 
  | r == 'A' = A

readHand :: String -> Hand
readHand s = map readRank s 


classifyHand :: Hand -> HandType
classifyHand hand
  | topFreq == 5 = FiveOfKind
  | topFreq == 4 = FourOfKind
  | freqs == [3,2] = FullHouse
  | freqs == [3,1,1] = ThreeOfKind 
  | freqs == [2,2,1] = TwoPair 
  | freqs == [2,1,1,1] = OnePair 
  | otherwise = HighCard
    where freqs = sortBy (flip compare) $ M.elems $ M.fromListWith (+) $ zip hand (repeat 1)
          topFreq = head freqs

replaceJ :: Rank -> Hand -> Hand 
replaceJ replacement hand = map (\c -> if (c==J) then replacement else c) hand


classifyHand2 :: Hand -> HandType 
classifyHand2 hand = bestHandType
    where hands = map (\x -> replaceJ x hand) [minBound..maxBound]
          bestHandType = maximum $ map classifyHand hands

compareCards2 :: Rank -> Rank -> Ordering 
compareCards2 a b 
  | (a==J) && (b==J) = EQ 
  | (a==J) = LT 
  | (b==J) = GT 
  | otherwise = compare a b 


compareHands :: Hand -> Hand -> Ordering
compareHands a b 
  | aHandType /= bHandType = compare aHandType bHandType
  | otherwise = compare a b  
    where aHandType = classifyHand a 
          bHandType = classifyHand b


compareHands2 :: Hand -> Hand -> Ordering 
compareHands2 a b 
  | aHandType /= bHandType = compare aHandType bHandType
  | otherwise = if tieBreaker == [] then EQ else head tieBreaker
    where aHandType = classifyHand2 a 
          bHandType = classifyHand2 b
          tieBreaker = filter (/=EQ) $ map (\(aa,bb) -> compareCards2 aa bb) $ zip a b


parse :: String -> [(Hand, Int)]
parse s = map readFn lstOfTupes
    where lstOfTupes = map words $ lines s
          readFn lst = (readHand $ head lst, read (lst!!1))


totalWinningsBy :: (Hand -> Hand -> Ordering) -> [(Hand, Int)] -> Int
totalWinningsBy compareFn handAndBets = foldl (\acc (r,b) -> acc + r*b) 0 rankAndBet
    where rankAndBet = zip [1..] $ map snd $ sortBy (\a b -> compareFn (fst a) (fst b)) handAndBets

totalWinnings :: [(Hand, Int)] -> Int 
totalWinnings = totalWinningsBy compareHands 

totalWinnings2 :: [(Hand, Int)] -> Int 
totalWinnings2 = totalWinningsBy compareHands2 


part1 :: String -> IO()
part1 filename = do 
                    contents <- readFile filename
                    let handAndBets = parse contents 
                    let winnings = totalWinnings handAndBets 
                    print winnings

part2 :: String -> IO()
part2 filename = do 
                    contents <- readFile filename
                    let handAndBets = parse contents 
                    let winnings = totalWinnings2 handAndBets 
                    print winnings