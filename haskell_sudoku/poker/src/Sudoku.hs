module Sudoku (bestHands) where

import Data.List as L

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
data Suit = S | H | C | D deriving (Bounded, Show, Eq, Read)
data Card = Card Rank Suit deriving (Show)
data Hand = Hand (Card, Card, Card, Card, Card) deriving (Show)

parseRank :: String -> Rank
parseRank "2" = Two
parseRank "3" = Three
parseRank "4" = Four
parseRank "5" = Five
parseRank "6" = Six
parseRank "7" = Seven
parseRank "8" = Eight
parseRank "9" = Nine
parseRank "10" = Ten
parseRank "J" = Jack
parseRank "Q" = Queen
parseRank "K" = King
parseRank "A" = Ace

parseCard :: String -> Card
parseCard css = Card (parseRank $ init css) (read $ [last css]) 

suit :: Card -> Suit
suit (Card _ s) = s

rank :: Card -> Rank
rank (Card r _) = r

parseHand :: String -> Hand 
parseHand handString = Hand $ tuplize $ map parseCard $ words handString
   where tuplize [a,b,c,d,e] = (a,b,c,d,e)

toList :: Hand -> [Card]
toList (Hand (a,b,c,d,e)) = [a,b,c,d,e]

isFlush :: [Card] -> Bool
isFlush [] = True
isFlush cs = (length uniqueSuits) == 1
  where uniqueSuits = L.nub $ map rank cs

isStraight :: [Card] -> Bool
isStraight [] = True
isStraight cs = (length cs) == (maxRank - minRank + 1)
  where maxRank = fromEnum $ maximum ranks
        minRank = fromEnum $ minimum ranks
	ranks = map rank cs

count :: [Card] -> [(Int, Rank)]
count cs = reverse $ sort $ map (\rs -> (length rs, head rs)) $ L.group $ sort ranks
  where ranks = map rank cs


isStraightFlush :: [Card] -> Bool
isStraightFlush cs = (isStraight cs) && (isFlush cs)

isFourOfKind :: [Card] -> Bool
isFourOfKind cs = (fst $ head $ count cs) == 4

isFullHouse :: [Card] -> Bool
isFullHouse cs = cardFreq == [3, 2]
  where cardFreq = map fst $ count cs

-- Flush
-- Straight
isThreeOfKind :: [Card] -> Bool
isThreeOfKind cs = (fst $ head $ count cs) == 3

isTwoPair :: [Card] -> Bool
isTwoPair _ = False

isPair :: [Card] -> Bool
isPair _ = False

handRank :: Hand -> (Int, Rank)
handRank hand 
  | isStraightFlush cs = (0, Ace)
  | isFourOfKind cs = (1, Ace)
  | isFullHouse cs = (2, Ace)
  | isFlush cs = (3, Ace)
  | isStraight cs = (4, Ace)
  | isThreeOfKind cs = (5, Ace)
  | isTwoPair cs = (6, Ace)
  | isPair cs = (7, Ace)
  | otherwise = (8, Ace)
    where cs = toList hand
          maxRank = maximum $ map rank cs



bestHands :: [String] -> Maybe [String]
bestHands = error "You need to implement this function!"

hands = ["6D 7D 8D 9D 10D", "3S 3H 3D 2S 2C", "AH KD QS JS 10C"]
