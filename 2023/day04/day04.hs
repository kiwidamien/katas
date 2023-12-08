module Day04 where

import Data.Char
import qualified Data.Set as S
import qualified Data.Map as M

type Card = ([Int], [Int])
type Rules = [Card]
type Deck = [Int]


str2int :: String -> Int 
str2int s = foldl (\b a -> b*10 + ord a - ord '0') 0 s


parseLine :: String -> Card
parseLine s = (winning, hand)
    where handData = tail $ snd $ break (==':') s
          (winningRepr, handRepr) = break (=='|') handData
          winning = map str2int $ words winningRepr
          hand = map str2int $ tail $ words handRepr  


numWinners :: Card -> Int 
numWinners (winning, hand) = length $ S.intersection (S.fromList winning) (S.fromList hand)


scoreCard :: Card -> Int 
scoreCard card
    | numWinningCards == 0 = 0
    | otherwise = 2^(numWinningCards - 1)
    where numWinningCards = numWinners card


part1 :: String -> IO()
part1 filename = do
        contents <- map parseLine <$> lines <$> readFile filename
        let scores = map scoreCard contents
        print (sum scores)


processCard :: Int -> Rules -> Deck -> Deck 
processCard cardIndex rules deck = map deckUpdateFn $ zip [0..] deck
    where card = rules !! cardIndex
          numWinningCards = numWinners card 
          numTimesThisCard = deck !! cardIndex
          deckUpdateFn (idx, value)
              | idx <= cardIndex = value
              | (idx <= cardIndex + numWinningCards) = value + numTimesThisCard 
              | otherwise = value


setup :: [String] -> (Rules, Deck)
setup s = (rules, take (length rules) $ repeat 1)
    where rules = map parseLine s


iterateDeck :: Rules -> Deck -> Deck 
iterateDeck rules deck = foldl (\acc cardIndex -> processCard cardIndex rules acc) deck [0..(length deck - 1)]


part2 :: String -> IO()
part2 filename = do
    contents <- lines <$> readFile filename
    let (rules, deck) = setup contents
    let numCards = iterateDeck rules deck 
    print (sum numCards)