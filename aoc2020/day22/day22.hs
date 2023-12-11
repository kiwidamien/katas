module Day22 where

type Deck = [Int]
type Decks = ([Int], [Int])


playRound :: Decks -> Decks
playRound (d1, []) = (d1, [])
playRound ([], d2) = ([], d2)
playRound (a:as, b:bs) = if a > b then (as ++ [a,b], bs) else (as, bs ++ [b,a])

playGame :: Decks -> Decks
playGame (d, []) = (d, [])
playGame ([], d) = ([], d)
playGame other = playGame $ playRound other

scoreDeck :: Deck -> Int
scoreDeck d = sum $ zipWith (*) (reverse d) [1..]

score :: Decks -> Int
score state = let (e1, e2) = playGame state in (scoreDeck e1) + (scoreDeck e2)
