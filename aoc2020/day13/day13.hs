module Day13 where

import Chinese

commaSep :: String -> [String]
commaSep ss = let (f,sec) = break (==',') ss in (if f=="" then [] else f:(commaSep $ drop 1 sec))


getFreq :: String -> [Integer]
getFreq s = map read $ filter (/="x") $ commaSep s

crtSetup :: String -> [(Integer, Integer)]
crtSetup s = map (\(r, m) -> (r, (read m)::Integer)) $ filter (\(_, c) -> c /= "x") $ zip [0..] sp
  where sp = commaSep s


ceil :: Integer -> Integer -> Integer
ceil num den
  | mod num den == 0 = div num den
  | otherwise = (div num den) + 1


findBusId :: [Integer] -> Integer -> (Integer, Integer)
findBusId freqs earliest = minimum $ map (\f -> ((ceil earliest f)*f, f)) freqs

waitTime :: [Integer] -> Integer -> Integer
waitTime freqs earliest = (fst $ findBusId freqs earliest) - earliest

part1 :: [Integer] -> Integer -> Integer 
part1 freqs earliest = busId * (waitTime freqs earliest)
  where busId = snd $ findBusId freqs earliest

part2 :: [(Integer, Integer)] -> (Integer, Integer) 
part2 cong = solve $ map (\(pr, m) -> (mod (m - pr) m, m)) cong

main :: IO ()
main = do 
  dump <- lines <$> readFile "input.txt"
  let waitTime:: Integer = read $ head  dump
  let freqs = getFreq $ head $ drop 1 dump 
  let p1 = part1 freqs waitTime

  let congruences = crtSetup $ head $ drop 1 dump
  let p2 = part2 congruences
  print(p1)
  print(p2)
