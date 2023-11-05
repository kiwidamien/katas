import Day04

main :: IO ()
main = do 
  input <- parse <$> readFile "example.txt"
  let part1 = countValidPassportsPart1 input
  print ("Part 1: Should see 2 valid passports ... " ++ (show part1))
