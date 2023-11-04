import Day03

main :: IO ()
main = do 
       input <- readFile "example.txt"
       let part1 = howManyTreesPart1 input
       print part1
