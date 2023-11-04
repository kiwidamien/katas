module Day03 (parse, howManyTreesPart1) where 

main :: IO ()
main = do 
    input <- readFile "example.txt"
    let part1 = howManyTreesPart1 $ parse input
    print ("Expect 7 collisions for part1, saw " ++ (show part1))

