import Day02

main :: IO ()
main = do 
         input <- readFile "example.txt"
         let result = countPasswordsPart1 input
         print result

