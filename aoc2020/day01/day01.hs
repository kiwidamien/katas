-- |
-- >>> productOfEntryPairsThatSumTo2020 [1721, 979, 366, 299, 675, 1456]
-- 514579
-- >>> productOfEntryTriplesThatSumTo2020 [1721, 979, 366, 299, 675, 1456]
-- 241861950


productOfEntryPairsThatSumTo2020 :: [Int] -> Int
productOfEntryPairsThatSumTo2020 nums = head $ map (\x -> x * (2020-x)) $ filter (\x -> elem (2020-x) nums) nums

productOfEntryTriplesThatSumTo2020 :: [Int] -> Int
productOfEntryTriplesThatSumTo2020 nums = head [x*y*z|x<-nums, y<-nums, z<-nums, x+y+z==2020] 

part01 = productOfEntryPairsThatSumTo2020
part02 = productOfEntryTriplesThatSumTo2020


main :: IO ()
main = do
    -- doctest ["day01.hs"]
    input <- map (\n -> (read n)::Int) <$> lines <$> readFile "input.txt"
    print (part01 input) 
    print (part02 input) 

