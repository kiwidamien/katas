import Data.Set (Set)
import qualified Data.Set as Set
 

expenseReportFindTwo :: [Int] -> Int
expenseReportFindTwo xs = head $ filter (\n -> elem (2020-n) xs) xs

multiplyExpenses :: [Int] -> Int 
multiplyExpenses expenses= e * (2020 - e)
  where e = expenseReportFindTwo expenses

expenseReportFindThree :: [Int] -> Int
expenseReportFindThree expenses= head $ [x*y*z| x<-expenses, y<-expenses, z<-expenses, x+y+z==2020]


part1:: IO Int
part1 = do
  let contents = lines <$> readFile  "input_part_1.txt"
  let expenses = map (\x -> read x::Int)  <$> contents
  multiplyExpenses <$> expenses

part2:: IO Int
part2 = do
  let contents = lines <$> readFile  "input_part_1.txt"
  let expenses = map (\x -> read x::Int)  <$> contents
  expenseReportFindThree <$> expenses

-- or

part2a :: IO Int
part2a = do
  expenses <- map (\x -> read x) . lines <$> readFile "input_part_1.txt" :: IO [Int]
  return $ expenseReportFindThree expenses