module Day04 (parse, countValidPassportsPart1) where 

import Data.Map (Map, fromList, findWithDefault)
import Data.Char 


splitOn :: Eq a => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn lst delim = let 
             (before, after) = break (== delim) lst 
	            in ( [before] ++ (splitOn (drop 1 after) delim))

parse :: String -> [String]
parse contents = map unwords $ splitOn (lines contents) ""

isValidPassportPart1 :: String -> Bool 
isValidPassportPart1 record = required 
  where fieldNames = map (\w -> fst $ break (==':') w) $ words record
        required = and (map (\x -> elem x fieldNames) [
	    "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"
	    ])

countValidPassportsPart1 :: [String] -> Int 
countValidPassportsPart1 records = length $ filter isValidPassportPart1 records


isValidByr :: Map String String -> Bool
isValidByr m = let 
    yr = findWithDefault "0000" "byr" m 
  in (yr >= "1920") && (yr <= "2002")

isValidIssue :: Map String String -> Bool
isValidIssue m = let
    yr = findWithDefault "0000" "iyr" m
  in (yr>="2010") && (yr<="2020")

isValidExpiry :: Map String String -> Bool
isValidExpiry m = let 
    yr = findWithDefault "0000" "eyr" m
  in (yr>="2020") && (yr <= "2030")


isValidHeight :: Map String String -> Bool
isValidHeight m
  | length rec < 2 = False
  | suffix == "cm" = (digits >= "150") && (digits <= "193")
  | suffix == "in" = (digits >= "59") && (digits <= "76")
  | otherwise = False
    where rec = findWithDefault "" "hgt" m 
          (digits, suffix) = break (isAlpha) rec 

isValidHairColor:: Map String String -> Bool 
isValidHairColor m = (head rec == '#') && (all (\c -> elem c "0123456789abcdef") $ tail rec)
  where rec = findWithDefault "!" "hcl" m

isValidEyeColor :: Map String String -> Bool
isValidEyeColor m = elem rec ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  where rec = findWithDefault "" "ecl" m

isValidPassportID :: Map String String -> Bool
isValidPassportID m = (length rec == 9) && (all isDigit rec)
  where rec = findWithDefault "" "pid" m 

isValidPassport :: Map String String -> Bool
isValidPassport m = foldl (&&) True $ map (\f -> f m) validationFuncs
  where validationFuncs = [ isValidPassportID, isValidEyeColor, isValidHairColor, isValidHeight, isValidExpiry, isValidIssue, isValidByr]

parseStringToPassport :: String -> Map String String
parseStringToPassport str = fromList $ map (\w -> let (f, _:s) = break (== ':') w in (f,s)) $ words str 


isValidPassportString :: String -> Bool 
isValidPassportString = isValidPassport . parseStringToPassport 

countValidPassportsPart2 :: [String] -> Int 
countValidPassportsPart2 lines = length $ filter isValidPassportString lines 

main :: IO ()
main = do 
    input <- parse <$> readFile "input.txt"
    let part1 = countValidPassportsPart1 input
    print("We see " ++ (show part1) ++ " valid Passports")
    let part2 = countValidPassportsPart2 input 
    print("We see " ++ (show part2) ++ " passports with valid values")
