numFromRange :: [Char] -> (Int, Int)
numFromRange s = (read f, read second)
  where f = takeWhile (\c -> c /= '-') s
        second = tail $ dropWhile (\c -> c /= '-') s

generateList :: (Int, Int) -> [Int]
generateList (start, stop) = [start..stop]

checkValid :: Int -> Bool
checkValid x
  | mod (length s) 2 == 1 = True
  | otherwise = (take half s) /= (drop half s)
  where s = (show x)::String
        half = div (length s) 2

canBeBuiltFromRepeatingSubstring :: String -> String -> Bool
canBeBuiltFromRepeatingSubstring substring full_string 
  | mod (length full_string) (length substring) /= 0 = False
  | otherwise = let
      num_repeats = div (length full_string) (length substring)
    in (concat $ take num_repeats $ repeat substring) == full_string 

getUpToHalfString :: String -> [String]
getUpToHalfString s = map (\n -> take n s) [1..half]
  where half = div (length s) 2


canBeBuiltFromRepeating :: Int -> Bool
canBeBuiltFromRepeating x = any id $ map (\sub -> canBeBuiltFromRepeatingSubstring sub s) $ getUpToHalfString s
    where s::String = show x

getInvalid :: (Int, Int) -> [Int]
getInvalid (start, stop) = filter (\y -> not $ checkValid y) $ generateList (start, stop)


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parse :: String -> [(Int, Int)]
parse contents = map numFromRange $ wordsWhen (\c -> c==',') $ contents


solution1 :: String -> Int
solution1 contents = sum $ concat $ map getInvalid $ parse contents

solution2 :: String -> Int
solution2 contents = sum $ concat $ map getInvalid' $ parse contents
  where getInvalid' (start, stop) = filter (\y -> canBeBuiltFromRepeating y) $ generateList (start, stop)