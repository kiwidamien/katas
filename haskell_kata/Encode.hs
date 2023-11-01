import Data.Char


-- ['a'..'z'] is all lowercase letters
translateChar :: Char -> Char
translateChar a 
    | a < 'a' = a 
    | a > 'z' = a
    | otherwise = chr $ ord 'z' - ord a + ord 'a'

stripNonAlpha :: String -> String
stripNonAlpha phrase = filter isAlphaNum phrase

preprocess :: String -> String
preprocess phrase = map toLower $ stripNonAlpha phrase

decode :: String -> String
decode cipherText = map translateChar $ preprocess cipherText

encode :: String -> String
encode plainText 
  | (length(p)) <=5 = map translateChar p 
  | otherwise = (take 5 $ map translateChar p) ++ " " ++ (encode $ drop 5 p)
  where p = preprocess plainText
