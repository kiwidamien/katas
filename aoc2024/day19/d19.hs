import Parse 
import qualified Data.Map as M 


tokens = parseTokens "r, wr, b, g, bwu, rb, gb, br"
examples = [
    "brwrr",
    "bggr",
    "gbbr",
    "rrbgbr",
    "ubwu",
    "bwurrg",
    "brgr",
    "bbrgwb"]

countWays :: [Token] -> M.Map String Int -> String -> Int 
countWays tokens cache s  = case (M.lookup s cache) of 
    Nothing -> 0
    (Just num) -> num
