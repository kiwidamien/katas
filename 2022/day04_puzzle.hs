type ElfShift = (Integer, Integer)

-- Algos
overlap_completely :: (ElfShift, ElfShift) -> Bool
overlap_completely ((fst_start, fst_end),(snd_start, snd_end))
  | fst_start == snd_start = True
  | fst_start < snd_start = fst_end >= snd_end
  | otherwise = overlap_completely ((snd_start, snd_end), (fst_start, fst_end))

overlap_at_all :: (ElfShift, ElfShift) -> Bool
overlap_at_all ((fst_start, fst_end),(snd_start, snd_end))
  | fst_start == snd_start = True
  | fst_start < snd_start = fst_end >= snd_start
  | otherwise = overlap_at_all ((snd_start, snd_end),(fst_start, fst_end))

-- Parsing
splitAtChar :: Char -> String -> (String, String)
splitAtChar chr str = (firstItem, tail secondItem)
    where (firstItem, secondItem) = break (==chr) str

toShift :: String -> ElfShift
toShift assignment = (read start, read end)
   where (start, end) = splitAtChar '-' assignment

toShiftPair :: String -> (ElfShift, ElfShift)
toShiftPair line = (toShift first, toShift second)
    where (first, second) = splitAtChar ',' line 


-- problems
part1 = do
  contents <- readFile "day04_input.txt"
  let pairs = map (toShiftPair) $ lines contents
  let num_overlaps = length $ filter overlap_completely pairs
  return num_overlaps

part2 = do
  contents <- readFile "day04_input.txt"
  let pairs = map (toShiftPair) $ lines contents
  let num_partial_overlaps = length $ filter overlap_at_all pairs
  return num_partial_overlaps

