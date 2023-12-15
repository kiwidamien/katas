module Day15 where 

import Data.Char 
import GHC.Data.ShortText (ShortText(contents))

data Lens = Lens String Int deriving (Show)
data Op = Append | Remove deriving (Show, Eq)
type Box = [Lens]


processChar :: Char -> Int -> Int 
processChar c current = multValue
  where newValue = current + ord c
        multValue = mod (17 * newValue) 256


processString :: String -> Int 
processString s = foldl (\s c -> processChar c s) 0 s 


parse :: String -> [String]
parse "" = []
parse s = let (fst, snd) = break (==',') s in fst : (parse $ drop 1 snd)


iniState :: [Box]
iniState = replicate 256 []

decompose :: String -> (Lens, Op)
decompose s = (lens, op)
    where op = if elem '=' s then Append else Remove
          lens = case op of
            Append -> let (label,_:snd) = break (=='=') s in Lens label (read snd)
            Remove -> Lens (init s) (-1)


boxNum :: Lens -> Int 
boxNum = processString . getLabel 

getLabel :: Lens -> String 
getLabel (Lens s _) = s

processBox :: [Box] -> String -> [Box]
processBox boxArray s 
    | op == Append = update boxArray box (changeLens lens)
    | op == Remove = update boxArray box (removeLens lens)
    where (lens, op) = decompose s
          box = boxNum lens
          update :: [Box] -> Int -> (Box -> Box) -> [Box]
          update boxes n fn = map (\(value, idx) -> if idx == n then fn value else value) $ zip boxes [0..]
          removeLens (Lens label _) box = filter (\bb -> (getLabel bb) /= label) box
          changeLens (Lens label focalLength) box = case (length $ filter (==label) $ map getLabel box) of
            0 -> box ++ [(Lens label focalLength)]
            otherwise -> map (\thisLens -> if (getLabel thisLens == label) then (Lens label focalLength) else thisLens) box


processSequence :: [String] -> [Box]
processSequence  = foldl processBox iniState 
            
focusPower :: [Box] -> Int 
focusPower boxes = sum $ concatMap (zipWith lensPower [1..]) boxes
  where lensPower :: Int -> Lens -> Int 
        lensPower slot lens = (boxNum lens + 1) * (getFocalLength lens) * slot
        getFocalLength (Lens _ f) = f 


part1 :: String -> IO ()
part1 filename = do 
    contents <- parse <$> readFile filename 
    let results = map processString contents
    print (sum results)

part2 :: String -> IO ()
part2 filename = do 
    contents <- parse <$> readFile filename
    let results = focusPower $ processSequence contents
    print results