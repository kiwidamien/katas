import Data.List (sort) 

data Line
  = HLine { left :: Int, right :: Int, y :: Int }
  | VLine { top  :: Int, bottom :: Int, x :: Int }
  deriving (Show)


commaSplit :: String -> [String]
commaSplit "" = []
commaSplit s = let
    firstPart = takeWhile (\c -> c /= ',') s
    lastPart = drop ((length firstPart) + 1) s
    in firstPart:(commaSplit lastPart)


parse :: String -> [[Int]]
parse contents = map (\line -> map read $ commaSplit line) $ lines contents

area :: [Int] -> [Int] -> Int
area xs ys = product $ map (\(c1, c2) -> abs (c1 - c2) + 1) $ zip xs ys

maxArea :: [[Int]] -> Int
maxArea verts = maximum $ map (\(v1, v2) -> area v1 v2) pairs
  where pairs = [(w1, w2) | (a, w1) <- zip [0..] verts, (b, w2) <- zip [0..] verts, a < b]

makeSegment :: [Int] -> [Int] -> Line
makeSegment p q= go (x1, y1) (x2, y2)
  where [x1, y1] = take 2 p
        [x2, y2] = take 2 q
        go (ax, ay) (bx, by)
          | ay == by = HLine {left = minimum [ax, bx], right = maximum [ax, bx], y = ay}
          | ax == bx = VLine {top = minimum [ay, by], bottom = maximum [ay, by], x = ax}

makeRectangle :: [Int] -> [Int]  -> [Line]
makeRectangle p q = [
  makeSegment [px, py] [qx, py], 
  makeSegment [qx, py] [qx, qy],
  makeSegment [qx, qy] [px, qy],
  makeSegment [px, qy] [px, py]]
  where [px, py] = take 2 p
        [qx, qy] = take 2 q


makePolygon :: [[Int]] -> [Line]
makePolygon pts = map (\(p1, p2) -> makeSegment p1 p2) $ zip looped (tail $ cycle pts)
  where looped = pts

doSegmentsCross :: Line -> Line -> Bool
doSegmentsCross (HLine {left=l1, right=r1, y=y1}) (HLine {left=l2, right=r2, y=y2}) = False
doSegmentsCross (VLine {top=t1, bottom=b1, x=x1}) (VLine {top=t2, bottom=b2, x=x2}) = False
doSegmentsCross (HLine {left=l1, right=r1, y=y1}) (VLine {top=t2, bottom=b2, x=x2})
  | x2 >= r1 = False
  | x2 <= l1 = False
  | b2 < y1 = False
  | t2 > y1 = False
  | (b2 == y1) && (x2 == r1) = False
  | (b2 == y1) && (x2 == l1) = False
  | (t2 == y1) && (x2 == r1) = False
  | (t2 == y1) && (x2 == l1) = False
  | otherwise = True
doSegmentsCross a b = doSegmentsCross b a

polygonsCross :: [Line] -> [Line] -> Bool
polygonsCross poly1 poly2 = any id $ map (uncurry doSegmentsCross) pairs
  where pairs = (,) <$> poly1 <*> poly2

--findLargest :: [[Int]] -> Int
findLargest verts = head $ filter (\(_, a, b) -> not $ polygonsCross (makeRectangle a b) poly) $ reverse $ sort $ map (\(p1, p2) -> (area p1 p2, p1, p2)) pairs
  where poly = makePolygon verts
        pairs = [(w1, w2) | (a, w1) <- zip [0..] verts, (b, w2) <- zip [0..] verts, a < b]


partOne :: String -> IO ()
partOne filename = do
  contents <- readFile filename
  let result = maxArea $ parse $ contents
  print(result)

partTwo :: String -> IO ()
partTwo filename = do
  contents <- readFile filename
  let result = findLargest $ parse $ contents
  print(result)

upperLeft = [2523,55695]
lowerRight = [52070,98440]
myRect = makeRectangle upperLeft lowerRight
poly = makePolygon <$> parse <$> readFile "input.txt"

crudeFilter :: Line -> Line -> Bool
crudeFilter (HLine {left=_, right=_, y=_}) (HLine {left=_, right=_, y=_}) = False
crudeFilter (VLine {top=_, bottom=_, x=_}) (VLine {top=_, bottom=_, x=_}) = False
crudeFilter (HLine {left=x1, right=x2, y=y}) (VLine {top=y1, bottom=y2, x=x})
  | x > x2 = False
  | x < x1 = False
  | y < y1 = False
  | y > y2 = False
  | otherwise = True
crudeFilter a b = crudeFilter b a