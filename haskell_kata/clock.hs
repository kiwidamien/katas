data Clock = Clock {
    clockHour :: Int,
    clockMin :: Int
} deriving (Eq, Show)

zeroPad :: Int -> String
zeroPad n = (if n< 10 then "0" ++ (show n) else (show n))

toMin :: Clock -> Int
toMin c = mod ((clockHour c) * 60 + (clockMin c)) (60 * 24)

fromMin :: Int -> Clock
fromMin minutes = Clock (div m 60) (mod m 60)
    where m = mod minutes (60 * 24)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = fromMin $ toMin $ Clock hour min  -- catch and wrap Clock 28 00 as 4 am

toString :: Clock -> String
toString clock = (zeroPad $ clockHour clock) ++ ":" ++ (zeroPad $ clockMin clock)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min clock = fromMin $ (+) (toMin clock) (toMin $ Clock hour min)
