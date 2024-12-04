module Read (Year(..), Day(..), inputAsStr, inputAsStrList) where

newtype Year = Year Int
instance Show Year where
    show (Year 2024) = "twentytwentyfour"
    show _ = error "Unsupported year"

newtype Day = Day Int
instance Show Day where
    show (Day 1) = "one"
    show (Day 2) = "two"
    show (Day 3) = "three"
    show (Day 4) = "four"
    show (Day 5) = "five"
    show (Day 6) = "six"
    show (Day 7) = "seven"
    show (Day 8) = "eight"
    show (Day 9) = "nine"
    show (Day 10) = "ten"
    show (Day 11) = "eleven"
    show (Day 12) = "twelve"
    show (Day 13) = "thirteen"
    show (Day 14) = "fourteen"
    show (Day 15) = "fifteen"
    show (Day 16) = "sixteen"
    show (Day 17) = "seventeen"
    show (Day 18) = "eighteen"
    show (Day 19) = "nineteen"
    show (Day 20) = "twenty"
    show (Day 21) = "twentyone"
    show (Day 22) = "twentytwo"
    show (Day 23) = "twentythree"
    show (Day 24) = "twentyfour"
    show (Day 25) = "twentyfive"

    show _ = error "Unsupported day"

getFilePath :: Year -> Day -> String
getFilePath year day = "src/" ++ show year ++ "/" ++ show day ++ "/input.txt"

inputAsStr :: Year -> Day -> IO String
inputAsStr year day = readFile $ getFilePath year day

inputAsStrList :: Year -> Day -> IO [String]
inputAsStrList year day = lines <$> readFile (getFilePath year day)