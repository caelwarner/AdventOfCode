module ListTools (count, differences, filterFirst, removeAt, rotateLeft, countInfix) where
import Data.List

count :: Eq a => a -> [a] -> Int
count x xs = length (filter (== x) xs)

differences :: [Int] -> [Int]
differences xs = zipWith (-) (tail xs) xs

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst f (h:t)
    | f h = t
    | otherwise = h:filterFirst f t

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (_:t) = t
removeAt i (h:t) = h:removeAt (i - 1) t

rotateLeft :: Int -> [a] -> [a]
rotateLeft _ [] = []
rotateLeft 0 xs = xs
rotateLeft n xs = take (length xs) $ drop n $ cycle xs

countInfix :: Eq a => [a] -> [a] -> Int
countInfix needle haystack = length [() | t <- tails haystack, needle `isPrefixOf` t]
