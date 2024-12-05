module ListTools (count, differences, filterFirst, removeAt) where
import Data.IntMap (split)

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
