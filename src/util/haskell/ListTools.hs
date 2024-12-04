module ListTools (count) where

count :: Eq a => a -> [a] -> Int
count x xs = length (filter (== x) xs)