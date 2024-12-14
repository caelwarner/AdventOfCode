module Parse (extractNumbers) where

import Data.Char

extractNumbers :: Read a => String -> [a]
extractNumbers "" = []
extractNumbers line = read (takeWhile isDigit line'):extractNumbers (dropWhile isDigit line')
    where line' = dropWhile (not . isDigit) line