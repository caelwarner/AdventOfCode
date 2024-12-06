module Main (main) where

import Read
import Run
import Data.List (transpose, intercalate)
import ListTools
import Text.Megaparsec
import Data.Void
import Control.Monad
import Text.Megaparsec.Char
import Data.Either.Extra

type Parser = Parsec Void String

data XMAS = XMAS deriving Show

diag :: [String] -> [String]
diag xs = concat [transpose $ diag' $ take 4 $ drop i xs | i <- [0..length xs - 4]]
    where diag' = zipWith drop [0..]

wordSearch :: [String] -> Int
wordSearch matrix = negativeSlope + positiveSlope + row + col
    where
        row = sum $ fmap (\l -> countInfix "XMAS" l + countInfix "SAMX" l) matrix
        col = sum $ (\l -> countInfix "XMAS" l + countInfix "SAMX" l) <$> transpose matrix
        negativeSlope = length $ filter (\l -> l == "XMAS" || l == "SAMX") $ diag matrix
        positiveSlope = length $ filter (\l -> l == "XMAS" || l == "SAMX") $ diag $ reverse matrix

parseXMAS :: Int -> Parser XMAS
parseXMAS width = do
    void $ char 'S'
    void $ lookAhead $ do
        void $ anySingleBut '\n'
        void $ char 'S'
        replicateM_ (width - 2) anySingle
        void $ char 'A'
        replicateM_ (width - 2) anySingle
        void $ char 'M'
        void $ anySingleBut '\n'
        void $ char 'M'
    return XMAS

parseWordSearch :: Int -> Parser [XMAS]
parseWordSearch width = many $ try $ skipManyTill anySingle $ try $ parseXMAS width

wordSearchCross :: [String] -> Int
wordSearchCross matrix = countXMAS width (intercalate "\n" matrix)
                       + countXMAS height (intercalate "\n" $ transpose matrix)
                       + countXMAS width (intercalate "\n" $ reverse matrix)
                       + countXMAS height (intercalate "\n" $ reverse $ transpose matrix)
    where
        width = 1 + length (head matrix)
        height = 1 + length matrix
        countXMAS w input = length $ fromRight [] $ runParser (parseWordSearch w) "input.txt" input

main :: IO ()
main = do
    contents <- inputAsStrList (Year 2024) (Day 4)

    run PartOne (wordSearch contents)
    run PartTwo (wordSearchCross contents)
