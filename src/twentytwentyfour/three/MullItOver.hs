module Main (main) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import Read
import Run
import Data.Either

type Parser = Parsec Void String

newtype Mul = Mul (Int, Int)
    deriving (Show)

parseNumber :: Parser Int
parseNumber = do
    n <- L.decimal
    if n >= 0 && n <= 999
        then return n
        else fail "Number must be between 1 and 3 digits"

parseMul :: Parser Mul
parseMul = do
    void (string "mul(")
    first <- parseNumber
    void (char ',')
    second <- parseNumber
    void (char ')')
    return $ Mul (first, second)

parseProgram :: Parser [Mul]
parseProgram = many $ try $ skipManyTill anySingle (try parseMul)

parseProgramAdvanced :: Parser [Mul]
parseProgramAdvanced = many $ try mulEnabled
    where 
        mulEnabled = skipManyTill anySingle (string "don't()" *> mulDisabled <|> try parseMul)
        mulDisabled = skipManyTill anySingle (string "do()" *> mulEnabled)

programResults :: Parser [Mul] -> String -> Int
programResults parser contents = sum $ fmap (\(Mul (a, b)) -> a * b) parsed
    where parsed = fromRight [] $ runParser parser "input.txt" contents

main :: IO ()
main = do
    contents <- inputAsStr (Year 2024) (Day 3)

    run PartOne (programResults parseProgram contents)
    run PartTwo (programResults parseProgramAdvanced contents)
