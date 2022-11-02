module ParsePython where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

data Python = Node Content [Python] deriving (Show, Eq)

data Content = Start | Arithmetic String | Assignment String | Number String deriving (Show, Eq)

parsePython :: String -> Either String Python
parsePython input = case parse pythonFile "" input of
    Left bundle ->  Left $ errorBundlePretty bundle
    Right result -> Right result

pythonFile :: Parser Python
pythonFile = do
    s <- many statement
    return $ Node Start s

statement :: Parser Python
statement = do
    s <- arithmetic
    return s 

arithmetic :: Parser Python
arithmetic = do
    n1 <- number <* hspace
    op <- arithOperator <* hspace
    n2 <- number <* hspace
    return $ op [n1, n2]

number :: Parser Python
number = do
    start <- some digitChar
    return $ Node (Number start) []

arithOperator :: Parser Python
arithOperator = do
    op <- oneOf "+-/*%"
    return $ Node (Arithmetic [op]) []

