module ParsePython where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

-- data Python = Node Content [Python] deriving (Show, Eq)

data Content = Start [Content] | Arith Char [Content] | Num String | Assignment String [Content] deriving (Show, Eq)

parsePython :: String -> Either String Content
parsePython input = case parse pythonFile "" input of
    Left bundle ->  Left $ errorBundlePretty bundle
    Right result -> Right result

pythonFile :: Parser Content
pythonFile = do
    s <- statement
    return $ Start [s]

statement :: Parser Content
statement = do
    s <- arithmetic
    return s 

arithmetic :: Parser Content
arithmetic = (try (do 
    a1 <- ((char '(' *> arithmetic <* char ')') <|> number) <* hspace
    op <- arithOperator <* hspace
    a2 <- arithmetic
    return $ Arith op [a1, a2]))
    <|> (char '(' *> arithmetic <* char ')')
    <|> number

number :: Parser Content
number = do
    sign <- string "-" <|> string ""
    start <- some digitChar
    end <- char '.' *> some digitChar <|> string ""
    return $ Num $ sign++start++(if null end then "" else '.':end)

arithOperator :: Parser Char
arithOperator = oneOf "+-/*%"


-- arithmetic :: Parser Python
-- arithmetic = (char '(' *> arithmetic <* char ')') 
--     <|> try (do
--     a1 <- number <* hspace
--     op <- arithOperator <* hspace
--     a2 <- arithmetic
--     return $ Node (Arithmetic op) [a1, a2])
--     <|> try (do
--     a1 <- arithmetic <* hspace
--     op <- arithOperator <* hspace
--     a2 <- number
--     return $ Node (Arithmetic op) [a1, a2])
--     -- <|> try (do
--     -- a1 <- arithmetic <* hspace
--     -- op <- arithOperator <* hspace
--     -- a2 <- arithmetic
--     -- return $ Node (Arithmetic op) [a1, a2])
--     <|> number

-- arithmetic :: Parser Content
-- arithmetic = do
--     first <- term

-- term :: Parser Content
-- term = (char '(' *> arithmetic <* char ')')
--     <|> (do
--             n <- number
--             op <- arithOperator
--             a <- arithmetic
--             return $ Node (Arithmetic op) [n, a])



-- expr, term :: Parser AExp
-- expr = do
--     n <- term
--     rest <- optional $ tok "*" *> expr
--     return $ maybe n (Mult n) rest
-- term = N <$> numParser
--     <|> V <$> strParser
--     <|> parenthesised expr

-- parenthesised = between (char '(') (char ')')

