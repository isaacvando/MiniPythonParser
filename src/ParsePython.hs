module ParsePython where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

data Content = Start [Content] | Arith Char [Content] | Num String | Assign String [Content] | Var String
    deriving (Show, Eq)

parsePython :: String -> Either String Content
parsePython input = case parse pythonFile "" input of
    Left bundle ->  Left $ errorBundlePretty bundle
    Right result -> Right result

pythonFile :: Parser Content
pythonFile = do
    s <- many $ statement <* space
    return $ Start s

statement :: Parser Content
statement = do
    s <- try assignment <|> arithmetic
    return s 

assignment :: Parser Content
assignment = try (do
    v <- variable <* hspace
    op <- assignOperator <* hspace
    exp <- arithmetic
    return $ Assign op [v, exp])
    <|> variable

variable :: Parser Content
variable = do
    first <- char '_' <|> letterChar
    rest <- many $ alphaNumChar <|> char '_'
    return $ Var (first:rest)

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

assignOperator :: Parser String
assignOperator = string "=" <|> string "+=" <|> string "-=" <|> string "*=" <|> string "/=" <?> "assignment operator"
