module ParsePython where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)
import Control.Monad.Fail -- temporary

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
    s <- try (arithmetic <* end) <|> try (assignment <* end) <|> (variable <* end)
    return s 

assignment :: Parser Content
assignment = do
    v <- variable <* hspace
    op <- assignOperator <* hspace
    exp <- try arithmetic <|> variable
    return $ Assign op [v, exp]

variable :: Parser Content
variable = do
    first <- char '_' <|> letterChar
    rest <- many $ alphaNumChar <|> char '_'
    let name = first:rest
    if name `elem` reserved then fail (name ++ " is a reserved word") else return $ Var name    -- TODO make this a good error message. 

arithmetic :: Parser Content
arithmetic = (try (do 
    a1 <- ((char '(' *> arithmetic <* char ')') <|> number <|> variable) <* hspace
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

reserved :: [String]
reserved = ["if", "else", "while", "for", "in", "or", "and"]

end :: Parser ()
end = (void eol) <|> eof