module ParsePython where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad (void)

type Parser = Parsec Void String

data Content = Start [Content] | Arith Char [Content] | Num String | Assign String [Content] | Var String
    deriving (Show, Eq)

parsePython :: String -> Either String Content
parsePython input = case parse pythonFile "" input of
    Left bundle ->  Left $ errorBundlePretty bundle
    Right result -> Right result

pythonFile :: Parser Content
pythonFile = do
    -- s <- many $ fmap Just statement <|> fmap (\_ -> Nothing) (hspace <* sep)
    -- eof
    -- return $ Start (foldr (\x acc -> case x of Just x -> x:acc; Nothing -> acc) [] s)
    s <- many $ many (hspace *> eol) *> statement
    many (hspace *> eol)
    eof
    return $ Start s

statement :: Parser Content
statement = try (arithmetic <* sep) <|> (assignment <* sep) <?> "statement"

assignment :: Parser Content
assignment = do
    v <- variable <* hspace
    op <- assignOperator <* hspace
    ex <- try arithmetic <|> variable
    return $ Assign op [v, ex]

variable :: Parser Content
variable = do
    first <- char '_' <|> letterChar
    rest <- many $ alphaNumChar <|> char '_'
    let name = first:rest
    if name `elem` reserved then fail (name ++ " is a reserved word") else return $ Var name

arithmetic :: Parser Content
arithmetic = (try (do 
    a1 <- ((char '(' *> arithmetic <* char ')') <|> number <|> variable) <* hspace
    op <- arithOperator <* hspace
    a2 <- arithmetic
    return $ Arith op [a1, a2]))
    <|> (char '(' *> arithmetic <* char ')')
    <|> number
    <|> variable

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

sep :: Parser ()
sep = hspace *> (void eol <|> eof)