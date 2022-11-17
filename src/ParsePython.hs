module ParsePython where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Data.Void (Void)
import Control.Monad (void)

type Parser = Parsec Void String

data Content = Start [Content]
    | Arith Char Content Content
    | Num String
    | Assign String Content Content
    | Var String
    | Cond String Content Content
    | Bool String
    | IfStatement [Content]
    | If Content [Content]
    | Elif Content [Content]
    | Else [Content]
    deriving (Show, Eq)

parsePython :: String -> Either String Content
parsePython input = case parse pythonFile "" input of
    Left bundle ->  Left $ errorBundlePretty bundle
    Right result -> Right result

pythonFile :: Parser Content
pythonFile = do
    s <- many $ try $ many (hspace *> eol) *> statement 0
    space *> eof
    return $ Start s

statement :: Int -> Parser Content
statement indent = (string (replicate (indent * 4) ' ') *> 
   ( try (arithmetic <* sep)
    <|> try (assignment <* sep)
    <|> try (conditional <* sep)
    <|> ifStatement indent
    <?> "statement"))

conditional :: Parser Content
conditional = try (char '(' *> hspace *> conditional <* hspace <* char ')')
    <|> try (do
    p1 <- (bool <|> arithmetic) <* hspace
    op <- condOperator <* hspace
    p2 <- bool <|> arithmetic
    return $ Cond op p1 p2)
    <|> bool 

ifStatement :: Int -> Parser Content
ifStatement indent = do
    ifExp <- try (do 
        void $ string "if" *> hspace
        cond <- getCond
        stmts <- getBlock
        return $ If cond stmts)
    elifExps <- many (do
        void $ string "elif" *> hspace
        cond <- getCond
        stmts <- getBlock
        return $ Elif cond stmts)
    elseExp <- optional (do
        void $ string "else:" <* hspace <* eol
        stmts <- getBlock
        return $ Else stmts)
    return $ IfStatement $ ifExp:elifExps ++ case elseExp of Nothing -> []; Just x -> [x]
        where 
            getCond = (try conditional <|> arithmetic) <* hspace <* char ':' <* hspace <* eol
            getBlock = some $ try $ statement (indent + 1)

assignment :: Parser Content
assignment = do
    v <- variable <* hspace
    op <- assignOperator <* hspace
    ex <- try arithmetic <|> variable
    return $ Assign op v ex

arithmetic :: Parser Content
arithmetic = try (do
    a1 <- (arithWithParens <|> number <|> variable) <* hspace
    op <- arithOperator <* hspace
    a2 <- arithmetic
    return $ Arith op a1 a2)
    <|> arithWithParens
    <|> number
    <|> variable
        where arithWithParens = char '(' *> hspace *> arithmetic <* hspace <* char ')'

variable :: Parser Content
variable = do
    first <- char '_' <|> letterChar
    rest <- many $ alphaNumChar <|> char '_'
    let name = first:rest
    if name `elem` reserved then fail (name ++ " is a reserved word") else return $ Var name

number :: Parser Content
number = do
    sign <- string "-" <|> string ""
    start <- some digitChar
    end <- char '.' *> some digitChar <|> string ""
    return $ Num $ sign++start++(if null end then "" else '.':end)

bool :: Parser Content
bool = fmap Bool $ string "True" <|> string "False"

arithOperator :: Parser Char
arithOperator = oneOf "+-/*%"

assignOperator :: Parser String
assignOperator = string "=" <|> string "+=" <|> string "-=" <|> string "*=" <|> string "/=" <?> "assignment operator"

condOperator :: Parser String
condOperator = try (string "<=") <|> try (string ">=") <|> string "<" <|> string ">"
                <|> string "==" <|> string "!=" <|> string "and" <|> string "or" <|> string "not"

reserved :: [String]
reserved = ["if", "else", "while", "for", "in", "or", "and", "True", "False"]

sep :: Parser ()
sep = hspace *> (void eol <|> eof)