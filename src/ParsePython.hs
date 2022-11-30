module ParsePython (parsePython, Content(..)) where

-- TODO: refactor to use Text.Megaparsec.Char.Lexer
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad (void, when)
import Data.Char (isSpace)

type Parser = Parsec Void String

data Content = Start [Content]
    | Var String
    | Num String
    | Bool String
    | Arith Char Content Content
    | Assign String Content Content
    | Cond String Content Content
    | IfStatement [Content]
    | If Content [Content]
    | Elif Content [Content]
    | Else [Content]
    | For Content Content [Content]
    | While Content [Content]
    | Call String [Content]
    | Kwarg String Content
    | Function String [String] [Content]
    | Return Content
    deriving (Show, Eq)

parsePython :: String -> Either String Content
parsePython input = case parse pythonFile "" input of
    Left bundle ->  Left $ errorBundlePretty bundle
    Right result -> Right result

pythonFile :: Parser Content
pythonFile = do
    inp <- getInput
    setInput $ (unlines . filter (not . all isSpace) . map (takeWhile (/= '#')) . lines) inp -- filter out vertical space and comments
    Start <$> (many (statement 0) <* space <* eof)

statement :: Int -> Parser Content
statement i = string (getSpaces i) *> 
    (try (arithmetic <* sep)
    <|> try (assignment <* sep)
    <|> try (conditional <* sep)
    <|> try (call <* sep)
    <|> try (ifStatement i)
    <|> try (forLoop i)
    <|> try (whileLoop i)
    <|> function i
    <?> "statement")

call :: Parser Content
call = try (do
        name <- identifier <* string "()"
        return $ Call name [])
    <|> (do
        name <- identifier
        arg1 <- char '(' *> argument
        args <- many (hspace *> char ',' *> hspace *> argument) <* char ')'
        return $ Call name (arg1:args))

function :: Int -> Parser Content
function i = try (do
        name <- string "def" *> hspace *> identifier <* string "()" <* hspace <* char ':' <* hspace <* eol
        body <- getBody
        ret <- getReturn
        when (ret == Nothing && null body) (fail "empty function body")
        return $ Function name [] (case ret of Nothing -> body; Just x -> body ++ [Return x]))
    <|> (do
        name <- string "def" *> hspace *> identifier <* char '(' <* hspace
        arg1 <- identifier
        args <- many (hspace *> char ',' *> hspace *> identifier) <* hspace <* char ')' <* hspace <* char ':' <* eol
        body <- getBody
        ret <- getReturn
        when (ret == Nothing && null body) (fail "empty function body")
        return $ Function name (arg1:args) (case ret of Nothing -> body; Just x -> body ++ [Return x]))
            where
                getBody = many $ try (statement (i + 1))
                getReturn = optional (string (getSpaces (i + 1) ++ "return") *> hspace *> (try call <|> try arithmetic <|> conditional <?> "return value") <* sep)

argument :: Parser Content
argument = kwarg <|> try call <|> try arithmetic <|> conditional <?> "function argument"
    where
        kwarg :: Parser Content 
        kwarg = do
            name <- try (identifier <* hspace <* char '=' <* hspace)
            arg <- try call <|> try arithmetic <|> conditional <?> "keyword function argument"
            return $ Kwarg name arg

ifStatement :: Int -> Parser Content
ifStatement i = do
    ifExp <- try (do 
        void $ string "if" *> hspace
        cond <- getCond
        stmts <- getBlock
        return $ If cond stmts)
    elifExps <- many (try $ do
        void $ string indent *> string "elif" *> hspace -- TODO: this is a bad temp solution
        cond <- getCond
        stmts <- getBlock
        return $ Elif cond stmts)
    elseExp <- optional (try $ do
        void $ string indent *> string "else:" <* hspace <* eol -- TODO: this is a bad temp solution
        stmts <- getBlock
        return $ Else stmts)
    return $ IfStatement $ ifExp:elifExps ++ case elseExp of Nothing -> []; Just x -> [x]
        where 
            getCond = (try conditional <|> arithmetic) <* hspace <* char ':' <* hspace <* eol
            getBlock = some $ statement (i + 1)
            indent = replicate (i * 4) ' '

forLoop :: Int -> Parser Content
forLoop i = do
    item <- string "for" *> hspace *> variable <* hspace
    collection <- string "in" *> hspace *> (try call <|> variable) <* hspace <* char ':' <* hspace <* eol
    body <- some $ statement (i + 1)
    return $ For item collection body

whileLoop :: Int -> Parser Content
whileLoop i = do
    cond <- string "while" *> hspace *> (try (arithmetic <* hspace <* char ':') <|> (conditional <* hspace <* char ':')) <* hspace <* eol
    body <- some $ statement (i + 1)
    return $ While cond body

assignment :: Parser Content
assignment = do
    v <- variable <* hspace
    op <- assignOperator <* hspace
    ex <- try conditional <|> arithmetic
    return $ Assign op v ex

conditional :: Parser Content
conditional = try (char '(' *> hspace *> conditional <* hspace <* char ')')
    <|> try (do
        p1 <- (try arithmetic <|> bool) <* hspace
        op <- condOperator <* hspace
        p2 <- try arithmetic <|> bool
        return $ Cond op p1 p2)
    <|> bool <* notFollowedBy alphaNumChar -- this ensures that a variable name like Truee is parsed correctly

arithmetic :: Parser Content
arithmetic = try (do
        a1 <- (arithWithParens <|> number <|> try call <|> variable) <* hspace
        op <- arithOperator <* hspace
        a2 <- arithmetic
        return $ Arith op a1 a2)
    <|> arithWithParens
    <|> number
    <|> try call
    <|> variable
        where arithWithParens = char '(' *> hspace *> arithmetic <* hspace <* char ')'

variable :: Parser Content
variable = Var <$> identifier

identifier :: Parser String
identifier = do
    first <- char '_' <|> letterChar
    rest <- many $ alphaNumChar <|> char '_'
    let name = first:rest
    if name `elem` reserved then fail (name ++ " is a reserved word") else return name

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
reserved = ["if", "else", "while", "for", "in", "or", "and", "True", "False", "return"]

sep :: Parser ()
sep = hspace *> (void eol <|> eof)

getSpaces :: Int -> String
getSpaces i = replicate (i * 4) ' '
