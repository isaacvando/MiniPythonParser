module Main (main) where

import ParsePython
import System.IO
import System.Directory
import Data.String.Interpolate (i)
import Data.List (concat)

main :: IO ()
main = do
    contents <- getContents
    case parsePython contents of
        Left parseError -> putStrLn parseError
        Right result -> do
            putStrLn $ "\nResult:\n" ++ show result
            createDirectoryIfMissing False "parseTree"
            writeFile "parseTree/tree.tex" (getTree result)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

getTree :: Content -> String
getTree (Start xs) = "\\documentclass[a4paper, landscape]{article}\n\\usepackage{qtree}\n\\usepackage{geometry}\n\\geometry{margin=0.5in}\n\n\\begin{document}\n    \\Tree [.Start "
    ++ concat (map (\x -> " " ++ getTree x ++ " ") xs) ++ "]\n\\end{document}\n"
getTree (Var st) = [i|[.{Var #{st}} ]|]
getTree (Num st) = [i|[.{Num #{st}} ]|]
getTree (Bool st) = [i|[.{Bool #{st}} ]|]
getTree (Arith op left right) = let normed = case op of '%' -> "\\%"; x -> [x] in
    [i|[.{Arith #{normed}} #{getTree left} #{getTree right} ]|]
getTree (Assign op left right) = [i|[.{Assign #{op}}  #{getTree left} #{getTree right} ]|]
getTree (Cond op left right) = [i|[.{Condition #{op}}  #{getTree left} #{getTree right} ]|]
getTree (IfStatement xs) = [i|[.IfStatement #{concat (map getTree xs)}]|]
getTree (If cond body) = [i|[.If [.Condition #{getTree cond} ] [.Body #{concat (map getTree body)}] ] |]
getTree (Elif cond body) = [i|[.{Else If} [.Condition #{getTree cond} ] [.Body #{concat (map getTree body)}] ] |]
getTree (Else body) = [i|[.Else [.Body #{concat (map getTree body)}] ] |]
getTree (For item collection body) = [i|[.For [.Item #{getTree item} ] [.Collection #{getTree collection} ] [.Body #{concat (map getTree body)}] ] |]
getTree (While cond body) = [i|[.For [.Condition #{getTree cond} ] [.Body #{concat (map getTree body)}] ] |]
getTree (Call name args) = [i|[.{Function Call} [.Name #{name} ] [.Args #{concat (map getTree args)} ] ] |]
getTree (Kwarg name arg) = [i|[.{Keyword Arg} [.Name #{name} ] [.Arg #{getTree arg} ] ] |]
getTree (Function name args body) = [i|[.Function [.Name #{name} ] [.Arguments #{unwords args} ] [.Body #{concat (map getTree body)}] ] |]
getTree (Return val) = [i|[.Return #{getTree val} ] |]



-- data Content = Start [Content]
--     | Var String
--     | Num String
--     | Bool String
--     | Arith Char Content Content
--     | Assign String Content Content
--     | Cond String Content Content
--     | IfStatement [Content]
--     | If Content [Content]
--     | Elif Content [Content]
--     | Else [Content]
--     | For Content Content [Content]
--     | While Content [Content]
--     | Call String [Content]
--     | Kwarg String Content
--     | Function String [String] [Content]
--     | Return Content
--     deriving (Show, Eq)