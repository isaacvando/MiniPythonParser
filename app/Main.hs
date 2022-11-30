module Main (main) where

import ParsePython
import System.Directory
import Data.String.Interpolate (i)

main :: IO ()
main = do
    contents <- getContents
    case parsePython contents of
        Left parseError -> putStrLn parseError
        Right result -> do
            putStrLn $ "Result:\n" ++ show result
            createDirectoryIfMissing False "parseTree"
            writeFile "parseTree/tree.tex" (getTree result)

-- TODO: use Text.LaTeX instead
getTree :: Content -> String
getTree (Start xs) = foldr (\x acc -> case x of '_' -> '\\':'_':acc; y -> y:acc) "" (start xs)
    where start ys = "\\documentclass[]{article}\n\\usepackage{tikz-qtree}\n\\usepackage[T1]{fontenc}\n\\usepackage{incgraph}\n\n\\begin{document}\n"
            ++ concat (map (\y -> "    \\begin{inctext} \\Tree " ++ getTree y ++ " \\end{inctext} \n") ys) ++ "\\end{document}\n"
getTree (Var st) = [i|[.{Var #{st}} ]|]
getTree (Num st) = [i|[.{Num #{st}} ]|]
getTree (Bool st) = [i|[.{Bool #{st}} ]|]
getTree (Arith op left right) = let normed = case op of '%' -> "\\%"; x -> [x] in
    [i|[.{Arith #{normed}} #{getTree left} #{getTree right} ]|]
getTree (Assign op left right) = [i|[.{Assign #{op}}  #{getTree left} #{getTree right} ]|]
getTree (Cond op left right) = [i|[.{Conditional #{op}}  #{getTree left} #{getTree right} ]|]
getTree (IfStatement xs) = [i|[.{If Statement} #{concat (map getTree xs)}]|]
getTree (If cond body) = [i|[.If [.Condition #{getTree cond} ] [.Body #{concat (map getTree body)}] ] |]
getTree (Elif cond body) = [i|[.{Else If} [.Condition #{getTree cond} ] [.Body #{concat (map getTree body)}] ] |]
getTree (Else body) = [i|[.Else [.Body #{concat (map getTree body)}] ] |]
getTree (For item collection body) = [i|[.For [.Item #{getTree item} ] [.Collection #{getTree collection} ] [.Body #{concat (map getTree body)}] ] |]
getTree (While cond body) = [i|[.For [.Condition #{getTree cond} ] [.Body #{concat (map getTree body)}] ] |]
getTree (Call name args) = [i|[.{Function Call} [.Name #{name} ] [.Args #{concat (map getTree args)} ] ] |]
getTree (Kwarg name arg) = [i|[.{Keyword Arg} [.Name #{name} ] [.Arg #{getTree arg} ] ] |]
getTree (Function name args body) = [i|[.Function [.Name #{name} ] [.Arguments #{unwords args} ] [.Body #{concat (map getTree body)}] ] |]
getTree (Return val) = [i|[.Return #{getTree val} ] |]
