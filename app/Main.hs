module Main (main) where

import ParsePython
import Text.LaTeX.Base
import Text.LaTeX.Packages.Trees.Qtree

main :: IO ()
main = do
    putStrLn "input:"
    input <- getContents
    case parsePython input of
        Left parseError -> putStrLn parseError
        Right result -> print result