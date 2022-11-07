module Main (main) where

import ParsePython

main :: IO ()
main = do
    input <- getContents
    case parsePython input of
        Left parseError -> putStrLn parseError
        Right result -> putStrLn $ "Result: " ++ show result