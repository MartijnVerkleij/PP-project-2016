module Main where

import Tokenizer
import Types
import Grammar
import FP_ParserGen         -- Touching this file leaves you at your own devices
import ASTBuilder
import Checker
import CodeGen
import Simulation
import System.FilePath

main :: IO()
main = do
    putStrLn "What file do you want to run? Please provide the relative path excluding the extension."
    fileName <- getLine :: IO String
    putStrLn "How many Sprockells do you want to use to run this file?"
    sprockells <- getLine :: IO String
    putStrLn $ "Running: " ++ fileName ++ ".shl"
    putStrLn $ "On " ++ sprockells ++ " Sprockells"

    file <- readFile $ fileName ++ ".shl"
    sysTest $
        replicate (read sprockells :: Int) $
        codeGen' (read sprockells :: Int) $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer file
        