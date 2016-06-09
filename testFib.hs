module TestFib where

import Tokenizer
import Types
import Grammar
import FP_ParserGen         -- Touching this file leaves you at your own devices
import Debug.Trace
import System.FilePath

main = do 
    a <- readFile "test/fib.txt"
    prpr $ 
        parse grammar Program $ 
        toTokenList $ 
        tokenizer a
