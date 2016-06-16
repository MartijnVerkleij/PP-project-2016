module Test where

import Tokenizer
import Types
import Grammar
import FP_ParserGen         -- Touching this file leaves you at your own devices
import FPPrac.Trees
import Debug.Trace
import System.FilePath
import ASTBuilder

prprFib = do 
    a <- readFile "test/fib.txt"
    prpr $ 
        parse grammar Program $ 
        toTokenList $ 
        tokenizer a

parseFib = do
    a <- readFile "test/fib.txt"
    showTree $
        toRoseTree $
        parse grammar Program $
        toTokenList $
        tokenizer a

astFib = do
    a <- readFile "test/fib.txt"
    showTree $
        astToRose $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

prprPrime = do 
    a <- readFile "test/prime.txt"
    prpr $ 
        parse grammar Program $ 
        toTokenList $ 
        tokenizer a

parsePrime = do
    a <- readFile "test/prime.txt"
    showTree $
        toRoseTree $
        parse grammar Program $
        toTokenList $
        tokenizer a

astPrime = do
    a <- readFile "test/prime.txt"
    showTree $
        astToRose $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

tokenizePrime = show $ toTokenList $  tokenizer "procedure fib(int i, int res) {    if ((i < 3)) {        res = i;    } else {        int a;        int b;        fib((i-1), a);        fib((i-2), b);        res = (a + b);    }}"