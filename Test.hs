module Test where

import Tokenizer
import Types
import Grammar
import FP_ParserGen         -- Touching this file leaves you at your own devices
import FPPrac.Trees
import Debug.Trace
import System.FilePath
import ASTBuilder
import Checker

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
        checker$
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

checkFib = do
    a <- readFile "test/fib.txt"
    showTree $
        astToRoseDebug $
        checker $
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
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

checkPrime = do
    a <- readFile "test/prime.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

astBanking = do
    a <- readFile "test/banking.txt"
    showTree $
        astToRose $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

checkBanking = do
    a <- readFile "test/banking.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

astPeterson = do
    a <- readFile "test/peterson.txt"
    showTree $
        astToRose $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

checkPeterson = do
    a <- readFile "test/peterson.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

checkPrint = do
    a <- readFile "test/print.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a