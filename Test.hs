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
import CodeGen

import BasicFunctions
import HardwareTypes
import Sprockell
import System
import Simulation

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

astFibDebug = do
    a <- readFile "test/fib.txt"
    showTree $
        astToRoseDebug $
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
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

astPrimeDebug = do
    a <- readFile "test/prime.txt"
    showTree $
        astToRoseDebug $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

checkPrime = do
    a <- readFile "test/prime.txt"
    showTree $
        astToRoseDebug $
        checker1 $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a
        
        
genPrime = do
    a <- readFile "test/prime.txt"
    putStr $ show $
        codeGen' 3 $
        checker1 $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a
        
runPrime = do
    a <- readFile "test/prime.txt"
    sysTest $
        replicate 3 $
        codeGen' 3 $
        checker1 $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

tokenizePrime = show $ toTokenList $  tokenizer "procedure fib(int i, int res) {    if ((i < 3)) {        res = i;    } else {        int a;        int b;        fib((i-1), a);        fib((i-2), b);        res = (a + b);    }}"
