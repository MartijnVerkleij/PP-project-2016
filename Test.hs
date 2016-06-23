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


-- ==================== Fib ====================
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

runFib = do
    a <- readFile "test/fib.txt"
    sysTest $
        replicate 3 $
        codeGen' 3 $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a


-- ==================== Prime ====================
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
        
runPrime = do
    a <- readFile "test/prime.txt"
    sysTest $
        replicate 3 $
        codeGen' 3 $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a


-- ==================== Banking ====================
prprBanking = do 
    a <- readFile "test/banking.txt"
    prpr $ 
        parse grammar Program $ 
        toTokenList $ 
        tokenizer a

parseBanking = do
    a <- readFile "test/banking.txt"
    showTree $
        toRoseTree $
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
        checker1 $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

runBanking = do
    a <- readFile "test/banking.txt"
    sysTest $
        replicate 3 $
        codeGen' 3 $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a


-- ==================== Peterson ====================
prprPeterson = do 
    a <- readFile "test/peterson.txt"
    prpr $ 
        parse grammar Program $ 
        toTokenList $ 
        tokenizer a

parsePeterson = do
    a <- readFile "test/peterson.txt"
    showTree $
        toRoseTree $
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

runPeterson = do
    a <- readFile "test/peterson.txt"
    sysTest $
        replicate 3 $
        codeGen' 3 $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a


{--- ==================== Lock ====================
prpr = do 
    a <- readFile "test/.txt"
    prpr $ 
        parse grammar Program $ 
        toTokenList $ 
        tokenizer a

parse = do
    a <- readFile "test/.txt"
    showTree $
        toRoseTree $
        parse grammar Program $
        toTokenList $
        tokenizer a

ast = do
    a <- readFile "test/.txt"
    showTree $
        astToRose $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

check = do
    a <- readFile "test/.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

run = do
    a <- readFile "test/.txt"
    sysTest $
        replicate 3 $
        codeGen' 3 $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a-}


-- ==================== Checker checks ====================
checkChecker = do
    a <- readFile "test/checker.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a