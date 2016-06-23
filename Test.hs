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


-- ==================== If ====================
prprIf = do 
    a <- readFile "test/if.txt"
    prpr $ 
        parse grammar Program $ 
        toTokenList $ 
        tokenizer a

parseIf = do
    a <- readFile "test/if.txt"
    showTree $
        toRoseTree $
        parse grammar Program $
        toTokenList $
        tokenizer a

astIf = do
    a <- readFile "test/if.txt"
    showTree $
        astToRose $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

checkIf = do
    a <- readFile "test/if.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

genIf = do
    a <- readFile "test/if.txt"
    putStr $ 
        show $
        codeGen' 3 $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

runIf = do
    a <- readFile "test/if.txt"
    sysTest $
        replicate 1 $
        codeGen' 1 $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a


-- ==================== While ====================
prprWhile = do 
    a <- readFile "test/while.txt"
    prpr $ 
        parse grammar Program $ 
        toTokenList $ 
        tokenizer a

parseWhile = do
    a <- readFile "test/while.txt"
    showTree $
        toRoseTree $
        parse grammar Program $
        toTokenList $
        tokenizer a

astWhile = do
    a <- readFile "test/while.txt"
    showTree $
        astToRose $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

checkWhile = do
    a <- readFile "test/while.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

runWhile = do
    a <- readFile "test/while.txt"
    sysTest $
        replicate 3 $
        codeGen' 3 $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

-- ==================== Nested Procedure Usage ====================
prprNested = do 
    a <- readFile "test/nested_procedures.txt"
    prpr $ 
        parse grammar Program $ 
        toTokenList $ 
        tokenizer a

parseNested = do
    a <- readFile "test/nested_procedures.txt"
    showTree $
        toRoseTree $
        parse grammar Program $
        toTokenList $
        tokenizer a

astNested = do
    a <- readFile "test/nested_procedures.txt"
    showTree $
        astToRose $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

checkNested = do
    a <- readFile "test/nested_procedures.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

runNested = do
    a <- readFile "test/nested_procedures.txt"
    sysTest $
        replicate 3 $
        codeGen' 3 $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a


-- ==================== Recursion ====================
prprRecursion = do 
    a <- readFile "test/recursion.txt"
    prpr $ 
        parse grammar Program $ 
        toTokenList $ 
        tokenizer a

parseRecursion = do
    a <- readFile "test/recursion.txt"
    showTree $
        toRoseTree $
        parse grammar Program $
        toTokenList $
        tokenizer a

astRecursion = do
    a <- readFile "test/recursion.txt"
    showTree $
        astToRose $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

checkRecursion = do
    a <- readFile "test/recursion.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

runRecursion = do
    a <- readFile "test/recursion.txt"
    sysTest $
        replicate 3 $
        codeGen' 3 $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a


-- ==================== If Else ====================
prprIfElse = do 
    a <- readFile "test/ifelse.txt"
    prpr $ 
        parse grammar Program $ 
        toTokenList $ 
        tokenizer a

parseIfElse = do
    a <- readFile "test/ifelse.txt"
    showTree $
        toRoseTree $
        parse grammar Program $
        toTokenList $
        tokenizer a

astIfElse = do
    a <- readFile "test/ifelse.txt"
    showTree $
        astToRose $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

checkIfElse = do
    a <- readFile "test/ifelse.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

runIfElse = do
    a <- readFile "test/ifelse.txt"
    sysTest $
        replicate 3 $
        codeGen' 3 $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a


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
