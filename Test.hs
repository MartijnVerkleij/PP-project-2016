module Test where

import Tokenizer
import Types
import Grammar
import FP_ParserGen         -- Touching this file leaves you at your own devices
import FPPrac.Trees
import Debug.Trace
import qualified Data.Map.Strict as Map
import System.FilePath
import ASTBuilder
import Checker
import CodeGen

import BasicFunctions
import HardwareTypes
import Sprockell
import System
import Simulation


-- ==================== Lists of Test Files and Conversions ====================
testSingle :: [String]
testSingle =    [ "cyclic_recursion"
                , "deep_expression"
                , "division_zero"
                , "fib"
                , "if"
                , "ifelse"
                , "infinite_busy_loop"
                , "infinite_loop"
                , "nested_procedures"
                , "prime"
                , "recursion"
                , "while"
                , "call_by_reference"
                , "blocks"
                ]

testMulti :: [(String, Int)]
testMulti =     [ ("banking", 4)
                , ("peterson", 3)
                ]

testAll :: [(String, Int)]
testAll =   single ++ multi
    where
        single  = map (\x       -> ("test/" ++ x ++ ".txt",1)) testSingle
        multi   = map (\(x,y)   -> ("test/" ++ x ++ ".txt", y)) testMulti

testConversion :: String -> String
testConversion x    = "test/" ++ (alias x) ++ ".shl"
    where
        alias :: String -> String
        alias x | x `elem` ["cyclic", "cycl"]
                    = "cyclic_recursion"
                | x `elem` ["deep", "expression"]
                    = "deep_expression"
                | x `elem` ["else"]
                    = "ifelse"
                | x `elem` ["inf", "loop", "infinite"]
                    = "infinite_loop"
                | x `elem` ["infbusy", "busy", "busyloop", "busy_loop", "infinite_busy", "infinitebusy"] 
                    = "infinite_busy_loop"
                | x `elem` ["nest", "nested", "proc", "procedures"]
                    = "nested_procedures"
                | x `elem` ["rec"]
                    = "recursion"
                | x `elem` ["call", "callby", "call_by", "ref", "reference"]
                    = "call_by_reference"
                | x `elem` ["block"]
                    = "blocks"
                | otherwise                                         
                    = x


-- ==================== Generalized Testing ====================
pr :: String -> IO ()
pr name = do
    a <- readFile $ testConversion name
    prpr $
        parse grammar Program $
        toTokenList $
        tokenizer a

par :: String -> IO ()
par name = do
    a <- readFile $ testConversion name
    showTree $
        toRoseTree $
        parse grammar Program $
        toTokenList $
        tokenizer a

ast :: String -> IO ()
ast name = do
    a <- readFile $ testConversion name
    showTree $
        astToRose $
        checker$
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

check :: String -> IO ()
check name = do
    a <- readFile $ testConversion name
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

gen :: String -> IO ()
gen name = do
    a <- readFile $ testConversion name
    putStr $
        sprILprpr $
        codeGen' ((Map.fromList testAll)Map.!(testConversion name)) $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

run :: String -> IO ()
run name = do
    a <- readFile $ testConversion name
    sysTest $
        replicate ((Map.fromList testAll)Map.!(testConversion name)) $
        codeGen' ((Map.fromList testAll)Map.!(testConversion name)) $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

debug :: String -> IO ()
debug name = do
    a <- readFile $ testConversion name
    runTest $
        replicate ((Map.fromList testAll)Map.!(testConversion name)) $
        codeGen' ((Map.fromList testAll)Map.!(testConversion name)) $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

write :: String -> IO ()
write name = do
    a <- readFile $ testConversion name
    writeFile ("gen/debug_" ++ name ++ ".txt") 
            (runTestStr $
            replicate ((Map.fromList testAll)Map.!(testConversion name)) $
            codeGen' ((Map.fromList testAll)Map.!(testConversion name)) $
            checker $
            pTreeToAst $
            parse grammar Program $
            toTokenList $
            tokenizer a)
    putStr (runTestStr $
            replicate ((Map.fromList testAll)Map.!(testConversion name)) $
            codeGen' ((Map.fromList testAll)Map.!(testConversion name)) $
            checker $
            pTreeToAst $
            parse grammar Program $
            toTokenList $
            tokenizer a)
    

-- ==================== Checker test ====================
checkChecker = do
    a <- readFile "test/checker.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a
