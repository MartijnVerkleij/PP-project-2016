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

--import BasicFunctions
--import HardwareTypes
import Sprockell


-- ==================== Lists of Test Files and Conversions ====================
testSingle :: [String]
testSingle =    [ "cyclic_recursion"    -- Run this one with more local memory
                , "deep_expression"     
                , "division_zero"       -- Deprecated
                , "fib"                 -- Run this one with more local memory
                , "if"
                , "ifelse"
                , "infinite_busy_loop"
                , "infinite_loop"
                , "nested_procedures"
                , "recursion"
                , "while"
                --, "call_by_reference"
                , "blocks"
                , "stuff"
                , "simple_proc"
                , "enum"
                ]

testMulti :: [(String, Int)]
testMulti =     [ ("banking", 3)
                , ("peterson", 3)
                , ("simple_concurrency", 3)
                , ("multiple_globals", 3)
                , ("join_test", 2)          -- Main thread must run forever
                , ("call_by_reference", 2)
                ]

testAll :: [(String, Int)]
testAll =   single ++ multi
    where
        single  = map (\x       -> ("test/" ++ x ++ ".shl",1)) testSingle
        multi   = map (\(x,y)   -> ("test/" ++ x ++ ".shl", y)) testMulti

testConversion :: String -> String
testConversion x    = "test/" ++ (alias x) ++ ".shl"


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
        | x `elem` ["simplep"]
            = "simple_proc"
        | x `elem` ["simplec", "simple_c", "concurrency", "con"]
            = "simple_concurrency"
        | x `elem` ["global", "globals"]
            = "multiple_globals"
        | otherwise
            = x


-- ==================== Generalized Testing ====================
token :: String -> IO ()
token name = do
    a <- readFile $ testConversion name
    putStr $
        show $
        toTokenList $
        tokenizer a

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

check1 :: String -> IO ()
check1 name = do
    a <- readFile $ testConversion name
    showTree $
        astToRoseDebug $
        checker1 $
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

runit :: String -> IO ()
runit name = do
    a <- readFile $ testConversion name
    run $
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
    runWithDebugger (debuggerSimplePrint myShow) $
        replicate ((Map.fromList testAll)Map.!(testConversion name)) $
        codeGen' ((Map.fromList testAll)Map.!(testConversion name)) $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a

{- Dows not work anymore; All run functions are IO() type.
write :: String -> IO ()
write name = do
    a <- readFile $ testConversion name
    writeFile ("gen/debug_" ++ (alias name) ++ ".txt") 
            (runWithDebugger (debuggerSimplePrint myShow) $
            replicate ((Map.fromList testAll)Map.!(testConversion name)) $
            codeGen' ((Map.fromList testAll)Map.!(testConversion name)) $
            checker $
            pTreeToAst $
            parse grammar Program $
            toTokenList $
            tokenizer a)
    putStr (runWithDebugger (debuggerSimplePrint myShow) $
            replicate ((Map.fromList testAll)Map.!(testConversion name)) $
            codeGen' ((Map.fromList testAll)Map.!(testConversion name)) $
            checker $
            pTreeToAst $
            parse grammar Program $
            toTokenList $
            tokenizer a)
    
-}
-- ==================== Checker test ====================
checkChecker :: IO()
checkChecker = do
    a <- readFile "test/checker.txt"
    showTree $
        astToRoseDebug $
        checker $
        pTreeToAst $
        parse grammar Program $
        toTokenList $
        tokenizer a
