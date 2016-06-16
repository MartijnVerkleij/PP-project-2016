module Checker where

import FPPrac.Trees
import Types

-- First pass
{-checker1 :: AST -> CheckTree


checker1 (ASTProgram asts) 
    = CheckTree ASTProgram [functions] [] []
        where
            functions   = $ map checkTree asts
checker1 (ASTProc str asts ast) 
    = 
checker1 (ASTArg ast1 ast2) 
    = 
checker1 ast
    = ast [] [] []-}

checker2 :: CheckTree -> CheckTree

checker2 (self@(ASTProgram asts), functions, globals, variables) 
    = tail $ map checker2 $ map (\x -> (x, functions, globals, variables)) asts
{-checker2 (self@(ASTProc str asts ast), functions, globals, variables) = 
checker2 (self@(ASTArg ast1 ast2), functions, globals, variables) = 
checker2 (self@(ASTBlock asts), functions, globals, variables) =
checker2 (self@(ASTDecl global {-global :: Bool-} type ast (Nothing)), functions, globals, variables)
    | global    =
    | otherwise =
checker2 (self@(ASTDecl global {-global :: Bool-} type ast1 (Just ast2)), functions, globals, variables)
    | global    =
    | otherwise =
checker2 (self@(ASTIf ast1 ast2 (Nothing)), functions, globals, variables) = 
checker2 (self@(ASTIf ast1 ast2 (Just ast3)), functions, globals, variables) = 
checker2 (self@(ASTWhile ast ast), functions, globals, variables) = 
checker2 (self@(ASTFork str asts), functions, globals, variables) = 
checker2 (self@(ASTJoin), functions, globals, variables) = 
checker2 (self@(ASTCall str asts), functions, globals, variables) = 
checker2 (self@(ASTAss ast1 ast2), functions, globals, variables) = 
checker2 (self@(ASTVar str), functions, globals, variables) = 
checker2 (self@(ASTInt str), functions, globals, variables) = 
checker2 (self@(ASTBool str), functions, globals, variables) = 
checker2 (self@(ASTType str), functions, globals, variables) = 
checker2 (self@(ASTOp ast1 str ast2), functions, globals, variables) = 
checker2 (self@(ASTUnary str ast), functions, globals, variables) = -}
