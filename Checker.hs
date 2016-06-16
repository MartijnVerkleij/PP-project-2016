module Checker where

import FPPrac.Trees
import Types

-- First pass
checker1 :: AST -> CheckTree




checker1 (ASTProgram asts) = 
checker1 (ASTProc str asts ast) =
checker1 (ASTArg ast1 ast2) = 
checker1 (ASTBlock asts) =
checker1 (ASTDecl global {-global :: Bool-} type ast (Nothing) 
    | global    =
    | otherwise =
checker1 (ASTDecl global {-global :: Bool-} type ast1 (Just ast2)
    | global    =
    | otherwise =
checker1 (ASTIf ast1 ast2 (Nothing))) = 
checker1 (ASTIf ast1 ast2 (Just ast3))) = 
checker1 (ASTWhile ast ast)) = 
checker1 (ASTFork str asts)) = 
checker1 (ASTJoin)) = 
checker1 (ASTCall str asts) = 
checker1 (ASTAss ast1 ast2) = 
checker1 (ASTVar str) = 
checker1 (ASTInt str) = 
checker1 (ASTBool str) = 
checker1 (ASTType str) = 
checker1 (ASTOp ast1 str ast2) = 
checker1 (ASTUnary str ast) = 
