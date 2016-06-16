module Checker where

import FPPrac.Trees
import Types

-- First pass
checker1 :: AST -> CheckTree




checker1 (ASTProgram asts) 
    = CheckTree ASTProgram [functions] [] []
        where
            functions   = $ map checkTree asts
checker1 (ASTProc str asts ast) 
    = 
checker1 (ASTArg ast1 ast2) 
    = 
checker1 ast
    = ast [] [] []
