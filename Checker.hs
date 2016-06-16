module Checker where

import FPPrac.Trees
import Types

-- First pass
checker1 :: AST -> CheckTree




checker1 self@(ASTProgram asts) 
    = (self, functions, [], [])
    where
        functions   = concat (map (\(ast, f, g, v) -> f) (map checkTree asts))
checker1 self@(ASTProc str asts ast) 
    = (self, [(str,types)], [], [])
    where
        types   = map (getType) asts
        getType :: AST -> Alphabet
        getType (ASTArg (ASTType typeStr) _)    = getAlphabet typeStr
        getType _ 
            = error "Shouldn't reach this: Checker.checker1"
checker1 ast
    = ast [] [] []

getAlphabet :: String -> Alphabet
getAlphabet "int"   = IntType
getAlphabet "bool"  = BoolType
getAlphabet _       = error "Type not recognised."