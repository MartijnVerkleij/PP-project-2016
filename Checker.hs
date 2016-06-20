module Checker where

import FPPrac.Trees
import Types
import Data.List
import Data.Char
import qualified Data.Map.Strict as Map

-- Wrapper
checker :: AST -> AST
checker ast = {-checker2 (-}checker1 ast{-) ([],[],[[]])-}

-- First pass
checker1 :: AST -> AST
checker1 (ASTProgram asts _) 
    = foldl' function (ASTProgram [] ([],[],[[]])) asts
    where
        function :: AST -> AST -> AST
        function (ASTProgram asts checks) (ASTGlobal varType varName ass _) 
            = ASTProgram (asts ++ [global]) mergedChecks
            where
                mergedChecks    = mergeGlobal (getStr varName, varType) checks
                global          = ASTGlobal varType varName ass mergedChecks
        function (ASTProgram asts checks) (ASTProc pid args stat _)
            = ASTProgram (asts ++ [procedure]) mergedChecks
            where
                argTypes        = map getType args
                mergedChecks    = mergeFunction (pid, argTypes) checks
                procedure       = ASTProc pid args stat mergedChecks
        function (ASTProgram asts checks) ast
            = ASTProgram (asts ++ [ast]) checks

{--- Second pass
checker2 :: AST -> CheckType -> AST

checker2 (ASTProgram asts check) _ 
    = ASTProgram (foldl' function [] asts) check
    where
        function :: [AST] -> AST -> [AST]
        function xs x   = xs ++ [(checker2 x check)]
checker2 self@(ASTGlobal varType varName Nothing _) _
    = self
checker2 self@(ASTGlobal varType varName (Just expr) (f,g,_)) _
    | varType == (getStrType $ checker2 expr)   = self
    | otherwise = error "Types do not match, type: " ++ (show varType) ++ " and expression: " ++ (show expr)-}




{-    
checker2 ((ASTProc str asts ast), functions, globals, variables) = 
checker2 ((ASTArg ast1 ast2), functions, globals, variables) = 
checker2 ((ASTBlock asts), functions, globals, variables) =
checker2 ((ASTDecl global {-global :: Bool-} type ast (Nothing)), functions, globals, variables)
    | global    =
    | otherwise =
checker2 (self@(ASTDecl global {-global :: Bool-} type ast1 (Just ast2)), functions, globals, variables)
    | global    =
    | otherwise =
checker2 ((ASTIf ast1 ast2 (Nothing)) functions globals variables) = 
checker2 ((ASTIf ast1 ast2 (Just ast3)) functions globals variables) = 
checker2 ((ASTWhile ast ast) functions globals variables) = 
checker2 ((ASTFork str asts) functions globals variables) = 
checker2 ((ASTJoin) functions globals variables) = 
checker2 ((ASTCall str asts) functions globals variables) = 
checker2 ((ASTAss ast1 ast2) functions globals variables) = 
checker2 ((ASTVar str) functions globals variables) = 
checker2 ((ASTInt str) functions globals variables) = 
checker2 ((ASTBool str) functions globals variables) = 
checker2 ((ASTType str) functions globals variables) = 
checker2 ((ASTOp ast1 str ast2) functions globals variables) = 
checker2 ((ASTUnary str ast) functions globals variables) = 
-}


getStr:: AST -> String
getStr (ASTVar str _)   = str
getStr x                = error $ "Cannot get string of: " ++ (show x)

getType :: AST -> Alphabet
getType (ASTArg (ASTType typeStr _) _ _)  
    = getAlphabet typeStr
getType _ 
    = error "Shouldn't reach this. In Checker.getType" 

getStrType :: String -> Alphabet
getStrType str  | length str == 0   = error "Variable length equals zero in Checker.getStrType"
                | all (== True) $ map (isDigit) str     = IntType
                | (str == "true") || (str == "false")   = BoolType
                | otherwise         = error $ "invalid type in Checker.getStrType, var: " ++ str

mergeFunction :: FunctionType -> CheckType -> CheckType
mergeFunction f@(pid,_) (fs,gs,vs)  
    | Map.member pid (Map.fromList fs) 
        = error $ "A procedure with pid " ++ (show pid) ++ " has already been defined."
    | otherwise 
        = (fs ++ [f],gs,vs)
mergeGlobal :: VariableType -> CheckType -> CheckType
mergeGlobal g@(id,_) (fs,gs,vs)
    | Map.member id (Map.fromList gs)
        = error $ "A global variable with id " ++ (show id) ++ " has already been defined."
    | otherwise
        =(fs,gs ++ [g],vs)
mergeVariable :: VariableType -> CheckType -> CheckType
mergeVariable v@(id,_) (fs,gs,(scope:scopes))
    | Map.member id (Map.fromList scope)
        = error $ "A variable with id " ++ (show id) ++ " has already been defined."
    | Map.member id (Map.fromList gs)
        = error $ "A global variable with id " ++ (show id) ++ " has already been defined."
    | otherwise
        = (fs,gs,((scope ++ [v]):scopes))

getAlphabet :: String -> Alphabet
getAlphabet "int"   = IntType
getAlphabet "bool"  = BoolType
getAlphabet _       = error "Type not recognised in Checker.getAlphabet"