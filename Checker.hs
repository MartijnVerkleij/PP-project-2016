module Checker where

import FPPrac.Trees
import Types
import Data.List
import qualified Data.Map.Strict as Map

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



getStr:: AST -> String
getStr (ASTVar str _)   = str
getStr x                = error $ "Cannot get string of: " ++ (show x)

getType :: AST -> Alphabet
getType (ASTArg (ASTType typeStr _) _ _)  
    = getAlphabet typeStr
getType _ 
    = error "Shouldn't reach this. In Checker.getType" 

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


--checker1 (ASTProgram asts) 
--    = CheckTreeProgram (map checker1 ) (functions, [], [])
--    where
--        functions   = concat (map (\(ast, f, g, v) -> f) (map checkTree asts))
--checker1 (ASTProc str asts ast) =
--checker1 (ASTArg ast1 ast2) = 
--checker1 (ASTBlock asts) =
--checker1 (ASTDecl global {-global :: Bool-} type ast (Nothing) 
--    | global    =
--    | otherwise =
--checker1 (ASTDecl global {-global :: Bool-} type ast1 (Just ast2)
--    | global    =
--    | otherwise =
--checker1 (ASTIf ast1 ast2 (Nothing))) = 
--checker1 (ASTIf ast1 ast2 (Just ast3))) = 
--checker1 (ASTWhile ast ast)) = 
--checker1 (ASTFork str asts)) = 
--checker1 (ASTJoin)) = 
--checker1 (ASTCall str asts) = 
--checker1 (ASTAss ast1 ast2) = 
--checker1 (ASTVar str) = 
--checker1 (ASTInt str) = 
--checker1 (ASTBool str) = 
--checker1 (ASTType str) = 
--checker1 (ASTOp ast1 str ast2) = 
--checker1 (ASTUnary str ast) = 




--checker2 :: CheckTree -> CheckTree

--checker2 (self@(ASTProgram asts), functions, globals, variables) 
--    = tail $ map checker2 $ map (\x -> (x, functions, globals, variables)) asts
--checker2 (self@(ASTProc str asts ast), functions, globals, variables) = 
--checker2 (self@(ASTArg ast1 ast2), functions, globals, variables) = 
--checker2 (self@(ASTBlock asts), functions, globals, variables) =
--checker2 (self@(ASTDecl global {-global :: Bool-} type ast (Nothing)), functions, globals, variables)
--    | global    =
--    | otherwise =
--checker2 (self@(ASTDecl global {-global :: Bool-} type ast1 (Just ast2)), functions, globals, variables)
--    | global    =
--    | otherwise =
--checker2 (self@(ASTIf ast1 ast2 (Nothing)) functions globals variables) = 
--checker2 (self@(ASTIf ast1 ast2 (Just ast3)) functions globals variables) = 
--checker2 (self@(ASTWhile ast ast) functions globals variables) = 
--checker2 (self@(ASTFork str asts) functions globals variables) = 
--checker2 (self@(ASTJoin) functions globals variables) = 
--checker2 (self@(ASTCall str asts) functions globals variables) = 
--checker2 (self@(ASTAss ast1 ast2) functions globals variables) = 
--checker2 (self@(ASTVar str) functions globals variables) = 
--checker2 (self@(ASTInt str) functions globals variables) = 
--checker2 (self@(ASTBool str) functions globals variables) = 
--checker2 (self@(ASTType str) functions globals variables) = 
--checker2 (self@(ASTOp ast1 str ast2) functions globals variables) = 
--checker2 (self@(ASTUnary str ast) functions globals variables) = 
