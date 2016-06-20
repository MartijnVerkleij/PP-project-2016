module Checker where

import FPPrac.Trees
import Types
import Data.List
import Data.Char
import qualified Data.Map.Strict as Map

-- Wrapper
checker :: AST -> AST
checker ast = checker2 (checker1 ast) ([],[],[[]])

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
                argPairs        = map getArg args
                mergedChecks    = mergeFunction (pid, argPairs) checks
                procedure       = ASTProc pid args stat mergedChecks
        function (ASTProgram asts checks) ast
            = ASTProgram (asts ++ [ast]) checks

-- Second pass
checker2 :: AST -> CheckType -> AST

checker2 (ASTProgram asts check) _ 
    = ASTProgram (foldl' function [] asts) check
    where
        function :: [AST] -> AST -> [AST]
        function xs x   = xs ++ [(checker2 x check)]
checker2 self@(ASTGlobal varType varName Nothing _) _
    = self
checker2 (ASTGlobal varType varName (Just expr) check) _
    | varType == (getExprType exprCheck) = (ASTGlobal varType varName (Just exprCheck) check)
    | otherwise     = error $ "Types do not match, type: " ++ (show varType) ++ " and expression: " ++ (show expr)
    where
        exprCheck   = checker2 expr check
checker2 (ASTProc pid args body check@(f,_,_)) _
    = ASTProc pid args bodyCheck newCheck
    where
        bodyCheck   = checker2 body newCheck
        newCheck    = foldl' addToScope (openScope check) ((Map.fromList f)Map.!pid)
checker2 (ASTBlock asts _) check
    = ASTBlock (foldl' function [] asts) newCheck
    where
        newCheck        = openScope check
        function :: [AST] -> AST -> [AST]
        function xs x   = xs ++ [(checker2 x newCheck)]
checker2 (ASTDecl varType var@(ASTVar varName _) Nothing _) check
    = ASTDecl varType var Nothing newCheck
    where
        newCheck    = addToScope check (varName, varType)
checker2 (ASTDecl varType var@(ASTVar varName _) (Just expr) _) check
    | varType == (getExprType exprCheck) && (nameCheck expr varName)
        = (ASTDecl varType var (Just exprCheck) newCheck)
    | otherwise     = error $ "Types do not match, type: " ++ (show varType) ++ " and expression: " ++ (show expr)
    where
        newCheck    = addToScope check (varName, varType)
        exprCheck   = checker2 expr newCheck
        nameCheck :: AST -> String -> Bool
        nameCheck (ASTVar varName _) id         = varName == id
        nameCheck (ASTAss ast1 ast2 _ _) id     = nameCheck ast1 id && (nameCheck ast2 id)
        nameCheck (ASTInt _ _) _                = True
        nameCheck (ASTBool _ _) _               = True
        nameCheck (ASTType _ _) _               = True
        nameCheck (ASTOp ast1 _ ast2 _ _) id    = nameCheck ast1 id && (nameCheck ast2 id)
        nameCheck (ASTUnary _ ast _ _) id       = nameCheck ast id
        nameCheck ast id 
            = error $ "Shouldn't be reached in Checker.checker2.nameCheck, with: " ++ id ++ " and: " ++ (show ast)
checker2 (ASTIf expr thenAst Nothing _) check
    | (getExprType exprCheck) == BoolType    = ASTIf exprCheck thenCheck Nothing check
    | otherwise     = error $ "Condition in if statement should be of type: bool, but isnt, in: " ++ (show expr)
    where
        exprCheck   = checker2 expr check
        thenCheck   = checker2 thenAst check
checker2 (ASTIf expr thenAst (Just elseAst) _) check
    | (getExprType exprCheck) == BoolType   = ASTIf exprCheck thenCheck (Just elseCheck) check
    | otherwise     = error $ "Condition in if statement should be of type: bool, but isnt, in: " ++ (show expr)
    where
        exprCheck   = checker2 expr check
        thenCheck   = checker2 thenAst check
        elseCheck   = checker2 elseAst check
checker2 (ASTWhile expr ast _) check
    | (getExprType exprCheck) == BoolType   = ASTWhile exprCheck astCheck check
    | otherwise     = error $ "Condition in while statement should be of type: bool, but isnt, in: " ++ (show expr)
    where
        exprCheck   = checker2 expr check
        astCheck    = checker2 ast check
checker2 self@(ASTFork pid args _) check
    | matchArgs pid args check  = ASTFork pid args check
    | otherwise 
        = error $ "Either function not declared or arguments did not match declared types in Checker.checker2, with " ++ (show self) ++ " and: " ++ (show check)
checker2 (ASTJoin _) check
    = ASTJoin check
checker2 self@(ASTCall pid args _) check
    | matchArgs pid args check  = ASTCall pid args check
    | otherwise 
        = error $ "Either function not declared or arguments did not match declared types in Checker.checker2, with " ++ (show self) ++ " and: " ++ (show check)
-- TODO: next ASTAss



matchArgs :: String -> [AST] -> CheckType -> Bool
matchArgs pid args check@(f,g,v)
    = (length argTypes == (length funcTypes)) && (all (==True) $ zipWith (==) argTypes funcTypes)
    where
        argTypes    = map snd $ map getArg args
        funcTypes   = map snd $ (Map.fromList f)Map.!pid

openScope :: CheckType -> CheckType
openScope (f,g,v) = (f,g,[]:v)

closeScope :: CheckType -> CheckType -- Unused?
closeScope check@(_,_,[])   = error $ "All scopes have already been closed in Checker.closeScope, CheckType: " ++ (show check)
closeScope (f,g,(v:vs))     = (f,g,vs)

addToScope :: CheckType -> (String, Alphabet) -> CheckType
addToScope (f,g,(v:vs)) pair@(id,_) 
    | Map.member id (Map.fromList g)
        = error $ "A global variable with id " ++ (show id) ++ " has already been declared."
    | Map.member id $ Map.fromList f
        = error $ "A procedure with pid " ++ (show id) ++ " has already been declared."
    | Map.member id (Map.fromList v)
        = error $ "A variable with id " ++ (show id) ++ " has already been declared."
    | otherwise = (f,g,((v ++ [pair]):vs))

getStr :: AST -> String
getStr (ASTVar str _)   = str
getStr x                = error $ "Cannot get string of: " ++ (show x)

getArg :: AST -> (String, Alphabet)
getArg (ASTArg (ASTType typeStr _) (ASTVar varName _) _)
    = (varName, getAlphabet typeStr)
getArg _ 
    = error "Shouldn't reach this. In Checker.getType" 

getStrType :: String -> Alphabet
getStrType str  | length str == 0   = error "Variable length equals zero in Checker.getStrType"
                | all (== True) $ map (isDigit) str     = IntType
                | (str == "true") || (str == "false")   = BoolType
                | otherwise         = error $ "invalid type in Checker.getStrType, var: " ++ str

getExprType :: AST -> Alphabet
getExprType (ASTAss _ _ (Just typeStr) _)   = typeStr
getExprType (ASTOp _ _ _ (Just typeStr) _)  = typeStr
getExprType (ASTUnary _ _ (Just typeStr) _) = typeStr
getExprType (ASTInt _ _)                    = IntType
getExprType (ASTBool _ _)                   = BoolType
getExprType (ASTVar varName (_,g,v))
    | Map.member varName (Map.fromList g)   = (Map.fromList g)Map.!varName
    | otherwise                             = iterVar varName v
    where
        iterVar :: String -> [[VariableType]] -> Alphabet
        iterVar varName []      = error $ "Variable: " ++ varName ++ " not declared"
        iterVar varName (vt:vts)  
            | Map.member varName (Map.fromList vt)  = (Map.fromList vt)Map.!varName
            | otherwise                             = iterVar varName vts
getExprType ast = error $ "Shouldn't reach this in Checker.getExprType. AST: " ++ (show ast)

getAlphabet :: String -> Alphabet
getAlphabet "int"   = IntType
getAlphabet "bool"  = BoolType
getAlphabet _       = error "Type not recognised in Checker.getAlphabet"

mergeFunction :: FunctionType -> CheckType -> CheckType
mergeFunction f@(pid,_) (fs,gs,vs)  
    | Map.member pid $ Map.fromList fs
        = error $ "A procedure with pid " ++ (show pid) ++ " has already been declared."
    | Map.member pid $ Map.fromList gs 
        = error $ "A global variable with id " ++ (show pid) ++ " has already been declared."
    | elem pid $ Map.keys $ Map.fromList $ concat vs 
        = error $ "A variable with id " ++ (show pid) ++ " has already been declared."
    | otherwise 
        = (fs ++ [f],gs,vs)
mergeGlobal :: VariableType -> CheckType -> CheckType
mergeGlobal g@(id,_) (fs,gs,vs)
    | Map.member id $ Map.fromList fs
        = error $ "A procedure with pid " ++ (show id) ++ " has already been declared."
    | Map.member id (Map.fromList gs)
        = error $ "A global variable with id " ++ (show id) ++ " has already been declared."
    | elem id $ Map.keys $ Map.fromList $ concat vs 
        = error $ "A variable with id " ++ (show id) ++ " has already been declared."
    | otherwise
        =(fs,gs ++ [g],vs)
mergeVariable :: VariableType -> CheckType -> CheckType
mergeVariable v@(id,_) (fs,gs,(scope:scopes))
    | Map.member id $ Map.fromList fs
        = error $ "A procedure with pid " ++ (show id) ++ " has already been declared."
    | Map.member id (Map.fromList scope)
        = error $ "A variable with id " ++ (show id) ++ " has already been declared."
    | Map.member id (Map.fromList gs)
        = error $ "A global variable with id " ++ (show id) ++ " has already been declared."
    | otherwise
        = (fs,gs,((scope ++ [v]):scopes))