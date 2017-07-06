module Checker where

import FPPrac.Trees
import Types
import Data.List
import Data.Char
import qualified Data.Map.Strict as Map
import Debug.Trace

-- Wrapper
checker :: AST -> AST
checker ast = checker2 (checker1 ast) ([],[],[[]],[])

-- First pass
-- Checks declarations
-- Stores globals
-- Stores functions and their arguments
checker1 :: AST -> AST
checker1 (ASTProgram asts _) 
    = foldl function (ASTProgram [] ([],[],[[]],[])) asts
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
        function (ASTProgram asts checks) (ASTEnum varName vars _)
            = ASTProgram (asts ++ [enum]) mergedEnums 
            where
                enumStrs        = map getStr vars
                mergedEnums     = mergeEnum (varName, enumStrs) checks
                enum            = ASTEnum varName vars mergedEnums
        function (ASTProgram asts checks) ast
            = ASTProgram (asts ++ [ast]) checks

-- Second pass
-- Checks the correctness of types and declarations/usage
checker2 :: AST -> CheckType -> AST
-- Checks all individual parts of the program in order
checker2 (ASTProgram asts check) _
    = foldl' function (ASTProgram prePart check) stats
    where
        prePart                         = map (\x -> checker2 x check) preStats
        (preStats, stats)               = span isPreStat asts
        isPreStat :: AST -> Bool
        isPreStat (ASTGlobal _ _ _ _)   = True
        isPreStat (ASTProc _ _ _ _)     = True
        isPreStat _                     = False
        function :: AST -> AST -> AST
        function (ASTProgram asts check) ast
            = ASTProgram (asts ++ [astCheck]) (getCheck astCheck)
            where
                astCheck = checker2 ast check
-- No checks
checker2 self@(ASTGlobal varType varName Nothing _) _
    = self
-- Checks the type of the expression
-- Makes sure the variable itself is not in the expression
checker2 (ASTGlobal varType varName@(ASTVar varId _) (Just expr) _) check
    | varType == (getExprType exprCheck) && (nameCheck expr varId)
        = (ASTGlobal varType varName (Just exprCheck) check)
    | otherwise     = error $ "Types do not match, type: " ++ (show varType) ++ " and expression: " ++ (show expr)
    where
        exprCheck   = checker2 expr check
-- Opens new scope and puts its arguments in it as variables
-- Checks its body
checker2 (ASTProc pid args body _) check@(f,_,_,_)
    = ASTProc pid args bodyCheck newCheck
    where
        bodyCheck   = checker2 body newCheck
        newCheck    = foldl' addToScope (openScope check) ((Map.fromList f)Map.!pid)
-- No checks
checker2 self@(ASTEnum _ _ _) _
    = self
-- Opens new scope
-- Checks its body in order
checker2 (ASTBlock asts _) check
    = foldl' function (ASTBlock [] newCheck) asts
    where
        newCheck    = openScope check
        function :: AST -> AST -> AST
        function (ASTBlock asts check) ast 
            = ASTBlock (asts ++ [astCheck]) (getCheck astCheck)
            where
                astCheck    = checker2 ast check
-- Adds variable to scope (see addToScope)
checker2 (ASTDecl varType var@(ASTVar varName _) Nothing _) check
    = ASTDecl varType var Nothing newCheck
    where
        newCheck    = addToScope check (varName, varType)
-- Adds variable to scope (see addToScope)
-- Check expression type
-- Does nameCheck
checker2 (ASTDecl varType var@(ASTVar varName _) (Just expr) _) check
    | varType == (getExprType exprCheck) && (nameCheck expr varName)
        = ASTDecl varType varCheck (Just exprCheck) newCheck
    | otherwise     = error $ "Types do not match or declared variable itself is used in assignment, type: " ++ (show varType) ++ " and expression: " ++ (show expr)
    where
        newCheck    = addToScope check (varName, varType)
        exprCheck   = checker2 expr newCheck
        varCheck    = checker2 var newCheck
-- Check expression type
-- Check body
checker2 (ASTIf expr thenAst Nothing _) check
    | (getExprType exprCheck) == BoolType    = ASTIf exprCheck thenCheck Nothing check
    | otherwise     = error $ "Condition in if statement should be of type: bool, but isnt, in: " ++ (show expr)
    where
        exprCheck   = checker2 expr check
        thenCheck   = checker2 thenAst check
-- Check expression type
-- Check bodies
checker2 (ASTIf expr thenAst (Just elseAst) _) check
    | (getExprType exprCheck) == BoolType   = ASTIf exprCheck thenCheck (Just elseCheck) check
    | otherwise     = error $ "Condition in if statement should be of type: bool, but isnt, in: " ++ (show expr)
    where
        exprCheck   = checker2 expr check
        thenCheck   = checker2 thenAst check
        elseCheck   = checker2 elseAst check
-- Check expression type
-- Check body
checker2 (ASTWhile expr ast _) check
    | (getExprType exprCheck) == BoolType   = ASTWhile exprCheck astCheck check
    | otherwise     = error $ "Condition in while statement should be of type: bool, but isnt, in: " ++ (show expr)
    where
        exprCheck   = checker2 expr check
        astCheck    = checker2 ast check
-- Check arguments (see matchArgs)
checker2 self@(ASTFork pid args _) check
    | matchArgs pid argsCheck check  = ASTFork pid argsCheck check
    | otherwise 
        = error $ "Either function not declared or arguments did not match declared types in Checker.checker2, with " ++ (show self) ++ " and: " ++ (show check)
    where
        argsCheck = map (\x -> checker2 x check) args
-- No checks
checker2 (ASTJoin _) check
    = ASTJoin check
-- Check arguments (see matchArgs)
checker2 self@(ASTCall pid args _) check
    | matchArgs pid argsCheck check  = ASTCall pid argsCheck check
    | otherwise 
        = error $ "Either function not declared or arguments did not match declared types in Checker.checker2, with " ++ (show self) ++ " and: " ++ (show check)
    where
        argsCheck = map (\x -> checker2 x check) args
-- Check expressions
checker2 (ASTPrint exprs _) check
    | null types
        = ASTPrint asts check
    | otherwise
        = error $ "Cannot print enumerated value(s) " ++ show types ++"."
    where
        types = map getStr (filter (\x -> getExprType x == EnumType) asts)
        asts = map (\x -> checker2 x check) exprs
-- Checks: 
checker2 (ASTExpr expr _ _) check
    = ASTExpr exprCheck (Just (getExprType exprCheck)) check
    where
        exprCheck = checker2 expr check
-- Check types
checker2 (ASTAss var expr _ _) check
    | (getExprType varCheck) == (getExprType exprCheck)  = ASTAss varCheck exprCheck (Just (getExprType varCheck)) check
    | otherwise     = error $ "Types do not match in assignment in Checker.checker2, with: " ++ (show var) ++ " and: " ++ (show expr)
    where
        exprCheck   = checker2 expr check
        varCheck    = checker2 var check
-- No checks
checker2 (ASTVar str _) check
    = ASTVar str check
-- No checks
checker2 (ASTInt str _) check
    = ASTInt str check
-- No checks
checker2 (ASTBool str _) check
    = ASTBool str check
-- No checks
checker2 (ASTType str _) check
    = ASTType str check
-- Check types
-- Set operation result type
checker2 (ASTOp expr1 op expr2 _ _) check
    | elem op ["&&", "||", "<>", "==", "!=", "+", "-", "*", "<=", ">=", "<", ">"] && expr1Type == (getExprType expr2Check) && (fst $ matchOpType op expr1Type)
        = ASTOp expr1Check op expr2Check (Just (snd (matchOpType op expr1Type))) check
    | otherwise     = error $ "Types do not match in operation in Checker.checker2, with " ++ op ++ " on: " ++ (show expr1) ++ " and: " ++ (show expr2)
    where
        expr1Type   = getExprType expr1Check
        expr1Check  = checker2 expr1 check
        expr2Check  = checker2 expr2 check
-- Check type
-- Set operation result type
checker2 (ASTUnary op expr _ _) check
    | elem op ["!", "-", "--", "++"] && fst (matchOpType op exprType) 
        = ASTUnary op exprCheck (Just (snd (matchOpType op exprType))) check
    | otherwise     = error $ "Types do not match in unary in Checker.checker2, with " ++ op ++ " on: " ++ (show expr)
    where
        exprType    = getExprType exprCheck
        exprCheck   = checker2 expr check

-- Take the operator and input type as argument, 
-- return whether the type matches the operator and the resulting operation type
matchOpType :: String -> Alphabet -> (Bool, Alphabet)
matchOpType op exprType
    | op `elem` ["==","!="]       = (True, BoolType)
    | otherwise                 = ((fst (opType op)) == exprType, snd (opType op))
    where
        opType :: String -> (Alphabet, Alphabet)
        opType op
            | op `elem` ["&&", "||", "<>", "!"]         = (BoolType, BoolType)
            | op `elem` ["+", "-", "*", "--", "++"]     = (IntType, IntType)
            | op `elem` ["<=", ">=", "<", ">"]          = (IntType, BoolType)
            | otherwise = error $ "Undefined operator, look either here or in the tokenizer: " ++ op

-- Check whether the length of the arguments matches the length of the declared procedure arguments
-- Check whether the types of every argument matches
matchArgs :: String -> [AST] -> CheckType -> Bool
matchArgs pid args check@(f,g,v,_)
    = (length argTypes == (length funcTypes)) && (all (==True) $ zipWith (==) argTypes funcTypes)
    where
        argTypes    = map getExprType args
        funcTypes   = map snd $ (Map.fromList f)Map.!pid

-- Open a scope
openScope :: CheckType -> CheckType
openScope (f,g,v,e) = (f,g,[]:v,e)

-- Closes a scope
closeScope :: CheckType -> CheckType
closeScope self@(_,_,[],_)    = error $ "No scopes left to close, in Checker.closeScope, with: " ++ (show self)
closeScope (f,g,(v:vs),e)     = (f,g,vs,e)

-- Add a variable to the deepest scope
-- Check whether the name conflicts other declarations, where variable shadowing is allowed for non-global variables
addToScope :: CheckType -> (String, Alphabet) -> CheckType
addToScope (f,g,(v:vs),e) pair@(id,_) 
    | Map.member id (Map.fromList g)
        = error $ "A global variable with id " ++ (show id) ++ " has already been declared."
    | Map.member id $ Map.fromList f
        = error $ "A procedure with pid " ++ (show id) ++ " has already been declared."
    | Map.member id (Map.fromList v)
        = error $ "A variable with id " ++ (show id) ++ " has already been declared."
    | otherwise = (f,g,((v ++ [pair]):vs),e)

-- Return the name of an ASTVar as a string
getStr :: AST -> String
getStr (ASTVar str _)   = str
getStr x                = error $ "Cannot get string of: " ++ (show x)

-- Return both the name and type of an ASTArg as a tuple
getArg :: AST -> (String, Alphabet)
getArg (ASTArg (ASTType typeStr _) (ASTVar varName _) _)
    = (varName, getAlphabet typeStr)
getArg self 
    = error $ "Shouldn't reach this. In Checker.getArg, with: " ++ (show self) 

-- Determine the type of a variable based on it's string only
getStrType :: String -> Alphabet
getStrType str  | length str == 0   = error "Variable length equals zero in Checker.getStrType"
                | all (== True) $ map (isDigit) str     = IntType
                | (str == "true") || (str == "false")   = BoolType
                | otherwise         = error $ "invalid type in Checker.getStrType, var: " ++ str

-- Return the type of an expression
-- Looks through all scopes to find a variable
getExprType :: AST -> Alphabet
getExprType (ASTAss _ _ (Just typeStr) _)   = typeStr
getExprType (ASTOp _ _ _ (Just typeStr) _)  = typeStr
getExprType (ASTUnary _ _ (Just typeStr) _) = typeStr
getExprType (ASTInt _ _)                    = IntType
getExprType (ASTBool _ _)                   = BoolType
getExprType (ASTExpr _ (Just typeStr) _)    = typeStr
getExprType (ASTVar varName (_,g,v,e))
    | Map.member varName (Map.fromList g)   = (Map.fromList g)Map.!varName
    | elem varName $ concat $ map (\(x,y) -> (x:y)) e 
                                            = EnumType
    | otherwise                             = iterVar varName v
    where
        iterVar :: String -> [[VariableType]] -> Alphabet
        iterVar varName []      = error $ "Variable: " ++ varName ++ " not declared in Checker.getExprType.iterVar"
        iterVar varName (vt:vts)  
            | Map.member varName (Map.fromList vt)  = (Map.fromList vt)Map.!varName
            | otherwise                             = iterVar varName vts
getExprType ast = error $ "Shouldn't reach this in Checker.getExprType. AST: " ++ (show ast)

-- Returns an Alphabet with the correct type of a type in string form
getAlphabet :: String -> Alphabet
getAlphabet "int"   = IntType
getAlphabet "bool"  = BoolType
getAlphabet _       = error "Type not recognised in Checker.getAlphabet"

-- Checks if the name of the function has been declared already
-- Adds the FunctionType to the CheckType
mergeFunction :: FunctionType -> CheckType -> CheckType
mergeFunction f@(pid,_) (fs,gs,vs,es)  
    | Map.member pid $ Map.fromList fs
        = error $ "A procedure with pid " ++ (show pid) ++ " has already been declared."
    | Map.member pid $ Map.fromList gs 
        = error $ "A global variable with id " ++ (show pid) ++ " has already been declared."
    | elem pid $ Map.keys $ Map.fromList $ concat vs 
        = error $ "A variable with id " ++ (show pid) ++ " has already been declared."
    | elem pid $ concat $ map (\(x,y) -> (x:y)) es
        = error $ "A value of an enumeration with id " ++ (show pid) ++ " has already been declared."
    | otherwise 
        = (fs ++ [f],gs,vs,es)

-- Checks if the name of the global has been declared already
-- Adds the VariableType to the CheckType
mergeGlobal :: VariableType -> CheckType -> CheckType
mergeGlobal g@(id,_) (fs,gs,vs,es)
    | Map.member id $ Map.fromList fs
        = error $ "A procedure with pid " ++ (show id) ++ " has already been declared."
    | Map.member id (Map.fromList gs)
        = error $ "A global variable with id " ++ (show id) ++ " has already been declared."
    | elem id $ Map.keys $ Map.fromList $ concat vs 
        = error $ "A variable with id " ++ (show id) ++ " has already been declared."
    | elem id $ concat $ map (\(x,y) -> (x:y)) es
        = error $ "A value of an enumeration with id " ++ (show id) ++ " has already been declared."
    | otherwise
        =(fs,gs ++ [g],vs,es)

-- Checks if the name of the variable has been declared already, only considering the deepest scope
-- Adds the VariableType to the CheckType
mergeVariable :: VariableType -> CheckType -> CheckType
mergeVariable v@(id,_) (fs,gs,(scope:scopes),es)
    | Map.member id $ Map.fromList fs
        = error $ "A procedure with pid " ++ (show id) ++ " has already been declared."
    | Map.member id (Map.fromList scope)
        = error $ "A variable with id " ++ (show id) ++ " has already been declared."
    | Map.member id (Map.fromList gs)
        = error $ "A global variable with id " ++ (show id) ++ " has already been declared."
    | elem id $ concat $ map (\(x,y) -> (x:y)) es
        = error $ "A value of an enumeration with id " ++ (show id) ++ " has already been declared."
    | otherwise
        = (fs,gs,((scope ++ [v]):scopes),es)

mergeEnum :: EnumCheckerType -> CheckType -> CheckType
mergeEnum e@(name,ids) (fs,gs,vs,es)
    | any (\id -> Map.member id $ Map.fromList fs) (name:ids)
        = error $ "A procedure with pid " ++ (show ids) ++ " has already been declared."
    | any (\id -> elem id $ Map.keys $ Map.fromList $ concat vs ) (name:ids)
        = error $ "A variable with id " ++ (show ids) ++ " has already been declared."
    | any (\id -> Map.member id (Map.fromList gs)) (name:ids)
        = error $ "A global variable with id " ++ (show ids) ++ " has already been declared."
    | any (\id -> elem id $ concat $ map (\(x,y) -> (x:y)) es) (name:ids)
        = error $ "A value of an enumeration with id " ++ (show ids) ++ " has already been declared."
    | otherwise
        = (fs,gs,vs,es ++ [e])


-- Return the type CheckType from any AST
getCheck :: AST -> CheckType
getCheck (ASTArg _ _ mergedChecks)      = mergedChecks
getCheck (ASTBlock _ mergedChecks)      = closeScope mergedChecks
getCheck (ASTDecl _ _ _ mergedChecks)   = mergedChecks
getCheck (ASTIf _ _ _ mergedChecks)     = mergedChecks
getCheck (ASTWhile _ _ mergedChecks)    = mergedChecks
getCheck (ASTFork _ _ mergedChecks)     = mergedChecks
getCheck (ASTJoin mergedChecks)         = mergedChecks
getCheck (ASTEnum _ _ mergedChecks)     = mergedChecks
getCheck (ASTCall _ _ mergedChecks)     = mergedChecks
getCheck (ASTPrint _ mergedChecks)      = mergedChecks
getCheck (ASTExpr _ _ mergedChecks)     = mergedChecks
getCheck (ASTAss _ _ _ mergedChecks)    = mergedChecks
getCheck (ASTVar _ mergedChecks)        = mergedChecks
getCheck (ASTInt _ mergedChecks)        = mergedChecks
getCheck (ASTBool _ mergedChecks)       = mergedChecks
getCheck (ASTType _ mergedChecks)       = mergedChecks
getCheck (ASTOp _ _ _ _ mergedChecks)   = mergedChecks
getCheck (ASTUnary _ _ _ mergedChecks)  = mergedChecks

-- Check if a variable name is not used in an expression
nameCheck :: AST -> String -> Bool
nameCheck (ASTVar varName _) id         = varName /= id
nameCheck (ASTAss ast1 ast2 _ _) id     = nameCheck ast1 id && (nameCheck ast2 id)
nameCheck (ASTInt _ _) _                = True
nameCheck (ASTBool _ _) _               = True
nameCheck (ASTType _ _) _               = True
nameCheck (ASTOp ast1 _ ast2 _ _) id    = nameCheck ast1 id && (nameCheck ast2 id)
nameCheck (ASTUnary _ ast _ _) id       = nameCheck ast id
nameCheck (ASTExpr ast _ _) id          = nameCheck ast id
nameCheck ast id 
    = error $ "Shouldn't be reached in Checker.checker2.nameCheck, with: " ++ id ++ " and: " ++ (show ast)
