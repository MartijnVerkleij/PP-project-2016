module CodeGen where

import BasicFunctions
import HardwareTypes
import Sprockell
import System

import Types
import Constants
import Checker
import Data.Maybe

codeGen :: AST -> Int -> [Instruction]

codeGen (ASTProgram asts 
    checkType@(functions, globals, variables)) threads
        =   threadControl ++ procsCode ++ exprsCode
        where
            begin_of_code = length (threadControl ++ procsCode) + 1
            (procs, exprs) = span isProcedure asts
            procsCode = concat $ map (\x -> codeGen x threads) procs
            exprsCode = concat $ map (\x -> codeGen x threads) exprs
            
            threadControl = 
                [ Compute Equal regSprID reg0 regE
                , Branch regE (Rel 2)
                , Jump (Rel 2)
                , Jump (Abs begin_of_code)
                , Load (ImmValue threadControlAddr)  regA
                , TestAndSet (IndAddr regA)
                , Receive regE
                , Branch regE (Rel 2)
                , Jump (Rel (-3))
                , Compute Incr regA reg0 regA
                , ReadInstr (IndAddr regA)
                , Receive regB
                
                ]
                where
                    threadControlAddr = (length globals) * global_record_size
            
            isProcedure :: AST -> Bool
            isProcedure (ASTProc _ _ _ _) = True
            isProcedure _ = False
            
codeGen (ASTGlobal varType astVar Nothing 
    checkType@(functions, globals, variables)) threads
        =   [ Load (ImmValue addr) regA         -- Load memory address of global's lock
            , TestAndSet (IndAddr regA)         -- Lock on ready bit
            , Receive regB                      --
            , Branch regB (Rel (-4))            -- Retry if lock fails
            , Load (ImmValue (addr + global_record_value)) 
                regC                            -- Load memory address of global value
            , WriteInstr reg0 (IndAddr regC)    -- Write value
            , WriteInstr reg0 (IndAddr regA)    -- Unlock value
            ]
        where
            addr = (global_record_size * (globalIndex (getStr astVar) globals))
            
codeGen (ASTGlobal varType astVar (Just astExpr) 
    checkType@(functions, globals, variables)) threads
        =   (codeGen astExpr threads) ++
            [ Pop (regE)                        -- pop value from expression
            , Load (ImmValue addr) regA         -- Load memory address of global's lock
            , TestAndSet (IndAddr regA)         -- Lock on ready bit
            , ReadInstr (IndAddr regA)          -- 
            , Receive regB                      --
            , Branch regB (Rel (-4))            -- Retry if lock fails
            , Load (ImmValue (addr + global_record_value)) 
                regC                            -- Load memory address of global value
            , WriteInstr regE (IndAddr regC)    -- Write value
            , WriteInstr reg0 (IndAddr regA)    -- Unlock value
            ]
        where
            addr = (global_record_size * (globalIndex (getStr astVar) globals))
codeGen (ASTProc pName astArgs astStat 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTArg astType astVar 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTBlock astStats 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTDecl vartype astVar Nothing 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTDecl vartype astVar (Just astExpr) 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTIf astExpr astThen Nothing
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTIf astExpr astThen (Just astElse) 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTWhile astExpr astStat 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTFork pName astArgs 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTJoin 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTCall fName astArgs 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTAss astVar astExpr 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTVar varName 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTInt value 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTBool value 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTType typeStr 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTOp astL op astR
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTUnary op astV 
    checkType@(functions, globals, variables)) threads
        = [Nop]

-- Find the index of a given Global. Used to calculate global address in memory.
globalIndex :: String -> [VariableType] -> Int
globalIndex var [] = error $ "Global \"" ++ (show var) ++ "\" is not defined."
globalIndex var ((xStr,_):xs)   | var == xStr   = 0
                                | otherwise     = 1 + globalIndex var xs
