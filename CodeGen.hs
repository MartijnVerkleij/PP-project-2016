module CodeGen where

import BasicFunctions
import HardwareTypes
import Sprockell
import System
import Types
import Constants
import Checker
import Data.Maybe

import Debug.Trace

codeGen' int ast = codeGen ast int

codeGen :: AST -> Int -> [Instruction]

codeGen (ASTProgram asts 
    checkType@(functions, globals, variables)) threads
        =   threadControl ++ procsCode ++ exprsCode
        where
            begin_of_code = length (threadControl ++ procsCode) - 3
            (procs, exprs) = span isProcedure asts
            procsCode = concat $ map (\x -> codeGen x threads) procs
            exprsCode = concat $ map (\x -> codeGen x threads) exprs
            
            threadControl = 
                [ Compute Equal regSprID reg0 regE
                , Branch regE (Rel 2)
                , Jump (Rel 2)
                , Jump (Rel begin_of_code)
                , Load (ImmValue threadControlAddr) regA
                , TestAndSet (IndAddr regA)     -- Grab rd lock
                , Receive regE
                , Branch regE (Rel 2)           -- successful lock -> +2
                , Jump (Rel (-3))               -- otherwise try again
                , Compute Incr regA reg0 regA
                , Compute Incr regA reg0 regA
                , ReadInstr (IndAddr regA)      -- Read jump address
                , Receive regB                  -- 
                , Push regB                     -- Need dem registers...
                , Compute Incr regA reg0 regA
                , Load (IndAddr regARP) regC    -- Load ARP (now 0) = regC
                , ReadInstr (IndAddr regA)      -- Read argcount = regD
                , Receive regD
                , Compute Equal regD reg0 regE  -- while still args left
                , Branch regE (Rel 9)           
                , Compute Incr regA reg0 regA
                , ReadInstr (IndAddr regA)      -- Read argument
                , Receive regB
                , Store regB (IndAddr regC)     -- Store in local memory
                , Compute Incr regC reg0 regC
                , Compute Decr regD reg0 regD
                , Jump (Rel (-9))               -- Back to while
                , Compute Incr regC reg0 regC
                , Load (ImmValue 5) regD        -- return address
                , Store regD (IndAddr regC)
                , Compute Incr regC reg0 regC
                , Store regARP (IndAddr regC)   -- Caller's ARP
                , Load (IndAddr regC) regARP    -- Set ARP to new scope
                , Pop regA                      -- pop procedure address
                , ComputeI Add regSprID thread_record_size regB 
                                                -- compute thread occupation bit 
                , TestAndSet (IndAddr regB)     -- Grab occupation bit
                , Receive regE
                , Branch regE (Rel 2)           -- successfully set -> +2
                , Jump (Rel (-3))               -- otherwise try again
                , Jump (Ind regA)               -- jump to procedure
                ]
                where
                    threadControlAddr = 1
            
            isProcedure :: AST -> Bool
            isProcedure (ASTProc _ _ _ _) = True
            isProcedure _ = False
            
codeGen (ASTGlobal varType astVar Nothing 
    checkType@(functions, globals, variables)) threads
        =   [ Load (ImmValue addr) regA         -- Load memory address of global's lock
            , TestAndSet (IndAddr regA)         -- Lock on ready bit
            , Receive regB                      --
            , Branch regB (Rel (-3))            -- Retry if lock fails
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
                addr = thread_record_size + threads 
                    + (global_record_size * (globalIndex (getStr astVar) globals))
codeGen (ASTProc pName astArgs astStat 
    checkType@(functions, globals, variables)) threads
        = codeGen astStat threads ++ 
            [ Compute Decr regARP reg0 regA 
            , Load (IndAddr regA) regE          -- Return address
            , Jump (Ind regE)                   -- Jump to it
            ]
codeGen (ASTArg astType astVar 
    checkType@(functions, globals, variables)) threads
        = [Nop] -- intentionally left blank
codeGen (ASTBlock astStats 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTDecl vartype astVar Nothing 
    checkType@(functions, globals, variables)) threads
        = (getMemAddr varNameStr variables) ++
            [ Store reg0 (IndAddr regE) ]
                where 
                    varNameStr = getStr astVar
codeGen (ASTDecl vartype astVar (Just astExpr) 
    checkType@(functions, globals, variables)) threads
        =   (codeGen astExpr threads) ++
            (getMemAddr varNameStr variables) ++
            [ Pop regD
            , Store reg0 (IndAddr regE) ]
                where 
                    varNameStr = getStr astVar
codeGen (ASTIf astExpr astThen Nothing
    checkType@(functions, globals, variables)) threads
        =   (codeGen astExpr threads) ++
            [ Pop regE
            , Branch regE (Rel (length thenGen))] ++
            thenGen
                where thenGen = codeGen astThen threads
codeGen (ASTIf astExpr astThen (Just astElse) 
    checkType@(functions, globals, variables)) threads
        =   (codeGen astExpr threads) ++
            [ Pop regE
            , Branch regE (Rel (length thenGen))] ++
            thenGen
            ++ [ Jump (Rel (length elseGen))]
            ++ elseGen
                where   thenGen = codeGen astThen threads
                        elseGen = codeGen astElse threads
codeGen (ASTWhile astExpr astStat 
    checkType@(functions, globals, variables)) threads
        =   exprGen ++
            [ Pop regE
            , Branch regE (Rel (1 + (length bodyGen)))] ++
            bodyGen ++
            [ Jump (Rel ((length (bodyGen ++ exprGen)) + 2))]
                where 
                    exprGen = codeGen astExpr threads
                    bodyGen = codeGen astStat threads
codeGen (ASTFork pName astArgs 
    checkType@(functions, globals, variables)) threads
        = [ Nop]
codeGen (ASTJoin 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTCall fName astArgs 
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTAss astVar astExpr _
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
codeGen (ASTOp astL op astR _
    checkType@(functions, globals, variables)) threads
        = [Nop]
codeGen (ASTUnary op astV _
    checkType@(functions, globals, variables)) threads
        = [Nop]

-- Find the index of a given Global. Used to calculate global address in memory.
globalIndex :: String -> [VariableType] -> Int
globalIndex var [] = error $ "Global -|" ++ (show var) ++ "|- is not defined."
globalIndex var ((xStr,_):xs)   | var == xStr   = 0
                                | otherwise     = 1 + globalIndex var xs

-- Find the memory location of a given variable in local memory and store it in regE.
getMemAddr :: String -> [[VariableType]] -> [Instruction]
getMemAddr varStr variables 
    = (replicate x (Load (IndAddr regARP) regE) ) ++ (trace (show variables) 
      [ ComputeI Add regE (y*4) regE
      , Load (IndAddr regE) regE ])
        where 
            (x,y) = findVar (0,0) varStr variables
            findVar :: (Int,Int) -> String -> [[VariableType]] -> (Int,Int)
            findVar _ str [] = (0,99) --  error ("Variable -|" ++ str ++ "|- was not found when calling getMemAddr.") 
            findVar (x,y) str ([]:scopes) = findVar (x+1,0) str scopes
            findVar (x,y) str (scope@(var@(str2,_):vars):scopes)
                | str == str2   = (x,y)
                | otherwise     = findVar (x,y+1) str (vars:scopes)
                
                
                
                
                
                
                
                
                
