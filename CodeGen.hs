module CodeGen where

import BasicFunctions
import HardwareTypes
import Sprockell
import System
import Types
import Constants
import Checker
import Data.Maybe

import qualified Data.Map.Strict as Map

import Debug.Trace

codeGen' int ast = insertErrorPointers ( insertACallPointers (insertFPointers instrss (pPointers, cPointers)) (pPointers, cPointers)) (pPointers, cPointers)
    where 
        (instrss, pPointers, cPointers) = findPointers' $ codeGen ast int

codeGen :: AST -> Int -> [Instruction]

codeGen (ASTProgram asts 
    checkType@(functions, globals, variables)) threads
        =   threadControl ++ procsCode ++ [Nop,Nop,Nop] ++ exprsCode ++ [Nop,EndProg]
        where
            
            
            threadControl = 
                [ Compute Equal regSprID reg0 regE
                , Branch regE (Rel 2)
                , Jump (Rel 6)
                , TestAndSet (DirAddr (threadControlAddr + 1))
                , Receive (regE)
                , Branch regE (Rel 2)
                , Jump (Rel (-3))
                , Load (ImmValue 1) regARP
                , Jump (Rel begin_of_code)
                , Load (ImmValue threadControlAddr) regA 
                                                -- Begin of thread control loop
                , ReadInstr (DirAddr fork_record_endp)
                , EndProg{-Receive regB -}                 -- Read end of program address
                , Compute Equal regB reg0 regE
                , Branch regE (Rel 2)           -- jump over endprog if not done
                , EndProg                       -- Endprog if done bit is set by main thread
                , TestAndSet (IndAddr regA)     -- Grab rd lock
                , Receive regE
                , Branch regE (Rel 2)           -- successful lock -> +2
                , Jump (Rel (-8))               -- otherwise try again
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
                , Load (ImmValue 9) regD        -- return address
                , Store regD (IndAddr regC)
                , Compute Incr regC reg0 regC
                , Store regARP (IndAddr regC)   -- Caller's ARP
                , Load (IndAddr regC) regARP    -- Set ARP to new scope
                , Pop regA                      -- pop procedure address
                , ComputeI Add regSprID fork_record_size regB 
                                                -- compute thread occupation bit 
                , TestAndSet (IndAddr regB)     -- Grab occupation bit
                , Receive regE
                , Branch regE (Rel 2)           -- successfully set -> +2
                , Jump (Rel (-3))               -- otherwise try again
                , Jump (Ind regA)               -- jump to procedure
                , Nop
                , Nop
                ]
                where
                    threadControlAddr = global_record_size * (length globals)
            
            begin_of_code = (lengthNoDebug (threadControl ++ procsCode)) - 8
            (procs, exprs) = span isProcedure asts
            procsCode = concat $ map (\x -> codeGen x threads) procs
            exprsCode = concat $ map (\x -> codeGen x threads) exprs
            
            
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
                addr = fork_record_size + threads 
                    + (global_record_size * (globalIndex (getStr astVar) globals))
codeGen (ASTProc pName astArgs astStat 
    checkType@(functions, globals, variables)) threads
        =   [ Debug ("**p" ++ pName) ] ++ 
            codeGen astStat threads ++ 
            [Compute Decr regARP reg0 regA 
            , Load (IndAddr regA) regE          -- Return address
            , Load (IndAddr regARP) regARP
            , Jump (Ind regE)                   -- Jump to it
            ]
codeGen (ASTArg astType astVar 
    checkType@(functions, globals, variables)) threads
        = getMemAddr astStr variables ++
            [ Push regE ]
            where
                astStr = getStr astVar
codeGen (ASTBlock astStats 
    checkType@(functions, globals, variables)) threads
        =   [ Compute Add regARP reg0 regC    -- Load ARP = regC
            , ComputeI Add regC (length (variables!1) + 1) regC -- Skip local data area
            , Store regARP (IndAddr regC)   -- Caller's ARP
            , Compute Add regC reg0 regARP    -- Set mini-ARP to new scope
            ] 
            ++ concat ( map (\x -> codeGen x threads) astStats ) ++
            [ Load (IndAddr regARP) regARP
            ]
codeGen (ASTDecl vartype astVar@(ASTVar _ _) Nothing 
    checkType@(functions, globals, variables)) threads
        = (getMemAddr varNameStr variables) ++
            [ Store reg0 (IndAddr regE) ]
                where 
                    varNameStr = getStr astVar
codeGen (ASTDecl vartype astVar Nothing 
    checkType@(functions, globals, variables)) threads
        = codeGen astVar threads
codeGen (ASTDecl vartype astVar (Just astExpr) 
    checkType@(functions, globals, variables)) threads
        =   (codeGen astExpr threads) ++
            (getMemAddr varNameStr variables) ++
            [ Pop regD
            , Store regD (IndAddr regE) ]
                where 
                    varNameStr = getStr astVar
codeGen (ASTIf astExpr astThen Nothing
    checkType@(functions, globals, variables)) threads
        =   (codeGen astExpr threads) ++
            [ Pop regE
            , ComputeI Xor regE 1 regE
            , Branch regE (Rel (lengthNoDebug thenGen))] ++
            thenGen
                where thenGen = codeGen astThen threads
codeGen (ASTIf astExpr astThen (Just astElse) 
    checkType@(functions, globals, variables)) threads
        =   (codeGen astExpr threads) ++
            [ Pop regE
            , ComputeI Xor regE 1 regE
            , Branch regE (Rel (lengthNoDebug thenGen + 2))] ++
            thenGen
            ++ [ Jump (Rel ((lengthNoDebug elseGen) + 1))]
            ++ elseGen
                where   thenGen = codeGen astThen threads
                        elseGen = codeGen astElse threads
codeGen (ASTWhile astExpr astStat 
    checkType@(functions, globals, variables)) threads
        =   exprGen ++
            [ Pop regE
            , ComputeI Xor regE 1 regE
            , Branch regE (Rel (2 + (lengthNoDebug bodyGen)))] ++
            bodyGen ++
            [ Jump (Rel (-((lengthNoDebug (bodyGen ++ exprGen)) + 3)))]
                where 
                    exprGen = codeGen astExpr threads
                    bodyGen = codeGen astStat threads
codeGen (ASTFork pName astArgs 
    checkType@(functions, globals, variables)) threads
        =   [ TestAndSet (DirAddr regA)     -- Grab wr lock
            , Receive regE
            , Branch regE (Rel 2)           -- successful lock -> +2
            , Jump (Rel (-3))               -- otherwise try again
            ] ++ (concat $ map (\x -> codeGen x threads) $ astArgs) ++
            [ Load (ImmValue (length astArgs)) regD
            , ComputeI Add regD fork_record_args regC
            , Pop regE
            , WriteInstr regE (IndAddr regC)
            , ComputeI Equal regC fork_record_args regE
            , Compute Decr regC reg0 regC
            , Branch regE (Rel (-4))
            , WriteInstr regD (DirAddr fork_record_argc)
            , Debug ("**c" ++ pName)        -- line of jump addr. is not known
            , Debug ""
            , Pop regD
            , WriteInstr regD (DirAddr fork_record_jump)
            , WriteInstr reg0 (DirAddr fork_record_rd)
            ]
codeGen (ASTJoin 
    checkType@(functions, globals, variables)) threads
        =   [ Compute NEq reg0 regSprID regE
            , Branch regE (Rel 2)
            , Debug "***ParallelJoinError***"
            , Load (ImmValue fork_record_size) regB
            , Load (ImmValue 0) regA
            , ReadInstr (IndAddr regB)
            , Receive regC
            , Compute Add regA regC regA
            , ComputeI NEq regB (fork_record_size + threads) regE
            , Compute Incr regB reg0 regB
            , Branch regE (Rel (-6)) 
            , Compute Equal regA reg0 regE
            , Branch regE (Rel 2)
            , Jump (Rel (-10))
            ]
codeGen (ASTCall pName astArgs 
    checkType@(functions, globals, variables)) threads
        =   concat ( map (\x -> codeGen x threads) astArgs ) ++
            [ Load (IndAddr regARP) regC    -- Load ARP (now 0) = regC
            , ComputeI Add regC (length variables) regC -- Skip local data area
            , Load (ImmValue (length astArgs)) regD -- Read argcount = regD
            , Compute Equal regD reg0 regE  -- while still args left
            , Branch regE (Rel 7)           
            , Compute Incr regA reg0 regA
            , Pop regB                      -- Read argument
            , Store regB (IndAddr regC)     -- Store in local memory
            , Compute Incr regC reg0 regC
            , Compute Decr regD reg0 regD
            , Jump (Rel (-7))               -- Back to while
            , Compute Incr regC reg0 regC
            , Debug ("**r" ++ pName) -- return address
            , Debug ""
            , Pop regD                      --
            , Store regD (IndAddr regC)     -- to ARP
            , Compute Incr regC reg0 regC   --
            , Store regARP (IndAddr regC)   -- Caller's ARP
            , Load (IndAddr regC) regARP    -- Set ARP to new scope
            , Debug ("**c" ++ pName)          -- line of procedure is not known
            , Debug ""
            , Pop regA
            , Jump (Ind regA)               -- jump to procedure
            , Debug ("**a" ++ pName)
            ]
codeGen (ASTExpr astExpr _ 
    checkType@(functions, globals, variables)) threads
        =   codeGen astExpr threads ++ 
            [ Pop reg0 ]

codeGen (ASTAss astVar astExpr _
    checkType@(functions, globals, variables)) threads
        =   (codeGen astExpr threads) ++ (getMemAddr (getStr astVar) variables) ++
            [ Pop regA -- Expr result
            , Store regA (IndAddr regE)
            , Push regA
            ]
codeGen (ASTVar varName 
    checkType@(functions, globals, variables)) threads
        | (findVar (0,0) varName variables) /= ((-1),(-1))
            =   getMemAddr varName variables ++
                [ Load (IndAddr regE) regD
                , Push regD
                ]
        | otherwise 
            =   
            [ Load (ImmValue addr) regA         -- Load memory address of global's lock
            , TestAndSet (IndAddr regA)         -- Lock on ready bit
            , Receive regB                      --
            , Branch regB (Rel (-3))            -- Retry if lock fails
            , Load (ImmValue (addr + global_record_value)) 
                regC                            -- Load memory address of global value
            , ReadInstr (IndAddr regC)          -- Read value
            , Receive regD                      --
            , Push regD
            , WriteInstr reg0 (IndAddr regA)    -- Unlock value
            ]
            where
                addr = (global_record_size * (globalIndex varName globals))
codeGen (ASTInt value 
    checkType@(functions, globals, variables)) threads
        =   [ Load (ImmValue (read value :: Int)) regE
            , Push regE ]
codeGen (ASTBool value 
    checkType@(functions, globals, variables)) threads
        =   [ Load (ImmValue bool) regE
            , Push regE ]
            where
                bool = case value of 
                        "true"    -> 1
                        "false"   -> 0
                        otherwise -> error "Parse error on boolean when emitting codegen (ASTBool _ _ _)"
codeGen (ASTType typeStr 
    checkType@(functions, globals, variables)) threads
        = [Nop] -- Intentionally left blank
codeGen (ASTOp astL op astR _
    checkType@(functions, globals, variables)) threads
        =   (codeGen astL threads) ++ 
            (codeGen astR threads) ++
            [ Pop regB 
            , Pop regA
            , Compute operation regA regB regC
            , Push regC
            ]
            where
                operation = case op of
                        "=="    -> Equal
                        "!="    -> NEq
                        "&&"    -> And
                        "||"    -> Or
                        "<>"    -> Xor
                        "+"     -> Add
                        "-"     -> Sub
                        "*"     -> Mul
                        "<="    -> LtE
                        ">="    -> GtE
                        "<"     -> Lt
                        ">"     -> Gt
                        ">>"    -> RShift
                        "<<"    -> LShift
codeGen (ASTUnary op astV _
    checkType@(functions, globals, variables)) threads
        | op == "!" = 
            (codeGen astV threads) ++ 
            [ Pop regA
            , ComputeI Xor regA 1 regC
            , Push regC
            ]
        | op == "-" = 
            (codeGen astV threads) ++ 
            [ Pop regA
            , Compute Sub regA regA regC
            , Compute Sub regC regA regC
            , Push regC
            ]
        | op == "--" = 
            (codeGen astV threads) ++ 
            [ Pop regA
            , Compute Decr regA reg0 regC
            , Push regC
            ]
        | op == "++" = 
            (codeGen astV threads) ++ 
            [ Pop regA
            , Compute Incr regA reg0 regC
            , Push regC
            ]

codeGen (ASTPrint astExprs 
    checkType@(functions, globals, variables)) threads
    =   (concat $ map (\x -> codeGen x threads) $reverse astExprs) ++ 
        (concat $ replicate (length astExprs) 
            [ Pop regE
            , PrintOut regE
            ]
        )
-- Find the index of a given Global. Used to calculate global address in memory.
globalIndex :: String -> [VariableType] -> Int
globalIndex var [] = error $ "Global -|" ++ (show var) ++ "|- is not defined."
globalIndex var ((xStr,_):xs)   | var == xStr   = 0
                                | otherwise     = 1 + globalIndex var xs

-- Find the memory location of a given variable in local memory and store it in regE.
getMemAddr :: String -> [[VariableType]] -> [Instruction]
getMemAddr varStr variables 
    = [ Compute Add regARP reg0 regE] ++
      (replicate x (Load (IndAddr regARP) regE) ++
      [ ComputeI Add regE (y + 1) regE
      {-, Load (IndAddr regE) regE -}])
        where 
            (x,y) = findVar (0,0) varStr variables




findVar :: (Int,Int) -> String -> [[VariableType]] -> (Int,Int)
findVar _ str [] = ((-1),(-1)) --  error ("Variable -|" ++ str ++ "|- was not found when calling getMemAddr.") 
findVar (x,y) str ([]:scopes) = findVar (x+1,0) str scopes
findVar (x,y) str (scope@(var@(str2,_):vars):scopes)
    | str == str2   = (x,y)
    | otherwise     = findVar (x,y+1) str (vars:scopes)

sprILprpr :: [Instruction] -> String
sprILprpr = sprILprpr' 0

sprILprpr' :: Int -> [Instruction] -> String
sprILprpr' _ [] = ""
sprILprpr' i (x:xs) =  (show i) ++ "    " ++ (show x) ++ "\n" ++ (sprILprpr' (i+1) xs)


--- Removing debug pointers
type ProcPointer = (String, Int)
type CallPointer = (String, Int)

findPointers' :: [Instruction] -> ([Instruction], [ProcPointer], [CallPointer])
findPointers' instrss = findPointers instrss 0 

findPointers :: [Instruction] -> Int -> ([Instruction], [ProcPointer], [CallPointer])
findPointers [] _ = ([],[],[])
findPointers (x@(Debug ('*':'*':'a':cName)):xs) i = (resCode,(fPointers),((cName, i):cPointers))
    where (resCode,fPointers,cPointers) = findPointers xs (i)
findPointers (x@(Debug ('*':'*':'p':fName)):xs) i = (resCode,((fName, i):fPointers),(cPointers))
    where (resCode,fPointers,cPointers) = findPointers xs (i)
findPointers (x:xs) i = ((x:instrss),(fPointers),(cPointers))
    where (instrss,fPointers,cPointers) = findPointers xs (i+1)


insertFPointers :: [Instruction] -> ([ProcPointer], [CallPointer]) -> [Instruction]
insertFPointers []  _ = []
insertFPointers [x]  _ = [x]
insertFPointers (x@(Debug ('*':'*':'c':fName)):(Debug ""):xs) ptrs@(progP,_)
    =   [ Load (ImmValue ((Map.fromList progP)Map.!fName)) regE
        , Push regE
        ]
        ++ (insertFPointers xs ptrs)
insertFPointers (x:xs) ptrs = (x: (insertFPointers xs ptrs))


insertACallPointers :: [Instruction] -> ([ProcPointer], [CallPointer]) -> [Instruction]
insertACallPointers [] _ = []
insertACallPointers [x]  _ = [x]
insertACallPointers (x@(Debug ('*':'*':'r':cName)):(Debug ""):xs) (progP, callP)
    =   [ Load (ImmValue (v)) regE
        , Push regE
        ] ++ (insertACallPointers xs (progP, newCallP))
    where
        takeItem :: [CallPointer] -> String -> (Int, [CallPointer])
        takeItem [] _ = (999999,[])
        takeItem (x@(cNameStr,number):xs) str 
            | cNameStr == str = (number,xs)
            | otherwise = (nr,x:nxs)
                where (nr,nxs) = takeItem xs str
        
        (v,newCallP) = takeItem callP cName
insertACallPointers (x:xs) ptrs@(progP, callP)
    = x : (insertACallPointers xs ptrs) 

insertErrorPointers :: [Instruction] -> ([ProcPointer], [CallPointer]) -> [Instruction]
insertErrorPointers l _ = l

 -- Helper function to accurately determine the final length of a certain piece of instructions.
lengthNoDebug :: [Instruction] -> Int 
lengthNoDebug [] = 0
lengthNoDebug ((Debug ('*':'*':'p':_)):xs) = lengthNoDebug xs
lengthNoDebug ((Debug ('*':'*':'a':_)):xs) = lengthNoDebug xs
lengthNoDebug (_:xs) = 1 + lengthNoDebug xs 
