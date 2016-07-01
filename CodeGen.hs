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
        =   threadControl ++
            [ ComputeI Add regSprID fork_record_size regB 
                                            -- unset occupation bit 
            , WriteInstr reg0 (IndAddr regB)
            , Jump (Abs 9)               -- Back to thread control loop
            ]
            ++ procsCode ++ globalsCode ++ exprsCode ++ 
            [ Load (ImmValue 1) regA
            , WriteInstr regA (DirAddr fork_record_endp)
            , EndProg
            ]
        where
            threadControl = 
                [ Compute Equal regSprID reg0 regE
                , Branch regE (Rel 2)
                , Jump (Rel 7)
                , TestAndSet (DirAddr (fork_record_rd))
                , Receive (regE)
                , Branch regE (Rel 2)
                , Jump (Rel (-3))
                , Load (ImmValue 0) regARP
                , Jump (Rel begin_of_code)
                
                , ReadInstr (DirAddr fork_record_endp)
                , Receive regB                  -- Read end of program address
                , Compute Equal regB reg0 regE
                , Branch regE (Rel 2)           -- jump over endprog if not done
                , EndProg                       -- Endprog if done bit is set by main thread
                , TestAndSet (DirAddr fork_record_rd) -- Grab rd lock
                , Receive regE
                , Branch regE (Rel 2)           -- successful lock -> +2
                , Jump (Rel (-8))               -- otherwise try again
                
                
                , ComputeI Add regSprID fork_record_size regB 
                                                -- compute thread occupation bit 
                , TestAndSet (IndAddr regB)     -- Grab occupation bit
                , Receive regE
                , Branch regE (Rel 2)           -- successfully set -> +2
                , Jump (Rel (-3))               -- otherwise try again
                , ReadInstr (DirAddr fork_record_jump) -- Read jump address
                , Receive regB                  -- 
                , Push regB                     -- Need dem registers...
                , ComputeI Add regARP 1 regC    -- Load ARP + 1 = regC
                , ReadInstr (DirAddr fork_record_argc)      -- Read argcount = regD
                , Receive regD
                , Load (ImmValue fork_record_args) regA
                                                -- set regA to first argument pointer
                
                , Compute Equal regD reg0 regE  -- while still args left
                , Branch regE (Rel 18)           
                , ReadInstr (IndAddr regA)      -- Read argument
                , Receive regB
                , Store regB (IndAddr regC)     -- Store in local memory
                , Compute Incr regA reg0 regA
                , Compute Incr regC reg0 regC
                , ReadInstr (IndAddr regA)      -- Read global_arg_addr
                , Receive regB
                , Store regB (IndAddr regC)     -- Store in local memory
                , Compute Incr regA reg0 regA
                , Compute Incr regC reg0 regC
                , ReadInstr (IndAddr regA)      -- Read local_arg_addr
                , Receive regB
                , Store regB (IndAddr regC)     -- Store in local memory
                , Compute Incr regA reg0 regA
                , Compute Incr regC reg0 regC
                , Compute Decr regD reg0 regD
                , Jump (Rel (-18))               -- Back to while
                , Load (ImmValue (length threadControl)) regD        
                                                -- return address
                , Store regD (IndAddr regC)
                , Compute Incr regC reg0 regC
                , Store regARP (IndAddr regC)   -- Caller's ARP
                , Compute Add regC reg0 regARP  -- Set ARP to new scope
                , Pop regA                      -- pop procedure address
                , WriteInstr reg0 (DirAddr fork_record_wr)    
                                                -- Unset wr lock, so another 
                                                -- thread can be served
                , Jump (Ind regA)               -- jump to procedure
                ]
            
            begin_of_code = (lengthNoDebug (threadControl ++ procsCode)) - 5
            (globalAsts, procexprs) = span isGlobal asts
            (procs, exprs) = span isProcedure procexprs
            procsCode = concat $ map (\x -> codeGen x threads) procs
            globalsCode = concat $ map (\x -> codeGen x threads) globalAsts
            exprsCode = concat $ map (\x -> codeGen x threads) exprs
            
            
            isProcedure :: AST -> Bool
            isProcedure (ASTProc _ _ _ _) = True
            isProcedure _ = False
            
            isGlobal :: AST -> Bool
            isGlobal (ASTGlobal _ _ _ _) = True
            isGlobal _ = False
                     
codeGen (ASTGlobal varType astVar Nothing 
    checkType@(functions, globals, variables)) threads
        =   [ Load (ImmValue addr) regA         -- Load memory address of global's lock
            , TestAndSet (IndAddr regA)         -- Lock on ready bit
            , Receive regB                      --
            , Branch regB (Rel 2)               -- Retry if lock fails
            , Jump (Rel (-3))                   --
            , Load (ImmValue (addr + global_record_value)) 
                regC                            -- Load memory address of global value
            , WriteInstr reg0 (IndAddr regC)    -- Write value
            , WriteInstr reg0 (IndAddr regA)    -- Unlock value
            ]
            where
                addr = fork_record_size + threads 
                    + (global_record_size * (globalIndex (getStr astVar) globals))
            
codeGen (ASTGlobal varType astVar (Just astExpr) 
    checkType@(functions, globals, variables)) threads
        =   (codeGen astExpr threads) ++
            [ Pop (regE)                        -- pop value from expression
            , Load (ImmValue addr) regA         -- Load memory address of global's lock
            , TestAndSet (IndAddr regA)         -- Lock on ready bit
            , Receive regB                      --
            , Branch regB (Rel (2))             -- Retry if lock fails
            , Jump (Rel (-3))  
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
        =   [ Debug ("**p" ++ pName) ] 
            ++ 
            -- PostCall: Load args in local data area
            [ Load (ImmValue (1 + proc_arp_argrec_size * (length astArgs))) regA
                    -- Value to align with first variable in AR
                    --   -- plus 1 to skip over return address
            , Compute Sub regARP regA regA
            , Load (ImmValue 1) regD            -- regD is number of arguments already done
            
            , ComputeI Gt regD (length astArgs) regE -- if all arguments done
            , Branch regE (Rel 7)               -- jump over folowing code:
            , Load (IndAddr regA) regB          -- Load argument value
            , Compute Add regARP regD regE      -- Load addr in local data area
            , Store regB (IndAddr regE)
            , Compute Incr regD reg0 regD       -- increment arg counter
            , ComputeI Add regA proc_arp_argrec_size regA          
                                                -- Jump over to next arg's value
            , Jump (Rel (-7))                   -- Back to while
            
            ]
            ++ codeGen astStat threads ++       -- Emit procedure contents
            -- PreReturn:
            -- Save argument values from local data area to given memaddrs to
            -- implement call-by-reference
            [ Load (ImmValue (1 + proc_arp_argrec_size * (length astArgs) -
                                                 proc_arp_arg_l_addr)) regA
                    -- Value to align with first variable in AR
                    --   -- plus 1 to skip over return address
            , Compute Sub regARP regA regA
            , ComputeI Add reg0 1 regD        -- regD is number of arguments already done
            
            , ComputeI Gt regD (length astArgs) regE -- if all arguments done
            , Branch regE (Rel (23))            -- Jump over next code:
            , Compute Add regARP regD regE      -- Load addr for value
            , Load (IndAddr regE) regC          -- Load final value of arg
            , Load (IndAddr regA) regB          -- Load local addr record
            , Compute Lt regB reg0 regE         -- local addr is a valid address
            , Branch regE (Rel 2)               -- 
            , Store regC (IndAddr regB)         -- Save value to local address
            , Compute Incr regA reg0 regA       -- Move argrec pointer to global addr
            , Load (IndAddr regA) regB          -- Load global addr record
            , Compute Lt regB reg0 regE         -- global addr is a valid address
            , Branch regE (Rel 10)              -- 
            
            , Compute Add regB reg0 regE        -- Load memory address of global's lock
            , TestAndSet (IndAddr regE)         -- Lock on ready bit
            , Receive regE                      --
            , Branch regE (Rel (2))             -- Retry if lock fails
            , Jump (Rel (-4))
            , ComputeI Add regB global_record_value regB
                                                -- Load memory address of global value
            , WriteInstr regC (IndAddr regB)    -- Write value
            , ComputeI Sub regB global_record_value regB
                                                -- Load memory address of global value
            , WriteInstr reg0 (IndAddr regB)    -- Unlock value
            
            , Compute Incr regD reg0 regD       -- increment arg counter
            , ComputeI Add regA 2 regA          -- Jump over to next arg's local addr
            , Jump (Rel (-23))                  -- Back to condition
            ] ++
            [ Compute Decr regARP reg0 regA 
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
            , Branch regE (Rel (lengthNoDebug thenGen + 1))] ++
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
codeGen (ASTFork pName astArgs -- TODO: Add code to insert global and local variables to write to
    checkType@(functions, globals, variables)) threads
        =   
            [ TestAndSet (DirAddr fork_record_wr)     -- Grab wr lock
            , Receive regE
            , Branch regE (Rel 2)           -- successful lock -> +2
            , Jump (Rel (-3))               -- otherwise try again
            ] ++ (concat $ reverse $ map (\x -> codeGen x threads) $ astArgs) ++
            [ Load (ImmValue (fork_record_args)) regC
            ]
            ++ (forkArgRecords astArgs variables globals threads) ++
            [ Load (ImmValue (length astArgs)) regD
            , WriteInstr regD (DirAddr fork_record_argc)
            , Debug ("**c" ++ pName)        -- line of jump addr. is not known
            , Debug ""
            , Pop regD
            , WriteInstr regD (DirAddr fork_record_jump)
            , WriteInstr reg0 (DirAddr fork_record_rd)
            
            , Load (ImmValue fork_record_wr) regB
                                            -- load write lock address
            , ReadInstr (IndAddr regB)      -- peek at writeLock
            , Receive regE
            , Branch regE (Rel 2)           -- if zero: continue;
            , Jump (Rel (-3))               -- otherwise wait for thread to start our fork instance
            ]
codeGen (ASTJoin 
    checkType@(functions, globals, variables)) threads
        =   [ Compute Equal reg0 regSprID regE
            , Branch regE (Rel 4)
            , Load (ImmValue 2) regA
            , PrintOut regA
            , EndProg
            , Load (ImmValue fork_record_size) regB
            , Load (ImmValue 0) regA
            , ReadInstr (IndAddr regB)
            , Receive regC
            , Compute Add regA regC regA
            , ComputeI NEq regB (fork_record_size + threads) regE
            , Compute Incr regB reg0 regB
            , Branch regE (Rel (-5)) 
            , Compute Equal regA reg0 regE
            , Branch regE (Rel 2)
            , Jump (Rel (-10))
            ]
codeGen (ASTCall pName astArgs -- TODO: Rewrite stuff so it actally pushes the indexes we need
    checkType@(functions, globals, variables)) threads
        =   concat ( reverse $ map (\x -> codeGen x threads) astArgs ) ++
            [ Compute Add regARP reg0 regC    -- Load ARP = regC
            , ComputeI Add regC ((length (variables!0)) + 1) regC -- Skip local data area
            , Load (ImmValue (length astArgs)) regD -- Read argcount = regD
            ] 
            ++ (emitArgRecords astArgs variables globals threads) ++
            [ Debug ("**r" ++ pName) -- return address
            , Debug ""
            , Pop regD                      --
            , Store regD (IndAddr regC)     -- to ARP
            , Compute Incr regC reg0 regC   --
            , Store regARP (IndAddr regC)   -- Caller's ARP
            , Compute Add regC reg0 regARP  -- Set ARP to new scope
            , Debug ("**c" ++ pName)        -- line of procedure is not known
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
        | (findVar (0,0) (getStr astVar) variables) /= ((-1),(-1)) =
            (codeGen astExpr threads) ++ (getMemAddr (getStr astVar) variables) ++
            [ Pop regA -- Expr result
            , Store regA (IndAddr regE)
            , Push regA
            ]
        | otherwise =
            (codeGen astExpr threads) ++ 
            [ Load (ImmValue addr) regA         -- Load memory address of global's lock
            , TestAndSet (IndAddr regA)         -- Lock on ready bit
            , Receive regB                      --
            , Branch regB (Rel 2)               -- Retry if lock fails
            , Jump (Rel (-4))                   -- 
            , Load (ImmValue (addr + global_record_value)) 
                regC                            -- Load memory address of global value
            , Pop regE                          -- Pop expression value
            , WriteInstr regE (IndAddr regC)    -- Write value
            , WriteInstr reg0 (IndAddr regA)    -- Unlock value
            ]
            where
                name = (getStr astVar)
                gIndex = globalIndex name globals
                addr = fork_record_size + threads 
                    + (global_record_size * (globalIndex (getStr astVar) globals))
codeGen (ASTVar varName 
    checkType@(functions, globals, variables)) threads
        | (findVar (0,0) varName variables) /= ((-1),(-1)) =
            getMemAddr varName variables ++
            [ Load (IndAddr regE) regD          -- Load variable value from local mem
            , Push regD                         -- Push to stack
            ]
        | otherwise =
            [ Load (ImmValue addr) regA         -- Load memory address of global's lock
            , TestAndSet (IndAddr regA)         -- Lock on ready bit
            , Receive regB                      --
            , Branch regB (Rel (2))
            , Jump (Rel (-4))                   -- Retry if lock fails
            , Load (ImmValue (addr + global_record_value)) 
                regC                            -- Load memory address of global value
            , ReadInstr (IndAddr regC)          -- Read value
            , Receive regD                      --
            , Push regD
            , WriteInstr reg0 (IndAddr regA)    -- Unlock value
            ]
            where
                addr = fork_record_size + threads 
                    + (global_record_size * (globalIndex varName globals))
codeGen (ASTInt value 
    checkType@(functions, globals, variables)) threads
        =   [ Load (ImmValue (read value :: Int)) regE  -- Load the integer value read from String
            , Push regE ]                       -- Push to stack
codeGen (ASTBool value 
    checkType@(functions, globals, variables)) threads
        =   [ Load (ImmValue bool) regE         -- Load boolean value read from String
            , Push regE ]                       -- Push to stack
            where
                bool = case value of  -- Interpret string value of boolean type
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
            [ Pop regB                          -- Pop the two arguments
            , Pop regA
            , Compute operation regA regB regC  -- Perform operation
            , Push regC                         -- Push result to stack
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
            (codeGen astV threads) ++   -- compute argument
            [ Pop regA                  -- pop argument
            , ComputeI Xor regA 1 regC  -- XOR with 1 gives the inverse boolean value
            , Push regC                 -- push result to stack
            ]
        | op == "-" = 
            (codeGen astV threads) ++       -- compute argument
            [ Pop regA                      -- pop argument
            , Compute Sub regA regA regC    -- Subtracting the value from itself twice
            , Compute Sub regC regA regC    -- gives the inverse value.
            , Push regC                     -- push result to stack
            ]
        | op == "--" = 
            (codeGen astV threads) ++       -- compute argument
            [ Pop regA                      -- pop argument
            , Compute Decr regA reg0 regC   -- Decrement it
            , Push regC                     -- Push result to stack
            ]
        | op == "++" = 
            (codeGen astV threads) ++       -- Compute argument
            [ Pop regA                      -- pop argument
            , Compute Incr regA reg0 regC   -- increment it
            , Push regC                     -- push result to stack
            ]

codeGen (ASTPrint astExprs 
    checkType@(functions, globals, variables)) threads
    =   (concat $ map (\x -> codeGen x threads) $reverse astExprs) ++ 
                                            -- Compute arguments (in reverse order
                                            -- because of stack behaviour)
        (concat $ replicate (length astExprs) -- Replicate the following code for 
                                            -- each expression:
            [ Pop regE                      -- pop argument
            , PrintOut regE                 -- Print value
            ]
        )
-- Find the index of a given Global. Used to calculate global address in memory.
globalIndex :: String -> [VariableType] -> Int
globalIndex var xs  | Map.notMember var vars    = -1
                    | otherwise                 = (Map.findIndex var vars)
            where vars = Map.fromList xs
-- Find the memory location of a given variable in local memory and store it in regE.
getMemAddr :: String -> [[VariableType]] -> [Instruction]
getMemAddr varStr variables 
    = [ Compute Add regARP reg0 regE] ++
      (replicate x (Load (IndAddr regE) regE) ++
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

-- | Builds up the instructions to add the record of an 
-- | argument. Argument value MUST be on the stack, and C
-- | must contain the pointer to where we can write the 
-- | record.
emitArgRecords :: [AST] -> [[VariableType]] -> [VariableType] -> Int -> [Instruction]
emitArgRecords [] _ _ _ = []
emitArgRecords (x@(ASTVar name _) : xs) variables globals threads =
    [ Pop regB                      -- Store value
    , Store regB (IndAddr regC)
    , Compute Incr regC reg0 regC   
    ] 
    ++ emitLocalArg coords ++ 
    [ Compute Incr regC reg0 regC 
    , Load (ImmValue (gIndex)) regB
    , Store regB (IndAddr regC)     -- Store global memory pointer in local 
                                    -- memory (if argument has one...)
    , Compute Incr regC reg0 regC
    ]
    ++ emitArgRecords xs variables globals threads
    where
        gIndex  | globalIndex name globals >= 0 =
             fork_record_size + threads + (global_record_size * (globalIndex name globals))
                | otherwise = (-1)
        coords = findVar (0,0) name variables
        
        emitLocalArg (-1,-1) = 
            [ Load (ImmValue (-1)) regB 
            , Store regB (IndAddr regC) 
            ]
        emitLocalArg (x,y) = 
            [ Compute Add regARP reg0 regE] ++
            (replicate x (Load (IndAddr regE) regE) ++
            [ ComputeI Add regE (y + 1) regE
            , Store regE (IndAddr regC)
            ])
emitArgRecords (x : xs) variables globals threads =
    [ Pop regB                      -- Store value
    , Store regB (IndAddr regC)
    , Compute Incr regC reg0 regC   
    
    , Load (ImmValue (-1)) regB
    , Store regB (IndAddr regC)     -- Store -1 as global memory pointer
    , Compute Incr regC reg0 regC
    , Load (ImmValue (-1)) regB     -- Store -1 as local memory pointer
    , Store regB (IndAddr regC)
    , Compute Incr regC reg0 regC ]
    ++ emitArgRecords xs variables globals threads
        
forkArgRecords :: [AST] -> [[VariableType]] -> [VariableType] -> Int -> [Instruction]
forkArgRecords [] _ _ _ = []
forkArgRecords (x@(ASTVar name _) : xs) variables globals threads =
    [ Pop regB                      -- Store value
    , WriteInstr regB (IndAddr regC)
    , Compute Incr regC reg0 regC   
    ] 
    ++ emitLocalArg coords ++ 
    [ Compute Incr regC reg0 regC 
    , Load (ImmValue (gIndex)) regB
    , WriteInstr regB (IndAddr regC)     -- Store global memory pointer in local 
                                    -- memory (if argument has one...)
    , Compute Incr regC reg0 regC
    ]
    ++ forkArgRecords xs variables globals threads
    where
        gIndex  | globalIndex name globals >= 0 =
             fork_record_size + threads + (global_record_size * (globalIndex name globals))
                | otherwise = (-1)
        coords = findVar (0,0) name variables
        
        emitLocalArg (-1,-1) = 
            [ Load (ImmValue (-1)) regB 
            , WriteInstr regB (IndAddr regC) 
            ]
        emitLocalArg (x,y) = 
            [ Compute Add regARP reg0 regE] ++
            (replicate x (Load (IndAddr regE) regE) ++
            [ ComputeI Add regE (y + 1) regE
            , WriteInstr regE (IndAddr regC)
            ])
forkArgRecords (x : xs) variables globals threads =
    [ Pop regB                      -- Store value
    , WriteInstr regB (IndAddr regC)
    , Compute Incr regC reg0 regC   
    
    , Load (ImmValue (-1)) regB
    , WriteInstr regB (IndAddr regC)     -- Store -1 as global memory pointer
    , Compute Incr regC reg0 regC
    , Load (ImmValue (-1)) regB     -- Store -1 as local memory pointer
    , WriteInstr regB (IndAddr regC)
    , Compute Incr regC reg0 regC ]
    ++ forkArgRecords xs variables globals threads
        


-- | Builds up the instructions to add the global index of an 
-- | argument i it has one, or -1 if it doesn't. Used to write
-- | back arguments, to realise call-by-reference.
emitGlobalWrites :: [AST] -> [VariableType] -> [Instruction]
emitGlobalWrites [] _ = []
emitGlobalWrites (x@(ASTVar name _) : xs) globals = 
    [ Load (ImmValue (gIndex)) regB
    , Store regB (IndAddr regC)     -- Store in local memory
    , Compute Incr regC reg0 regC
    ] ++ emitGlobalWrites xs globals
    where
        gIndex = globalIndex name globals
