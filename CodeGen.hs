module CodeGen where

--import BasicFunctions
--import HardwareTypes
import Sprockell
import Types
import Constants
import Checker
import Data.Maybe
import Data.List

import qualified Data.Map.Strict as Map

import Debug.Trace

codeGen' int ast = insertACallPointers (insertFPointers instrss (pPointers, cPointers)) (pPointers, cPointers)
    where 
        (instrss, pPointers, cPointers) = findPointers' $ codeGen ast int

codeGen :: AST -> Int -> [Instruction]

codeGen (ASTProgram asts 
    checkType@(functions, globals, variables,_)) threads
        =   threadControl 
            ++
            -----------------------------------------
            -- Post-return for fork-run procedures --
            -----------------------------------------
            [ ComputeI Add regSprID fork_record_size regB 
                                                -- unset occupation bit 
            , WriteInstr reg0 (IndAddr regB)
            , Jump (Abs 9)                      -- Back to thread control loop
            ]
            ++ 
            procsCode                           -- Procedures
            ++ 
            globalsCode                         -- Global declarations
            ++ 
            exprsCode                           -- Main code
            ++ 
            -----------------------
            -- Post-program code --
            -----------------------
            [ Load (ImmValue 1) regA            -- Set endP record to 1
            , WriteInstr regA (DirAddr fork_record_endp)
            , EndProg                           -- Which tells threads to stop execution
            ]
        where
            threadControl = 
                [ Branch regSprID (Rel 6)       -- SprID > 0 --> thread control loop
                , TestAndSet (DirAddr (fork_record_rd))
                                                -- Set rd value to default state
                , Receive (regE)                --
                , Branch regE (Rel 2)           --
                , Jump (Rel (-3))               --
                , Jump (Rel begin_of_code)      -- Jump to global declarations, 
                                                -- followed by main code.
                
                -------------------------
                -- Thread Control Loop --
                -------------------------
                , ReadInstr (DirAddr fork_record_endp)
                , Receive regB                  -- Read end of program address
                , Compute Equal regB reg0 regE
                , Branch regE (Rel 2)           -- jump over endprog if not done
                , EndProg                       -- Endprog if done bit is set by main thread
                , TestAndSet (DirAddr fork_record_rd) -- Grab rd lock
                , Receive regE
                , Branch regE (Rel 2)           -- successful lock -> +2
                , Jump (Rel (-8))               -- otherwise try again
                
                ------------------------------
                -- Fork-run threads PreCall --
                ------------------------------
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
            
            begin_of_code = (lengthNoDebug (threadControl ++ procsCode)) - 2
                -- Computes line number that regular code starts at.
            (globalAsts, procexprs) = span isGlobal asts
            (procs, exprs) = span isProcedure procexprs
            procsCode = (concat $ map (\x -> codeGen x threads) procs) ++ [Nop, Nop]
            globalsCode = concat $ map (\x -> codeGen x threads) globalAsts
            exprsCode = concat $ map (\x -> codeGen x threads) exprs
            
            
            isProcedure :: AST -> Bool
            isProcedure (ASTProc _ _ _ _) = True
            isProcedure _ = False
            
            isGlobal :: AST -> Bool
            isGlobal (ASTGlobal _ _ _ _) = True
            isGlobal _ = False
                     
-- Declares a global in global memory, with its standard initial value .
-- Globals are saved from memory address 31 upwards, in pairs with a mutation bit. 
codeGen (ASTGlobal varType astVar Nothing 
    checkType@(functions, globals, variables,_)) threads
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
            
-- Same as function above, except that now an expression can be assigned to the variable.
codeGen (ASTGlobal varType astVar (Just astExpr) 
    checkType@(functions, globals, variables,_)) threads
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
               
codeGen (ASTEnum _ _ _) _
        = []
                    
-- Procedure node. Emits post-call and pre-return code that passes through variables.
codeGen (ASTProc pName astArgs astStat 
    checkType@(functions, globals, variables,_)) threads
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
                
-- Resolves the memory address of the argument (in local memory)
-- and pushes it to stack. Only found in procedure declarations, so 
-- it does not return its value, as one might expect.
codeGen (ASTArg astType astVar 
    checkType@(functions, globals, variables,_)) threads
        = getMemAddr astStr variables ++
            [ Push regE ]
            where
                astStr = getStr astVar
            
-- Starts a block statement. Any block is preceded by opening a new scope
-- with an AR-like structure, that simply contains a pointer to the 
-- higher scope's parent AR, followed by any variables declared 
-- in this scope.
codeGen (ASTBlock astStats 
    checkType@(functions, globals, variables,_)) threads
        =   [ Compute Add regARP reg0 regC      -- Load ARP = regC
            , ComputeI Add regC (length (variables!1) + 1) regC -- Skip local data area
            , Store regARP (IndAddr regC)       -- Caller's ARP
            , Compute Add regC reg0 regARP      -- Set mini-ARP to new scope
            ] 
            ++ concat ( map (\x -> codeGen x threads) astStats ) ++
            [ Load (IndAddr regARP) regARP      -- Exit scope by assigning parent's ARP
            ]
            
-- Declares a variable in the current scope. A free memory address has been created
-- by the block this variable belongs to, where the value is saved.
codeGen (ASTDecl vartype astVar@(ASTVar _ _) Nothing 
    checkType@(functions, globals, variables,_)) threads
        = (getMemAddr varNameStr variables) ++  -- 
            [ Store reg0 (IndAddr regE) ]       -- Initialises with 0
                where 
                    varNameStr = getStr astVar
codeGen (ASTDecl vartype astVar Nothing 
    checkType@(functions, globals, variables,_)) threads
        = codeGen astVar threads                -- Passed on to AST below, which
                                                -- is a primitive data type
codeGen (ASTDecl vartype astVar (Just astExpr) 
    checkType@(functions, globals, variables,_)) threads
        =   (codeGen astExpr threads) ++        -- Evaluate expression
            (getMemAddr varNameStr variables) ++
            [ Pop regD                          -- pop result
            , Store regD (IndAddr regE) ]       -- Initialises variable with result
                where 
                    varNameStr = getStr astVar
                    
                    
-- Starts an if-statement. Evaluates the expression, which decides which of the 
-- two blocks to execute (then- or else-block). If no second block exists, false
-- results in the if block will be skipped.
codeGen (ASTIf astExpr astThen Nothing
    checkType@(functions, globals, variables,_)) threads
        =   (codeGen astExpr threads) ++        -- Evaluate expression
            [ Pop regE
            , ComputeI Xor regE 1 regE          -- Invert value of expression to 
                                                -- fit with branch bahaviour
            , Branch regE (Rel (lengthNoDebug thenGen + 1))
            ] 
            ++ thenGen                          -- Content code
                where thenGen = codeGen astThen threads
codeGen (ASTIf astExpr astThen (Just astElse) 
    checkType@(functions, globals, variables,_)) threads
        =   (codeGen astExpr threads) ++        -- Evaluate expression
            [ Pop regE
            , ComputeI Xor regE 1 regE          -- Invert value of expression to 
                                                -- fit with branch bahaviour
            , Branch regE (Rel (lengthNoDebug thenGen + 2))
            ] 
            ++ thenGen                          -- Content behind If
            ++ [ Jump (Rel ((lengthNoDebug elseGen) + 1))] -- Jump over else
            ++ elseGen                          -- Contant behind else
                where   thenGen = codeGen astThen threads
                        elseGen = codeGen astElse threads
                        
                        
-- While code. As long as the expression is true, the code in the following block
-- will be executed. The expression is re-evaluated after each execution of the
-- block.
codeGen (ASTWhile astExpr astStat 
    checkType@(functions, globals, variables,_)) threads
        =   exprGen ++
            [ Pop regE
            , ComputeI Xor regE 1 regE
            , Branch regE (Rel (2 + (lengthNoDebug bodyGen)))] ++
            bodyGen ++
            [ Jump (Rel (-((lengthNoDebug (bodyGen ++ exprGen)) + 3)))]
                where 
                    exprGen = codeGen astExpr threads
                    bodyGen = codeGen astStat threads
                    
                    
-- Fork works like a normal call, except that it will be executed on a different
-- sprockell. Therefore its local arguments are passed on call-by-value. Any global
-- variables that are passed as arguments are referenced, but will only be visible
-- after the procedure behind the fork call finishes. This can be ensured with the
-- join keyword.
codeGen (ASTFork pName astArgs
    checkType@(functions, globals, variables,_)) threads
        | threads > 1 =    
            [ TestAndSet (DirAddr fork_record_wr)     -- Grab wr lock
            , Receive regE
            , Branch regE (Rel 2)           -- successful lock -> +2
            , Jump (Rel (-3))               -- otherwise try again
            ] ++ (concat $ reverse $ map (\x -> codeGen x threads) $ astArgs) ++
                                            -- evaluate arguments to stack
            [ Load (ImmValue (fork_record_args)) regC
                                            -- prepare record for emission
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
            
        | otherwise     = error ("Insufficient threads avaiable for parallel execution " ++
            "in fork call to " ++ pName)
        where
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
                , WriteInstr regB (IndAddr regC)    -- Store -1 as global memory pointer
                , Compute Incr regC reg0 regC
                , Load (ImmValue (-1)) regB     -- Store -1 as local memory pointer
                , WriteInstr regB (IndAddr regC)
                , Compute Incr regC reg0 regC ]
                ++ forkArgRecords xs variables globals threads
-- Join statement, iterates over all thread's occupation bits and ensures that 
-- they are all zero before continuing.
codeGen (ASTJoin 
    checkType@(functions, globals, variables,_)) threads
        =   [ Compute Equal reg0 regSprID regE
            , Branch regE (Rel 4)
            , Load (ImmValue 2) regA
            , WriteInstr regA numberIO
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
            
            
-- Calls a procedure sequentially. Any separate variables that are given as an 
-- argument are handled call-by-reference. Sets up the AR before jumping to the
-- procedure.
codeGen (ASTCall pName astArgs -- TODO: Rewrite stuff so it actally pushes the indexes we need
    checkType@(functions, globals, variables,_)) threads
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
            
-- Naked expression as a statement. Executes the expression, and pops the value
-- it produced as it is not needed anymore and would fill up the stack otherwise.
codeGen (ASTExpr astExpr _ 
    checkType@(functions, globals, variables,_)) threads
        =   codeGen astExpr threads ++ 
            [ Pop reg0 ]

-- Assignment. Local variables are stored in their declared AR's, global variables
-- are written (but NOT assigned!) atomically.
codeGen (ASTAss astVar astExpr _
    checkType@(functions, globals, variables,_)) threads
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
            , Push regE                         -- Push to stack, until assignments are not expressions anymore!
            ]
            where
                name = (getStr astVar)
                gIndex = globalIndex name globals
                addr = fork_record_size + threads 
                    + (global_record_size * (globalIndex (getStr astVar) globals))


-- A naked variable, as used in expressions. Pushes its value to stack. Global
-- variables are read atomically.
codeGen (ASTVar varName 
    checkType@(functions, globals, variables,enums)) threads
        | (findVar (0,0) varName variables) /= ((-1),(-1)) =
            getMemAddr varName variables ++
            [ Load (IndAddr regE) regD          -- Load variable value from local mem
            , Push regD                         -- Push to stack
            ]
        | enum > -1 =
            [ Load (ImmValue enum) regE         -- Load enumeration index value of enum
            , Push regE ]                       -- push to stack
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
                enum = findEnum varName 0 enums
                addr = fork_record_size + threads 
                    + (global_record_size * (globalIndex varName globals))
                    
                    
-- Literal integer value, is pushed to the stack.
codeGen (ASTInt value 
    checkType@(functions, globals, variables,_)) threads
        =   [ Load (ImmValue (read value :: Int)) regE  -- Load the integer value read from String
            , Push regE ]                       -- Push to stack


-- Literal boolean value, is pushed to the stack.
codeGen (ASTBool value 
    checkType@(functions, globals, variables,_)) threads
        =   [ Load (ImmValue bool) regE         -- Load boolean value read from String
            , Push regE ]                       -- Push to stack
            where
                bool = case value of  -- Interpret string value of boolean type
                        "true"    -> 1
                        "false"   -> 0
                        otherwise -> error "Parse error on boolean when emitting codegen (ASTBool _ _ _)"
codeGen (ASTType typeStr 
    checkType@(functions, globals, variables,_)) threads
        = [Nop] -- Intentionally left blank; never called.


-- Binary operation. Evaluates both expressions in order, calculates the result 
-- and pushes its result to the stack.
codeGen (ASTOp astL op astR _
    checkType@(functions, globals, variables,_)) threads
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


-- Unary Operator. Evaluates the expression, calculates the result and pushes 
-- the result to the stack.
codeGen (ASTUnary op astV _
    checkType@(functions, globals, variables,_)) threads
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

-- Print statement. Prints its arguments in order, one argument per line. Uses the
-- self-defined non-standard SprIL instruction PrintOut, that uses trace to print
-- the value to stdout.
codeGen (ASTPrint astExprs 
    checkType@(functions, globals, variables,_)) threads
    =   (concat $ map (\x -> codeGen x threads) $reverse astExprs) ++ 
                                            -- Compute arguments (in reverse order
                                            -- because of stack behaviour)
        (concat $ replicate (length astExprs) -- Replicate the following code for 
                                            -- each expression:
            [ Pop regE                      -- pop argument
            , WriteInstr regE numberIO
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



-- Finds the closest declaration of a variable by its name and the checker's scope
-- state for a given node. 
-- First argument is the number of scopes one must traverse upwards
-- Second argument is the offset from the ARP in that scope
findVar :: (Int,Int) -> String -> [[VariableType]] -> (Int,Int)
findVar _ str [] = ((-1),(-1))
findVar (x,y) str ([]:scopes) = findVar (x+1,0) str scopes
findVar (x,y) str (scope@(var@(str2,_):vars):scopes)
    | str == str2   = (x,y)
    | otherwise     = findVar (x,y+1) str (vars:scopes)

-- Find enumeration index value
-- usage: <enum string> 0 <enumcheckertype>
findEnum :: String -> Int -> [EnumCheckerType] -> Int
findEnum str _ []                 
    = -1
findEnum str i ((_,enum):enums)   
    | isJust index  = fromJust index + i
    | otherwise     = findEnum str (i + length enum) enums
    where
        index = elemIndex str enum

-- Prettyprints SprIL code
sprILprpr :: [Instruction] -> String
sprILprpr = sprILprpr' 0

sprILprpr' :: Int -> [Instruction] -> String
sprILprpr' _ [] = ""
sprILprpr' i (x:xs) =  (show i) ++ "    " ++ (show x) ++ "\n" ++ (sprILprpr' (i+1) xs)


--- Removing debug pointers
type ProcPointer = (String, Int)
type CallPointer = (String, Int)

-- Second pass in the code generator. 
-- 
-- Finds pointers placed in the code by their uniqe debug instructions.
--
-- **a : After-call pointer used to locate the return address for a call.
-- **p : Procedure pointer used to locate the jump address for a call or fork.
--
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

-- Inserts found procedure pointer in the code by the keyword '**c'. Pushes it to stack.
insertFPointers :: [Instruction] -> ([ProcPointer], [CallPointer]) -> [Instruction]
insertFPointers []  _ = []
insertFPointers [x]  _ = [x]
insertFPointers (x@(Debug ('*':'*':'c':fName)):(Debug ""):xs) ptrs@(progP,_)
    =   [ Load (ImmValue ((Map.fromList progP)Map.!fName)) regE
        , Push regE
        ]
        ++ (insertFPointers xs ptrs)
insertFPointers (x:xs) ptrs = (x: (insertFPointers xs ptrs))

-- Inserts found call return addresses in the code by the keyword '**r'. Pushes it to stack.
insertACallPointers :: [Instruction] -> ([ProcPointer], [CallPointer]) -> [Instruction]
insertACallPointers [] _ = []
insertACallPointers [x]  _ = [x]
insertACallPointers (x@(Debug ('*':'*':'r':cName)):(Debug ""):xs) (progP, callP)
    =   [ Load (ImmValue (v)) regE
        , Push regE
        ] ++ (insertACallPointers xs (progP, newCallP))
    where
        (v,newCallP) = takeItem callP cName
        takeItem :: [CallPointer] -> String -> (Int, [CallPointer])
        takeItem [] _ = (999999,[])
        takeItem (x@(cNameStr,number):xs) str 
            | cNameStr == str = (number,xs)
            | otherwise = (nr,x:nxs)
                where (nr,nxs) = takeItem xs str
        
-- fall-through
insertACallPointers (x:xs) ptrs@(progP, callP)
    = x : (insertACallPointers xs ptrs) 


 -- Helper function to accurately determine the final length of a certain piece 
 -- Needed because aforementioned labels may occur in the code that do not count
 -- for the final code length.
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
