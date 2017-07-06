import Sprockell
import Data.Char


{-|
    This program demonstrates the character IO system.

    We ask the users name.
    And print a custom greeting.
-}
prog :: [Instruction]
prog =
    writeString "What is your name? " ++
    [ Load (ImmValue $ ord '\n') regE   -- ASCII code newline in regE for later reference

    -- "beginInputLoop": 39
    , ReadInstr charIO                  -- Request a character from stdin
    , Receive regA                      -- Save it in regA (as ASCII code)
    , Branch regA (Rel 2)
    , Jump (Rel (-3))                   -- got 0, no input available, try again

    -- got input char
    , Compute Equal regA regE regC      -- check if it's a newline (remember: in regE)
    , Branch regC (Rel 4)               -- then jump to "inputDone"
    , Store regA (IndAddr regB)         -- else store character in local memory
    , Compute Incr regB regB regB
    , Jump (Rel (-8))                   -- "beginInputLoop"
    ]
    -- "inputDone"
    ++ writeString "Hello "
    ++
    -- "beginLoopOutput"
    [ Load (IndAddr regD) regA
    , WriteInstr regA charIO
    , Compute Incr regD regD regD
    , Compute NEq regB regD regC
    , Branch regC (Rel (-4))            -- target "loopOutput"
    ]
    ++ writeString "!\n"
    ++ [EndProg]


-- | Generate code to print a (Haskell) String
writeString :: String -> [Instruction]
writeString str = concat $ map writeChar str

-- | Generate code to print a single character
writeChar :: Char -> [Instruction]
writeChar c =
    [ Load (ImmValue $ ord c) regA
    , WriteInstr regA charIO
    ]



-- =====================================================================
-- examples for running with/without debug information
-- =====================================================================

-- main: run without debugger
main = run [prog]

-- main_0: no debug information
main_0 = runWithDebugger noDebugger [prog]


-- main_1: shows all intermediate local states
main_1 = runWithDebugger (debuggerSimplePrint showLocalMem) [prog]

showLocalMem :: DbgInput -> String
showLocalMem ( _ , systemState ) = show $ localMem $ head $ sprStates systemState


-- main_2: shows local state only in case the sprockell writes to it
main_2 = runWithDebugger (debuggerPrintCondWaitCond showLocalMem doesLocalMemWrite never) [prog]

doesLocalMemWrite :: DbgInput -> Bool
doesLocalMemWrite (instrs,st) = any isStoreInstr instrs
    where
        isStoreInstr (Store _ _) = True
        isStoreInstr _           = False
