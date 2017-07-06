import Sprockell

{-|
    This program demonstrates the numeric IO system.

    We ask the user for a number.
    And compute and output all the Fibonacci numbers smaller that that number.
-}

prog :: [Instruction]
prog = [ ReadInstr numberIO            -- ask the user for a number
       , Receive regE                  -- save the number in regE

       , Load (ImmValue 0) regA        -- first number
       , Load (ImmValue 1) regB        -- second number

       -- "beginloop"
       , Compute Gt regA regE regC     -- regA > regE ?
       , Branch regC (Abs 13)          -- then jump to target "end"
       , WriteInstr regA numberIO      -- output regA
       , Compute Add regA regB regA
       , Compute Gt regB regE regC     -- regB > regE
       , Branch regC (Abs 13)          -- target "end"
       , WriteInstr regB numberIO      -- output regB
       , Compute Add regA regB regB
       , Jump (Rel (-8))               -- target "beginloop"

       -- "end"
       , EndProg
       ]

-- run the prog on 1 Sprockell core
main = run [prog]
