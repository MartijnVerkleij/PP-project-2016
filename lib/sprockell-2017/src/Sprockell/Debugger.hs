module Sprockell.Debugger where
import Sprockell.HardwareTypes
import Control.Monad (when, void)
import Text.Printf

-- | A Debugger will get this as input each clock cycle:
--   * a list of current 'Instruction's, one for each Sprockell core
--   * the 'SystemState' after executing those instructions
type DbgInput = ([Instruction], SystemState)


-- | A DebuggerF represents a debugger for the Sprockell system
--
-- It's a function from a abstract debugger state and a 'DebuggerInput'
-- to an IO action returning an potentially updated debugger state and SystemState.
type DebuggerF st = st -> DbgInput -> IO (st, SystemState)

-- |  A Debugger is a combination of a 'DebuggerF' and its initial state.
type Debugger  st = (DebuggerF st, st)



-- | No debugging at all, just run the system.
noDebugger :: Debugger ()
noDebugger = (noDebuggerF,())
    where
        noDebuggerF st (_,sys) = return (st,sys)

-- | Given a show function for the SystemState,
--   this creates a 'Debugger' that prints the 'SystemState'
--   at each time step using the supplied show function.
debuggerSimplePrint :: (DbgInput -> String) -> Debugger ()
debuggerSimplePrint showF = debuggerPrintCondWaitCond showF always never

-- | Like 'debuggerSimplePrint',
--   but pauses execution after each time step
--   and waits for you to hit enter to continue.
--   It let you step through the execution slowly.
debuggerSimplePrintAndWait :: (DbgInput -> String) -> Debugger ()
debuggerSimplePrintAndWait showF = debuggerPrintCondWaitCond showF always always


debuggerPrintCondWaitCond :: (DbgInput -> String) -- ^ show function used to show (parts of) the state
                          -> (DbgInput -> Bool)   -- ^ when this condition is True the state is printed
                          -> (DbgInput -> Bool)   -- ^ when this condition is True execution pauses
                          -> Debugger ()
debuggerPrintCondWaitCond showF showCond waitCond = (dbg, ())
    where
        dbg dbgSt now@(instrs,sysSt) = do
            when (showCond now) (putStrLn $ showF now)
            when (waitCond now) (void getLine) -- wait for enter key
            return (dbgSt,sysSt)


-- ======================================================================
-- examples of show functions that you could with the above debuggers
-- ======================================================================
myShow :: DbgInput -> String
myShow (instrs,s) = printf "instrs: %s\nsprStates:\n%s\nrequests: %s\nreplies: %s\nrequestFifo: %s\nsharedMem: %s\n"
                    (show instrs)
                    (unlines $ map show $ sprStates s)
                    (show $ requestChnls s)
                    (show $ replyChnls s)
                    (show $ requestFifo s)
                    (show $ sharedMem s)

myShow' (instrs,s) = show instrs ++ "\n"
                     ++ (unlines $ map show $ sprStates s)


-- ======================================================================
-- examples of conditions that can be used with debuggerPrintCondWaitCond
-- ======================================================================
always,never :: DbgInput -> Bool
always = const True
never  = const False

-- | Checks whether any core in executing a Jump instruction
whenJumping :: DbgInput -> Bool
whenJumping (instrs,st) = any isJump instrs
    where
        isJump (Jump _) = True
        isJump _        = False

-- | Checks whether any of the program counters are in a given range
pcInRange :: CodeAddr -> CodeAddr -> (DbgInput -> Bool)
pcInRange min max (_,sysSt) = any inRange $ map pc $ sprStates sysSt
    where
        inRange x = min <= x && x <= max
