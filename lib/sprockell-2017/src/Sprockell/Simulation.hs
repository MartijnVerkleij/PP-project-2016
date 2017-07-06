module Sprockell.Simulation where

import Control.DeepSeq
import Sprockell.BasicFunctions
import Sprockell.HardwareTypes
import Sprockell.Sprockell
import Sprockell.System
import Sprockell.Debugger

import System.IO         (BufferMode(..),stdin,stdout,hGetBuffering,hSetBuffering)
import Control.Exception (bracket)

-- ====================================================================================================
-- Sprockell Test
-- ====================================================================================================

sprockellSim :: InstructionMem
                -> SprockellState
                -> [Reply]
                -> [(Instruction, SprockellState, Request)]

sprockellSim instrs s []     = []
sprockellSim instrs s (i:is) | instr /= EndProg    = (instr,s',o) : sprockellSim instrs s' is
                             | otherwise           = []
                where
                  (s',o) = sprockell instrs s i
                  instr  = instrs ! pc s

initSprockellState :: Value -> SprockellState
initSprockellState sprID = SprState
        { pc       = 0
        , sp       = localMemSize
        , regbank  = replicate regbankSize 0 <~ (regSprID,sprID)
        , localMem = fromList $ replicate localMemSize 0
        }

sprTest :: Value -> InstructionMem -> [Reply] -> IO ()
sprTest sprID instrs input = putStr
                           $ unlines
                           $ map show
                           $ sprockellSim instrs (initSprockellState sprID) input

-- ====================================================================================================
-- System Test
-- ====================================================================================================

systemSim :: Debugger st -> [InstructionMem] -> SystemState -> Clock -> IO ()
systemSim (dbg,dbgSt) instrss s []     = return ()
systemSim (dbg,dbgSt) instrss s (t:ts) | sysHalted = return ()
                                       | otherwise = do
                                           let curInstrs = zipWith (!) instrss (map pc $ sprStates s)
                                           s' <- deepseq s $ system instrss s t
                                           (dbgSt',s'') <- dbg dbgSt (curInstrs,s')
                                           systemSim (dbg,dbgSt') instrss s'' ts
    where
        instrs    = zipWith (!) instrss (map pc $ sprStates s)
        sysHalted = (and $ map (==EndProg) $ zipWith (!) instrss $ map pc $ sprStates s)
                  && (and $ map and $ map (map (==NoRequest)) $ requestChnls s)
                  && (and $ map (\(_,r) -> r == NoRequest) $ requestFifo s)


initSystemState nrOfSprockells = SystemState
        { sprStates     = map initSprockellState [0 .. nrOfSprockells-1]
        , requestChnls  = replicate nrOfSprockells $ replicate channelDelay NoRequest
        , replyChnls    = replicate nrOfSprockells $ replicate channelDelay Nothing
        , requestFifo   = []
        , sharedMem     = fromList $ replicate shMemSize 0
        }

run :: [[Instruction]] -> IO ()                                  -- list of programs per Sprockell
run = runWithDebugger noDebugger

runWithDebugger :: Debugger st -> [[Instruction]] -> IO ()       -- debugger + list of programs per Sprockell
runWithDebugger dbg instrss = do
    bracket setupBuffering
            restoreBuffering
            (\_ -> systemSim dbg instrss' (initSystemState nrOfSprockells) clock)
    return ()
    where nrOfSprockells = length instrss
          instrss' = map fromList instrss  -- conversion to Memory

setupBuffering :: IO (BufferMode,BufferMode)
setupBuffering = do
    oldin  <- hGetBuffering stdin
    oldout <- hGetBuffering stdout
    hSetBuffering stdin  NoBuffering  -- needed to make charIO work nicely
    hSetBuffering stdout NoBuffering
    return (oldin,oldout)

restoreBuffering :: (BufferMode,BufferMode) -> IO ()
restoreBuffering (modeIn,modeOut) = do
    hSetBuffering stdin  modeIn
    hSetBuffering stdout modeOut
