module Sprockell.BasicFunctions where

-- ==========================================================================================================
-- Some elementary constants and Functions
-- ==========================================================================================================
reg0          = 0    :: Int                          -- names for registers. reg0 is ALWAYS 0
regSprID      = 1    :: Int                          -- regSprID: contains the sprockellID
regA          = 2    :: Int                          -- usage of registers A-F to be chosen by the user
regB          = 3    :: Int
regC          = 4    :: Int
regD          = 5    :: Int
regE          = 6    :: Int
regARP        = 7    :: Int                          -- PP31: self-defined ARP register, replacing regF
regSP         = regbankSize                          -- register for stack pointer
regPC         = regbankSize + 1                      -- register for program counter

-- defines the number of registers excluding the stack pointer & program counter
regbankSize   =  8   :: Int
localMemSize  = 1024 :: Int

shMemSize     = 64   :: Int
channelDelay  =  4   :: Int


intBool True  = 1                                    -- Bool-to-Int
intBool False = 0

(+>>) :: a -> [a] -> [a]
x +>> xs = [x] ++ init xs                            -- shift value into buffer at the beginning

(<<+) :: [a] -> a -> [a]
xs <<+ x = tail xs ++ [x]                            -- shift value into buffer at the end

($>) :: (a->b) -> [a] -> [b]                         -- infix notation for map
f  $>  xs = map f xs

(|$|) :: [a->b] -> [a] -> [b]
fs |$| xs = zipWith (\f x -> f x) fs xs              -- parallel application of a list of functions
                                                     -- to an equally long list of arguments
