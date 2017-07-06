{-# LANGUAGE RecordWildCards #-}

module Sprockell.System where

import Sprockell.BasicFunctions
import Sprockell.HardwareTypes
import Sprockell.Sprockell
import Data.Char   (ord,chr)
import System.IO   (Handle,stdin,hGetChar,hReady,BufferMode(..),hSetBuffering)
import Text.Printf (printf)
import Text.Read   (readMaybe)
import Control.Exception (finally)

numberIOaddr,charIOaddr :: MemAddr
numberIOaddr = 0x10000
charIOaddr   = 0x10001

numberIO, charIO :: AddrImmDI
numberIO = DirAddr numberIOaddr
charIO   = DirAddr charIOaddr

-- ===================================================================================
shMem :: SharedMem
         -> (SprID,Request)
         -> IO (SharedMem, (SprID,Reply))

shMem sharedMem (i,req)
    | isNumberIOreq req = do
        reply <- case req of
                    NoRequest     -> return Nothing
                    ReadReq a     -> do
                        let prompt = printf "Sprockell %i asks for a number: " i
                        fmap Just (promptForRead prompt)
                    WriteReq v a  -> do
                        printf "Sprockell %i says %i\n" i v
                        return Nothing
                    TestReq a     -> error $ "TestAndSet on IO address: " ++ show req ++ " not supported"
        return (sharedMem,(i,reply))
    | isCharIOreq req = do
        reply <- case req of
                    NoRequest     -> return Nothing
                    ReadReq a     -> do
                        --mChar :: Maybe Char
                        mChar <- hGetCharNonBlocking stdin
                        return (Just $ maybe 0 ord mChar)
                    WriteReq v a  -> do
                        putChar $ chr v
                        return Nothing
                    TestReq a     -> error $ "TestAndSet on IO address: " ++ show req ++ " not supported"
        return (sharedMem,(i,reply))
    | otherwise   = return (sharedMem', (i,reply))
        where
          (reply, sharedMem')   = case req of
                NoRequest                          -> ( Nothing            , sharedMem )
                ReadReq a                          -> ( Just (sharedMem!a) , sharedMem )
                WriteReq v a                       -> ( Nothing            , sharedMem <~ (a,v))
                TestReq a     | sharedMem!a == 0   -> ( Just 1             , sharedMem <~ (a,1))
                              | otherwise          -> ( Just 0             , sharedMem )

-- | Non-blocking variant of hGetChar
--   When no input is available we return Nothing
hGetCharNonBlocking :: Handle -> IO (Maybe Char)
hGetCharNonBlocking h = do
    ready <- hReady h
    if ready then fmap Just $ hGetChar h
             else return Nothing

-- | Non-blocking variant of getChar
getCharNonBlocking :: IO (Maybe Char)
getCharNonBlocking = hGetCharNonBlocking stdin

-- | Display a prompt, get a line of input and try to 'read' it.
-- If the 'read' fails print an error and try again.
promptForRead :: Read a => String -> IO a
promptForRead prompt = do
    setupBuffering
    promptForRead'  `finally`  restoreBuffering
    where
        setupBuffering   = hSetBuffering stdin LineBuffering  -- needed to make line editing work
        restoreBuffering = hSetBuffering stdin NoBuffering

        promptForRead' = do
            putStr prompt
            l <- getLine
            case readMaybe l of
                Nothing -> do putStrLn "Invalid input"
                              promptForRead'
                Just x  -> return x

reqAddr :: Request -> Maybe MemAddr
reqAddr req = case req of
    NoRequest    -> Nothing
    ReadReq a    -> Just a
    WriteReq _ a -> Just a
    TestReq a    -> Just a

isNumberIOreq, isCharIOreq :: Request -> Bool
isNumberIOreq req = reqAddr req == Just numberIOaddr
isCharIOreq   req = reqAddr req == Just charIOaddr


updateFifo :: RequestFifo
              -> IndRequests
              -> (RequestFifo, (SprID,Request))
updateFifo requestFifo chRequests = (requestFifo', req)
        where
          req  | not $ null requestFifo = head requestFifo
               | otherwise              = (0, NoRequest)
          requestFifo' = drop 1 requestFifo ++ filter ((/=NoRequest).snd) chRequests

-- ===================================================================================
transferA :: (RequestChannels, ReplyChannels)
                -> (ParRequests)
                -> ((RequestChannels), (ParReplies, IndRequests))

transferA (requestChnls,replyChnls) (sprRequests) = ( (requestChnls'), (outReplies,outRequests) )
        where
          -- ->->->->
          outRequests   = zip [0..] $ map head requestChnls                                             -- <<== TODO: abstract away from softare/hardware
          requestChnls' = zipWith (<<+) requestChnls sprRequests

          -- <-<-<-<-
          n             = length replyChnls                                                             -- <<== TODO: abstraction difficult:
          --inReplies     = replicate n Nothing <~ (i,shMemReply)                                         --              no parameter n in CLaSH
          outReplies    = map head replyChnls
          --replyChnls'   = zipWith (<<+) replyChnls inReplies

transferB :: ReplyChannels
                -> (SprID, Reply)
                -> ReplyChannels
transferB replyChnls (i,shMemReply) = replyChnls'
        where
          -- <-<-<-<-
          n             = length replyChnls                                                             -- <<== TODO: abstraction difficult:
          inReplies     = replicate n Nothing <~ (i,shMemReply)                                         --              no parameter n in CLaSH
          replyChnls'   = zipWith (<<+) replyChnls inReplies

-- ===================================================================================
system :: [InstructionMem] -> SystemState -> t -> IO SystemState

system instrss systemState _ = do
    let
          SystemState{..} = systemState

          -- Sprockells
          (sprStates',sprRequests)                              = unzip $ sprockell $> instrss |$| sprStates |$| chReplies

          -- Communication
          ((requestChnls'), (chReplies,chRequests)) = transferA (requestChnls,replyChnls) (sprRequests)

          (requestFifo',request) = updateFifo requestFifo chRequests
          -- Shared Memory
    (sharedMem', (i,shMemReply))           <- shMem sharedMem request
    let
          replyChnls' = transferB replyChnls (i,shMemReply)
          systemState' = SystemState
                { sprStates     = sprStates'
                , requestChnls  = requestChnls'
                , replyChnls    = replyChnls'
                , requestFifo   = requestFifo'
                , sharedMem     = sharedMem'
                }

    return systemState'
