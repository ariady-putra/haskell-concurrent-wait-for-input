{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Data.Time

import System.IO

printCurrentTime :: IO ()
printCurrentTime = getCurrentTime >>= print

popInput :: TVar Char -> IO Char
popInput tvInput = do
    input <- readTVarIO tvInput
    atomically $ do -- clear Input TVar
        writeTVar tvInput minBound
    return input

tickInput :: Chan Char -> IO ()
tickInput cInput = forever $ do
    threadDelay 1_000_000
    writeChan cInput minBound -- unblocks readChan

{- MVar
waitInput :: MVar Char -> IO ()
waitInput mvInput = forever $ do
    isInput <- hWaitForInput stdin 500
    when isInput $ do
        c <- getChar -- blocking
        tryPutMVar mvInput c
        writeChan cInput c
        return ()
-}
waitInput :: TVar Char -> IO ()
waitInput tvInput = forever $ do
    c <- getChar -- blocking
    atomically $ do
        writeTVar tvInput c
{- Chan
waitInput :: Chan Char -> IO ()
waitInput cInput = forever $ do
    c <- getChar -- blocking
    writeChan cInput c
-}

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    {- MVar
    mvInput <- newEmptyMVar
    forkIO $ waitInput mvInput
    -}
    tvInput <- newTVarIO minBound
    forkIO $ waitInput tvInput
    {- Chan
    cInput <- newChan
    forkIO $ waitInput cInput
    forkIO $ tickInput cInput
    -}
    forever $ do -- main loop
        printCurrentTime
        {- MVar
        input <- tryTakeMVar mvInput
        case input of
            Just i  -> print i
            Nothing -> return ()
        threadDelay 500_000
        -}
        input <- popInput tvInput
        unless (input == minBound) $ do
            print input
        threadDelay 1_000_000
        {- Chan
        input <- readChan cInput -- blocking
        unless (input == minBound) $ do
            print input
        -}
    
