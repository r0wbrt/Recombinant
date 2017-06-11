{-

Copyright 2017 Robert Christian Taylor

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-}

module Main where

import           Control.Monad         (when)
import qualified Data.ByteString.Lazy  as BS
import           Data.Text             (pack, strip, unpack)
import qualified Data.Vector           as V
import qualified Messages              as Messages
import           Settings              (InputError (..), ModeOfOperation (..),
                                        Settings (blockSize, combinedPath, mode),
                                        getInterleavePattern, getPathList,
                                        parseInput, parserList)
import           System.Console.GetOpt (usageInfo)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure, exitSuccess)
import           System.IO             (Handle, hIsEOF, hSetBinaryMode, IOMode (ReadMode, WriteMode), hClose)
import GHC.IO.Handle.FD (openFileBlocking)


-- | Main functional entry point
main :: IO ()
main = do
    input <- getArgs
    let parseResults = parseInput input
    case parseResults of
         Right settings            -> runProgram settings
         Left (InvalidOptions msg) -> writeErrorMessage msg >> exitFailure
         Left (ErrorMessages msg)  -> writeErrorMessage msg >> exitFailure
         Left ShowAbout             -> do
             progName <- getProgName
             putStr (unlines $ Messages.extendedHelpMessage progName) >> exitSuccess
         Left ShowHelp           -> do
             progName <- getProgName
             putStr (Messages.shortHelpMessage progName parserList) >> exitSuccess

    exitSuccess


safeOpenRead :: String -> IO (Handle)
safeOpenRead path = do
    h <- openFileBlocking path ReadMode 
    hSetBinaryMode h True
    return h


safeOpenWrite :: String -> IO (Handle)
safeOpenWrite path = do
    h <- openFileBlocking path WriteMode 
    hSetBinaryMode h True
    return h


-- | The main execution branch
runProgram :: Settings -> IO ()
runProgram settings = do

    -- Open secondary streams
    let openFunction = case (mode settings) of
                        Multiplex   -> safeOpenRead
                        Demultiplex -> safeOpenWrite

    handlers <- mapM (\path -> openFunction path) (getPathList settings)

    -- Open main combined stream
    stream <- case (mode settings) of
                   Multiplex   -> safeOpenWrite (combinedPath settings)
                   Demultiplex -> safeOpenRead (combinedPath settings)

    --Set up the handles in their proper order and cycle them forever.
    let handleList = cycle $ interleaveHandles (getInterleavePattern settings) handlers

    --Branch based on mode of execution
    case (mode settings) of

         Multiplex -> multiplexStreams (blockSize settings) handleList stream
         Demultiplex -> demultiplexStreams (blockSize settings) handleList stream

         -- All other modes are invalid. The validators in settings should have
         -- handled this.
         _ -> error "Invalid Mode"

    hClose stream
    mapM_ (\h -> hClose h) handlers

-- | Branch of program executed when mode is set to multiplex
multiplexStreams :: Int -> [Handle] -> Handle -> IO ()
multiplexStreams blockSize (h:hs) multiplexedStreamH = do

        getblockReturn <- getBlock blockSize h
        case getblockReturn of
                            Just block -> handleBlock block
                            Nothing    -> return ()

    where handleBlock block = do
              writeBlock multiplexedStreamH block
              multiplexStreams blockSize hs multiplexedStreamH


-- | Branch of program to execute when mode is set to demultiplex
demultiplexStreams :: Int -> [Handle] -> Handle -> IO ()
demultiplexStreams blockSize (h:hs) multiplexedStreamH = do
        getblockReturn <- getBlock blockSize multiplexedStreamH
        case getblockReturn of
                                  Just block -> handleBlock block
                                  Nothing    -> return ()
    where handleBlock block = do
            writeBlock h block
            demultiplexStreams blockSize hs multiplexedStreamH


-- | Gets a block from a stream.
getBlock :: Int -> Handle -> IO (Maybe BS.ByteString)
getBlock size handle = do
    isEof <- hIsEOF handle
    if isEof then return Nothing else do
        block <- BS.hGet handle size
        return $ if fromIntegral (BS.length block) /= size then Nothing else Just block


-- | Generates a new list based on the supplied pattern
interleaveHandles :: [Int] -> [a] -> [a]
interleaveHandles pattern handles = map (\pos -> handleVector V.! pos) pattern
    where handleVector = V.fromList handles


-- | Writes a block of data to the output.
writeBlock :: Handle -> BS.ByteString -> IO ()
writeBlock h block = BS.hPut h block


-- | Writes a error caused by invalid command line input to the console
writeErrorMessage :: [String] -> IO ()
writeErrorMessage msg = putStr adjMsg
    where header = "Command Input Error:"
          adjMsg = unlines $ ["", header, ""] ++ (map (\s -> "\t" ++ s) $ concatMap convertIntoLines msg) ++ [""]


-- | Converts a string into a list of lines
convertIntoLines :: String -> [String]
convertIntoLines "" = [""]
convertIntoLines line = filter (\l -> (unpack $ strip $ pack l) /= "") $ lines line
