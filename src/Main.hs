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

module Main (module Main) where

import qualified Data.ByteString.Lazy as BS
import           Data.Text            (pack, strip, unpack)
import qualified Data.Vector          as V
import           GHC.IO.Handle.FD     (openFileBlocking)
import           Messages
import           Settings             (InputError (..), ModeOfOperation (..),
                                       Settings (blockSize, combinedPath, mode),
                                       getInterleavePattern, getPathList,
                                       parseInput, parserList)
import           System.Environment   (getArgs, getProgName)
import           System.Exit          (exitFailure, exitSuccess)
import           System.IO            (BufferMode (BlockBuffering), Handle,
                                       IOMode (ReadMode, WriteMode), hClose,
                                       hIsEOF, hSetBinaryMode, hSetBuffering)


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


-- | Opens a file for reading
safeOpenRead :: String -> IO Handle
safeOpenRead path = do
    h <- openFileBlocking path ReadMode -- Needed to open named pipes
    setUpHandleOptions h
    return h


-- | Opens a file for writing
safeOpenWrite :: String -> IO Handle
safeOpenWrite path = do
    h <- openFileBlocking path WriteMode -- Needed to open named pipes
    setUpHandleOptions h
    return h


-- | Sets up common handle options
setUpHandleOptions :: Handle -> IO ()
setUpHandleOptions h = do
    hSetBinaryMode h True
    hSetBuffering h (BlockBuffering (Just 65536))


-- | The main execution branch
runProgram :: Settings -> IO ()
runProgram settings = do

    -- Open secondary streams
    let openFunction = case mode settings of
                        Multiplex   -> safeOpenRead
                        Demultiplex -> safeOpenWrite
                        _           -> error "Invalid Mode"

    handlers <- mapM openFunction (getPathList settings)

    -- Open main combined stream
    stream <- case mode settings of
                   Multiplex   -> safeOpenWrite (combinedPath settings)
                   Demultiplex -> safeOpenRead (combinedPath settings)
                   _           -> error "Invalid Mode"

    --Set up the handles in their proper order and cycle them forever.
    let handleList = cycle $ interleaveHandles (getInterleavePattern settings) handlers

    --Branch based on mode of execution
    case mode settings of

         Multiplex -> multiplexStreams (blockSize settings) handleList stream
         Demultiplex -> demultiplexStreams (blockSize settings) handleList stream

         -- All other modes are invalid. The validators in settings should have
         -- handled this.
         _ -> error "Invalid Mode"

    hClose stream
    mapM_ hClose handlers

-- | Branch of program executed when mode is set to multiplex
multiplexStreams :: Int -> [Handle] -> Handle -> IO ()
multiplexStreams _ [] _ = return ()
multiplexStreams blockSz (h:hs) multiplexedStreamH = do

        getblockReturn <- getBlock blockSz h
        case getblockReturn of
                            Just block -> handleBlock block
                            Nothing    -> return ()

    where handleBlock block = do
              writeBlock multiplexedStreamH block
              multiplexStreams blockSz hs multiplexedStreamH


-- | Branch of program to execute when mode is set to demultiplex
demultiplexStreams :: Int -> [Handle] -> Handle -> IO ()
demultiplexStreams _ [] _ = return ()
demultiplexStreams blockSz (h:hs) multiplexedStreamH = do
        getblockReturn <- getBlock blockSz multiplexedStreamH
        case getblockReturn of
                                  Just block -> handleBlock block
                                  Nothing    -> return ()
    where handleBlock block = do
            writeBlock h block
            demultiplexStreams blockSz hs multiplexedStreamH


-- | Gets a block from a stream.
getBlock :: Int -> Handle -> IO (Maybe BS.ByteString)
getBlock size handle = do
    isEof <- hIsEOF handle
    if isEof then return Nothing else do
        block <- BS.hGet handle size
        return $ if fromIntegral (BS.length block) /= size then Nothing else Just block


-- | Generates a new list based on the supplied pattern
interleaveHandles :: [Int] -> [a] -> [a]
interleaveHandles hPattern handles = map (\pos -> handleVector V.! pos ) hPattern
    where handleVector = V.fromList handles


-- | Writes a block of data to the output.
writeBlock :: Handle -> BS.ByteString -> IO ()
writeBlock = BS.hPut


-- | Writes a error caused by invalid command line input to the console
writeErrorMessage :: [String] -> IO ()
writeErrorMessage msg = putStr adjMsg
    where header = "Command Input Error:"
          adjMsg = unlines $ ["", header, ""] ++ map (\s -> "\t" ++ s)  (concatMap convertIntoLines msg) ++ [""]


-- | Converts a string into a list of lines
convertIntoLines :: String -> [String]
convertIntoLines "" = [""]
convertIntoLines line = filter (\l -> unpack  (strip $ pack l) /= "") $ lines line
