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

import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs, getProgName)
import           Data.Text            (pack, strip, unpack)
import Program.Recombinant 
import Program.Recombinant.CommandLine (InputError (..), optionList, optionValidators, parseInput, shortHelpMessage, fromCommandLineConfig, extendedHelpMessage)

-- | Recombinant main program entry point
main :: IO ()
main = do
    
    -- Pull in options from the command line
    commandLineInput <- getArgs
    
    -- Parse them into the command line config and then validate them for 
    -- correctness.
    let commandLineParseResult = parseInput optionList optionValidators commandLineInput
    
    
    case commandLineParseResult of
            
        -- This branch is entered when the user supplies an invalid option 
        -- via the command line.
        Left (InvalidOptions msg) -> writeErrorMessage msg >> exitFailure
        
        -- This branch is entered when the value to an option is invalid.
        Left (ErrorMessages msg)  -> writeErrorMessage msg >> exitFailure
        
        -- This branch fires when the user wants the extended help message.
        -- The option that triggers this on the command line is --help.
        Left ShowAbout             -> do
                
            progName <- getProgName
            putStr (unlines $ extendedHelpMessage progName) >> exitSuccess
            
        -- This branch fires for the short help message. This message 
        -- does not include an extended description of the program.
        -- Only a conside list of options and how to use them.
        Left ShowHelp           -> do
            
            progName <- getProgName
            putStr (shortHelpMessage progName optionList) >> exitSuccess
        
        -- Runs main recombinant program
        Right commandLineConfig -> do
            
            config <- fromCommandLineConfig commandLineConfig
            
            runRecombinant config
            
            exitSuccess


-- | Writes a error caused by invalid command line input to the console
writeErrorMessage :: [String] -> IO ()
writeErrorMessage msg = putStr adjMsg
    where header = "Command Input Error:"
          adjMsg = unlines $ ["", header, ""] ++ map (\s -> "\t" ++ s)  (concatMap convertIntoLines msg) ++ [""]


-- | Converts a string into a list of lines
convertIntoLines :: String -> [String]
convertIntoLines "" = [""]
convertIntoLines line = filter (\l -> unpack  (strip $ pack l) /= "") $ lines line
