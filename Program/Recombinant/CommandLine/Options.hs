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

module Program.Recombinant.CommandLine.Options
    ( optionNumberOfChannels
    , optionBlockSize
    , optionPathPattern
    , optionCombinedPath
    , optionMode
    , optionInterleavePattern
    , optionHelp
    , optionAbout
    , customPathWrapper
    ) where

import           Program.Recombinant.CommandLine.Config (CommandLineConfig (..),
                                                         CommandLineMonad,
                                                         InputError (..),
                                                         SpecifiedModeOfOperation (..))

import           Control.Monad.Except                   (throwError)
import           Data.Char                              (toUpper)
import           System.Console.GetOpt                  (ArgDescr (NoArg, ReqArg),
                                                         OptDescr (..))
import           Text.Parsec                            (parse)
import           Text.Parsec.Language                   (emptyDef)
import           Text.Parsec.Token                      (commaSep, integer,
                                                         makeTokenParser)


-- | Wraps non options into a builder, used to later build a record
customPathWrapper :: String -> CommandLineConfig -> CommandLineMonad CommandLineConfig
customPathWrapper path settings = return $ settings { customPaths = customPaths settings ++ [path] }


-- | Option parser for command line option --channels
optionNumberOfChannels :: OptDescr (CommandLineConfig -> CommandLineMonad CommandLineConfig)
optionNumberOfChannels = Option shortOptionsNames longOptionNames (ReqArg handler argExp) description
    where description = "Sets the number of channels"
          longOptionNames = ["channels"]
          shortOptionsNames = ['c']
          argExp = "#"
          handler input record = do
              n <- parseIntoInt "--channels (-c)" input
              return $ record {numberOfChannels = n }


-- | Option parser for command line option --blockSize
optionBlockSize :: OptDescr (CommandLineConfig -> CommandLineMonad CommandLineConfig)
optionBlockSize = Option shortOptionsNames longOptionNames (ReqArg handler argExp) description
    where description = "Sets the size of a block"
          longOptionNames = ["blockSize"]
          shortOptionsNames = ['b']
          argExp = "#"
          handler input record = do
              n <- parseIntoInt "--blockSize (-b)" input
              return $ record {blockSize = n }


-- | Option parser for command line option --pathPattern
optionPathPattern :: OptDescr (CommandLineConfig -> CommandLineMonad CommandLineConfig)
optionPathPattern = Option shortOptionsNames longOptionNames (ReqArg handler argExp) description
    where description = "Sets the pattern used to auto generate the input and output paths"
          longOptionNames = ["pathPattern"]
          shortOptionsNames = ['p']
          argExp = "FILE"
          handler input record = return $ record {pathPattern = input }


-- | Option parser for command line option --combinedPath
optionCombinedPath :: OptDescr (CommandLineConfig -> CommandLineMonad CommandLineConfig)
optionCombinedPath = Option shortOptionsNames longOptionNames (ReqArg handler argExp) description
    where description = "Path to the combined file"
          longOptionNames = ["combinedPath"]
          shortOptionsNames = ['o']
          argExp = "FILE"
          handler input record = return $ record {combinedPath = input }


-- | Option parser for command line --mode
optionMode :: OptDescr (CommandLineConfig -> CommandLineMonad CommandLineConfig)
optionMode = Option shortOptionsNames longOptionNames (ReqArg handler argExp) description
    where description = "Sets the mode to run in"
          longOptionNames = ["mode"]
          shortOptionsNames = ['m']
          argExp = "multiplex | demultiplex"
          handler input record = return $ record {mode = getModeFromString input }


-- | Process the input string into the actual mode of execution
getModeFromString :: String -> SpecifiedModeOfOperation
getModeFromString input = case map toUpper input of
                                                "MULTIPLEX"   -> Multiplex
                                                "DEMULTIPLEX" -> Demultiplex
                                                _             -> ModeInvalid


-- | Parses the interleave pattern command line option. --interleavePattern
optionInterleavePattern :: OptDescr (CommandLineConfig -> CommandLineMonad CommandLineConfig)
optionInterleavePattern = Option shortOptionsNames longOptionNames (ReqArg handler argExp) description
    where description = "Sets the interleave pattern of the streams"
          longOptionNames = ["interleavePattern"]
          shortOptionsNames = ['i']
          argExp = "1,2,3,2,2,1"
          handler input record = do
              list <- parseInterleavePattern "--interleavePattern (-i)" input
              return $ record { interleavePattern = list }


-- | Parses the interleave patten which is specified as a string of integers
--   seperated by a comma into a list of integers. eg "1,2,3,4" into [1,2,3,4]
parseInterleavePattern :: String -> String -> CommandLineMonad [Int]
parseInterleavePattern option input = case parse (commaSep lexer (integer lexer)) option input of
                                        Left err -> throwError $ ErrorMessages [show err]
                                        Right list -> return (map fromIntegral list)
    where lexer = makeTokenParser emptyDef


-- | Handles the help input option.
optionHelp :: OptDescr (CommandLineConfig -> CommandLineMonad CommandLineConfig)
optionHelp = Option shortOptionsNames longOptionNames (NoArg handler) description
    where description = "Show this help message"
          longOptionNames = []
          shortOptionsNames = ['h']
          handler :: CommandLineConfig -> CommandLineMonad CommandLineConfig -- Declare type explicitly since it can't be inferred.
          handler _ = throwError ShowHelp


-- | Handles the about input option.
optionAbout :: OptDescr (CommandLineConfig -> CommandLineMonad CommandLineConfig)
optionAbout = Option shortOptionsNames longOptionNames (NoArg handler) description
    where description = "Shows a longer help message"
          longOptionNames = ["help"]
          shortOptionsNames = []
          handler :: CommandLineConfig -> CommandLineMonad CommandLineConfig -- Declare type explicitly since it can't be inferred.
          handler _ = throwError ShowAbout


-- | Parse a string into an Int using parsec
parseIntoInt :: String -> String -> CommandLineMonad Int
parseIntoInt option input = case parse (integer lexer) option input of
                                Left err -> throwError $ ErrorMessages [show err]
                                Right result -> return $ fromIntegral result
    where lexer = makeTokenParser emptyDef
