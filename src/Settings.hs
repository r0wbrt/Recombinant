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

--TODO - Validators

module Settings
    ( Settings (..)
    , InputError (..)
    , parseInput
    , getPathList
    , parserList
    , ModeOfOperation (..)
    , getInterleavePattern
    ) where


import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.Identity
import           Data.Char              (toUpper)
import           System.Console.GetOpt  (ArgDescr (NoArg, ReqArg),
                                         ArgOrder (ReturnInOrder),
                                         OptDescr (..), getOpt)
import           Text.Parsec            (parse)
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.Token      (commaSep, integer, makeTokenParser)

-- | The setting record to store parsed options in
data Settings = Settings
 { numberOfChannels  :: Int
 , mode              :: ModeOfOperation
 , customPaths       :: [String]
 , pathPattern       :: String
 , blockSize         :: Int
 , interleavePattern :: [Int]
 , combinedPath      :: String
 }


-- | Data structure specifying what type of input error has occured.
data InputError = InvalidOptions [String] | ErrorMessages [String] | ShowHelp | ShowAbout

-- | Setting monad in which the parsing of the command line options takes place.
--   Provides error handling mechanism.
type SettingMonad = ExceptT InputError Identity


-- | Starting point to build the setting record
startingSettings :: Settings
startingSettings = Settings
 { numberOfChannels = -1
 , mode = NotSpecified
 , customPaths = []
 , pathPattern = ""
 , blockSize = 512
 , interleavePattern = []
 , combinedPath = ""
 }


-- | List of functions to run after the record is intially built from the
--   supplied user input.
recordValidators :: [Settings -> SettingMonad Settings]
recordValidators = []


-- | Command line parsers which will convert the string list into its associated
--   record form.
parserList :: [OptDescr (Settings -> SettingMonad Settings)]
parserList = [optionMode, optionNumberOfChannels, optionBlockSize, optionPathPattern, optionCombinedPath, optionInterleavePattern, optionHelp, optionAbout]


-- | Converts command line input into a settings record.
parseInput :: [String] -> Either InputError Settings
parseInput input = runIdentity $ runExceptT mainExecution
    where mainExecution = do
            inputParsers <- convertInputIntoBuilders parserList input
            buildRecord (inputParsers ++ recordValidators)


-- | Converts a input list into a list of builders which will be used to create
--   the settings record.
convertInputIntoBuilders :: [OptDescr (Settings -> SettingMonad Settings)]
    -> [String] -> SettingMonad [Settings -> SettingMonad Settings]
convertInputIntoBuilders parsers input =
    case getOpt (ReturnInOrder customPathWrapper) parsers input of
        (results, [], [])     -> return results
        (_, _, errorMessages) -> throwError $ ErrorMessages errorMessages


-- | Builds a record using a list of builders.
buildRecord :: [Settings -> SettingMonad Settings] -> SettingMonad Settings
buildRecord = foldl (>>=) (return startingSettings)


-- | Returns the paths to use.
getPathList :: Settings -> [String]
getPathList settings = if customPaths settings /= [] then customPaths settings else generatePaths (numberOfChannels settings) (pathPattern settings)


-- | Returns the appropriate interleave pattern
getInterleavePattern :: Settings -> [Int]
getInterleavePattern settings = if null (interleavePattern settings) then [0 .. (numberOfChannels settings - 1)] else interleavePattern settings

-- | Generates the path list from the number of channels and the supplied
--   pattern
generatePaths :: Int -> String -> [String]
generatePaths nOfChannels hPattern = zipWith (\a b -> a ++ show b) (replicate nOfChannels hPattern) [1 .. nOfChannels]


-- | Wraps non options into a builder, used to later build a record
customPathWrapper :: String -> Settings -> SettingMonad Settings
customPathWrapper path settings = return $ settings { customPaths = customPaths settings ++ [path] }

-- | Specifier for the mode of operation for Recombinant
data ModeOfOperation = Multiplex | Demultiplex | NotSpecified | ModeInvalid

-- | Option parser for command line option --channels
optionNumberOfChannels :: OptDescr (Settings -> SettingMonad Settings)
optionNumberOfChannels = Option shortOptionsNames longOptionNames (ReqArg handler argExp) description
    where description = "Sets the number of channels"
          longOptionNames = ["channels"]
          shortOptionsNames = ['c']
          argExp = "#"
          handler input record = do
              n <- parseIntoInt "--channels (-c)" input
              return $ record {numberOfChannels = n }


-- | Option parser for command line option --blockSize
optionBlockSize :: OptDescr (Settings -> SettingMonad Settings)
optionBlockSize = Option shortOptionsNames longOptionNames (ReqArg handler argExp) description
    where description = "Sets the size of a block"
          longOptionNames = ["blockSize"]
          shortOptionsNames = ['b']
          argExp = "#"
          handler input record = do
              n <- parseIntoInt "--blockSize (-b)" input
              return $ record {blockSize = n }


-- | Option parser for command line option --pathPattern
optionPathPattern :: OptDescr (Settings -> SettingMonad Settings)
optionPathPattern = Option shortOptionsNames longOptionNames (ReqArg handler argExp) description
    where description = "Sets the pattern used to auto generate the input and output paths"
          longOptionNames = ["pathPattern"]
          shortOptionsNames = ['p']
          argExp = "FILE"
          handler input record = return $ record {pathPattern = input }


-- | Option parser for command line option --combinedPath
optionCombinedPath :: OptDescr (Settings -> SettingMonad Settings)
optionCombinedPath = Option shortOptionsNames longOptionNames (ReqArg handler argExp) description
    where description = "Path to the combined file"
          longOptionNames = ["combinedPath"]
          shortOptionsNames = ['o']
          argExp = "FILE"
          handler input record = return $ record {combinedPath = input }


-- | Option parser for command line --mode
optionMode :: OptDescr (Settings -> SettingMonad Settings)
optionMode = Option shortOptionsNames longOptionNames (ReqArg handler argExp) description
    where description = "Sets the mode to run in"
          longOptionNames = ["mode"]
          shortOptionsNames = ['m']
          argExp = "multiplex | demultiplex"
          handler input record = return $ record {mode = getModeFromString input }


-- | Process the input string into the actual mode of execution
getModeFromString :: String -> ModeOfOperation
getModeFromString input = case map toUpper input of
                                                "MULTIPLEX"   -> Multiplex
                                                "DEMULTIPLEX" -> Demultiplex
                                                _             -> ModeInvalid


-- | Parses the interleave pattern command line option. --interleavePattern
optionInterleavePattern :: OptDescr (Settings -> SettingMonad Settings)
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
parseInterleavePattern :: String -> String -> SettingMonad [Int]
parseInterleavePattern option input = case parse (commaSep lexer (integer lexer)) option input of
                                        Left err -> throwError $ ErrorMessages [show err]
                                        Right list -> return (map fromIntegral list)
    where lexer = makeTokenParser emptyDef


-- | Handles the help input option.
optionHelp :: OptDescr (Settings -> SettingMonad Settings)
optionHelp = Option shortOptionsNames longOptionNames (NoArg handler) description
    where description = "Show this help message"
          longOptionNames = []
          shortOptionsNames = ['h']
          handler :: Settings -> SettingMonad Settings -- Declare type explicitly since it can't be inferred.
          handler _ = throwError ShowHelp


-- | Handles the about input option.
optionAbout :: OptDescr (Settings -> SettingMonad Settings)
optionAbout = Option shortOptionsNames longOptionNames (NoArg handler) description
    where description = "Shows a longer help message"
          longOptionNames = ["help"]
          shortOptionsNames = []
          handler :: Settings -> SettingMonad Settings -- Declare type explicitly since it can't be inferred.
          handler _ = throwError ShowAbout


-- | Parse a string into an Int using parsec
parseIntoInt :: String -> String -> SettingMonad Int
parseIntoInt option input = case parse (integer lexer) option input of
                                Left err -> throwError $ ErrorMessages [show err]
                                Right result -> return $ fromIntegral result
    where lexer = makeTokenParser emptyDef
