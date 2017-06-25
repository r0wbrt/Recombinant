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

module Program.Recombinant.CommandLine.Config 
    ( CommandLineConfig (..) 
    , CommandLineMonad
    , SpecifiedModeOfOperation (..)
    , InputError (..)
    , startingSettings
    , buildRecord
    , getPathList
    , getInterleavePattern
    , generatePaths
    ) where

import           Control.Monad.Except   (ExceptT)
import           Control.Monad.Identity

-- | Command Line monad in which the parsing of the command line options takes place.
--   Provides error handling mechanism.
type CommandLineMonad = ExceptT InputError Identity

-- | Specifier for the mode of operation for Recombinant
data SpecifiedModeOfOperation = Multiplex | Demultiplex | NotSpecified | ModeInvalid


-- | The setting record to store parsed options in
data CommandLineConfig = CommandLineConfig
 { numberOfChannels  :: Int
 , mode              :: SpecifiedModeOfOperation
 , customPaths       :: [String]
 , pathPattern       :: String
 , blockSize         :: Int
 , interleavePattern :: [Int]
 , combinedPath      :: String
 }

-- | Data structure specifying what type of input error has occured.
data InputError = InvalidOptions [String] | ErrorMessages [String] | ShowHelp | ShowAbout

-- | Starting point to build the setting record
startingSettings :: CommandLineConfig
startingSettings = CommandLineConfig
 { numberOfChannels = -1
 , mode = NotSpecified
 , customPaths = []
 , pathPattern = ""
 , blockSize = 512
 , interleavePattern = []
 , combinedPath = ""
 }

-- | Builds a record using a list of builders.
buildRecord :: [CommandLineConfig -> CommandLineMonad CommandLineConfig] -> CommandLineMonad CommandLineConfig
buildRecord = foldl (>>=) (return startingSettings)


-- | Returns the paths to use.
getPathList :: CommandLineConfig -> [String]
getPathList settings = if customPaths settings /= [] then customPaths settings else generatePaths (numberOfChannels settings) (pathPattern settings)


-- | Returns the appropriate interleave pattern
getInterleavePattern :: CommandLineConfig -> [Int]
getInterleavePattern settings = if null (interleavePattern settings) then [0 .. (numberOfChannels settings - 1)] else interleavePattern settings


-- | Generates the path list from the number of channels and the supplied
--   pattern
generatePaths :: Int -> String -> [String]
generatePaths nOfChannels hPattern = zipWith (\a b -> a ++ show b) (replicate nOfChannels hPattern) [1 .. nOfChannels]
