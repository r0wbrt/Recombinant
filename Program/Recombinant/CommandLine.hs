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


module Program.Recombinant.CommandLine
    ( module Program.Recombinant.CommandLine.Config
    , module Program.Recombinant.CommandLine.Options
    , module Program.Recombinant.CommandLine.OptionValidators
    , optionValidators
    , optionList
    , parseInput
    , shortHelpMessage
    ) where

import           Control.Monad.Except                             (throwError)
import           Control.Monad.Except                             (runExceptT)
import           Control.Monad.Identity
import           Program.Recombinant.CommandLine.Config
import           Program.Recombinant.CommandLine.Options
import           Program.Recombinant.CommandLine.OptionValidators
import           System.Console.GetOpt                            (ArgOrder (ReturnInOrder),
                                                                   OptDescr (..),
                                                                   getOpt,
                                                                   usageInfo)

-- | List of functions to run after the record is intially built from the
--   supplied user input.
optionValidators :: [CommandLineConfig -> CommandLineMonad CommandLineConfig]
optionValidators = [validatorNumberOfChannels, validatorMode, validatorPaths, validatorBlockSize, validatorInterleavePattern, validatorCombinedPath]


-- | Command line parsers which will convert the string list into its associated
--   record form.
optionList :: [OptDescr (CommandLineConfig -> CommandLineMonad CommandLineConfig)]
optionList = [optionMode, optionNumberOfChannels, optionBlockSize, optionPathPattern, optionCombinedPath, optionInterleavePattern, optionHelp, optionAbout]


-- | Converts command line input into a settings record.
parseInput :: [OptDescr (CommandLineConfig -> CommandLineMonad CommandLineConfig)] -> [CommandLineConfig -> CommandLineMonad CommandLineConfig] -> [String] -> Either InputError CommandLineConfig
parseInput parsers validators input = runIdentity $ runExceptT mainExecution
    where mainExecution = do
            inputParsers <- convertInputIntoBuilders parsers input
            buildRecord (inputParsers ++ validators)


-- | Converts a input list into a list of builders which will be used to create
--   the settings record.
convertInputIntoBuilders :: [OptDescr (CommandLineConfig -> CommandLineMonad CommandLineConfig)]
    -> [String] -> CommandLineMonad [CommandLineConfig -> CommandLineMonad CommandLineConfig]
convertInputIntoBuilders parsers input =
    case getOpt (ReturnInOrder customPathWrapper) parsers input of
        (results, [], [])     -> return results
        (_, _, errorMessages) -> throwError $ ErrorMessages errorMessages



shortHelpMessage :: String -> [OptDescr a] -> String
shortHelpMessage progName = usageInfo ("Usage: "++progName++" [OPTIONS...]")
