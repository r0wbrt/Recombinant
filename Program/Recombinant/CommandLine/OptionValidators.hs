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


module Program.Recombinant.CommandLine.OptionValidators 
    ( validatorBlockSize
    , validatorCombinedPath
    , validatorInterleavePattern
    , validatorMode
    , validatorNumberOfChannels
    , validatorPaths
    ) where

import           Control.Monad.Except   (throwError, when)
import Program.Recombinant.CommandLine.Config 
    ( CommandLineMonad
    , SpecifiedModeOfOperation (..)
    , CommandLineConfig (..)
    , InputError (..)
    , getPathList
    , getInterleavePattern
    )

validatorNumberOfChannels :: CommandLineConfig -> CommandLineMonad CommandLineConfig
validatorMode :: CommandLineConfig -> CommandLineMonad CommandLineConfig
validatorPaths :: CommandLineConfig -> CommandLineMonad CommandLineConfig
validatorBlockSize :: CommandLineConfig -> CommandLineMonad CommandLineConfig
validatorInterleavePattern :: CommandLineConfig -> CommandLineMonad CommandLineConfig
validatorCombinedPath :: CommandLineConfig -> CommandLineMonad CommandLineConfig


validatorNumberOfChannels settings =
    if numberOfChannels settings < 1 then throwError (ErrorMessages
        [ "The number of specified channels, -c, must be greater then zero."
        , "The supplied value " ++ show (numberOfChannels settings) ++ " does not satisfy that requirment."
        ])
                                                            else return settings

validatorMode settings = case mode settings of
                                    Multiplex -> return settings
                                    Demultiplex -> return settings
                                    NotSpecified -> throwError (ErrorMessages ["The mode of operation -m (--mode) must be specified."])
                                    ModeInvalid -> throwError (ErrorMessages["The supplied mode of operation was invalid."])


validatorPaths settings = if length (getPathList settings) /= numberOfChannels settings then
                            throwError (ErrorMessages ["The number of supplied paths does not match the number of channels -c."])
                                                                                          else return settings


validatorBlockSize settings = if blockSize settings < 1 then throwError $ ErrorMessages ["Option block size -b must have a value greater then zero."]
                                                        else return settings


validatorInterleavePattern settings = do
    mapM_ (\pos -> when (pos - 1 > numberOfChannels settings) (throwError (ErrorMessages ["Interleave position option out of range."]))) (getInterleavePattern settings)
    return settings

validatorCombinedPath settings = if null (combinedPath settings) then throwError (ErrorMessages ["The combined path (-o) was not specified."]) else return settings
 
