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

import           Data.Text             (pack, strip, unpack)
import qualified Messages              as Messages
import           Settings              (InputError (..), parseInput, parserList)
import           System.Console.GetOpt (usageInfo)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure, exitSuccess)

main :: IO ()
main = do
    input <- getArgs
    let parseResults = parseInput input
    case parseResults of
         Right settings            -> exitSuccess
         Left (InvalidOptions msg) -> writeErrorMessage msg >> exitFailure
         Left (ErrorMessages msg)  -> writeErrorMessage msg >> exitFailure
         Left ShowAbout             -> do
             progName <- getProgName
             putStr (unlines $ Messages.extendedHelpMessage progName) >> exitSuccess
         Left ShowHelp           -> do
             progName <- getProgName
             putStr (Messages.shortHelpMessage progName parserList) >> exitSuccess

    exitSuccess


-- | Writes a error caused by invalid command line input to the console
writeErrorMessage :: [String] -> IO ()
writeErrorMessage msg = putStr adjMsg
    where header = "Command Input Error:"
          adjMsg = unlines $ ["", header, ""] ++ (map (\s -> "\t" ++ s) $ concatMap convertIntoLines msg) ++ [""]


convertIntoLines :: String -> [String]
convertIntoLines "" = [""]
convertIntoLines line = filter (\l -> (unpack $ strip $ pack l) /= "") $ lines line
