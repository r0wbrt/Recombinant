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

module Messages (extendedHelpMessage, shortHelpMessage) where
import           System.Console.GetOpt (OptDescr (..), usageInfo)

shortHelpMessage :: String -> [OptDescr a] -> String
shortHelpMessage progName options = usageInfo ("Usage: "++progName++" [OPTIONS...]") options

extendedHelpMessage :: String -> [String]
extendedHelpMessage progName = concat $
    [ sSection "Name" [progName ++ " - multiplexes and demultiplexes multiple streams"]
    , sSection "Synopsis"
        [ sToSingleLine [ progName
                 , " "
                 , sUnderline "[Options]"
                 , "... "
                 , sUnderline "[FILES]"
                 , "..."
                 ]
        ]
    , sSection "Description"
        (
            [ "Multiplexes and demultiplexes streams and files."
            , sEmptyLine
            , "The following options are mandatory"
            , sEmptyLine
            ]
            ++ optionMode ++ optionNumberOfChannels ++ optionStream ++
            [ sEmptyLine
            , "The following options may be conditionally omitted "
            , sEmptyLine
            ]
            ++ optionPathPattern ++
            [ sEmptyLine
            , "The following settings may be omitted. The default value of each option is"
            , "described in its description."
            , sEmptyLine
            ] ++ optionBlockSize ++ optionInterleavePattern ++
            [ sEmptyLine
            , "The following options will provide detail about the program."
            , sEmptyLine
            ] ++ optionHelp
        )
    , sSection "Author" ["Written by Robert Christian Taylor"]
    , sSection "Reporting Bugs" [sToSingleLine ["Report bugs to ", sUnderline "https://github.com/r0wbrt/Recombinant/issues"]]
    , sSection "Copyright"
        [ "Copyright © 2017 Robert Christian Taylor."
        , sEmptyLine
        , "Licensed under the Apache License, Version 2.0 (the \"License\");"
        , "you may not use this file except in compliance with the License."
        , "You may obtain a copy of the License at"
        , sEmptyLine
        , "http://www.apache.org/licenses/LICENSE-2.0"
        , sEmptyLine
        , "Unless required by applicable law or agreed to in writing, software"
        , "distributed under the License is distributed on an \"AS IS\" BASIS,"
        , "WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."
        , "See the License for the specific language governing permissions and"
        , "limitations under the License."
        ]
    ]

optionMode :: [String]
optionMode =
    sOption "-m, --mode" ""
        [ "Sets the combining mode. Can be set to either multiplex"
        , "or demultiplex. In multiplex mode, a set number"
        , sToSingleLine ["of channels set by option ", sBold "-n", "."]
        , sToSingleLine ["The streams are interleaved in blocks of size ", sBold "-b", "."]
        , sEmptyLine
        , "In demultiplex mode, the opposite occurs. The stream"
        , sToSingleLine ["is broken down into", sBold " -n", " streams. The"]
        , sToSingleLine ["streams are deinterleaved in blocks of size ", sBold "-b", "."]
        ]

optionNumberOfChannels :: [String]
optionNumberOfChannels =
    sOption "-n --channels" "[NUMBER OF CHANNELS]"
        [ "Sets the number of channel to use. This must be"
        , "set to an integer value"
        , "greater than or equal to one."
        ]

optionStream :: [String]
optionStream =
    sOption "-o --stream" "[FILE]"
        [ sToSingleLine ["When ", sBold "-m", "=", sUnderline "multiplex", " this is the path the multiplexed output will be written to."]
        , sToSingleLine ["When ", sBold "-m", "=", sUnderline "demultiplex", " this is the path to the input which will be demultiplexed from."]
        ]

optionPathPattern :: [String]
optionPathPattern =
    sOption "-p --pattern" "[FILE]"
        [ "Sets the expected path to multiplex or demultiplex to. This path will be copied"
        , sToSingleLine [sBold "-n", " times. For each stream, the path will have a number"]
        , "appended to the end representing that stream. So for example, if the path is"
        , sToSingleLine [sUnderline "/myPath", ", and there are 2 streams, then the 2 streams will get"]
        , "directed to the following paths:"
        , (sIndent $ sUnderline "/myPath0")
        , (sIndent $ sUnderline "/myPath1")
        , sEmptyLine
        , "Altenatively, the file paths can be specified manually by"
        , "specifying them after the program arguments."
        ]

optionBlockSize :: [String]
optionBlockSize =
    sOption "-b --blockSize" "[SIZE IN BYTES]"
        [ sToSingleLine ["Sets the size of the block to read from each stream."]
        , sEmptyLine
        , sToSingleLine ["In ", sBold "-m", "=", sUnderline "multiplex", ", the program will read"]
        , sToSingleLine ["in blocks of size ", sBold "-b", " from each stream and combine them"]
        , sToSingleLine ["into a single multiplexed stream following pattern", sBold "-i", "."]
        , sEmptyLine
        , sToSingleLine ["In ", sBold "-m", "=", sUnderline "demultiplex", " the program will read"]
        , sToSingleLine ["blocks of size ", sBold "-b", " from ", sBold "-o", " and write them to"]
        , sToSingleLine ["the output streams."]
        ]

optionInterleavePattern :: [String]
optionInterleavePattern =
    sOption "-i --interleavePattern" "[PATTERN]"
        [ "Sets the pattern for interleaving the streams."
        , "The format of the pattern is a comma seperated list of numbers"
        , "where each number represents an individual stream."
        , sToSingleLine ["The stream count begins at 1 and continues to ", sBold "-n"]
        , sEmptyLine
        , sToSingleLine ["Example: To interleave 5 channels (", sBold "-n", "=", sUnderline "5", ") in the order"]
        , sToSingleLine ["one, three, five, two, four, the value passed to ", sBold "-n", " would be "]
        , sUnderline "1,3,5,2,4."
        ]

optionHelp :: [String]
optionHelp =
    sOption "-h --help" ""
        [ sToSingleLine ["Provides a help message about this program. Passing in", sBold "-h"]
        , "will produce a consise list of options."
        , sEmptyLine
        , sToSingleLine ["Passing in ", sBold "--help", " will produce this message"]
        ]

sUnderline :: String -> String
sUnderline text = "\ESC[4m" ++ text ++ "\ESC[0m"

sSection :: String -> [String] -> [String]
sSection title body =[sBold title] ++ (map sIndent body) ++ [sEmptyLine]

sOption :: String -> String -> [String] -> [String]
sOption name arg description = header ++ body ++ footer
    where header = [ sToSingleLine [sBold name] ++ (if arg /= "" then ("=" ++ sUnderline arg) else "")]
          body = map sIndent description
          footer = [sEmptyLine]

sBold :: String -> String
sBold text = "\ESC[1m" ++ text ++ "\ESC[0m"

sIndent :: String -> String
sIndent text = "\t" ++ text

sToSingleLine :: [String] -> String
sToSingleLine line = concat line

sEmptyLine :: String
sEmptyLine = ""
