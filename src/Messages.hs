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

module Messages (extendedHelpMessage) where

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
            ] -- ++ optionInterleavePattern ++ optionBlockSize ++ optionCustomPaths
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
        , sToSingleLine ["is broken down into", sBold "-n", " streams. The"]
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
