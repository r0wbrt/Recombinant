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

module Program.Recombinant.HelpTextTools
    ( sUnderline
    , sSection
    , sOption
    , sBold
    , sIndent
    , sToSingleLine
    , sEmptyLine
    ) where


sUnderline :: String -> String
sUnderline text = "\ESC[4m" ++ text ++ "\ESC[0m"

sSection :: String -> [String] -> [String]
sSection title body =[sBold title] ++ map sIndent body ++ [sEmptyLine]

sOption :: String -> String -> [String] -> [String]
sOption name arg description = header ++ body ++ footer
    where header = [ sToSingleLine [sBold name] ++ (if arg /= "" then "=" ++ sUnderline arg else "")]
          body = map sIndent description
          footer = [sEmptyLine]

sBold :: String -> String
sBold text = "\ESC[1m" ++ text ++ "\ESC[0m"

sIndent :: String -> String
sIndent text = "\t" ++ text

sToSingleLine :: [String] -> String
sToSingleLine = concat

sEmptyLine :: String
sEmptyLine = "" 
