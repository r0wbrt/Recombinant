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


module Program.Recombinant.Config
    ( Config (..)
    , ModeOfOperation (..)
    ) where

import Program.Recombinant.StreamIO (Resource)


-- | Specifier for the mode of operation for Recombinant
data ModeOfOperation = Multiplex | Demultiplex 


-- | Config settings for the Recombinant program
data Config = Config
 { mode              :: ModeOfOperation
 , handlers          :: [Resource]
 , blockSize         :: Int
 , interleavePattern :: [Int]
 , aggregatedHandle  :: Resource
 }