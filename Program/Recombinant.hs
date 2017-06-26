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

module Program.Recombinant
    ( runRecombinant
    , module Program.Recombinant.Config
    , module Program.Recombinant.StreamIO
    ) where

import           Control.Monad                (when)
import qualified Data.Vector                  as V
import           Program.Recombinant.Config
import           Program.Recombinant.StreamIO

runRecombinant :: Config -> IO ()
runRecombinant config = do
    let handleList = interleaveHandles (interleavePattern config) (handles config)
    let aggHandle = aggregatedHandle config
    let bytes = blockSize config
    case mode config of
         Multiplex -> whileListM_ (\s -> copyToStream s aggHandle bytes) (cycle handleList)
         Demultiplex -> whileListM_ (\d -> copyToStream aggHandle d bytes) (cycle handleList)


-- | Generates a new list based on the supplied pattern
interleaveHandles :: [Int] -> [a] -> [a]
interleaveHandles hPattern hList = map (\pos -> handleVector V.! pos ) hPattern
    where handleVector = V.fromList hList

-- | Loops over a list until the handler returns false.
whileListM_ :: (a -> IO Bool) -> [a] -> IO ()
whileListM_ action (x:xs) = do
    res <- action x
    when res $ whileListM_ action xs
whileListM_ _ [] = return ()
