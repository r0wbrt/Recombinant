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

module Program.Recombinant.StreamIO (Resource (..), ResourceType (..), openResource, copyToStream, closeResource ) where


import           System.IO (Handle, hIsEOF, hSetBinaryMode, IOMode, hSeek, SeekMode (RelativeSeek), hClose)
import System.Posix.IO (fdToHandle, handleToFd, dup, closeFd)
import System.Posix.Types (Fd (..))
import System.Posix.Files (isNamedPipe, getFileStatus)
import           GHC.IO.Handle.FD     (openFileBlocking, openBinaryFile)
import qualified Data.ByteString.Lazy as BS
import Foreign.C.Types


data ResourceType = TypePath | TypeSharedMemorySegment
data Resource = Pipe Fd Handle | SharedMemorySegment | File Fd Handle | NullHandle



foreign import ccall "spliceHandler" c_spliceHandler ::
        Fd
    ->  Bool
    ->  Fd
    ->  Bool
    ->  Int
    ->  IO (Int)

foreign import ccall "copyRange" c_copyRange ::
        Fd
    ->  Fd
    ->  Int
    ->  IO (Int)

-- |Open a resource given the supplied path.
openResource :: String -> ResourceType -> IOMode -> IO Resource
openResource path TypePath mode = do
    pipeCheck <- isPipe path
    if  pipeCheck then 
        do
            openPipe path mode
                   else do
                       openFile path mode

openResource _ TypeSharedMemorySegment _ = error "Shared Memory resource passing is not implemented (yet)."

-- | Closes a resource
closeResource :: Resource -> IO ()
closeResource (File fd h) = do
    hClose h
    closeFd fd

closeResource (Pipe fd h) = do
    hClose h
    closeFd fd

closeResource NullHandle = return ()
closeResource SharedMemorySegment = return ()

-- |Copies data from one resource to another idealling using fast kernel 
--  primitives when availiable. 
copyToStream :: Resource -> Resource -> Int -> IO Bool
copyToStream (Pipe sourceFd _) (Pipe destFd _) bytes = do
    result <- c_spliceHandler sourceFd True destFd True bytes
    return $ result == 1
    
copyToStream (Pipe sourceFd sourceH) (File destFd destH) bytes = do
        result <- c_spliceHandler sourceFd True destFd False bytes
        handleCReturn result sourceH destH bytes
    
copyToStream (File sourceFd sourceH) (Pipe destFd destH) bytes = do
        result <- c_spliceHandler sourceFd False destFd True bytes
        handleCReturn result sourceH destH bytes
        
copyToStream (File sourceFd sourceH) (File destFd destH) bytes = do
        result <- c_copyRange sourceFd destFd bytes
        handleCReturn result sourceH destH bytes

copyToStream NullHandle _ _ = return $ False -- Null sources return EOF.

copyToStream (Pipe _ sourceH) NullHandle bytes = do
    eof <- hIsEOF sourceH
    if eof then return False else do
        block <- BS.hGet sourceH bytes 
        return $ fromIntegral (BS.length block) == bytes

copyToStream (File _ h) NullHandle bytes = do
    eof <- hIsEOF h
    if eof then return False else do 
        hSeek h RelativeSeek (fromIntegral bytes)
        return True

copyToStream _ _ _ = error "Shared Memory resource passing is not implemented (yet)."


-- |Handles the return from the c copy functions. A value of -1 means EOF was
--  reached or some other unrecoverable error.
--  0 means the copy could not be done using the kernel primitive so do the
--  copy in user space.
--  1 means the copy was a success. 
handleCReturn :: Int -> Handle -> Handle -> Int -> IO Bool
handleCReturn ret sourceH destH bytes = do
    case ret of
         0 -> inProcessCopy sourceH destH bytes
         1 -> return $ True
         _ -> return $ False


-- | Returns true if and only if the passed in path is a named pipe.
isPipe :: String -> IO Bool
isPipe path = do
    fileInfo <- getFileStatus path
    return $ isNamedPipe fileInfo


-- |Opens a named pipe and returns the Posix Fd describing it.
openPipe :: String -> IOMode -> IO Resource
openPipe path mode = do
    h <- openFileBlocking path mode
    hSetBinaryMode h True
    fd <- handleToFd h
    fd2 <- dup fd
    newH <- fdToHandle fd2
    return $ Pipe fd newH


-- |Opens a file and returns the Posix Fd describing it.
openFile :: String -> IOMode -> IO Resource
openFile path mode = do
    h <- openBinaryFile path mode
    fd <- handleToFd h
    fd2 <- dup fd
    newH <- fdToHandle fd2
    return $ File fd newH


-- |Copies one file to another file in process memory.
inProcessCopy :: Handle -> Handle -> Int -> IO Bool
inProcessCopy sourceH destH bytes = do
    eof <- hIsEOF sourceH
    if eof then return False 
           else do
               block <- BS.hGet sourceH bytes 
               BS.hPut destH block
               return $ fromIntegral (BS.length block) == bytes

