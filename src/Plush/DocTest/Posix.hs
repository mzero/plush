{-
Copyright 2012 Google Inc. All Rights Reserved.

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

module Plush.DocTest.Posix (
    readProcessSharingStdoutStderr
    )
    where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import qualified Control.Exception as C
import System.Exit (ExitCode(..))
import System.Posix.IO (createPipe, fdToHandle)
import System.Process (createProcess, proc, std_in, std_out, std_err,
                       StdStream(..), waitForProcess)
import System.IO (hGetContents, hFlush, hClose, hPutStr)
import System.IO.Error (mkIOError)
import GHC.IO.Exception (IOErrorType(..) )

readProcessSharingStdoutStderr
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readProcessSharingStdoutStderr cmd args input = do
    (pipeInFd, pipeOutFd) <- createPipe
    procOutH <- fdToHandle pipeOutFd
    outh <- fdToHandle pipeInFd
    (Just inh, _, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = UseHandle procOutH,
                                       std_err = UseHandle procOutH }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ C.evaluate (length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return output
     ExitFailure r ->
      ioError (mkIOError OtherError ("readProcess: " ++ cmd ++
                                     ' ':unwords (map show args) ++
                                     " (exit " ++ show r ++ ")")
                                 Nothing Nothing)
