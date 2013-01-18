{-
Copyright 2013 Google Inc. All Rights Reserved.

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

{-# Language RecordWildCards #-}

module Plush.Job.StdIO (
    RunningIO(..),
    StdIOParts,
    makeStdIOParts,
    stdIOChildPrep,
    stdIOMasterPrep,
    stdIOLocalPrep,
    stdIOCloseMasters,
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (Value)
import System.Posix
import qualified System.Posix.Missing as PM

import Plush.Job.Output
import Plush.Run.Posix (stdJsonOutput)


-- | These are the parent side of the pipes and terminals to a running job.
-- Note that the sense is inverted for the parent. That is: The parent writes
-- to 'rioStdIn' to have the child read from 'stdInput'. The parent reads from
-- 'rioStdOut' to read what the child has written to 'stdOutput'.
data RunningIO = RIO {
       rioStdIn :: Fd
     , rioStdOut :: OutputStream Char
     , rioStdErr :: OutputStream Char
     , rioJsonOut :: OutputStream Value
     }


-- | An opaque type holding the file descriptors used when setting up the
-- standard IO environment for command execuction.
data StdIOParts = StdIOParts
    { master1, master2, masterJ, slave1, slave2, slaveJ :: Fd }

-- | Allocate the pieces needed for a standard IO execution environment.
makeStdIOParts :: IO StdIOParts
makeStdIOParts = do
    (m1, s1) <- openPseudoTerminal
    (m2, s2) <- openPseudoTerminal
    (mJ, sJ) <- createPipe
    mapM_ (\fd -> setFdOption fd CloseOnExec True) [m1, m2, mJ]
    return $ StdIOParts m1 m2 mJ s1 s2 sJ

-- | Prepare a child process for standard IO. Note that this will also set up
-- a the controlling terminal and session. Call this in the child side of a
-- fork.
stdIOChildPrep :: StdIOParts -> IO ()
stdIOChildPrep sp@StdIOParts {..} = do
    stdIOCloseMasters sp
    _ <- createSession
    PM.setControllingTerminal slave1
    stdIOInstallSlaves sp

-- | Prepare the master process for communicating with the child. Call this in
-- the parent side of a fork.
stdIOMasterPrep :: StdIOParts -> IO RunningIO
stdIOMasterPrep sp@StdIOParts {..} = do
    stdIOCloseSlaves sp
    stdIOBuildRio sp

-- | Prepare a standard IO environment for the master process itself. Unlike,
-- 'stdIOChildPrep' the controlling terminal and session are not altered.
stdIOLocalPrep :: StdIOParts -> IO RunningIO
stdIOLocalPrep sp = do
    stdIOInstallSlaves sp
    stdIOBuildRio sp


stdIOInstallSlaves :: StdIOParts -> IO ()
stdIOInstallSlaves sp@StdIOParts {..} = do
    _ <- dupTo slave1 stdInput
    _ <- dupTo slave1 stdOutput
    _ <- dupTo slave2 stdError
    _ <- dupTo slaveJ stdJsonOutput
    stdIOCloseSlaves sp

stdIOBuildRio :: StdIOParts -> IO RunningIO
stdIOBuildRio StdIOParts {..} = do
    RIO master1
        <$> outputStreamUtf8 master1
        <*> outputStreamUtf8 master2
        <*> outputStreamJson masterJ

stdIOCloseSlaves :: StdIOParts -> IO ()
stdIOCloseSlaves StdIOParts {..} = do
    closeFd slave1
    closeFd slave2
    closeFd slaveJ

stdIOCloseMasters :: StdIOParts -> IO ()
stdIOCloseMasters StdIOParts {..} = do
    closeFd master1
    closeFd master2
    closeFd masterJ
