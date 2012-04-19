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

module Plush.Job (
    -- * Shell Thread
    startShell,
    ShellThread,

    -- * Jobs
    JobName,
    Status(..),
    RunningState,
    submitJob,
    reviewJobs,

    -- * Job Output
    OutputItem(..),
    gatherOutput,

    ) where


import Control.Applicative ((<$>), (<*>))
import Control.Concurrent
import Data.Aeson (Value)
import System.IO
import System.Posix

import Plush.Job.Output
import Plush.Run
import Plush.Run.Posix (stdJsonOutput)
import Plush.Run.ShellExec
import Plush.Types


-- | Requests to the shell thread are given an identifier by the submitter.
type JobName = String

-- | As output is gathered, it is return in pieces, tagged by the source.
data OutputItem = OiStdOut String | OiStdErr String | OiJsonOut [Value]


-- | An opaque type, holding information about a running job.
-- See 'gatherOutput'
data RunningState = RS {
       rsStdOut :: OutputStream Char
     , rsStdErr :: OutputStream Char
     , rsJsonOut :: OutputStream Value
     }

-- | A job can be in one of these states:
data Status
    = Running RunningState
        -- ^ the 'RunningState' value can be used with 'gatherOutput'
    | JobDone Int [OutputItem]
        -- ^ the exit status of the job, and any remaining gathered output

type Request = (JobName, CommandList)
type ScoreBoard = [(JobName, Status)]

-- | A handle to the shell thread.
data ShellThread = ShellThread
    { stJobRequest :: MVar Request
    , stScoreBoard :: MVar ScoreBoard
    }

-- | Start the shell as an indpendent thread, which will process jobs and
-- report on their status.
--
-- N.B.: The shell uses 'stdout' and 'stderr' for its own purposes, and so after
-- the shell is stated, they must not be used for communicating. The returned
-- handles are tied to the original 'stdout' and 'stderr' and can be used to
-- communicate to the original streams.
startShell :: Runner -> IO (ShellThread, Handle, Handle)
startShell runner = do
    origStdOut <- dupTo stdOutput 10 >>= fdToHandle
    origStdErr <- dupTo stdError 11 >>= fdToHandle

    rs <- RS <$> (pipeFor stdOutput >>= outputStreamUtf8)
             <*> (pipeFor stdError >>= outputStreamUtf8)
             <*> (pipeFor stdJsonOutput >>= outputStreamJson)

    jobRequestVar <- newEmptyMVar
    scoreBoardVar <- newMVar []
    _ <- forkIO $ shellThread jobRequestVar scoreBoardVar rs runner

    return (ShellThread jobRequestVar scoreBoardVar, origStdOut, origStdErr)
  where
    pipeFor destFd = do
        (readFd, writeFd) <- createPipe
        _ <- dupTo writeFd destFd
        closeFd writeFd
        return readFd


shellThread :: MVar Request -> MVar ScoreBoard -> RunningState -> Runner -> IO ()
shellThread jobRequestVar scoreBoardVar rs = go
  where
    go runner = do
        (j, cl) <- takeMVar jobRequestVar
        modifyMVar_ scoreBoardVar $ \sb ->
            return $ (j, Running rs) : sb
        runner' <- runCommandList cl runner
        (exitStatus, runner'') <- run getLastExitCode runner'
        modifyMVar_ scoreBoardVar $ \sb -> do
            let (f,sb') = findJob j sb
            case f of
                Just (Running rs') -> do
                    oi <- gatherOutput rs'
                    return $ (j, JobDone exitStatus oi):sb'
                _ -> return sb
        go runner''


-- | Submit a job for processing.
-- N.B.: This may block until the shell thread is able to receive a request.
submitJob :: ShellThread -> JobName -> CommandList -> IO ()
submitJob st job cl = putMVar (stJobRequest st) (job, cl)

-- | Review jobs. Running jobs and recently done jobs are be passed to the
-- supplied review action. The list of results from the review action is
-- returned.
--
-- N.B.: Done jobs are removed from internal book-keeping once reviewed.
reviewJobs :: ShellThread -> (JobName -> Status -> IO a) -> IO [a]
reviewJobs st act = modifyMVar (stScoreBoard st) $ \sb -> do
    r <- mapM (uncurry act) sb
    return (filter running sb, r)
  where
    running (_, (JobDone _ _)) = False
    running _ = True

-- | Find a job by name in a 'ScoreBoard'. Returns the first 'Status' found,
-- if any, and a copy of the 'ScoreBoard' without that job enry.
findJob :: JobName -> ScoreBoard -> (Maybe Status, ScoreBoard)
findJob j ((j',s):sbs) | j == j' = (Just s,sbs)
findJob j (sb:sbs) = let (s,sbs') = findJob j sbs in (s,sb:sbs')
findJob _ [] = (Nothing,[])

-- | Gather as much output is available from stdout, stderr, and jsonout.
gatherOutput :: RunningState -> IO [OutputItem]
gatherOutput rs = do
    out <- wrap OiStdOut <$> getAvailable (rsStdOut rs)
    err <- wrap OiStdErr <$> getAvailable (rsStdErr rs)
    jout <- wrap OiJsonOut <$> getAvailable (rsJsonOut rs)
    return $ out ++ err ++ jout
  where
    wrap _ [] = []
    wrap c vs = [c vs]



