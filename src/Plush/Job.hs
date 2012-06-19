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

    -- * Job Input
    offerInput,

    -- * Job Output
    OutputItem(..),
    gatherOutput,

    ) where


import Control.Applicative ((<$>), (<*>))
import Control.Concurrent
import Control.Monad (when)
import Data.Aeson (Value)
import System.Exit
import System.IO
import System.Posix
import qualified System.Posix.Missing as PM

import Plush.Job.Output
import Plush.Run
import Plush.Run.Execute
import Plush.Run.Posix (decodeExitCode, encodeExitCode,
    stdJsonOutput, toByteString, write)
import Plush.Run.ShellExec
import Plush.Types


-- | Requests to the shell thread are given an identifier by the submitter.
type JobName = String

-- | As output is gathered, it is return in pieces, tagged by the source.
data OutputItem = OiStdOut String | OiStdErr String | OiJsonOut [Value]


-- | An opaque type, holding information about a running job.
-- See 'gatherOutput'
data RunningState = RS {
       rsStdIn :: Fd -- | this is the FD to write to inject into FD
     , rsStdOut :: OutputStream Char
     , rsStdErr :: OutputStream Char
     , rsJsonOut :: OutputStream Value
     , rsProcessID :: Maybe ProcessID
     , rsCheckDone :: IO ()
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
    _origInFd <- PM.dupFdCloseOnExec stdInput upperFd
    origOutFd <- PM.dupFdCloseOnExec stdOutput upperFd
    origErrFd <- PM.dupFdCloseOnExec stdError upperFd

    nfd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
    devNullFd <- PM.dupFdCloseOnExec nfd upperFd
    closeFd nfd
    mapM_ (dupTo devNullFd) [0..9] -- reserve the low FDs, yes, this is insane

    origStdOut <- fdToHandle origOutFd
    origStdErr <- fdToHandle origErrFd

    jobRequestVar <- newEmptyMVar
    scoreBoardVar <- newMVar []
    _ <- forkIO $ shellThread jobRequestVar scoreBoardVar devNullFd runner

    return (ShellThread jobRequestVar scoreBoardVar, origStdOut, origStdErr)
  where
    upperFd = Fd 20


shellThread :: MVar Request -> MVar ScoreBoard -> Fd -> Runner -> IO ()
shellThread jobRequestVar scoreBoardVar devNullFd = go
  where
    go runner = do
        request <- takeMVar jobRequestVar

        (master1, slave1) <- openPseudoTerminal
        (master2, slave2) <- openPseudoTerminal
        _ <- slave1 `dupTo` stdInput
        slave1 `moveTo` stdOutput
        slave2 `moveTo` stdError

        readFromJsonOutput <- outPipeFor stdJsonOutput

        frs <- RS master1
                <$> outputStreamUtf8 master1
                <*> outputStreamUtf8 master2
                <*> outputStreamJson readFromJsonOutput

        let closeMasters = mapM_ (\fd -> closeFd fd `catch` const (return ()))
                                [ master1, master2, readFromJsonOutput ]

        runner' <- runJob scoreBoardVar request frs closeMasters runner

        mapM_ (dupTo devNullFd) [0..9] -- keep 'em reserved


        go runner'

    outPipeFor destFd = createPipe >>= \(r, w) -> w `moveTo` destFd >> return r
    srcFd `moveTo` destFd = dupTo srcFd destFd >> closeFd srcFd


runJob :: MVar ScoreBoard -> Request
    -> (Maybe ProcessID -> IO () -> RunningState) -> IO ()
    -> Runner -> IO Runner
runJob scoreBoardVar (j, cl) frs closeMs r0 = do
    (et, r1) <- run (execType cl) r0
    case et of
        ExecuteForeground -> foreground r1
        ExecuteMidground  -> background r1
        ExecuteBackground -> foreground r1 -- TODO: fix!
  where
    foreground runner = do
        setUp Nothing (return ())
        (exitStatus, runner') <- runCommand runner
        cleanUp exitStatus
        return runner'

    background runner = do
        pid <- forkProcess (closeMs >> runCommand runner >>= exitWith . encodeExitCode . fst)
        setUp (Just pid) $ checkUp pid
        return runner

    runCommand runner = runCommandList cl runner >>= run getLastExitCode

    setUp mpid checker = do
        modifyMVar_ scoreBoardVar $ \sb ->
            return $ (j, Running $ frs mpid checker) : sb

    checkUp pid = do
        mStat <- getProcessStatus False False pid
        case mStat of
            Nothing             -> return ()
            Just (Exited ec)    -> cleanUp $ decodeExitCode ec
            Just (Terminated _) -> cleanUp 129
            Just (Stopped _)    -> return ()

    cleanUp exitStatus = do
        modifyMVar_ scoreBoardVar $ \sb -> do
            let (f,sb') = findJob j sb
            case f of
                Just (Running rs') -> do
                    oi <- gatherOutput rs'
                    return $ (j, JobDone exitStatus oi):sb'
                _ -> return sb
        closeMs



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
reviewJobs st act = do
    readMVar (stScoreBoard st) >>= sequence_ . map checker
    modifyMVar (stScoreBoard st) $ \sb -> do
        r <- mapM (uncurry act) sb
        return (filter running sb, r)
  where
    checker (_, Running rs) = rsCheckDone rs
    checker _ = return ()

    running (_, JobDone _ _) = False
    running _ = True

-- | Find a job by name in a 'ScoreBoard'. Returns the first 'Status' found,
-- if any, and a copy of the 'ScoreBoard' without that job enry.
findJob :: JobName -> ScoreBoard -> (Maybe Status, ScoreBoard)
findJob j ((j',s):sbs) | j == j' = (Just s,sbs)
findJob j (sb:sbs) = let (s,sbs') = findJob j sbs in (s,sb:sbs')
findJob _ [] = (Nothing,[])


-- | Give input to a running job. If the job isn't running, then the input is
-- dropped on the floor. The boolean indicates if EOF should be signaled after
-- the input is sent.
offerInput :: ShellThread -> JobName -> String -> Bool -> Maybe Signal -> IO ()
offerInput st job input eof sig = modifyMVar_ (stScoreBoard st) $ \sb -> do
    case findJob job sb of
        (Just (Running rs), _) -> do  -- TODO: should catch errors here
            when (not $ null input) $ write (rsStdIn rs) $ toByteString input
            when eof $ closeFd (rsStdIn rs)
            case (sig, rsProcessID rs) of
                (Just s, Just p) -> signalProcess s p
                _ -> return ()
        _ -> return ()
    return sb

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



