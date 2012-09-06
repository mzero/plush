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
    submitJob,
    pollJobs,
    offerInput,

    ) where


import Control.Applicative ((<$>), (<*>))
import Control.Concurrent
import qualified Control.Exception as Exception
import Control.Monad (when)
import Data.Aeson (encode, ToJSON, Value)
import System.Exit
import System.IO
import System.Posix
import qualified System.Posix.Missing as PM

import Plush.Job.Output
import Plush.Job.Types
import Plush.Parser
import Plush.Run
import Plush.Run.Execute
import Plush.Run.Posix (stdJsonOutput, toByteString, write)
import Plush.Run.ShellExec



-- | An opaque type, holding information about a running job.
-- See 'gatherOutput'
data RunningState = RS {
       rsStdIn :: Fd -- | this is the FD to write to inject into FD
     , rsStdOut :: OutputStream Char
     , rsStdErr :: OutputStream Char
     , rsJsonOut :: OutputStream Value
     , rsProcessID :: Maybe ProcessID
     , rsCheckDone :: IO ()
     , rsHistory :: Maybe Fd -- | the history file for this command
     }

-- | A job can be in one of these states:
data Status
    = Running RunningState
        -- ^ the 'RunningState' value can be used with 'gatherOutput'
    | JobDone ExitCode [OutputItem]
        -- ^ the exit status of the job, and any remaining gathered output

type ScoreBoard = [(JobName, Status)]

-- | A handle to the shell thread.
data ShellThread = ShellThread
    { stJobRequest :: MVar CommandRequest
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


shellThread :: MVar CommandRequest -> MVar ScoreBoard -> Fd -> Runner -> IO ()
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

        let closeMasters = mapM_ (\fd -> closeFd fd `Exception.catch` ignore)
                [ master1, master2, readFromJsonOutput ]
            -- TODO(elaforge): catch something more specific
            ignore :: Exception.SomeException -> IO ()
            ignore = const (return ())

        runner' <- runJob scoreBoardVar request frs closeMasters runner

        mapM_ (dupTo devNullFd) [0..9] -- keep 'em reserved


        go runner'

    outPipeFor destFd = createPipe >>= \(r, w) -> w `moveTo` destFd >> return r
    srcFd `moveTo` destFd = dupTo srcFd destFd >> closeFd srcFd


runJob :: MVar ScoreBoard -> CommandRequest
    -> (Maybe ProcessID -> IO () -> Maybe Fd -> RunningState) -> IO ()
    -> Runner -> IO Runner
runJob scoreBoardVar (CommandRequest j rec (CommandItem cmd)) frs closeMs r0 = do
    case parseNextCommand cmd of
        Left _errs -> return r0    -- FIXME
        Right (cl, _rest) -> runJob' cl
  where
    runJob' cl = do
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
            pid <- forkProcess (closeMs >> runCommand runner >>= exitWith . fst)
            setUp (Just pid) $ checkUp pid
            return runner

        runCommand runner = runCommandList cl runner >>= run getLastExitCode

    setUp mpid checker = do
        hFd <- if rec
            then Just `fmap` openFd "/tmp/history" ReadWrite (Just ownerRWMode) defaultFileFlags
            else return Nothing
        let rs = frs mpid checker hFd
        writeHistory rs (CommandItem cmd)
        modifyMVar_ scoreBoardVar $ \sb ->
            return $ (j, Running rs) : sb

    ownerRWMode = ownerReadMode `unionFileModes` ownerWriteMode

    checkUp pid = do
        mStat <- getProcessStatus False False pid
        case mStat of
            Nothing             -> return ()
            Just (Exited ec)    -> cleanUp ec
            Just (Terminated _) -> cleanUp $ ExitFailure 129
            Just (Stopped _)    -> return ()

    cleanUp exitCode = do
        modifyMVar_ scoreBoardVar $ \sb -> do
            let (f,sb') = findJob j sb
            case f of
                Just (Running rs') -> do
                    oi <- gatherOutput rs'
                    writeHistory rs' $ HiFinished $ FinishedItem exitCode
                    maybe (return ()) closeFd (rsHistory rs')
                    return $ (j, JobDone exitCode oi):sb'
                _ -> return sb
        closeMs



-- | Submit a job for processing.
-- N.B.: This may block until the shell thread is able to receive a request.
submitJob :: ShellThread -> CommandRequest -> IO ()
submitJob st cr = putMVar (stJobRequest st) cr

-- | Poll jobs for status. A `RunningItem` or `FinishedItem` is returned for
-- each job the shell knows about. `OutputItem` entries may be returned for
-- either kind of job, and will come first.
--
-- N.B.: Finished jobs are removed from internal book-keeping once reviewed.
pollJobs :: ShellThread -> IO [ReportOne StatusItem]
pollJobs st = do
    readMVar (stScoreBoard st) >>= sequence_ . map checker
    modifyMVar (stScoreBoard st) $ \sb -> do
        r <- mapM (uncurry report) sb
        return (filter running sb, concat r)
  where
    checker (_, Running rs) = rsCheckDone rs
    checker _ = return ()

    running (_, JobDone _ _) = False
    running _ = True

    report :: JobName -> Status -> IO [ReportOne StatusItem]
    report job (Running rs) = do
        ois <- gatherOutput rs
        report' job ois $ SiRunning RunningItem

    report job (JobDone e ois) =
        report' job ois $ SiFinished (FinishedItem e)

    report' job ois si = return $ map (ReportOne job) $ map SiOutput ois ++ [si]

-- | Gather as much output is available from stdout, stderr, and jsonout.
gatherOutput :: RunningState -> IO [OutputItem]
gatherOutput rs = do
    out <- wrap OutputItemStdOut <$> getAvailable (rsStdOut rs)
    err <- wrap OutputItemStdErr <$> getAvailable (rsStdErr rs)
    jout <- wrap OutputItemJsonOut <$> getAvailable (rsJsonOut rs)
    let ois = out ++ err ++ jout
    mapM_ (writeHistory rs . HiOutput) ois
    return ois
  where
    wrap _ [] = []
    wrap c vs = [c vs]


writeHistory :: (ToJSON a) => RunningState -> a -> IO ()
writeHistory rs s = maybe (return ()) (flip write $ encode s) $ rsHistory rs


-- | Find a job by name in a 'ScoreBoard'. Returns the first 'Status' found,
-- if any, and a copy of the 'ScoreBoard' without that job enry.
findJob :: JobName -> ScoreBoard -> (Maybe Status, ScoreBoard)
findJob j ((j',s):sbs) | j == j' = (Just s,sbs)
findJob j (sb:sbs) = let (s,sbs') = findJob j sbs in (s,sb:sbs')
findJob _ [] = (Nothing,[])


-- | Give input to a running job. If the job isn't running, then the input is
-- dropped on the floor.
offerInput :: ShellThread -> JobName -> InputItem -> IO ()
offerInput st job input = modifyMVar_ (stScoreBoard st) $ \sb -> do
    case findJob job sb of
        (Just (Running rs), _) -> send input rs
            -- TODO: should catch errors here
        _ -> return ()
    return sb
  where
    send (InputItemInput s) rs = write (rsStdIn rs) $ toByteString s
    send (InputItemEof) rs = closeFd (rsStdIn rs)
    send (InputItemSignal sig) rs =
        maybe (return ()) (signalProcess sig) $ rsProcessID rs


