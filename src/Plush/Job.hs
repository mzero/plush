{-
Copyright 2012-2013 Google Inc. All Rights Reserved.

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
    withHistory,

    ) where


import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad (forM_, unless, void)
import qualified Control.Monad.Exception as Exception
import System.Exit
import System.IO
import System.Posix
import qualified System.Posix.Missing as PM

import Plush.Job.History
import Plush.Job.Output
import Plush.Job.StdIO
import Plush.Job.Types
import Plush.Run
import Plush.Run.Execute (execType, ExecuteType(..))
import Plush.Run.Posix (write)
import Plush.Run.Posix.Utilities (toByteString)
import Plush.Run.ShellExec (getFlags, setFlags)
import qualified Plush.Run.ShellFlags as F



-- | An opaque type, holding information about a running job.
-- See 'gatherOutput'
data RunningState = RS {
       rsIO :: RunningIO
     , rsProcessID :: Maybe ProcessID
     , rsCheckDone :: IO ()
     , rsHistory :: HistoryFile
     }

-- | A job can be in one of these states:
data Status
    = Running RunningState
        -- ^ the 'RunningState' value can be used with 'gatherOutput'
    | JobDone ExitCode [OutputItem]
        -- ^ the exit status of the job, and any remaining gathered output
    | ParseError String
        -- ^ the command didn't parse

type ScoreBoard = [(JobName, Status)]

-- | A handle to the shell thread.
data ShellThread = ShellThread
    { stJobRequest :: MVar CommandRequest
    , stScoreBoard :: MVar ScoreBoard
    , stHistory :: MVar History
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
    origInFd <- PM.dupFdCloseOnExec stdInput upperFd
    origOutFd <- PM.dupFdCloseOnExec stdOutput upperFd
    origErrFd <- PM.dupFdCloseOnExec stdError upperFd

    reserveShellFds

    origStdOut <- fdToHandle origOutFd
    origStdErr <- fdToHandle origErrFd

    foregroundStdIOParts <- makeStdIOParts
    foregroundRIO <- stdIOLocalPrep foregroundStdIOParts

    jobRequestVar <- newEmptyMVar
    scoreBoardVar <- newMVar []
    historyVar <- initHistory runner >>= newMVar
    let st = ShellThread jobRequestVar scoreBoardVar historyVar
    let childPrep = do
            mapM_ closeFdSafe [origInFd, origOutFd, origErrFd]
                -- TODO(mzero): closeFdSafe because origOutFd mysteriously
                -- is closed by the time we fork a child... why?
            stdIOCloseMasters foregroundStdIOParts
    _ <- forkIO $ shellThread st foregroundRIO origStdErr childPrep runner

    return (st, origStdOut, origStdErr)
  where
    upperFd = Fd 20

    -- | Reserve FDs 0 - 9 as these can addressed by shell commands.
    reserveShellFds = do
        devNullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
        forM_ [0..9] $ \fd -> do
            unless (devNullFd == fd) $ void $ dupTo devNullFd fd
            setFdOption fd CloseOnExec True
        unless (devNullFd <= 0) $ closeFd devNullFd



shellThread :: ShellThread -> RunningIO -> Handle -> IO () -> Runner -> IO ()
shellThread st foregroundRIO origStdErr childPrep = go
  where
    go runner =
        (takeMVar (stJobRequest st) >>= runJob' runner >>= go)
        `Exception.catchAll`
        (\e -> hPutStrLn origStdErr (show e) >> go runner)
    runJob' = runJob st foregroundRIO childPrep

runJob :: ShellThread -> RunningIO -> IO () -> Runner -> CommandRequest -> IO Runner
runJob st foregroundRIO childPrep r0
        cr@(CommandRequest j record (CommandItem cmd)) = do
    (pr, r1) <- run (parse cmd) r0
        -- This won't echo even if verbose (-v) or parseout (-P) are set
    case pr of
        Left errs -> parseError errs >> return r1
        Right (cl, _rest) -> do
            (et, r2) <- run (execType cl) r1
            case et of
                ExecuteForeground -> foreground cl r2
                ExecuteMidground  -> background cl r2
                ExecuteBackground -> foreground cl r2 -- TODO: fix!
  where
    foreground cl runner = do
        setUp Nothing foregroundRIO (return ())
        (exitStatus, runner') <- runCommand cl runner
        cleanUp (return ()) exitStatus
        return runner'

    background cl runner = do
        sp <- makeStdIOParts
        pid <- forkProcess $ do
                    childPrep
                    stdIOChildPrep sp
                    (exitStatus, _runner') <- runCommand cl runner
                    exitWith exitStatus
        rio <- stdIOMasterPrep sp
        setUp (Just pid) rio $ checkUp (stdIOCloseMasters sp) pid
        return runner

    runCommand cl runner = run (runEnv $ execute cl) runner
    runEnv act = if record
        then act
        else Exception.bracket getFlags setFlags $
                \f -> setFlags (utilFlags f) >> act
            -- If the command is not being recorded, it is being run by the
            -- the front end itself. In this case, don't let the noexec (-n)
            -- or xtrace (-x) interfere with the execution or output.
    utilFlags f = f { F.noexec = False, F.xtrace = False }

    parseError errs =
        modifyMVar_ (stScoreBoard st) $ \sb ->
            return $ (j, ParseError errs) : sb

    setUp mpid rio checker = do
        hFd <- modifyMVar (stHistory st) $ startHistory cr
        let rs = RS rio mpid checker hFd
        writeHistory (rsHistory rs) (HiCommand $ CommandItem cmd)
        modifyMVar_ (stScoreBoard st) $ \sb ->
            return $ (j, Running rs) : sb

    checkUp closeMs pid = do
        mStat <- getProcessStatus False False pid
        case mStat of
            Nothing             -> return ()
            Just (Exited ec)    -> cleanUp closeMs ec
            Just (Terminated _) -> cleanUp closeMs $ ExitFailure 129
            Just (Stopped _)    -> return ()

    cleanUp closeMs exitCode = do
        modifyMVar_ (stScoreBoard st) $ \sb -> do
            let (f,sb') = findJob j sb
            case f of
                Just (Running rs') -> do
                    oi <- gatherOutput rs'
                    writeHistory (rsHistory rs')
                        $ HiFinished $ FinishedItem exitCode
                    endHistory (rsHistory rs')
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

    running (_, Running _) = True
    running _ = False

    report :: JobName -> Status -> IO [ReportOne StatusItem]
    report job (Running rs) = do
        ois <- gatherOutput rs
        report' job ois $ SiRunning RunningItem

    report job (JobDone e ois) =
        report' job ois $ SiFinished (FinishedItem e)

    report job (ParseError e) = return $ map (ReportOne job)
        [ SiParseError (ParseErrorItem e)
        , SiFinished (FinishedItem $ ExitFailure 2)
        ]

    report' job ois si = return $ map (ReportOne job) $ map SiOutput ois ++ [si]

-- | Gather as much output is available from stdout, stderr, and jsonout.
-- Note that the order of output to stderr and stdout is lost given the way we
-- collect these from separate pttys. This infelicity is the trade off for being
-- able which text came from stderr vs. stdout. stderr is gathered and reported
-- which tends to work better. It also ensures that xtrace (-x) output preceeds
-- the command's output.
gatherOutput :: RunningState -> IO [OutputItem]
gatherOutput rs = do
    err <- wrap OutputItemStdErr <$> getAvailable (rioStdErr $ rsIO rs)
    out <- wrap OutputItemStdOut <$> getAvailable (rioStdOut $ rsIO rs)
    jout <- wrap OutputItemJsonOut <$> getAvailable (rioJsonOut $ rsIO rs)
    let ois = err ++ out ++ jout
    mapM_ (writeOutput (rsHistory rs)) ois
    return ois
  where
    wrap _ [] = []
    wrap c vs = [c vs]



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
    send (InputItemInput s) rs = write (rioStdIn $ rsIO rs) $ toByteString s
    send (InputItemEof) rs = closeFd (rioStdIn $ rsIO rs)
    send (InputItemSignal sig) rs =
        maybe (return ()) (signalProcess sig) $ rsProcessID rs


-- | Wrapper for adapting functions on History.
withHistory :: ShellThread -> (History -> IO a) -> IO a
withHistory st = withMVar (stHistory st)
