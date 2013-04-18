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

{-# LANGUAGE OverloadedStrings #-}

module Plush.Server (
    ServerType(..),     -- re-exported
    ServerStart(..),

    serverStart,
    serverStop,
    serverStatus,
    serverRun,

    serversList,
    )
    where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as Ex
import Control.Monad (void, when)
import Control.Monad.Exception (catchIOError)
import Data.Aeson
import System.Posix
import System.Process (rawSystem)

import Plush.Run
import Plush.Run.Posix.Return (success, exitMsg)
import Plush.Run.Posix.Utilities (outStrLn)
import Plush.Run.Types
import Plush.Server.Remote
import Plush.Server.Status
import Plush.Server.WebServer
import Plush.Utilities

-- | Information needed to start a server. Gleaned from the command line.
data ServerStart = LocalStart (IO Runner) | RemoteStart String

serverStartType :: ServerStart -> ServerType
serverStartType (LocalStart _) = LocalServer
serverStartType (RemoteStart endpoint) = RemoteServer endpoint

-- | Check to see if a given process is running.
-- This function never throws, and will return `False` when access permissions
-- restrict inquiring about a process.
checkIfRunning :: ProcessID -> IO Bool
checkIfRunning pid = (getProcessPriority pid >> return True)
    `Ex.catch` ((\_ -> return False) :: IOError -> IO Bool)

-- | Check if a server seems to be up and running. If the server information
-- appears stale, this function will remove it.
check :: ServerType -> IO (Either String ServerInfo)
check st = readServerInfo st >>= maybe missing present
  where
    missing = return $ Left "no server info found"
    present si = do
        r <- checkIfRunning $ siPid si
        if r
            then return $ Right si
            else do
                removeServerInfo st
                makeLogger si >>= ($ "appears dead, removing server info")
                return $ Left "server appears dead, removing server info"

-- | Start a server, if it isn't already running.
start :: ServerStart -> Maybe Int -> IO (Either String ServerInfo)
start sst mPort =
    readServerInfo (serverStartType sst) >>= maybe missing present
  where
    present _ = return $ Left
                    "Not starting: server info found.\n\
                    \Use --server status to find out more.\n\
                    \Use --server stop to stop it."
    missing = case sst of
        (LocalStart mkRunner) -> startLocal mkRunner mPort
        (RemoteStart endpoint) -> startRemote endpoint mPort

-- | Stop a server. Several means are tired, including both TERM and KILL
-- signals.
stop :: ServerType -> IO (Either String ServerInfo)
stop st = readServerInfo st >>= maybe missing present
  where
    missing = return $ Left "no server info found"
    present si = do
        stopped <- tryToStop
            [ notRunning
            , gracefulStop st
            , signalStop softwareTermination
            , signalStop killProcess
            ]
        if stopped
            then do
                removeServerInfo st
                return $ Right si
            else return $ Left "couldn't stop the process even with kill"
      where
        tryToStop [] = logStop "couldn't stop" >> return False
        tryToStop ((stopper, whenStopped):more) = do
            _ <- stopper
            r <- checkIfRunning (siPid si)
            if r then tryToStop more else whenStopped >> return True

        notRunning = (return (), logStop "already stopped")
        gracefulStop LocalServer = (return (), return ())
        gracefulStop (RemoteServer _) = (stopRemote si, return ())
        signalStop sig =
            ( signalProcess sig (siPid si) >> threadDelay 100000
                -- TODO(mzero) is .1s correct?
            , logStop $ "stopped by signal " ++ show sig
            )
        logStop msg = makeLogger si >>= ($ msg)

-- | Format the output of one of the server operations based on the requested
-- verbosity level.
report :: ServerType -> (ServerInfo -> String)
    -> OutputVerbosity -> Either String ServerInfo -> IO ExitCode
report _ _ OutputQuiet (Left _) = return $ ExitFailure 1
report _ _ OutputQuiet (Right _) = return ExitSuccess

report st _ OutputNormal (Left msg) =
    exitMsg 1 $ serverTypePrefix st ++ ": " ++ msg
report _  f OutputNormal (Right si) =
    outStrLn (serverInfoPrefix si ++ ": " ++ f si) >> success

report _ _ OutputJson (Left msg) = do
    outStrLn (encode $ object ["error" .= msg])
    return $ ExitFailure 1
report _ _ OutputJson (Right si) = outStrLn (encode si) >> success

-- | Start a server command.
serverStart :: ServerStart -> OutputVerbosity -> Maybe Int -> IO ExitCode
serverStart sst verbosity mPort =
    start sst mPort >>= report (serverStartType sst) startedMsg verbosity
  where
    startedMsg si = "started\n" ++ startUrl si

-- | Stop a server command.
serverStop :: ServerType -> OutputVerbosity -> IO ExitCode
serverStop st verbosity =
    stop st >>= report st (const $ "stopped") verbosity

-- | Find status of a server command.
serverStatus :: ServerType -> OutputVerbosity -> IO ExitCode
serverStatus st verbosity = check st >>= report st statusMsg verbosity
  where
    statusMsg si = startUrl si

-- | Start a server if not already running, and then (optionally) launch a
-- web browser on the URL of the server.
serverRun :: ServerStart -> OutputVerbosity -> Bool -> IO ExitCode
serverRun sst verbosity launch = do
    s1 <- check st
    s2 <- either (const $ start sst Nothing) (return . Right) s1
    case (s2, launch) of
        (Right si, True) -> do
            let url = startUrl si
            ec <- safeRawSystem "xdg-open" [url]
            when (ec /= ExitSuccess) $ void $ safeRawSystem "open" [url]
        _ -> return ()
    report (serverStartType sst) msg verbosity s2
  where
    st = serverStartType sst
    safeRawSystem cmd args = rawSystem cmd args
        `catchIOError` (\_ -> return $ ExitFailure 1)
    msg si = "running, "
        ++ if launch then "launching web page" else "url:\n" ++ startUrl si

-- | List information about all servers known.
serversList :: OutputVerbosity -> IO ExitCode
serversList verbosity =
    allKnownServers >>= mapM list >>= return . summarizeExit
  where
    list st = check st >>= report st (const "running") verbosity
    summarizeExit = intToExitCode . maximum . (0:) . map exitCodeToInt
