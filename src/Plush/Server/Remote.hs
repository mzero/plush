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

module Plush.Server.Remote (
    startRemote,
    stopRemote,
    )
    where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Error (catchError, runErrorT, throwError)
import Data.Aeson
import Data.String (fromString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Network.Socket (socketPort)
import System.Process (rawSystem, readProcessWithExitCode)

import Plush.Run.Types
import Plush.Server.Status
import Plush.Server.Warp

-- | Start an ssh connection to a remote host, and estabilsh a forwarded
-- connection to the plush server running locally there.
--
-- A ssh connection in master mode is started first, letting the user interact
-- with the process to authenticate as needed before it detaches. Then, using
-- that connection, further ssh commands probe the pid of the detached ssh
-- master, start the remote plush server, and finally establish a local port
-- forwarded to the remote.
startRemote :: String -> Maybe Int -> IO (Either String ServerInfo)
startRemote endpoint mPort = runErrorT $ do
    cfp <- lift sshControlFilePath >>=
                    maybe (throwError "HOME not set in environment") return
    sshUser "ssh"
        [ "-f"                          -- background after auth
        , "-M"                          -- control master mode
        , "-S", cfp                     -- control path
        , "-o", "ControlPersist=yes"    -- keep open
        , "-N"                          -- no command to execute
        , "-y"                          -- log to syslog, not stderr
            -- TODO(mzero): should capture stderr and log to our log file
        , endpoint
        ]
    flip catchError (killMaster cfp) $ do
        (_, pidline) <- ssh "ssh check"
            [ "-S", cfp
            , "-O", "check"
            , endpoint
            ]
        localPid <- decodePidLine pidline
        (siline, _) <- ssh "remote plush"
            [ "-S", cfp
            , "-T"
            , endpoint
            , "plush", "--local", "--json", "no-launch"
            ]
        remoteInfo <- decodeServerInfo siline
        let remotePort = siPort remoteInfo
        localPort <- maybe (lift genPort) return mPort
        _ <- ssh "port forwarding"
            [ "-S", cfp
            , "-O", "forward"
            , "-L", "localhost:" ++ show localPort
                        ++ ":localhost:" ++ show remotePort
            , endpoint
            ]
        lift $ do
            let localInfo = ServerInfo
                                { siType = RemoteServer endpoint
                                , siPid = localPid
                                , siPort = localPort
                                , siKey = siKey remoteInfo
                                }
            logger <- makeLogger localInfo
            logger $ "start, port " ++ show localPort
            writeServerInfo (RemoteServer endpoint) localInfo
            return localInfo

  where
    sshUser stage args = do
        ec <- lift $ rawSystem "ssh" args
        gateExitCode ec stage ""

    ssh stage args = do
        (ec, out, err) <- lift $ readProcessWithExitCode "ssh" args ""
        gateExitCode ec stage $ '\n' : out ++ err
        return (out, err)

    killMaster cfp e = do
        _ <- lift $ readProcessWithExitCode
            "ssh" [ "-S", cfp, "-O", "exit", endpoint] ""
        throwError e

    gateExitCode (ExitFailure n) stage out =
        throwError $ stage ++ " failure (" ++ show n ++ ")" ++ out
    gateExitCode ExitSuccess _ _ = return ()

    decodeServerInfo s = case (decode' $ T.encodeUtf8 $ T.pack s) of
        Nothing -> throwError $ "remote plush output error: " ++ s
        Just si -> return si

    decodePidLine s = go s
      where
        go [] = throwError $ "ssh check pid parse failed: " ++ s
        go ('p':'i':'d':'=':t) = case reads t of
            ((n,_):_) -> return n
            _ -> go []
        go (_:t) = go t

    genPort :: IO Int
    genPort = do
        sock <- bindServerSocket Nothing (fromString "127.0.0.1")
        port <- fromIntegral `fmap` socketPort sock
        closeServerSocket sock
        return port

-- | Stop a connection to a remote host.
stopRemote :: ServerInfo -> IO ()
stopRemote si = sshControlFilePath >>= \mcfp -> case (mcfp, siType si) of
    (Just cfp, RemoteServer endpoint) -> do
        _ <- readProcessWithExitCode
                "ssh" [ "-S", cfp, "-O", "exit", endpoint] ""
        threadDelay 100000 -- TODO(mzero): is 0.1s the right delay?
        makeLogger si >>= ($ "stop")
    _ -> return ()
