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

module Plush.Server.Warp (
    bindServerSocket,
    closeServerSocket,
    Warp.runSettingsSocket,
    )
    where


import qualified Control.Exception as Ex
import Data.Streaming.Network (bindPortTCP)
import Network.Socket (fdSocket, sClose, Socket)
import qualified Network.Wai.Handler.Warp as Warp
import System.IO.Error (alreadyInUseErrorType, isAlreadyInUseError, mkIOError)
import System.Posix.IO (FdOption(CloseOnExec), setFdOption)


-- | Create a server socket.
--
-- If a port number is supplied, then only that port is tried. Failure is
-- signaled by throwing an IOException. If no port number is supplied, then
-- a free port is hunted for. Use `socketPort` to extract the port used from
-- the return socket.
bindServerSocket :: Maybe Int -> Warp.HostPreference -> IO Socket
bindServerSocket (Just port) host = do
    socket <- bindPortTCP port host
    setSocketCloseOnExec socket `Ex.onException` closeServerSocket socket
    return socket
bindServerSocket Nothing host = tryBind [29500..29599]
  where
    tryBind [] = Ex.throwIO $ mkIOError alreadyInUseErrorType
        "No ports could be bound" Nothing Nothing
    tryBind (p:ps) = bindServerSocket (Just p) host `Ex.catch` tryNext ps
    tryNext ps e | isAlreadyInUseError e = tryBind ps
                 | otherwise = Ex.throwIO e

-- | Close a server socket.
closeServerSocket :: Socket -> IO ()
closeServerSocket = sClose


-- | Sockets used by the web server need to be set so that they are not leaked
-- into processes fork/exec'd by the shell.
--
-- NOTE: There is a race condition here. In the time between creating the socket
-- (via either 'socket' or 'accept') and calling this function, another thread
-- can fork/exec a process that will capture a copy of this socket.
--
-- Modern linux has the SOCK_CLOEXEC flag for socket, and the new accept4
-- call for setting FD_CLOEXEC atomically with the socket creation. Alas,
-- neither the Haskell nor Mac OS X have these yet.
--
-- TODO(mzero): A possible solution is put the server and shell in separate
-- processes.
setSocketCloseOnExec :: Socket -> IO ()
setSocketCloseOnExec socket =
    setFdOption (fromIntegral $ fdSocket socket) CloseOnExec True
