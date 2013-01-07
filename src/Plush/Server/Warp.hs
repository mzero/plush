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
    runSettings,
    )
    where


import Control.Exception (bracket)
import Data.Conduit.Network (bindPort)
import Network.Sendfile (sendfileWithHeader, FileRange(PartOfFile))
import Network.Socket (accept, fdSocket, sClose, Socket)
import qualified Network.Socket.ByteString as Sock
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import System.Posix.IO (FdOption(CloseOnExec), setFdOption)


-- | A reimplementation of 'Warp.runSettings' to gain access to the sockets.
-- This is to keep them from leaking into exec'd processes.
runSettings :: Warp.Settings -> Wai.Application -> IO ()
runSettings settings app = bracket sBind sClose $ \socket -> do
    setSocketCloseOnExec socket
    Warp.runSettingsConnection settings (getter socket) app
  where
    sBind = bindPort (Warp.settingsPort settings) (Warp.settingsHost settings)
    getter socket = do
        (conn, sa) <- accept socket
        setSocketCloseOnExec conn
        return (socketConnection conn, sa)

-- | This should be exported by Warp, but isn't.
socketConnection :: Socket -> Warp.Connection
socketConnection s = Warp.Connection
    { Warp.connSendMany = Sock.sendMany s
    , Warp.connSendAll = Sock.sendAll s
    , Warp.connSendFile = \fp off len act hdr -> sendfileWithHeader s fp (PartOfFile off len) act hdr
    , Warp.connClose = sClose s
    , Warp.connRecv = Sock.recv s bytesPerRead
    }
  where
    bytesPerRead = 4096

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
