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

{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module System.Posix.Missing (
    dupFd, dupFdCloseOnExec,
    loginTerminal,
    setControllingTerminal,
    ) where

import Control.Monad (when)
import Foreign.C
import Foreign.Ptr (nullPtr, Ptr)
import System.Posix
import System.Posix.Internals

#include "HsUnix.h"

-- | This is like 'dup', but takes an extra argument. The new file descriptor
-- will be the lowest free file descriptor not less than the second argument.
-- The 'CloseOnExec' flag (FD_CLOEXEC) will be cleared on the new descriptor.
dupFd :: Fd -> Fd -> IO Fd
dupFd = dupFdViaFcntl (#const F_DUPFD)

-- | Same as 'dupFd', but the 'CloseOnExec' flag (FD_CLOEXEC) will be set.
dupFdCloseOnExec :: Fd -> Fd -> IO Fd
-- dupFdCloseOnExec = dupFdViaFcntl (#const F_DUPFD_CLOEXEC)
    -- While this is POSIX, is is relatively new, and many systems do not
    -- support this operation. The following code simulates it.
dupFdCloseOnExec src base = do
    dest <- dupFd src base
    setFdOption dest CloseOnExec True
    return dest

dupFdViaFcntl :: CInt -> Fd -> Fd -> IO Fd
dupFdViaFcntl op (Fd srcFd) (Fd minFd) = Fd `fmap`
  throwErrnoIfMinus1 "dupFdViaFcntl"
                      (c_fcntl_write srcFd op (fromIntegral minFd))

-- | Prepares for login on the tty, which should be a real tty or the slave
-- from a call to `openPseudoTerminal`. A new session will be started, the tty
-- will become the controlling terminal, and the tty is dup'd down to become
-- the standard in, out and err FDs.
loginTerminal :: Fd -> IO ()

#ifdef HAVE_OPENPTY
    -- In theory this should be testing HAVE_LOGIN_TTY, but that doesn't exist.
    -- The calls login_tty, openpty, and forkpty are part of the same BSD module
    -- and so are all generally present together or not.
loginTerminal (Fd ttyFd) =
    throwErrnoIfMinus1_ "loginTerminal" (c_login_tty ttyFd)

foreign import ccall unsafe "login_tty"
  c_login_tty :: CInt -> IO CInt

#else
loginTerminal tty = do
    _ <- createSession
    setControllingTerminal tty
    _ <- dupTo tty stdInput
    _ <- dupTo tty stdOutput
    _ <- dupTo tty stdError
    when (tty > stdError) $ closeFd tty
#endif

-- | Set the controlling terminal of the process. The tty should be a real tty
-- or the slave from a call to 'openPseudoTerminal'.
setControllingTerminal :: Fd -> IO ()

#ifdef TIOCSCTTY
setControllingTerminal (Fd ttyfd) =
    throwErrnoIfMinus1_ "setControllingTerminal"
        (ioctl ttyfd (#const TIOCSCTTY) nullPtr)

foreign import ccall ioctl :: FD -> CULong -> Ptr a -> IO CInt
#else
-- POSIX doesn't spec how to set the controlling terminal! The ioctl TIOSCTTY
-- method above is generally available, but some systems don't have it. The
-- second most common way to set the controlling terminal is to open it when no
-- other terminal is opened in the process. That is what this alternative
-- implementation does. Be forewarned that they may be systems that use yet some
-- third unknown method.
setControllingTerminal tty = do
    ttyName <- getTerminalName tty
    when (tty /= stdInput) $ closeFd stdInput
    when (tty /= stdOutput) $ closeFd stdOutput
    when (tty /= stdError) $ closeFd stdError
    openFd ttyName ReadWrite Nothing defaultFileFlags >>= closeFd
#endif

