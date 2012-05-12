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

module System.Posix.Missing (
    dupFd, dupFdCloseOnExec
    ) where

import Foreign.C
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
dupFdCloseOnExec = dupFdViaFcntl (#const F_DUPFD_CLOEXEC)


dupFdViaFcntl :: CInt -> Fd -> Fd -> IO Fd
dupFdViaFcntl op (Fd srcFd) (Fd minFd) = Fd `fmap`
  throwErrnoIfMinus1 "dupFdViaFcntl"
                      (c_fcntl_write srcFd op (fromIntegral minFd))
