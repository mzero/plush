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

{-# Language FlexibleContexts, TypeFamilies #-}

{-| This module represents the low level Posix interface. It is mostly a
    re-export of the interface from the System.Posix module tree. However,
    all operations in IO are instead exported as versions in (PosixIO m) => m.
    This enables code to be written to this Posix interface, but then be run
    in either IO, or in other monads that offer the implementation of Posix,
    but perhaps, don't actually affect the underlying system. See TestExec.
-}


module Plush.Run.Posix (
    -- * PosixLike monad
    PosixLike(..),
    PosixLikeFileStatus(..),

    -- * Misc
    -- ** Environment bindings
    Bindings,

    -- * Re-exports
    -- ** from System.Exit
    ExitCode(..),
    -- ** from System.Posix.Types
    module System.Posix.Types,
    -- ** from System.Posix.Files
    stdFileMode, accessModes,
    -- ** from System.Posix.IO
    stdInput, stdOutput, stdError,
    OpenMode(..), OpenFileFlags(..), defaultFileFlags,

    -- * Misc
    stdJsonInput, stdJsonOutput,
) where

import Control.Monad.Exception (MonadException)
import qualified Data.ByteString.Lazy as L
import System.Exit
import System.Posix.Files (stdFileMode, accessModes)
import System.Posix.IO (stdInput, stdOutput, stdError,
    OpenMode(..), OpenFileFlags(..), defaultFileFlags)
import System.Posix.Types

import qualified Plush.Run.Posix.IO as IO


type Bindings = [(String, String)]

-- | The low-level operations that make up the Posix interface.
--
-- Where named the same as a function in 'System.Posix', see that module for
-- documentation. A few operations here are slightly higher, and replace the
-- lower level primitives. These are documented here.
--
-- These are just the operations needed to implement the shell command language
-- and the built-in commands. The shell can operate entirely within a monad
-- of this class. See 'TextExec' for one such monad. 'IO' is another.
class (Functor m, Monad m, MonadException m,
        PosixLikeFileStatus (FileStatus m)) => PosixLike m where
    -- from System.Posix.Directory

    createDirectory :: FilePath -> FileMode -> m ()
    removeDirectory :: FilePath -> m ()

    -- | Return the entries in a directory, including "." and "..".
    -- This replaces 'openDirStream' & family, since this is the only use anyone
    -- ever makes of those functions.
    getDirectoryContents :: FilePath -> m [FilePath]

    getWorkingDirectory :: m FilePath
    changeWorkingDirectory :: FilePath -> m ()

    -- from System.Posix.Env

    getInitialEnvironment :: m Bindings

    -- from System.Posix.Files

    -- | Type of file status values used with an instance of PosixLike
    type FileStatus m :: *

    getFileStatus :: FilePath -> m (FileStatus m)
    getSymbolicLinkStatus :: FilePath -> m (FileStatus m)
    isExecutable :: FilePath -> m Bool

    removeLink :: FilePath -> m ()

    setFileTimes :: FilePath -> EpochTime -> EpochTime -> m ()
    touchFile :: FilePath -> m ()

    -- From System.Posix.IO

    openFd :: FilePath -> OpenMode -> Maybe FileMode -> OpenFileFlags -> m Fd
    createFile :: FilePath -> FileMode -> m Fd
    closeFd :: Fd -> m ()

    dupTo :: Fd -> Fd -> m ()
    dupFdCloseOnExec :: Fd -> Fd -> m Fd
    setCloseOnExec :: Fd -> m ()
    -- ^ Convenience @setCloseOnExec fd -> 'setFdOption' fd 'CloseOnExec' 'True'@

    -- | Read all the available input. If the input is a seekable stream, it
    -- is rewound to the begining, first. Simply returns an empty result if
    -- the stream is empty.
    readAll :: Fd -> m L.ByteString
    -- | Write to an output. If the output is a seekable stream, it is seeked
    -- to the end.
    write :: Fd -> L.ByteString -> m ()

    -- From System.Posix.User

    -- | A safe lookup of home directory info. See `getUserEntryForName`.
    getUserHomeDirectoryForName :: String -> m (Maybe FilePath)

    -- | A check for possibly elivated privledges. Checks that for both the
    -- user IDs and group IDs, the real and effective versions match.
    -- See §2.5.3
    realAndEffectiveIDsMatch :: m Bool

    -- From System.Process

    getProcessID :: m Int

    execProcess :: FilePath     -- ^ Path to exec
                -> Bindings     -- ^ Environment variable bindings
                -> String       -- ^ Command name
                -> [String]     -- ^ Arguments
                -> m ExitCode

    -- | Run a computation, and returning what it wrote to stdout
    captureStdout :: m ExitCode -> m (ExitCode, L.ByteString)

    -- | A high level primitive that runs each computation in an environment
    -- with the stdout of each piped to the stdin of next. The original stdin
    -- is piped to the first, and the stdout of the last is the original stdout.
    pipeline :: [m ExitCode] -> m ExitCode

    -- | A high level primitive that returns an open Fd that when read will
    -- supply the content given.
    contentFd :: L.ByteString -> m Fd



-- | File status data as returned by 'getFileStatus' and 'getSymbolicLinkStatus'
class PosixLikeFileStatus s where
    accessTime :: s -> EpochTime
    modificationTime :: s -> EpochTime

    isRegularFile :: s -> Bool
    isDirectory :: s -> Bool
    isSymbolicLink :: s -> Bool

stdJsonInput, stdJsonOutput :: Fd
stdJsonInput = Fd 3
stdJsonOutput = Fd 4


instance PosixLike IO where
    createDirectory             = IO.createDirectory
    removeDirectory             = IO.removeDirectory
    getDirectoryContents        = IO.getDirectoryContents
    getWorkingDirectory         = IO.getWorkingDirectory
    changeWorkingDirectory      = IO.changeWorkingDirectory

    getInitialEnvironment       = IO.getInitialEnvironment

    type FileStatus IO          = IO.FileStatus
    getFileStatus               = IO.getFileStatus
    getSymbolicLinkStatus       = IO.getSymbolicLinkStatus
    isExecutable                = IO.isExecutable

    removeLink                  = IO.removeLink
    setFileTimes                = IO.setFileTimes
    touchFile                   = IO.touchFile

    openFd                      = IO.openFd
    createFile                  = IO.createFile
    closeFd                     = IO.closeFd

    dupTo                       = IO.dupTo
    dupFdCloseOnExec            = IO.dupFdCloseOnExec
    setCloseOnExec              = IO.setCloseOnExec

    readAll                     = IO.readAll
    write                       = IO.write

    getUserHomeDirectoryForName = IO.getUserHomeDirectoryForName
    realAndEffectiveIDsMatch    = IO.realAndEffectiveIDsMatch
    getProcessID                = IO.getProcessID

    execProcess                 = IO.execProcess
    captureStdout               = IO.captureStdout
    pipeline                    = IO.pipeline
    contentFd                   = IO.contentFd

instance PosixLikeFileStatus IO.FileStatus where
    accessTime                  = IO.accessTime
    modificationTime            = IO.modificationTime
    isRegularFile               = IO.isRegularFile
    isDirectory                 = IO.isDirectory
    isSymbolicLink              = IO.isSymbolicLink
