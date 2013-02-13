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

{-# Language FlexibleContexts, TypeFamilies, ScopedTypeVariables #-}

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

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Monad (foldM, when)
import Control.Monad.Exception (MonadException, catchIf, catchIOError)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as B
import Data.Foldable (forM_)
import Foreign.Ptr (castPtr, plusPtr)
import qualified GHC.IO.Exception as GHC
import System.Exit
import System.Posix.Files (stdFileMode, accessModes)
import System.Posix.IO (stdInput, stdOutput, stdError,
    OpenMode, OpenFileFlags, defaultFileFlags)
import System.Posix.Types
import qualified System.IO as IO
import qualified System.IO.Error as IO
import qualified System.Posix as P
import qualified System.Posix.Missing as PM


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


instance PosixLike IO where
    createDirectory = P.createDirectory
    removeDirectory = P.removeDirectory

    getDirectoryContents = ioGetDirectoryContents

    getWorkingDirectory = P.getWorkingDirectory
    changeWorkingDirectory = P.changeWorkingDirectory

    getInitialEnvironment = P.getEnvironment

    type FileStatus IO = P.FileStatus

    getFileStatus = P.getFileStatus
    getSymbolicLinkStatus = P.getSymbolicLinkStatus
    isExecutable path = P.fileAccess path False False True

    removeLink = P.removeLink

    setFileTimes = P.setFileTimes
    touchFile = P.touchFile

    openFd = P.openFd
    createFile = P.createFile
    closeFd = P.closeFd

    readAll = ioReadAll
    write = ioWrite

    dupTo a b = P.dupTo a b >> return ()
    dupFdCloseOnExec = PM.dupFdCloseOnExec
    setCloseOnExec fd = P.setFdOption fd P.CloseOnExec True

    getUserHomeDirectoryForName s =
        (Just . P.homeDirectory <$> P.getUserEntryForName s)
            `catchIOError` (\_ -> return Nothing)

    realAndEffectiveIDsMatch = do
        usersMatch <- (==) <$> P.getRealUserID <*> P.getEffectiveUserID
        groupsMatch <- (==) <$> P.getRealGroupID <*> P.getEffectiveGroupID
        return $ usersMatch && groupsMatch

    getProcessID = fromIntegral <$> P.getProcessID
    execProcess = ioExecProcess
    captureStdout = ioCaptureStdout
    pipeline = ioPipeline
    contentFd = ioContentFd


instance PosixLikeFileStatus P.FileStatus where
    accessTime = P.accessTime
    modificationTime = P.modificationTime

    isRegularFile = P.isRegularFile
    isDirectory = P.isDirectory
    isSymbolicLink = P.isSymbolicLink


-- | 'readAll' for 'IO': Seek to the start (if possible), and read as much as
-- possible.
ioReadAll :: Fd -> IO L.ByteString
ioReadAll fd = do
    ignoreUnsupportedOperation $ P.fdSeek fd IO.AbsoluteSeek 0
    go [] >>= return . L.fromChunks . reverse
  where
    go bs = next >>= maybe (return bs) (go . (:bs))
    next = readBuf `catchIOError` (\_ -> return Nothing)
    readBuf = do
      b <- B.createAndTrim bufSize $ (\buf ->
                fromIntegral `fmap` P.fdReadBuf fd buf bufSize)
      return $ if B.null b then Nothing else Just b
    bufSize :: Num a => a
    bufSize = 4096

-- | 'write' for 'IO': Seek to the end, and write.
ioWrite :: Fd -> L.ByteString -> IO ()
ioWrite fd = mapM_ (flip B.unsafeUseAsCStringLen writeBuf) . L.toChunks
  where
    writeBuf (p, n) | n > 0 = do
        ignoreUnsupportedOperation $ P.fdSeek fd IO.SeekFromEnd 0
        m <- fromIntegral `fmap` P.fdWriteBuf fd (castPtr p) (fromIntegral n)
        when (0 <= m && m <= n) $ writeBuf (p `plusPtr` m, n - m)
    writeBuf _ = return ()


-- | 'execProcess' for 'IO'
ioExecProcess fp env cmd args = do
        pid <- P.forkProcess $
            PM.executeFile0 fp cmd args env `catchIOError` handler
        mStat <- P.getProcessStatus True False pid
        case mStat of
            Just (P.Exited ec)  -> return ec
            _                   -> return $ ExitFailure 129
  where
    handler _ = P.exitImmediately $ ExitFailure 127
        -- NOTE(mzero): §2.9.1 seems to imply 126 in this case, but all other
        -- shells return 127

-- | 'pipeline' for 'IO': fork each action, connected by a daisy chained
-- series of pipes. The first action gets the original stdInput, the last
-- command gets the original stdOutput.
ioPipeline :: [IO ExitCode] -> IO ExitCode
ioPipeline actions = next Nothing actions >>= waitAll
  where
    next pPrev [] = forM_ pPrev closeBoth >> return []
    next pPrev [cz] = (:[]) <$> seg pPrev cz Nothing
        -- TODO: run cz in foreground once we can stash stdInput reliably

    next pPrev (c:cs) = do
        pNext <- Just <$> P.createPipe   -- (readSide, writeSide)
        (:) <$> seg pPrev c pNext <*> next pNext cs

    seg pIn c pOut = do
        pid <- P.forkProcess $ do
            forM_ pIn $ \p@(r,_w) -> dupTo r stdInput >> closeBoth p
            forM_ pOut $ \p@(_r,w) -> dupTo w stdOutput >> closeBoth p
            c >>= P.exitImmediately
        forM_ pIn closeBoth
        return pid

    closeBoth (r,w) = closeFd r >> closeFd w

    waitAll = foldM (const wait) ExitSuccess

    wait pid = do
        st <- P.getProcessStatus True False pid
        case st of
            Just (P.Exited e) -> return e
            _ -> return $ ExitFailure 129

ioCaptureStdout :: IO ExitCode -> IO (ExitCode, L.ByteString)
ioCaptureStdout action = do
    (readFd, writeFd) <- P.createPipe
    pid <- P.forkProcess $ do
        closeFd readFd
        dupTo writeFd stdOutput
        action >>= P.exitImmediately
    closeFd writeFd
    out <- readAll readFd
    closeFd readFd
    st <- P.getProcessStatus True False pid
    case st of
        Just (P.Exited e) -> return (e, out)
        _ -> return (ExitFailure 129, out)


ioContentFd :: L.ByteString -> IO Fd
ioContentFd content = do
    (readFd, writeFd) <- P.createPipe
    _ <- forkIO $ write writeFd content >> closeFd writeFd
    return readFd


ignoreUnsupportedOperation :: IO a -> IO ()
ignoreUnsupportedOperation act =
    catchIf ((== GHC.UnsupportedOperation) . IO.ioeGetErrorType)
        (act >> return ())
        (const $ return ())



stdJsonInput, stdJsonOutput :: Fd
stdJsonInput = Fd 3
stdJsonOutput = Fd 4

-- | 'getDirectoryContents' for 'IO': Open a DirStream and read it all.
ioGetDirectoryContents :: FilePath -> IO [FilePath]
ioGetDirectoryContents fp = do
    ds <- P.openDirStream fp
    contents <- readUntilNull ds
    P.closeDirStream ds
    return contents
  where
    readUntilNull ds = do
        entry <- P.readDirStream ds
        if null entry
            then return []
            else readUntilNull ds >>= return . (entry :)

