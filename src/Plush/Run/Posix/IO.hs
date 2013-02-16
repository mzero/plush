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

{-# Language TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| This module exports the instance of PosixIO for IO. -}

module Plush.Run.Posix.IO () where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Monad (foldM, when)
import Control.Monad.Exception (catchIOError, catchIf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as B
import Data.Foldable (forM_)
import Foreign.Ptr (castPtr, plusPtr)
import qualified GHC.IO.Exception as GHC
import System.Exit
import System.Posix.Types
import qualified System.IO as IO
import qualified System.IO.Error as IO
import qualified System.Posix as P
import qualified System.Posix.Missing as PM

import qualified Plush.Run.Posix as PL

instance PL.PosixLike IO where
    createDirectory             = P.createDirectory
    removeDirectory             = P.removeDirectory
    getDirectoryContents        = getDirectoryContents
    getWorkingDirectory         = P.getWorkingDirectory
    changeWorkingDirectory      = P.changeWorkingDirectory

    getInitialEnvironment       = P.getEnvironment

    type FileStatus IO          = P.FileStatus
    getFileStatus               = P.getFileStatus
    getSymbolicLinkStatus       = P.getSymbolicLinkStatus
    isExecutable                = isExecutable

    removeLink                  = P.removeLink
    setFileTimes                = P.setFileTimes
    touchFile                   = P.touchFile

    openFd                      = P.openFd
    createFile                  = P.createFile
    closeFd                     = P.closeFd

    dupTo                       = dupTo
    dupFdCloseOnExec            = PM.dupFdCloseOnExec
    setCloseOnExec              = setCloseOnExec

    readAll                     = readAll
    write                       = write

    getUserHomeDirectoryForName = getUserHomeDirectoryForName
    realAndEffectiveIDsMatch    = realAndEffectiveIDsMatch
    getProcessID                = getProcessID

    execProcess                 = execProcess
    captureStdout               = captureStdout
    pipeline                    = pipeline
    contentFd                   = contentFd

instance PL.PosixLikeFileStatus P.FileStatus where
    accessTime                  = P.accessTime
    modificationTime            = P.modificationTime
    isRegularFile               = P.isRegularFile
    isDirectory                 = P.isDirectory
    isSymbolicLink              = P.isSymbolicLink


getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents fp = do
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


isExecutable :: FilePath -> IO Bool
isExecutable path = P.fileAccess path False False True

dupTo :: Fd -> Fd -> IO ()
dupTo a b = P.dupTo a b >> return ()

setCloseOnExec :: Fd -> IO ()
setCloseOnExec fd = P.setFdOption fd P.CloseOnExec True

-- | 'readAll' for 'IO': Seek to the start (if possible), and read as much as
-- possible.
readAll :: Fd -> IO L.ByteString
readAll fd = do
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
write :: Fd -> L.ByteString -> IO ()
write fd = mapM_ (flip B.unsafeUseAsCStringLen writeBuf) . L.toChunks
  where
    writeBuf (p, n) | n > 0 = do
        ignoreUnsupportedOperation $ P.fdSeek fd IO.SeekFromEnd 0
        m <- fromIntegral `fmap` P.fdWriteBuf fd (castPtr p) (fromIntegral n)
        when (0 <= m && m <= n) $ writeBuf (p `plusPtr` m, n - m)
    writeBuf _ = return ()


getUserHomeDirectoryForName :: String -> IO (Maybe FilePath)
getUserHomeDirectoryForName s =
    (Just . P.homeDirectory <$> P.getUserEntryForName s)
        `catchIOError` (\_ -> return Nothing)

realAndEffectiveIDsMatch :: IO Bool
realAndEffectiveIDsMatch = do
    usersMatch <- (==) <$> P.getRealUserID <*> P.getEffectiveUserID
    groupsMatch <- (==) <$> P.getRealGroupID <*> P.getEffectiveGroupID
    return $ usersMatch && groupsMatch

getProcessID :: IO Int
getProcessID = fromIntegral <$> P.getProcessID


-- | 'execProcess' for 'IO'
execProcess :: FilePath -> [(String, String)] -> String -> [String] -> IO ExitCode
execProcess fp env cmd args = do
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

captureStdout :: IO ExitCode -> IO (ExitCode, L.ByteString)
captureStdout action = do
    (readFd, writeFd) <- P.createPipe
    pid <- P.forkProcess $ do
        P.closeFd readFd
        _ <- P.dupTo writeFd P.stdOutput
        action >>= P.exitImmediately
    P.closeFd writeFd
    out <- readAll readFd
    P.closeFd readFd
    st <- P.getProcessStatus True False pid
    case st of
        Just (P.Exited e) -> return (e, out)
        _ -> return (ExitFailure 129, out)

-- | 'pipeline' for 'IO': fork each action, connected by a daisy chained
-- series of pipes. The first action gets the original stdInput, the last
-- command gets the original stdOutput.
pipeline :: [IO ExitCode] -> IO ExitCode
pipeline actions = next Nothing actions >>= waitAll
  where
    next pPrev [] = forM_ pPrev closeBoth >> return []
    next pPrev [cz] = (:[]) <$> seg pPrev cz Nothing
        -- TODO: run cz in foreground once we can stash stdInput reliably

    next pPrev (c:cs) = do
        pNext <- Just <$> P.createPipe   -- (readSide, writeSide)
        (:) <$> seg pPrev c pNext <*> next pNext cs

    seg pIn c pOut = do
        pid <- P.forkProcess $ do
            forM_ pIn $ \p@(r,_w) -> P.dupTo r P.stdInput >> closeBoth p
            forM_ pOut $ \p@(_r,w) -> P.dupTo w P.stdOutput >> closeBoth p
            c >>= P.exitImmediately
        forM_ pIn closeBoth
        return pid

    closeBoth (r,w) = P.closeFd r >> P.closeFd w

    waitAll = foldM (const wait) ExitSuccess

    wait pid = do
        st <- P.getProcessStatus True False pid
        case st of
            Just (P.Exited e) -> return e
            _ -> return $ ExitFailure 129

contentFd :: L.ByteString -> IO Fd
contentFd content = do
    (readFd, writeFd) <- P.createPipe
    _ <- forkIO $ write writeFd content >> P.closeFd writeFd
    return readFd


ignoreUnsupportedOperation :: IO a -> IO ()
ignoreUnsupportedOperation act =
    catchIf ((== GHC.UnsupportedOperation) . IO.ioeGetErrorType)
        (act >> return ())
        (const $ return ())


