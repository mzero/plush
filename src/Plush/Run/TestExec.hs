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

{-# Language FlexibleInstances, TypeSynonymInstances, TypeFamilies  #-}

module Plush.Run.TestExec (
    TestState(), initialTestState,
    TestExec, runTest, testOutput,
    )
    where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException)
import Control.Monad (unless, when)
import Control.Monad.Exception (ExceptionT, runExceptionT, throwM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import qualified Data.ByteString.Lazy as L
--import qualified Data.ByteString.Lazy.UTF8 as L
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as I
import Data.List (foldl')
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import System.FilePath
import System.IO.Error

import Plush.Run.BuiltIns
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.Types


-- File System

type DirPath = FilePath
type Name = String
data Entry = FileItem Int | DirItem

data IType = IDevNull | IFile | IExec | IPipe
data INode = INode IType L.ByteString

data FileSystem = FileSystem
    { fsTree :: M.HashMap DirPath (M.HashMap Name Entry)
    , fsStore :: I.IntMap INode
    }

fsDirectoryContents :: FileSystem -> DirPath -> Maybe [Name]
fsDirectoryContents (FileSystem tree _) dp =
    M.lookup dp tree >>= return . (".":) . ("..":) . M.keys

fsDirectoryExists :: FileSystem -> DirPath -> Bool
fsDirectoryExists (FileSystem tree _) dp = isJust $ M.lookup dp tree

fsFileExists :: FileSystem -> DirPath -> Name -> Bool
fsFileExists fs dp n = maybe False isFile $ fsItemEntry fs dp n
  where
    isFile (FileItem _) = True
    isFile _ = False

fsExecutable :: FileSystem -> DirPath -> Name -> Maybe (Utility TestExec)
fsExecutable fs dp n = fsItemEntry fs dp n >>= iNode >>= getExec
  where
    iNode (FileItem i) = I.lookup i (fsStore fs)
    iNode _ = Nothing
    getExec (INode IExec _) = M.lookup n testExecs
    getExec _ = Nothing
    -- TODO(mzero): should return Either String with erorr messages

fsItemEntry :: FileSystem -> DirPath -> Name -> Maybe Entry
fsItemEntry _ "/" "." = Just DirItem
fsItemEntry (FileSystem tree _) dp n = M.lookup dp tree >>= M.lookup n

-- NB: the add and remove functions will simply do nothing if the operation
-- would produce an illegal state. It is the job of the higher level code
-- to test preconditions, and signal failures.

fsAddDirectory :: FileSystem -> DirPath -> Name -> FileSystem
fsAddDirectory fs@(FileSystem tree _) dp n =
    if isJust (fsItemEntry fs dp n)
        then fs
        else fs { fsTree = addDir . addEntry $ tree }
  where
    addEntry = M.adjust (M.insert n DirItem) dp
    addDir = M.insert (dp </> n) M.empty

fsAddINode :: IType -> FileSystem -> DirPath -> Name -> FileSystem
fsAddINode itype fs@(FileSystem tree store) dp n =
    if isJust (fsItemEntry fs dp n)
        then fs
        else FileSystem tree' store'
  where
    i = if I.null store then 0 else (+ 1) . fst . I.findMax $ store
    tree' = M.adjust (M.insert n (FileItem i)) dp tree
    store' = I.insert i (INode itype L.empty) store

fsAddFile :: FileSystem -> DirPath -> Name -> FileSystem
fsAddFile = fsAddINode IFile


fsRemove :: FileSystem -> DirPath -> Name -> FileSystem
fsRemove fs@(FileSystem tree _) dp n =
    case fsItemEntry fs dp n of
        Nothing -> fs
        Just (FileItem _) -> fs { fsTree = removeEntry tree }
        Just DirItem -> if maybe False M.null (M.lookup dpn tree)
            then fs { fsTree = removeDir . removeEntry $ tree }
            else fs
  where
    dpn = dp </> n
    removeEntry = M.adjust (M.delete n) dp
    removeDir = M.delete dpn

fsTruncFile :: FileSystem -> DirPath -> Name -> FileSystem
fsTruncFile fs@(FileSystem _tree store) dp n =
    case fsItemEntry fs dp n of
        Just (FileItem i) -> case I.lookup i store of
            Just (INode itype _) ->
                fs { fsStore = I.insert i (INode itype L.empty) store }
            _ -> fs
        _ -> fs

initialFileSystem :: FileSystem
initialFileSystem = foldl' (flip ($)) rootFS $
    map addDir [ ("/", "bin")
               , ("/", "dev")
               , ("/", "home")
               , ("/", "tmp")
               , ("/", "proc")
               ]
    ++ map addDev [("null", IDevNull)]
    ++ map addPipe ["stdin", "stdout", "stderr"]
    ++ map addExec (M.keys testExecs)
  where
    rootFS = FileSystem rootTree I.empty
    rootTree = M.singleton "/" M.empty
    addDir (dp, n) fs = fsAddDirectory fs dp n
    addDev (n, itype) fs = fsAddINode itype fs "/dev" n
    addPipe n fs = fsAddINode IPipe fs "/proc" n
    addExec n fs = fsAddINode IExec fs "/bin" n


testExecs :: M.HashMap String (Utility TestExec)
testExecs = pseudoExecs


data FDesc = FDesc {
    fdReadAll :: TestExec L.ByteString,
    fdWrite :: L.ByteString -> TestExec ()
    }


iNodeFDesc :: Int -> IType -> FDesc
iNodeFDesc i itype = FDesc iReadAll iWrite
  where
    iReadAll = do
        s <- lift get
        case I.lookup i $ fsStore $ tsFileSystem s of
            Nothing -> return L.empty
            Just (INode IDevNull _) -> return L.empty
            Just (INode IFile bs) -> return bs
            Just (INode IExec _) -> return L.empty
            Just (INode IPipe bs) -> iSetBuf s L.empty >> return bs
    iWrite as = do
        s <- lift get
        case I.lookup i $ fsStore $ tsFileSystem s of
            Nothing -> return ()
            Just (INode IDevNull _) -> return ()
            Just (INode IFile bs) -> iSetBuf s (L.append bs as)
            Just (INode IExec _) -> return ()
            Just (INode IPipe bs) -> iSetBuf s (L.append bs as)
    iSetBuf s nbs = do
        let nis = INode itype nbs
        let fs = tsFileSystem s
        let fs' = fs { fsStore = I.insert i nis $ fsStore fs }
        let s' = s { tsFileSystem = fs' }
        lift $ put s'

contentFDesc :: L.ByteString -> FDesc
contentFDesc content = FDesc (return content) (\_ -> return ())


data TestState = TestState
    { tsWorkingDir :: FilePath      -- ^ absolute path to current working dir
    , tsFileSystem :: FileSystem    -- ^ mock file system
    , tsFDescs :: I.IntMap FDesc    -- ^ file descriptors
    }

initialTestState :: TestState
initialTestState = snd $ runTest startup ts0
  where
    startup = do
        _ <- openFile "/proc" "stdin"
        _ <- openFile "/proc" "stdout"
        _ <- openFile "/proc" "stderr"
        return ()
        -- TODO: refactor to call a lower level function to ensure which FDs
        -- these end up on, rather than relying on the allocation order
    ts0 = TestState
        { tsWorkingDir = "/home"
        , tsFileSystem = initialFileSystem
        , tsFDescs = I.empty
        }

canonicalizePath :: TestState -> FilePath -> (FilePath, DirPath, Name)
canonicalizePath ts fp = parts $ reducePath $ tsWorkingDir ts </> fp
  where
    parts [] = ("", "", "") -- should never happen
    parts ["/"] = ("/", "/", ".")
    parts [n] = (n, ".", n) -- relative paths should never happen
    parts (n:ns) = let dp = joinPath (reverse ns) in (dp </> n, dp, n)

nextFreeAfter :: I.IntMap a -> I.Key -> Int
nextFreeAfter m i = if i `I.notMember` m then i else nextFreeAfter m (succ i)

-- Test Execution Monad

type TestExec = ExceptionT (State TestState)
runTest :: TestExec a -> TestState -> (Either SomeException a, TestState)
runTest = runState . runExceptionT



testOutput :: TestExec (String, String)
testOutput = do
    so <- utos `fmap` readAll stdOutput
    se <- utos `fmap` readAll stdError
    return (so, se)
  where
    utos = LT.unpack . LT.decodeUtf8With T.lenientDecode

instance PosixLike TestExec where
    createDirectory fp _mode = runFilePrim fp $ \_s fs fpc dpc n -> do
        directoryMustNotExist "createDirectory" fp fs fpc
        directoryMustExist "createDirectory" fp fs dpc
        updateFileSystem $ fsAddDirectory fs dpc n

    removeDirectory fp = runFilePrim fp $ \_s fs fpc dpc n -> do
        directoryMustExist "removeDirectory" fp fs fpc
        unless (maybe False ((==2).length) $ fsDirectoryContents fs fpc) $
            raise illegalOperationErrorType "removeDirectory" fp
        updateFileSystem $ fsRemove fs dpc n

    getDirectoryContents fp = runFilePrim fp $ \_s fs fpc _dpc _n -> do
        directoryMustExist "getDirectoryContents" fp fs fpc
        let contents = fsDirectoryContents fs fpc
        return $ fromMaybe [] contents

    getWorkingDirectory = lift get >>= return . tsWorkingDir
    changeWorkingDirectory fp = runFilePrim fp $ \s fs fpc _dpc _n -> do
        directoryMustExist "changeWorkingDirectory" fp fs fpc
        updateTestState $ s { tsWorkingDir = fpc }

    getInitialEnvironment = return
        [ ("HOME", "/home")
        , ("LOGNAME", "tester")
        , ("PATH", "/bin")
        , ("PWD", "/home")
        , ("SHELL", "/usr/bin/plush")
        , ("TMPDIR", "/tmp")
        ]

    type FileStatus TestExec = Entry

    getFileStatus fp = runFilePrim fp $ \_s fs _fpc dpc n -> do
        let stat = fsItemEntry fs dpc n
        maybe (raise doesNotExistErrorType "getFileStatus" fp) return stat
    getSymbolicLinkStatus = getFileStatus

    isExecutable fp = runFilePrim fp $ \_s fs _fpc dpc n -> return $
        isJust $ fsExecutable fs dpc n

    removeLink fp = runFilePrim fp $ \_s fs _fpc dpc n -> do
        fileMustExist "removeLink" fp fs dpc n
        updateFileSystem $ fsRemove fs dpc n

    setFileTimes _ _ _ = return ()
    touchFile fp = runFilePrim fp $ \_s fs _fpc dpc n -> do
        fileMustExist "touchFile" fp fs dpc n

    openFd fp _rw mMode opts = runFilePrim fp $ \_s fs _fpc dpc n -> do
        when (isNothing mMode) $ fileMustExist "openFd" fp fs dpc n
        if (fsFileExists fs dpc n)
            then when (trunc opts) $ updateFileSystem $ fsTruncFile fs dpc n
            else updateFileSystem $ fsAddFile fs dpc n
        openFile dpc n
    createFile fp _mode = runFilePrim fp $ \_s fs _fpc dpc n -> do
        fileMustNotExist "createFile" fp fs dpc n
        directoryMustExist "createFile" fp fs dpc
        updateFileSystem $ fsAddFile fs dpc n
        openFile dpc n
    closeFd fd = runFdPrim "closeFd" fd $ \s fds _desc -> do
        lift $ put s { tsFDescs = I.delete (fromIntegral fd) fds }

    readAll fd = runFdPrim "readAll" fd $ \_s _fds desc -> fdReadAll desc
    write fd bs = runFdPrim "write" fd $ \_s _fds desc -> fdWrite desc bs
    dupTo fdFrom fdTo =
        runFdPrim "dupTo" fdFrom $ \s fds desc ->
            lift $ put s { tsFDescs = I.insert (fromIntegral fdTo) desc fds }
    dupFdCloseOnExec fdFrom fdMin =
        runFdPrim "dupFdCloseOnExec" fdFrom $ \s fds desc ->
            let dest = nextFreeAfter fds $ fromIntegral fdMin
            in lift $ do
                put s { tsFDescs = I.insert dest desc fds }
                return $ fromIntegral dest

    setCloseOnExec _ = return ()

    getUserHomeDirectoryForName s = return $ lookup s homeDirs
      where
        homeDirs = [("tester","/home"), ("root","/"), ("nobody","/tmp")]
    realAndEffectiveIDsMatch = return True

    getProcessID = return 42

    execProcess fp _env _cmd args = runFilePrim fp $ \_s fs _fpc dpc n -> do
        case fsExecutable fs dpc n of
            Just util -> utilExecute util args
                -- TODO(mzero): doesn't handle environment correctly, but
                -- currently none of the pseudoExecs care about it
            Nothing -> exitMsg 127
                        $ fp ++ ": No such file or directory, or not executable"

    captureStdout action = (,) <$> action <*> readAll stdOutput

    pipeline [] = return ExitSuccess
    pipeline (c0:cs) = c0 >>= next cs
      where
        next [] e = return e
        next (c:cs') _ = do
            readAll stdOutput >>= write stdInput
            c >>= next cs'

    contentFd = openFDesc . contentFDesc

instance PosixLikeFileStatus Entry where
    accessTime _ = fromInteger 0
    modificationTime _ = fromInteger 0

    isRegularFile (FileItem _) = True
    isRegularFile _ = False
    isDirectory DirItem = True
    isDirectory _ = False
    isSymbolicLink _ = False


type FilePrim a = TestState -> FileSystem
                -> FilePath -> DirPath -> Name -> a

runFilePrim :: FilePath -> FilePrim (TestExec a) -> TestExec a
runFilePrim fp prim = do
    s <- lift get
    let fs = tsFileSystem s
    let (fpc, dpc, n) = canonicalizePath s fp
    prim s fs fpc dpc n

type FdPrim a = TestState -> I.IntMap FDesc -> FDesc -> a

runFdPrim :: String -> Fd -> FdPrim (TestExec a) -> TestExec a
runFdPrim fn fd prim = do
    s <- lift get
    let fds = tsFDescs s
    case (I.lookup (fromIntegral fd) fds) of
        Nothing -> throwM $ mkIOError illegalOperationErrorType fn
                                Nothing (Just $ "<" ++ show fd ++ ">")
        Just desc -> prim s fds desc


updateFileSystem :: FileSystem -> TestExec ()
updateFileSystem fs = lift $ modify (\s -> s { tsFileSystem = fs })

updateTestState :: TestState -> TestExec ()
updateTestState s = lift . put $ s

openFDesc :: FDesc -> TestExec Fd
openFDesc fdesc = do
    s <- lift get
    let fd = freeDesc 0 $ tsFDescs s
    lift $ put s { tsFDescs = I.insert fd fdesc $ tsFDescs s}
    return $ fromIntegral fd
  where
    freeDesc fd fds = if I.member fd fds then freeDesc (fd + 1) fds else fd

openFile :: DirPath -> Name -> TestExec Fd
openFile dp n = do
    fs <- lift $ gets tsFileSystem
    case fsItemEntry fs dp n of
        Just (FileItem i) ->
            case I.lookup i $ fsStore fs of
                Just (INode itype _) -> openFDesc $ iNodeFDesc i itype
                _ -> return (-1) -- TODO: should never happen
        _ -> return (-1) -- TODO: should never happen

fileMustExist :: String -> FilePath -> FileSystem -> DirPath -> Name -> TestExec ()
fileMustExist fn fp fs dp n =
    unless (fsFileExists fs dp n) $ raise doesNotExistErrorType fn fp

fileMustNotExist :: String -> FilePath -> FileSystem -> DirPath -> Name -> TestExec ()
fileMustNotExist fn fp fs dp n =
    when (fsFileExists fs dp n) $ raise alreadyExistsErrorType fn fp

directoryMustExist :: String -> FilePath -> FileSystem -> DirPath -> TestExec ()
directoryMustExist fn fp fs dp =
    unless (fsDirectoryExists fs dp) $ raise doesNotExistErrorType fn fp

directoryMustNotExist :: String -> FilePath -> FileSystem -> DirPath -> TestExec ()
directoryMustNotExist fn fp fs dp =
    when (fsDirectoryExists fs dp) $ raise alreadyExistsErrorType fn fp


raise :: IOErrorType -> String -> FilePath -> TestExec a
raise iot fn fp = throwM $ mkIOError iot fn Nothing (Just fp)
