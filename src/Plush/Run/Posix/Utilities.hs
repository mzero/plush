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

{-# Language FlexibleInstances, TypeSynonymInstances #-}

{-| This module represents the low level Posix interface. It is mostly a
    re-export of the interface from the System.Posix module tree. However,
    all operations in IO are instead exported as versions in (PosixIO m) => m.
    This enables code to be written to this Posix interface, but then be run
    in either IO, or in other monads that offer the implementation of Posix,
    but perhaps, don't actually affect the underlying system. See TestExec.
-}


module Plush.Run.Posix.Utilities (
    -- * Input convenience functions
    PosixInStr(..),
    readAllFile,
    writeAllFile,

    -- * Output convenience functions
    -- $outstr
    PosixOutStr(..),
    outStr, outStrLn, errStr, errStrLn, jsonOut,
    writeStr, writeStrLn,

    -- * File and directory existance
    doesFileExist, doesDirectoryExist,
    -- * Path simplification
    simplifyPath, reducePath,

    -- * Exit, Error, and Status
    Returnable, Errorable, isSuccess,
    success, failure,
    exitMsg,
    notSupported,
    shellError,
    returnError,
    -- ** Monadic combintors
    shellSequence, ifError, untilErrorM, bindVars,
) where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.Exception (bracket, catchIOError)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import System.Exit
import System.FilePath
import qualified System.Posix.Files as P

import Plush.Run.Posix
import Plush.Run.Types

-- | String like classes that can be used with 'readAll' and friends.
-- Instances for strings use lenient UTF-8 decoding.
class PosixInStr s where
    fromByteString :: L.ByteString -> s

instance PosixInStr B.ByteString where
    fromByteString = B.concat . L.toChunks

instance PosixInStr L.ByteString where
    fromByteString = id

instance PosixInStr String where
    fromByteString = LT.unpack . fromByteString

instance PosixInStr LT.Text where
    fromByteString = LT.decodeUtf8With T.lenientDecode

instance PosixInStr T.Text where
    fromByteString = LT.toStrict . fromByteString


-- | Read all of a file.
readAllFile :: (PosixLike m, PosixInStr s) => FilePath -> m s
readAllFile fp = fmap fromByteString $ bracket open closeFd readAll
  where
    open = openFd fp ReadOnly Nothing defaultFileFlags

-- | Write all of a file.
writeAllFile :: (PosixLike m, PosixOutStr s) => FilePath -> s -> m ()
writeAllFile fp s = bracket open closeFd (flip write $ toByteString s)
  where
    open = openFd fp WriteOnly (Just ownerRWMode) fileFlags
    ownerRWMode = P.ownerReadMode `P.unionFileModes` P.ownerWriteMode
    fileFlags = defaultFileFlags { trunc = True }

-- $outstr
-- Use these in place of 'putStr' and 'putStrLn', (as well as
-- 'hPutStr' and 'hPutStrLn').
-- Note that these are unbuffered.

-- | String like classes that can be used with 'outStr' and friends.
class PosixOutStr s where
    toByteString :: s -> L.ByteString
    toByteStringLn :: s -> L.ByteString
    toByteStringLn = flip L.snoc nl . toByteString
      where nl = toEnum $ fromEnum '\n'

instance PosixOutStr B.ByteString where
    toByteString = L.fromChunks . (:[])

instance PosixOutStr L.ByteString where
    toByteString = id

instance PosixOutStr String where
    toByteString = toByteString . LT.pack
    toByteStringLn = toByteStringLn . LT.pack

instance PosixOutStr LT.Text where
    toByteString = LT.encodeUtf8
    toByteStringLn = LT.encodeUtf8 . flip LT.snoc '\n'

instance PosixOutStr T.Text where
    toByteString = L.fromChunks . (:[]) . T.encodeUtf8
    toByteStringLn = L.fromChunks . (:[B.singleton nl]) . T.encodeUtf8
      where nl = toEnum $ fromEnum '\n'

writeStr, writeStrLn :: (PosixOutStr s, PosixLike m) => Fd -> s -> m ()
writeStr fd = write fd . toByteString
writeStrLn fd = write fd . toByteStringLn

outStr, outStrLn, errStr, errStrLn :: (PosixOutStr s, PosixLike m) => s -> m ()
outStr = writeStr stdOutput
errStr = writeStr stdError
outStrLn = writeStrLn stdOutput
errStrLn = writeStrLn stdError

jsonOut :: (A.ToJSON a, PosixLike m) => a -> m ()
jsonOut = write stdJsonOutput . A.encode


-- | Safe query, in that all 'IOError's are mapped to 'False'. For example, if
-- the file does exist, but the user can't read it, this will return 'False'.
doesFileExist :: (PosixLike m) => FilePath -> m Bool
doesFileExist fp =
    catchIOError (getFileStatus fp >>= return . isRegularFile) (\_ -> return False)

-- | Safe query, in that all 'IOError's are mapped to 'False'. For example, if
-- the dir does exist, but the user can't read it, this will return 'False'.
doesDirectoryExist :: (PosixLike m) => FilePath -> m Bool
doesDirectoryExist fp =
    catchIOError (getFileStatus fp >>= return . isDirectory) (\_ -> return False)


-- | Simplify a file path, elminiating . and .. components (if possible)
simplifyPath :: FilePath -> FilePath
simplifyPath = joinPath . reverse . reducePath


-- | Reduce the path, elminiating . and .. components if possible.
-- Returns a list of path elements, in reverse order. Each has no slash,
-- except the last, which is a single slash if the path is absolute.
reducePath :: FilePath -> [String]
reducePath = simp [] . splitDirectories
  where
    simp       ys          []  = ys
    simp       ys (('/':_):xs) = simp ("/":ys) xs
    simp       ys    ( ".":xs) = simp      ys  xs
    simp ya@(y:ys)   ("..":xs)
          | y == "/"           = simp      ya  xs
          | y /= ".."          = simp      ys  xs
    simp       ys    (   x:xs) = simp   (x:ys) xs


-- | Types that can be used as a return value from a shell operation. See
-- "Plush.Run.Types" for a description of the different return types. This
-- typeclass enables a common set of utilities for such values.
class Returnable e where
    returnableSuccess       :: e
    intToReturnable         :: Int -> e
    isSuccess               :: e -> Bool

instance Returnable ExitCode where
    returnableSuccess       = ExitSuccess
    intToReturnable         = intToExitCode
    isSuccess ExitSuccess   = True
    isSuccess _             = False

instance Returnable ErrorCode where
    returnableSuccess       = ErrorCode returnableSuccess
    intToReturnable         = ErrorCode . intToReturnable
    isSuccess               = isSuccess . errorExitCode

instance Returnable ShellStatus where
    returnableSuccess       = StStatus returnableSuccess
    intToReturnable         = StStatus . intToReturnable
    isSuccess (StStatus e)  = isSuccess e
    isSuccess _             = False

-- | Return as successful.
success :: (Monad m, Returnable e) => m e
success = return returnableSuccess

-- | Return as failed with an exit status of 1.
failure :: (Monad m, Returnable e) => m e
failure = return $ intToReturnable 1

-- | Return as failed, printing a message on 'stderr'. The first argument
-- is the exit status value.
exitMsg :: (PosixLike m, Returnable e) => Int -> String -> m e
exitMsg e msg = do
    errStrLn msg `catchIOError` (\_ -> return ())
    return $ intToReturnable e

-- | Types that can be used as a return value from shell operations that can
-- produce a shell error. See "Plush.Run.Types" for a description of these
-- types.
class (Returnable e) => Errorable e where
    isError :: e -> Bool
    exitCodeToError :: ExitCode -> e
    errorToStatus :: e -> ShellStatus

instance Errorable ErrorCode where
    isError             = not . isSuccess
    exitCodeToError     = ErrorCode
    errorToStatus e     = if isSuccess e then returnableSuccess
                                         else StError e

instance Errorable ShellStatus where
    isError (StError _) = True
    isError _           = False
    exitCodeToError e   = if isSuccess e then returnableSuccess
                                         else StError (exitCodeToError e)
    errorToStatus       = id

-- | Shell error from a utility because some feature is not (yet) supported.
notSupported :: (PosixLike m, Errorable e) => String -> m e
notSupported s = shellError 121 ("*** Not Supported: " ++ s)

-- | Generate a shell error. This will exit a non-interactive shell.
shellError :: (PosixLike m, Errorable e) => Int -> String -> m e
shellError e msg = exitCodeToError <$> exitMsg e msg

-- | Given a possible error, return that as an error 'ShellStatus'.
-- Otherwise just return success. This is useful in contexts where an operation
-- produces a possible error of one type, but needs to be returned as another.
returnError :: (Monad m, Errorable e) => e -> m ShellStatus
returnError = return . errorToStatus

-- | Perform each action in turn, stopping if one returns anything other than
-- a 'StStatus' value. Return the result of the last action executed. If the
-- list is empty, returns @StStatus ExitSuccess@.
shellSequence :: (Monad m) => [m ShellStatus] -> m ShellStatus
shellSequence actions = go actions returnableSuccess
  where
    go (a:as) (StStatus _) = a >>= go as
    go _ st = return st

-- | If the last argument is an error, then supply it to the error handling
-- function (first argument). Otherwise, return the second argument.
--
-- Often used in a monadic context like so:
--
-- > setShellVar name value >>= ifError returnError $ do
-- >      -- code to execute if not an error
ifError :: (Errorable e) => (e -> a) -> a -> e -> a
ifError f a e = if isError e then f e else a

-- | Sequence a list actions, stopping if any of them return an error. Returns
-- the result of the last action run, or success if there were no actions.
untilErrorM :: (Monad m, Errorable e) => [m e] -> m e
untilErrorM = foldM (\e a -> ifError return a e) returnableSuccess

-- | Apply an action (usually 'setShellVar' or 'setEnvVar') to a set of
-- bindings. This uses 'untilErrorM' to stop on the first error if any, and
-- return the result of the last action run.
bindVars :: (Monad m, Errorable e) =>
    (String -> String -> m e) -> Bindings -> m e
bindVars f = untilErrorM . map (uncurry f)
