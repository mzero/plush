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

    -- * Output convenience functions
    -- $outstr
    PosixOutStr(..),
    outStr, outStrLn, errStr, errStrLn, jsonOut,
    writeStr, writeStrLn,

    -- * File and directory existance
    doesFileExist, doesDirectoryExist,
    -- * Path simplification
    simplifyPath, reducePath,
    -- * 'ExitCode' utilities
    andThen, andThenM, untilFailureM,
) where

import Control.Monad (liftM2)
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

import Plush.Run.Posix

-- | String like classes that can be used with 'readAll' and friends.
-- Instances for strings use lenient UTF-8 decoding.
class PosixInStr s where
    fromByteString :: L.ByteString -> s

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
readAllFile fp = bracket open closeFd (fmap fromByteString . readAll)
  where
    open = openFd fp ReadOnly Nothing defaultFileFlags


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

-- | Returns the first 'ExitCode' that fails. (Could have been a
-- | Monoid if we owned ExitCode.)
andThen :: ExitCode -> ExitCode -> ExitCode
andThen ExitSuccess exitCode = exitCode
andThen exitCode _ = exitCode

-- | Sequence 'ExitCode'-returning operations until failure.
andThenM :: (Monad m) => m ExitCode -> m ExitCode -> m ExitCode
andThenM = liftM2 andThen

-- | Sequence a list of 'ExitCode'-returning operations until failure.
untilFailureM :: (Monad m) => (a -> m ExitCode) -> [a] -> m ExitCode
untilFailureM f as = foldr andThenM (return ExitSuccess) (map f as)

