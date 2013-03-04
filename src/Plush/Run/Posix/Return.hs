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

{-| Functions for handling the various type of return types in POSIX operations.

    See "Plush.Run.Types" for a description of the types 'ExitCode',
    'ErrorCode', and 'ShellStatus'.
-}


module Plush.Run.Posix.Return (
    Returnable, Errorable, isSuccess,

    -- * Common returns
    success, failure,
    exitMsg,

    -- * Errors
    notSupported,
    shellError,
    returnError,

    -- * Sequencing combintors
    shellSequence,
    ifError,
    untilErrorM,
    bindVars,
) where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.Exception (catchIOError)
import System.Exit

import Plush.Run.Posix
import Plush.Run.Posix.Utilities (errStrLn)
import Plush.Run.Types


-- | Types that can be used as a return value from a shell operation. This
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
-- produce a shell error.
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
