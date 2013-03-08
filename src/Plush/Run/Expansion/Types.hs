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

{-# Language GeneralizedNewtypeDeriving #-}


module Plush.Run.Expansion.Types (
    Expansion,

    runExpansion,
    evalExpansion,
    withExpansion,
    withExpansion',

    noteError,
    noteShellError,
    noteExitCode,
    )
where

import Control.Applicative (Applicative)
import Control.Monad.Exception (MonadException(..))
import Control.Monad.Trans.Class (lift, MonadTrans)
import qualified Control.Monad.Trans.State as ST

import Plush.Run.Posix
import Plush.Run.Posix.Return
import Plush.Run.ShellExec
import Plush.Run.Types

-- | Expansion is defined in the spec in a way that is both functional and
-- stateful! It always produces an expansion, but in some cases it may shell
-- error. Further, the last exit status from the expansion is retained because
-- exucting a command with no command name (just variable assignments) uses it.
--
-- Expansions are therefore defined in a small 'Expansion' monad. See
-- 'evalExpansion', 'withExpansion', and 'runExpansion' for ways to run them.
newtype ExpansionT m a = ExpansionT (ST.StateT ExpansionStatus m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadException)

type Expansion m = ExpansionT (ShellExec m)


-- | The state of an expansion operation.
data ExpansionStatus =
     ExpansionStatus { exError :: ErrorCode
                     , exLastExitCode :: ExitCode
                     }

expansionStartStatus :: ExpansionStatus
expansionStartStatus = ExpansionStatus { exError = ErrorCode ExitSuccess,
                                         exLastExitCode = ExitSuccess }

-- | Make note in an expansion of an exit status from an executed command. These
-- are from command expansion. The result of an expansion includes the last such
-- exit status.
noteExitCode :: (Monad m) => ExitCode -> Expansion m ()
noteExitCode ec = ExpansionT $
    ST.modify $ \s -> s { exLastExitCode = ec }

-- | Make note of a shell error during expansion. Only the first such error
-- (whos value really is an error) is retained. Note that expansion continues
-- even when there is such an error.
noteError :: (Monad m) => ErrorCode -> Expansion m ()
noteError ec = ExpansionT $
    ST.modify $ \s -> s { exError = ifError id ec $ exError s }

-- | This is a convenience function that calls 'shellError' and then notes it.
noteShellError :: (PosixLike m) => Int -> String -> Expansion m ()
noteShellError e s = lift (shellError e s) >>= noteError

-- | This is the most generalized way to run an expansion. The result is either
-- a shell error, or a pair of the expansion result, and the last exit status
-- from any command expansions.
runExpansion :: (Monad m) =>
    ExpansionT m a -> m (Either ErrorCode (a, ExitCode))
runExpansion (ExpansionT st) = do
    (a, ex) <- ST.runStateT st expansionStartStatus
    return $ ifError Left (Right (a, exLastExitCode ex)) $ exError ex

-- | Perform an expansion, ignoring any shell errors (though their messages
-- will have been printed to stderr) and ignoring the last exit code if any.
evalExpansion :: (Monad m) => ExpansionT m a -> m a
evalExpansion (ExpansionT st) = ST.evalStateT st expansionStartStatus

-- | This is the common way to run an expansion. If the result is an error
-- then the action isn't run, and the error is returned instead.
withExpansion :: (Monad m) =>
    Expansion m a -> (a -> ShellExec m ShellStatus)
    -> ShellExec m ShellStatus
withExpansion eAct shAct = withExpansion' eAct (shAct . fst)

-- | Same 'as withExpansion', but provides the last exit code as well.
withExpansion' :: (Monad m) =>
    Expansion m a -> ((a, ExitCode) -> ShellExec m ShellStatus)
    -> ShellExec m ShellStatus
withExpansion' eAct shAct = runExpansion eAct >>= either returnError shAct
