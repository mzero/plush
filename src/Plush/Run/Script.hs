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

module Plush.Run.Script (
    parse,
    parseInput,

    runCommand,
    runScript,
    runFile,
)
where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Exception (catchIOError)

import Plush.Parser
import Plush.Pretty
import {-# SOURCE #-} Plush.Run.Execute         -- see Execute.hs-boot
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.ShellExec
import Plush.Run.Types
import qualified Plush.Run.ShellFlags as F


-- | Parse a command, in the state of the shell. Extracts the aliases from
-- the shell state to give to the parser. This action should have no side
-- effects, and the transformed shell state from this action can be safely
-- ignored.
parse :: (PosixLike m) => String -> ShellExec m ParseCommandResult
parse s = getAliases >>= return . flip parseCommand s

-- | Like 'parse', but the string is considered "shell input", and so is
-- subject the @verbose (@-v@) and @parseout@ (@-P@) shell flags. Therefore,
-- this operation can have output other potential side effects.
parseInput :: (PosixLike m) => String -> ShellExec m ParseCommandResult
parseInput s = do
    flags <- getFlags
    when (F.verbose flags) $ errStrLn s
    pcr <- parse s
    when (F.parseout flags) $ case pcr of
        Right (cl, _) -> errStrLn $ pp cl
        _ -> return ()
    return pcr

-- | Run a single command, parsed out of a string.
runCommand :: (PosixLike m) => String -> ShellExec m (ShellStatus, Maybe String)
runCommand cmds = do
    pcr <- parseInput cmds
    case pcr of
        Left errs -> shellError 125 errs >>= return . (\st -> (st, Nothing))
        Right (cl, rest) -> execute cl >>= return . (\st -> (st, Just rest))

-- | Run all the commands, parsed from a string
runScript :: (PosixLike m) => String -> ShellExec m ShellStatus
runScript cmds0 = rc (StStatus ExitSuccess, Just cmds0)
  where
    rc (StStatus _, Just cmds) | not (null cmds) = runCommand cmds >>= rc
    rc (st, _) = return st

-- | Run all commands from a file.
runFile :: (PosixLike m) => FilePath -> ShellExec m ShellStatus
runFile fp = do
    mscript <- (Just <$> readAllFile fp) `catchIOError` (\_ -> return Nothing)
    case mscript of
        Nothing -> shellError 127 $ "file couldn't be read: " ++ fp
        Just script -> runScript script

