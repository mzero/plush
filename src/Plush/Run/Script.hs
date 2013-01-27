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

import Control.Monad (when)

import Plush.Parser
import Plush.Pretty
import Plush.Run.Execute
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.ShellExec
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
runCommand :: (PosixLike m) => String -> ShellExec m (ExitCode, Maybe String)
runCommand cmds = do
    pcr <- parseInput cmds
    case pcr of
        Left errs -> errStrLn errs >> return (ExitFailure 125, Nothing)
        Right (cl, rest) -> shellExec cl >>= return . (\ec -> (ec, Just rest))

-- | Run all the commands, parsed from a string
runScript :: (PosixLike m) => String -> ShellExec m ExitCode
runScript cmds0 = rc (ExitSuccess, Just cmds0)
  where
    rc (_, Just cmds) | not (null cmds) = runCommand cmds >>= rc
    rc (ec, _) = setLastExitCode ec >> return ec

-- | Run all commands from a file.
runFile :: (PosixLike m) => FilePath -> ShellExec m ExitCode
runFile fp = readAllFile fp >>= runScript
    -- TODO(mzero): should this handle errors?
