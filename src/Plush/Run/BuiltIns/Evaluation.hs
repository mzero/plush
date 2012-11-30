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

module Plush.Run.BuiltIns.Evaluation (
    dot,
    eval
    )
where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Control.Monad.Exception (bracket_)
import Data.List (intercalate)
import System.FilePath

import Plush.Parser
import Plush.Run.BuiltIns.Utilities
import Plush.Run.Execute
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.ShellExec
import Plush.Run.Types


eval :: (PosixLike m) => SpecialUtility m
eval = SpecialUtility . const $ Utility evalExec emptyAnnotate
  where
    evalExec args = let cmdline = dropWhile (== ' ') $ intercalate " " args in
        if null cmdline
            then success
            else run cmdline

    run cmdline = do
        aliases <- getAliases
        case parseCommand aliases cmdline of
            Left errs -> exitMsg 127 errs
            Right (cl, _rest) -> shellExec cl >> getLastExitCode


dot :: (PosixLike m) => SpecialUtility m
dot = SpecialUtility . const $ Utility dotExec emptyAnnotate
  where
    dotExec [] = exitMsg 1 "dot missing name of script"
    dotExec (script:args) = do
        scriptPaths <- if '/' `elem` script
            then return [script]
            else map (</> script) . splitSearchPath <$> getVarDefault "PATH" ""
        runFirstFound args scriptPaths

    runFirstFound args (fp:fps) = do
        b <- doesFileExist fp -- TODO: check if it can be read
            -- NOTE: dot doesn't require that the script be exectuable
        if b
            then do
                setLastExitCode ExitSuccess
                script <- readAllFile fp
                oldArgs <- getArgs
                bracket_
                    (unless (null args) $ setArgs args)
                    (unless (null args) $ setArgs oldArgs)
                    (runScript script)
            else
                runFirstFound args fps

    runFirstFound _ [] = exitMsg 127 "script file not found"
        -- TODO: should be a shell error, exiting non-interactive shells

    runScript "" = getLastExitCode;
    runScript cmds = do
        aliases <- getAliases
        case parseCommand aliases cmds of
            Left errs -> exitMsg 127 errs
            Right (cl, rest) -> shellExec cl >> runScript rest


