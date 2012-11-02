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

{-# Language TupleSections #-}

module Plush.Run.Command (
    FoundCommand(..),
    commandSearch,
    ) where

import Control.Applicative ((<$>))
import Control.Monad
import System.FilePath

import {-# SOURCE #-} Plush.Run.BuiltIns -- see BuiltIns.hs-boot
import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Run.Types
import Plush.Types

-- | Use the command name to find a command. Returns information about where
-- the command was found, and a @'ShellUtility' m@ to run it. Implements the
-- search algorithm in §2.9.1.
commandSearch :: (PosixLike m) => String
    -> (FunctionDefinition -> Bindings -> Args -> ShellExec m ExitCode)
    -> ShellExec m (FoundCommand,
                    Bindings -> Args -> ShellExec m ExitCode,
                    Args -> ShellExec m [[Annotation]])
commandSearch cmd execFun
    | '/' `elem` cmd = external cmd
    | otherwise  =
        -- §2.9.1 Command Search and Execution: 1 a: special built-ins
        search special setShellVars SpecialCommand $

        -- §2.9.1 Command Search and Execution: 1 b: functions
        searchFun $

        -- §2.9.1 Command Search and Execution: 1 c: utilities
        search direct withEnvVars DirectCommand $

        -- §2.9.1 Command Search and Execution: 1 d: PATH
        findOnPath
  where
    findOnPath = do
        getVarDefault "PATH" "" >>= go . map (</> cmd) . splitSearchPath
      where
        go (fp:fps) = do
            b <- doesFileExist fp -- TODO: check if it can be executed
            if b
                then -- §2.9.1 Command Search and Execution: 1 d i a: regular built-ins
                     search builtin withEnvVars (BuiltInCommand fp) $
                     -- §2.9.1 Command Search and Execution: 1 d i b: exec from fs
                     external fp
                else go fps
        go [] = search builtin withEnvVars (BuiltInCommand "/???") $
                -- §2.9.1 Command Search and Execution: 1 d ii: unsuccessful
                unknown
            -- TODO: technically shouldn't run the builtins if not found in
            -- during path search. But for now, since the test environemnt
            -- doesn't have them, this will fail to run anything!

    search lkup processBindings fc alt =
        maybe alt (\util -> return (fc, (processBindings $ utilExecute util),
                                    utilAnnotate util))
        $ lkup cmd

    searchFun alt = do
        maybeFun <- getFun cmd
        maybe alt (\f -> return (FunCallCommand, execFun f, emptyAnnotate)) maybeFun

    external fp = return (ExecutableCommand fp, externalExec fp, emptyAnnotate)
    externalExec fp = withEnvVars $ \args -> do
        env <- getEnv
        execProcess env fp args

    unknown = return (UnknownCommand, unknownExec, emptyAnnotate)
    unknownExec _ _ = exitMsg 127 ("Unknown command: " ++ cmd)

    -- TODO: unsure if emptyAnnotate is correct here, or perhaps defaultAnnotate

setShellVars :: (PosixLike m) =>
                (Args -> ShellExec m ExitCode) ->
                Bindings -> Args -> ShellExec m ExitCode
setShellVars exec bindings args = untilFailureM setShellVar bindings `andThenM` exec args
  where
    setShellVar (name, val) = setVarEntry name (VarShellOnly, VarReadWrite, Just val)

withEnvVars :: (PosixLike m) =>
               (Args -> ShellExec m ExitCode) ->
               Bindings -> Args -> ShellExec m ExitCode
withEnvVars exec bindings args = do
    prev <- currentVarEntries bindings
    finally (setEnvVars bindings `andThenM` exec args)
        (forM_ prev restoreVarEntries)

currentVarEntries :: (PosixLike m) => Bindings -> ShellExec m [(String, Maybe VarEntry)]
currentVarEntries as = mapM (\(name, _) -> (name,) <$> getVarEntry name) as

setEnvVars :: (PosixLike m) => Bindings -> ShellExec m ExitCode
setEnvVars bindings = untilFailureM setEnvVar bindings
  where
    setEnvVar (name, val) = setVarEntry name (VarExported, VarReadWrite, Just val)

restoreVarEntries (name, Just val) = void $ setVarEntry name val
restoreVarEntries (name, Nothing) = void $ unsetVarEntry name
